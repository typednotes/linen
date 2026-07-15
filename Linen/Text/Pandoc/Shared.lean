/-
  `Linen.Text.Pandoc.Shared` — shared reader/writer helpers.

  ## Haskell source

  Ported from `Text.Pandoc.Shared` in the `pandoc` package
  (v3.10, `src/Text/Pandoc/Shared.hs`).

  This is pandoc's grab-bag of utility functions used across readers and
  writers.  The pure, widely-reused helpers are ported here: list/text
  processing (`splitBy`, `splitTextBy`, `splitTextByIndices`, `trim`,
  `stripTrailingNewlines`, `toRomanNumeral`, `tabFilter`, `tshow`), the
  AST helpers (`stringify`, `removeFormatting`, `capitalize`, `compactify`,
  `orderedListMarkers`, `extractSpaces`, `blocksToInlines`, task-list
  conversion, `uniqueIdent`/`textToIdentifier`, `isTightList`,
  `onlySimpleTableCells`, `combineAttr`, `addMetaField`, `figureDiv`) and the
  path helpers (`collapseFilePath`).

  ### Deviations from upstream

  * `Text` → `String`; `Set`/`Map` → `List`/`Linen.Data.Map`.
  * `renderTags'` renders `tagsoup` `Tag` values to text.  `tagsoup` is not
    ported (it is flagged as a reader-side prerequisite in
    `docs/imports/pandoc/dependencies.md`, deferred out of this tier), and no
    `Tag` type exists in `linen` yet, so `renderTags'` is **omitted**; it is
    only used by the (deferred) HTML reader/writer path.
  * `normalizeDate` (which parses ~30 human date formats via `time`'s
    `parseTimeM`) is scoped to the ISO `YYYY-MM-DD` / `YYYY/MM/DD` passthrough
    forms; the full multi-format parser is deferred with the readers that use
    it.
  * `makeSections`/`makeSectionsWithOffsets` (header→`Div` section nesting) is
    used only by the writer tier (deferred); it is left for that tier rather
    than ported speculatively here.
  * `inDirectory`/`filteredFilesFromArchive` (IO / `zip-archive`) are out of
    scope (deferred subtrees).
-/

import Linen.Text.Pandoc.Definition
import Linen.Text.Pandoc.Builder
import Linen.Text.Pandoc.Walk
import Linen.Text.Pandoc.Extensions
import Linen.Text.Pandoc.Asciify
import Linen.Text.DocLayout

namespace Linen.Text.Pandoc
namespace Shared

open Linen.Text.Pandoc (Inline Block Attr nullAttr)

-- ── List processing ───────────────────────────────────────────────────

/-- Split a list into maximal runs of non-separator elements, dropping runs of
    separators (a leading separator yields a leading empty group, matching
    upstream `splitBy`).  Implemented as a structural `foldr`. -/
def splitBy (p : α → Bool) (xs : List α) : List (List α) :=
  let step (x : α) (acc : List (List α)) : List (List α) :=
    if p x then
      match acc with
      | [] :: _ => acc            -- merge a run of separators
      | _ => [] :: acc            -- start a new group
    else
      match acc with
      | g :: gs => (x :: g) :: gs
      | [] => [[x]]
  xs.foldr step []

/-- `splitBy` specialised to strings/`Char`s. -/
def splitTextBy (p : Char → Bool) (s : String) : List String :=
  (splitBy p s.toList).map String.ofList

/-- Take characters up to display width `w`, returning the taken chars, the
    remainder, and the width consumed. -/
private def takeWidth (w : Int) : List Char → (List Char × List Char × Int)
  | [] => ([], [], 0)
  | c :: cs =>
      if w <= 0 then ([], c :: cs, 0)
      else
        let cw := Text.DocLayout.charWidth c
        let (taken, rest, consumed) := takeWidth (w - cw) cs
        (c :: taken, rest, consumed + cw)

/-- Split a string at the given (display-width) index positions, respecting
    `charWidth`. -/
def splitTextByIndices (indices : List Int) (s : String) : List String :=
  let rec go (offset : Int) (cs : List Char) : List Int → List (List Char)
    | [] => [cs]
    | i :: is =>
        let (chunk, rest, consumed) := takeWidth (i - offset) cs
        chunk :: go (offset + consumed) rest is
  (go 0 s.toList indices).map String.ofList

-- ── Text processing ───────────────────────────────────────────────────

/-- `show` producing a `String`. -/
def tshow {α : Type} [ToString α] (x : α) : String := toString x

/-- Wrap a string in ASCII double quotes. -/
def inquotes (s : String) : String := "\"" ++ s ++ "\""

/-- Remove trailing `\n` characters. -/
def stripTrailingNewlines (s : String) : String :=
  String.ofList ((s.toList.reverse.dropWhile (· == '\n')).reverse)

/-- Is `c` ASCII whitespace? -/
def isWS (c : Char) : Bool := c == ' ' || c == '\t' || c == '\n' || c == '\r'

/-- Remove leading ASCII whitespace. -/
def triml (s : String) : String := String.ofList (s.toList.dropWhile isWS)

/-- Remove trailing ASCII whitespace. -/
def trimr (s : String) : String := String.ofList ((s.toList.reverse.dropWhile isWS).reverse)

/-- Remove leading and trailing ASCII whitespace. -/
def trim (s : String) : String := triml (trimr s)

/-- Trim whitespace.  (Upstream additionally preserves a single space before a
    trailing backslash in math; that nuance is simplified to a plain trim.) -/
def trimMath (s : String) : String := trim s

/-- Remove the first and last characters. -/
def stripFirstAndLast (s : String) : String :=
  String.ofList (((s.toList.drop 1).reverse.drop 1).reverse)

/-- Convert camelCase to hyphen-separated lowercase (list worker). -/
private def camelCaseGo : List Char → List Char
  | [] => []
  | [a] => [a.toLower]
  | a :: b :: rest =>
      if !a.isUpper && b.isUpper then a.toLower :: '-' :: camelCaseGo (b :: rest)
      else a.toLower :: camelCaseGo (b :: rest)
  termination_by l => l.length

/-- Convert camelCase to hyphen-separated lowercase. -/
def camelCaseToHyphenated (s : String) : String :=
  String.ofList (camelCaseGo s.toList)

/-- The Roman-numeral rendering of a `Nat`, peeling one symbol at a time. -/
private def romanNat (n : Nat) : String :=
  if h : n ≥ 1000 then "M" ++ romanNat (n - 1000)
  else if h : n ≥ 900 then "CM" ++ romanNat (n - 900)
  else if h : n ≥ 500 then "D" ++ romanNat (n - 500)
  else if h : n ≥ 400 then "CD" ++ romanNat (n - 400)
  else if h : n ≥ 100 then "C" ++ romanNat (n - 100)
  else if h : n ≥ 90 then "XC" ++ romanNat (n - 90)
  else if h : n ≥ 50 then "L" ++ romanNat (n - 50)
  else if h : n ≥ 40 then "XL" ++ romanNat (n - 40)
  else if h : n ≥ 10 then "X" ++ romanNat (n - 10)
  else if h : n ≥ 9 then "IX" ++ romanNat (n - 9)
  else if h : n ≥ 5 then "V" ++ romanNat (n - 5)
  else if h : n ≥ 4 then "IV" ++ romanNat (n - 4)
  else if h : n ≥ 1 then "I" ++ romanNat (n - 1)
  else ""
  termination_by n
  decreasing_by all_goals omega

/-- Convert an integer (`0 < n < 4000`) to an uppercase Roman numeral; values
    `≥ 4000` or `< 0` yield `"?"`, and `0` yields `""` (matching upstream). -/
def toRomanNumeral (n : Int) : String :=
  if n ≥ 4000 || n < 0 then "?"
  else romanNat n.toNat

/-- Expand tabs to spaces given a tab-stop width (0 leaves tabs intact),
    resetting the column count at each newline. -/
def tabFilter (tabStop : Int) (s : String) : String :=
  if tabStop <= 0 then s
  else
    let rec go (col : Int) : List Char → List Char
      | [] => []
      | c :: cs =>
          if c == '\n' then '\n' :: go 0 cs
          else if c == '\t' then
            let n := tabStop - (col % tabStop)
            (List.replicate n.toNat ' ') ++ go (col + n) cs
          else c :: go (col + 1) cs
    String.ofList (go 0 s.toList)

-- ── Safe read ─────────────────────────────────────────────────────────

/-- Safely parse an integer, returning `none` on failure. -/
def safeRead (t : String) : Option Int := (trim t).toInt?

/-- `String`-based `safeRead`. -/
def safeStrRead (s : String) : Option Int := safeRead s

-- ── Dates ─────────────────────────────────────────────────────────────

/-- Two-digit zero-pad. -/
private def pad2 (s : String) : String := if s.length == 1 then "0" ++ s else s

/-- Normalise an ISO-ish date to `YYYY-MM-DD` (scoped to the ISO passthrough
    forms; see the module deviation note). -/
def normalizeDate (s : String) : Option String :=
  let t := trim s
  let parts :=
    if t.any (· == '-') then t.splitOn "-"
    else if t.any (· == '/') then t.splitOn "/"
    else [t]
  match parts with
  | [y, m, d] =>
      if y.length == 4 && (safeRead y).isSome && (safeRead m).isSome && (safeRead d).isSome
      then some s!"{y}-{pad2 m}-{pad2 d}"
      else none
  | [y] => if y.length == 4 && (safeRead y).isSome then some y else none
  | _ => none

-- ── Ordered-list markers ──────────────────────────────────────────────

/-- The bijective base-26 digits of `n` (1 → `A`, 26 → `Z`, 27 → `AA`, …),
    using `base` as the code point of the first letter. -/
private def alphaDigits (base : Nat) (m : Nat) : List Char :=
  match m with
  | 0 => []
  | n + 1 =>
      alphaDigits base (n / 26) ++ [Char.ofNat (base + n % 26)]
  termination_by m
  decreasing_by omega

/-- The alphabetic list marker for a (1-based) index. -/
private def alphaMarker (upper : Bool) (i : Int) : String :=
  String.ofList (alphaDigits (if upper then 'A'.toNat else 'a'.toNat) i.toNat)

/-- Produce the first `count` ordered-list markers for the given start,
    numbering style, and delimiter (upstream returns an infinite list; here it
    is bounded by `count`, which is always the number of list items). -/
def orderedListMarkersN (count : Nat) (attrs : ListAttributes) : List String :=
  let (start, style, delim) := attrs
  let nums : List Int := (List.range count).map (fun i => start + Int.ofNat i)
  let render (n : Int) : String :=
    match style with
    | .DefaultStyle | .Decimal => toString n
    | .Example => toString n
    | .UpperAlpha => alphaMarker true n
    | .LowerAlpha => alphaMarker false n
    | .UpperRoman => toRomanNumeral n
    | .LowerRoman => (toRomanNumeral n).toLower
  nums.map fun n =>
    let inside := render n
    match delim with
    | .DefaultDelim | .Period => inside ++ "."
    | .OneParen => inside ++ ")"
    | .TwoParens => "(" ++ inside ++ ")"

-- ── Inline/Block AST helpers ──────────────────────────────────────────

/-- Replace a `Note` inline with an empty `Str` (used before `stringify`). -/
def deNote : Inline → Inline
  | .Note _ => .Str ""
  | x => x

/-- The text contribution of a single inline for `stringify`. -/
private def stringifyInline : Inline → String
  | .Str t => t
  | .Code _ t => t
  | .Math _ t => t
  | .Space => " "
  | .SoftBreak => " "
  | .LineBreak => " "
  | .RawInline _ _ => ""
  | .Note _ => ""
  | _ => ""

/-- Convert an AST fragment to plain text, dropping formatting and footnotes. -/
def stringify {b : Type} [Walkable Inline b] (x : b) : String :=
  Walkable.query (c := String) stringifyInline (walk deNote x)

/-- Strip inline formatting, keeping only the text-like leaves. -/
def removeFormatting {b : Type} [Walkable Inline b] (x : b) : List Inline :=
  let go : Inline → List Inline
    | i@(.Str _) => [i]
    | i@(.Space) => [i]
    | i@(.SoftBreak) => [i]
    | i@(.LineBreak) => [i]
    | i@(.Code _ _) => [i]
    | i@(.Math _ _) => [i]
    | _ => []
  Walkable.query (c := List Inline) go (walk deNote x)

/-- Uppercase all `Str` text in an AST fragment. -/
def capitalize {b : Type} [Walkable Inline b] (x : b) : b :=
  walk (b := b) (fun (i : Inline) => match i with | .Str t => .Str t.toUpper | y => y) x

/-- Pull leading/trailing `Space`/`SoftBreak` out of inline content before
    applying `f`. -/
def extractSpaces (f : Inlines → Inlines) (ils : Inlines) : Inlines :=
  let contents := ils.toList
  let isSp : Inline → Bool := fun x => x matches .Space | .SoftBreak
  let (lead, rest) := (contents.takeWhile isSp, contents.dropWhile isSp)
  let (trail, mid) := (rest.reverse.takeWhile isSp, (rest.reverse.dropWhile isSp).reverse)
  ⟨lead⟩ ++ f ⟨mid⟩ ++ ⟨trail.reverse⟩

-- ── List compaction ───────────────────────────────────────────────────

/-- Is a list of item-blocks "tight" (each item starts with `Plain`, or is a
    nested tight list)? -/
def isTightList (items : List (List Block)) : Bool :=
  items.all fun item =>
    match item with
    | [] => true
    | (.Plain _) :: _ => true
    | _ => false

/-- If no list item contains a `Para` beyond a possible final one, convert the
    final `Para` of each item to `Plain` (tight-list rendering). -/
def compactify (items : List Blocks) : List Blocks :=
  let lists := items.map (·.toList)
  -- Find the number of Para blocks that are the *last* block of some item.
  let hasNonFinalPara : Bool := lists.any fun bs =>
    match bs.reverse with
    | (.Para _) :: rest => rest.any (· matches .Para _)
    | _ => bs.any (· matches .Para _)
  if hasNonFinalPara then items
  else
    items.map fun blocks =>
      match blocks.toList.reverse with
      | (.Para ils) :: revInit => ⟨revInit.reverse ++ [.Plain ils]⟩
      | _ => blocks

/-- `compactify` for definition lists. -/
def compactifyDL (items : List (Inlines × List Blocks)) : List (Inlines × List Blocks) :=
  items.map fun (term, defs) => (term, compactify defs)

/-- Join inline lines with hard line breaks into one `Para`. -/
def linesToPara (lns : List (List Inline)) : Block :=
  .Para ((lns.intersperse [.LineBreak]).flatten)

/-- Is a block a `Header`? -/
def isHeaderBlock : Block → Bool
  | .Header _ _ _ => true
  | _ => false

/-- Wrap a figure's caption and body in a `Div` with class `"figure"`. -/
def figureDiv (attr : Attr) (caption : Caption) (body : List Block) : Block :=
  let (ident, classes, kvs) := attr
  .Div (ident, "figure" :: classes, kvs)
    (body ++ (match caption with | .Caption _ capt => capt))

-- ── Attributes / metadata ─────────────────────────────────────────────

/-- Merge two attributes: union the classes, prefer the first for id and
    conflicting key-value pairs. -/
def combineAttr (a b : Attr) : Attr :=
  let (id₁, cls₁, kv₁) := a
  let (id₂, cls₂, kv₂) := b
  let id' := if id₁ == "" then id₂ else id₁
  let cls' := cls₁ ++ cls₂.filter (fun c => !cls₁.contains c)
  let kv' := kv₁ ++ kv₂.filter (fun p => !(kv₁.map (·.1)).contains p.1)
  (id', cls', kv')

/-- Set or merge a metadata field, combining into a list on collision. -/
def addMetaField {α : Type} [ToMetaValue α] (key : String) (val : α) (mta : Meta) : Meta :=
  let mv := ToMetaValue.toMetaValue val
  match lookupMeta key mta with
  | some (.MetaList xs) => ⟨mta.unMeta.insert' key (.MetaList (xs ++ [mv]))⟩
  | some x => ⟨mta.unMeta.insert' key (.MetaList [x, mv])⟩
  | none => ⟨mta.unMeta.insert' key mv⟩

/-- HTML tags rendered as class-tagged `Span`s (`kbd`, `mark`, `dfn`, `abbr`). -/
def htmlSpanLikeElements : List String := ["kbd", "mark", "dfn", "abbr"]

-- ── Identifiers ───────────────────────────────────────────────────────

/-- Convert text to a slug-style identifier, per the enabled extensions. -/
def textToIdentifier (exts : Extensions) (t : String) : String :=
  let ascii := if extensionEnabled .Ext_ascii_identifiers exts
               then Asciify.toAsciiText t else t
  let ok (c : Char) : Bool := c.isAlphanum || c == '_' || c == '-' || c == '.' || isWS c
  let filtered := ascii.toLower.toList.filter ok
  -- collapse whitespace runs into single '-'
  let words := (splitBy isWS filtered).filter (· != [])
  let joined := String.intercalate "-" (words.map String.ofList)
  -- drop leading non-letters unless gfm identifiers are enabled
  if extensionEnabled .Ext_gfm_auto_identifiers exts then joined
  else String.ofList (joined.toList.dropWhile (fun c => !c.isAlpha))

/-- Convert inlines to a slug-style identifier. -/
def inlineListToIdentifier (exts : Extensions) (ils : List Inline) : String :=
  textToIdentifier exts (stringify ils)

/-- Generate a unique identifier from inlines, appending `-1`, `-2`, … to
    avoid clashes with the `used` set.  Among `base` and `base-1 … base-(n+1)`
    (with `n = used.length`) at least one is free, so this is a finite search. -/
def uniqueIdent (exts : Extensions) (ils : List Inline) (used : List String) : String :=
  let base0 := inlineListToIdentifier exts ils
  let base := if base0 == "" then "section" else base0
  let candidates := base :: (List.range (used.length + 1)).map (fun i => s!"{base}-{i + 1}")
  (candidates.find? (fun c => !used.contains c)).getD base

-- ── Task lists ────────────────────────────────────────────────────────

/-- Apply `f` to the first `Plain`/`Para` of the first list item, gated by
    `Ext_task_lists`. -/
def handleTaskListItem (f : List Inline → List Inline) (exts : Extensions)
    (item : List Block) : List Block :=
  if !extensionEnabled .Ext_task_lists exts then item
  else
    match item with
    | (.Plain ils) :: rest => .Plain (f ils) :: rest
    | (.Para ils) :: rest => .Para (f ils) :: rest
    | other => other

/-- Convert ASCII checkboxes (`[ ]`, `[x]`) at the start of a list item into
    Unicode ballot glyphs. -/
def taskListItemFromAscii (exts : Extensions) : List Block → List Block :=
  handleTaskListItem (fun ils =>
    match ils with
    | .Str "[" :: .Space :: .Str "]" :: .Space :: rest => .Str "☐" :: .Space :: rest
    | .Str "[x]" :: .Space :: rest => .Str "☒" :: .Space :: rest
    | .Str "[X]" :: .Space :: rest => .Str "☒" :: .Space :: rest
    | other => other) exts

/-- The inverse of `taskListItemFromAscii`. -/
def taskListItemToAscii (exts : Extensions) : List Block → List Block :=
  handleTaskListItem (fun ils =>
    match ils with
    | .Str "☐" :: .Space :: rest => .Str "[" :: .Space :: .Str "]" :: .Space :: rest
    | .Str "☒" :: .Space :: rest => .Str "[x]" :: .Space :: rest
    | other => other) exts

-- ── Squashing blocks into inlines ─────────────────────────────────────

/-- The default separator when flattening blocks to inlines. -/
def defaultBlocksSeparator : Inlines := linebreak

mutual

/-- Flatten a single block to inlines. -/
def blockToInlines (sep : Inlines) : Block → Inlines
  | .Plain ils => ⟨ils⟩
  | .Para ils => ⟨ils⟩
  | .LineBlock lns => ⟨(lns.intersperse [.LineBreak]).flatten⟩
  | .CodeBlock attr code => Many.singleton (.Code attr code)
  | .RawBlock fmt code => Many.singleton (.RawInline fmt code)
  | .BlockQuote blks => blocksToInlinesWithSep sep blks
  | .Header _ _ ils => ⟨ils⟩
  | .Div _ blks => blocksToInlinesWithSep sep blks
  | _ => str ""

/-- Flatten a block list to inlines, separating blocks with `sep`. -/
def blocksToInlinesWithSep (sep : Inlines) : List Block → Inlines
  | [] => ⟨[]⟩
  | [b] => blockToInlines sep b
  | b :: bs => blockToInlines sep b ++ sep ++ blocksToInlinesWithSep sep bs

end

/-- Flatten a block list to inlines with the default separator. -/
def blocksToInlines' (bs : List Block) : Inlines :=
  blocksToInlinesWithSep defaultBlocksSeparator bs

/-- Flatten a block list to a plain `List Inline`. -/
def blocksToInlines (bs : List Block) : List Inline :=
  (blocksToInlines' bs).toList

-- ── File path helpers ─────────────────────────────────────────────────

/-- Collapse `.`/`..` segments in a `/`-separated path, keeping any leading
    `..` at the root. -/
def collapseFilePath (fp : String) : String :=
  let parts := (fp.splitOn "/")
  let rec go (acc : List String) : List String → List String
    | [] => acc.reverse
    | "." :: rest => go acc rest
    | "" :: rest => go acc rest
    | ".." :: rest =>
        match acc with
        | [] => go [".."] rest
        | ".." :: _ => go (".." :: acc) rest
        | _ :: accRest => go accRest rest
    | p :: rest => go (p :: acc) rest
  let collapsed := go [] parts
  let leadingSlash := fp.startsWith "/"
  let joined := String.intercalate "/" collapsed
  if joined == "" then (if leadingSlash then "/" else ".")
  else (if leadingSlash then "/" ++ joined else joined)

end Shared
end Linen.Text.Pandoc
