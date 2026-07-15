/-
  `Linen.Text.DocLayout` — a Wadler/Leijen-style document layout engine.

  ## Haskell source

  Ported from `Text.DocLayout` in the `doclayout` package (v0.5.0.3,
  `src/Text/DocLayout.hs`).

  Provides the `Doc a` document algebra (`Text`/`BreakingSpace`/`Prefixed`/
  `Block`/`Concat`/`Styled`/`Linked`/… constructors) with its
  `Append`/`Inhabited` (monoid) and `IsString` structure, the smart
  constructors and layout combinators (`literal`, `text`, `char`, `<+>`, `$$`,
  `$+$`, `vcat`, `hsep`, `nest`, `hang`, `flush`, `prefixed`, `lblock`/
  `rblock`/`cblock`, `vfill`, `chomp`, `nestle`, `nowrap`, …), the ANSI styling
  helpers (`bold`, `italic`, `underlined`, `strikeout`, `fg`, `bg`, `link`),
  the `charWidth`/`realLength` display-width machinery, and the
  `render`/`renderPlain`/`renderANSI` renderers over a `State`-threaded layout
  engine.

  ### Deviations from upstream

  * Haskell's strict `Text` (used in `Prefixed`/`AfterBreak`/`Linked` and the
    render state) becomes Lean's `String`; the display-width builder output
    (`build`) becomes a `String`.  Widths, columns and offsets are `Int`
    throughout (matching upstream's `Int`), so `Int.toNat` guards the
    `replicate`-style calls (negative counts give the empty run, as Haskell's
    `replicate` does).
  * `charWidth` is a wcwidth-style approximation: zero width for combining
    marks / joiners / variation selectors, width `2` for East-Asian wide and
    emoji ranges, width `1` otherwise.  Upstream's full grapheme-cluster
    `MatchState` machinery and the multi-thousand-entry `baseEmojis` table
    (from the `emojis` package) are collapsed to this range test — the accepted
    "large data table out of scope" simplification.  The
    `isSkinToneModifier`/`isEmojiVariation`/`isZWJ` predicates are kept.
  * `NonEmpty (FlatDoc a)` payloads become plain `List` (non-emptiness is a
    construction invariant of `flatten`).
  * The `Generic`/`Data`/`Typeable` derivations are dropped (no Lean analogue).
  * The render worker `renderList`, and the query/normalising combinators
    `getOffset`/`chomp`/`nestle`, recurse over freshly-computed lists /
    re-associated `Concat` spines.  They terminate, but the termination
    arguments are non-structural custom measures; following the codebase
    precedent for such loops (`Data.Conduit`, `Data.StreamK`, `Data.Stream`)
    they are written `unsafe` rather than carrying heavy well-founded proofs.
    Everything else (including `flatten` and the length-decreasing `normalize`)
    is total.
-/

import Linen.Text.DocLayout.ANSIFont
import Linen.Text.DocLayout.Attributed
import Linen.Text.DocLayout.HasChars
import Linen.Data.String

namespace Text.DocLayout

/- ── Display width ─────────────────────────────────────────────────── -/

private def inR (o lo hi : Nat) : Bool := lo ≤ o && o ≤ hi

/-- Is `c` an emoji skin-tone modifier (`U+1F3FB`–`U+1F3FF`)? -/
def isSkinToneModifier (c : Char) : Bool := inR c.toNat 0x1F3FB 0x1F3FF

/-- Is `c` an emoji variation selector (`U+FE0E`–`U+FE0F`)? -/
def isEmojiVariation (c : Char) : Bool := inR c.toNat 0xFE0E 0xFE0F

/-- Is `c` the zero-width joiner (`U+200D`)? -/
def isZWJ (c : Char) : Bool := c.toNat == 0x200D

/-- The display width of a character (a wcwidth-style approximation; see the
module deviations note). -/
def charWidth (c : Char) : Int :=
  let o := c.toNat
  if o == 0 then 0
  -- Zero-width: combining marks, joiners, variation selectors.
  else if inR o 0x0300 0x036F || inR o 0x0483 0x0489 || inR o 0x0591 0x05BD
        || inR o 0x0610 0x061A || inR o 0x064B 0x065F || inR o 0x0670 0x0670
        || inR o 0x06D6 0x06DC || inR o 0x0E31 0x0E3A || inR o 0x0EB1 0x0EBC
        || inR o 0x1AB0 0x1AFF || inR o 0x1DC0 0x1DFF || inR o 0x200B 0x200F
        || inR o 0x20D0 0x20FF || inR o 0xFE00 0xFE0F || inR o 0xFE20 0xFE2F then 0
  -- Wide (East-Asian W/F) and emoji: width 2.
  else if inR o 0x1100 0x115F || inR o 0x2E80 0x303E || inR o 0x3041 0x33FF
        || inR o 0x3400 0x4DBF || inR o 0x4E00 0x9FFF || inR o 0xA000 0xA4CF
        || inR o 0xAC00 0xD7A3 || inR o 0xF900 0xFAFF || inR o 0xFE30 0xFE4F
        || inR o 0xFF00 0xFF60 || inR o 0xFFE0 0xFFE6 || inR o 0x1F300 0x1FAFF
        || inR o 0x20000 0x3FFFD then 2
  else 1

/-- The rendered width of a string-like value: the sum of its `charWidth`s.
(Upstream's grapheme-context `realLengthNarrowContext`; here a plain sum.) -/
def realLength {b : Type} [IsString b] [Append b] [Inhabited b] [HasChars b]
    (x : b) : Int :=
  HasChars.foldrChar (fun c acc => charWidth c + acc) 0 x

/- ── The `Doc` document algebra ────────────────────────────────────── -/

/-- A layout document over payload type `a`. -/
inductive Doc (a : Type) where
  /-- Text with its specified display width. -/
  | Text (width : Int) (s : a)
  /-- A block with a width and a list of attributed lines. -/
  | Block (width : Int) (lines : List (Attributed a))
  /-- A vertically expandable fill: each line contains the given text. -/
  | VFill (width : Int) (s : a)
  /-- Already-rendered attributed text (needs no further cooking). -/
  | CookedText (width : Int) (s : Attributed a)
  /-- Document with every line prefixed with the given string. -/
  | Prefixed (pref : String) (doc : Doc a)
  /-- Rendered only before non-blank content. -/
  | BeforeNonBlank (doc : Doc a)
  /-- Laid out flush to the left margin (no prefix). -/
  | Flush (doc : Doc a)
  /-- A space or line break, resolved in context. -/
  | BreakingSpace
  /-- Text printed only at the start of a line. -/
  | AfterBreak (t : String)
  /-- Newline unless already at the start of a line. -/
  | CarriageReturn
  /-- An unconditional newline. -/
  | NewLine
  /-- Ensure a number of blank lines. -/
  | BlankLines (n : Int)
  /-- Two documents concatenated. -/
  | Concat (x : Doc a) (y : Doc a)
  /-- A styled document. -/
  | Styled (style : StyleReq) (doc : Doc a)
  /-- A hyperlinked document. -/
  | Linked (target : String) (doc : Doc a)
  /-- The empty document. -/
  | Empty
deriving Repr, Inhabited

/-- Is this the empty document? -/
def isEmpty : Doc a → Bool
  | .Empty => true
  | _ => false

/-- Monoid append: `Empty` is the identity, otherwise `Concat`. -/
instance : Append (Doc a) where
  append
    | a, .Empty => a
    | .Empty, b => b
    | a, b => .Concat a b

/-- `Empty` is the monoid identity. -/
instance : Inhabited (Doc a) where
  default := .Empty

/-- The flattened intermediate form consumed by the render worker. -/
inductive FlatDoc (a : Type) where
  | FText (width : Int) (s : a)
  | FBlock (width : Int) (lines : List (Attributed a))
  | FVFill (width : Int) (s : a)
  | FCookedText (width : Int) (s : Attributed a)
  | FPrefixed (pref : String) (ds : List (FlatDoc a))
  | FBeforeNonBlank (ds : List (FlatDoc a))
  | FFlush (ds : List (FlatDoc a))
  | FBreakingSpace
  | FAfterBreak (ds : List (FlatDoc a))
  | FCarriageReturn
  | FNewLine
  | FBlankLines (n : Int)
  | FStyleOpen (style : StyleReq)
  | FStyleClose
  | FLinkOpen (target : String)
  | FLinkClose
deriving Repr, Inhabited

/-- The mutable state threaded through the renderer.  `output` is accumulated
in reverse order. -/
structure RenderState (a : Type) where
  output : List (Attr a)
  pfx : String
  prefixA : a
  prefixTrimmedA : a
  prefixLen : Int
  usePrefix : Bool
  lineLength : Option Int
  column : Int
  newlines : Int
  fontStack : List Font
  linkTarget : Option String

/-- The document-render state monad. -/
abbrev DocState (a : Type) := StateM (RenderState a) Unit

variable {a : Type} [IsString a] [Append a] [Inhabited a] [HasChars a]

/- ── Smart constructors ────────────────────────────────────────────── -/

/-- `mconcat` for documents (right fold with the `Empty`-collapsing append). -/
def hcatList (ds : List (Doc a)) : Doc a := ds.foldr (· ++ ·) .Empty

/-- Turn a string-like value into a `Doc`, splitting on newlines. -/
def literal (x : a) : Doc a :=
  hcatList <| List.intersperse .NewLine <|
    (HasChars.splitLines x).map fun s =>
      if HasChars.isNull s then .Empty else .Text (realLength s) s

/-- A `Doc` from a `String`. -/
def text (s : String) : Doc a := literal (IsString.fromString s)

/-- A single-character `Doc`. -/
def char (c : Char) : Doc a := text (String.singleton c)

/-- The empty document. -/
def empty : Doc a := .Empty

/-- A newline unless already at line start. -/
def cr : Doc a := .CarriageReturn

/-- A breaking space. -/
def space : Doc a := .BreakingSpace

/-- Ensure one blank line. -/
def blankline : Doc a := .BlankLines 1

/-- Ensure `n` blank lines. -/
def blanklines (n : Int) : Doc a := .BlankLines n

instance : IsString (Doc a) where
  fromString s := text s

/- ── Concatenation combinators ─────────────────────────────────────── -/

/-- `x <+> y`: `x`, a breaking space, then `y` (dropping empties). -/
def besideSp (x y : Doc a) : Doc a :=
  if isEmpty x then y else if isEmpty y then x else x ++ space ++ y

/-- `x $$ y`: `x` above `y` (a `cr` between). -/
def aboveCR (x y : Doc a) : Doc a :=
  if isEmpty x then y else if isEmpty y then x else x ++ cr ++ y

/-- `x $+$ y`: `x` above `y` with a blank line between. -/
def aboveBlank (x y : Doc a) : Doc a :=
  if isEmpty x then y else if isEmpty y then x else x ++ blankline ++ y

@[inherit_doc] scoped infixr:65 " <+> " => besideSp
@[inherit_doc] scoped infixr:60 " $$ " => aboveCR
@[inherit_doc] scoped infixr:60 " $+$ " => aboveBlank

/-- Horizontal concatenation. -/
def hcat (ds : List (Doc a)) : Doc a := hcatList ds

/-- Concatenate with breaking spaces between. -/
def hsep (ds : List (Doc a)) : Doc a := ds.foldr besideSp .Empty

/-- Concatenate vertically (`cr` between). -/
def vcat (ds : List (Doc a)) : Doc a := ds.foldr aboveCR .Empty

/-- Concatenate vertically with blank lines between. -/
def vsep (ds : List (Doc a)) : Doc a := ds.foldr aboveBlank .Empty

/- ── Indentation / prefixing combinators ───────────────────────────── -/

/-- Prefix every line of `doc` with `pref`. -/
def prefixed (pref : String) (doc : Doc a) : Doc a :=
  if isEmpty doc then .Empty else .Prefixed pref doc

/-- Lay `doc` out flush left (ignoring the current prefix). -/
def flush (doc : Doc a) : Doc a :=
  if isEmpty doc then .Empty else .Flush doc

/-- Indent by `ind` spaces. -/
def nest (ind : Int) (doc : Doc a) : Doc a :=
  prefixed (String.ofList (List.replicate ind.toNat ' ')) doc

/-- `hang ind start doc`: `start` then `doc` indented by `ind`. -/
def hang (ind : Int) (start doc : Doc a) : Doc a := start ++ nest ind doc

/-- Render `doc` only when non-blank content follows. -/
def beforeNonBlank (doc : Doc a) : Doc a := .BeforeNonBlank doc

/-- Text emitted only at the start of a line. -/
def afterBreak (t : String) : Doc a := .AfterBreak t

/-- Unfold a document into a flat list of its atoms. -/
def unfoldD : Doc a → List (Doc a)
  | .Empty => []
  | .Concat (x@(.Concat _ _)) y => unfoldD x ++ unfoldD y
  | .Concat x y => x :: unfoldD y
  | x => [x]

/-- Turn every breaking space into a hard space, forbidding line breaks. -/
def nowrap (doc : Doc a) : Doc a :=
  hcatList <| (unfoldD doc).map fun
    | .BreakingSpace => .Text 1 (IsString.fromString " ")
    | x => x

/- ── Enclosing / styling combinators ───────────────────────────────── -/

/-- `inside start end contents` wraps `contents` between `start` and `end`. -/
def inside (start «end» contents : Doc a) : Doc a := start ++ contents ++ «end»

/-- Wrap in braces `{ … }`. -/
def braces (d : Doc a) : Doc a := inside (char '{') (char '}') d

/-- Wrap in brackets `[ … ]`. -/
def brackets (d : Doc a) : Doc a := inside (char '[') (char ']') d

/-- Wrap in parentheses `( … )`. -/
def parens (d : Doc a) : Doc a := inside (char '(') (char ')') d

/-- Wrap in single quotes. -/
def quotes (d : Doc a) : Doc a := inside (char '\'') (char '\'') d

/-- Wrap in double quotes. -/
def doubleQuotes (d : Doc a) : Doc a := inside (char '"') (char '"') d

/-- Apply a style request (dropping it on `Empty`). -/
def styled (s : StyleReq) : Doc a → Doc a
  | .Empty => .Empty
  | x => .Styled s x

/-- Boldface. -/
def bold (d : Doc a) : Doc a := styled (.RWeight .Bold) d

/-- Italic. -/
def italic (d : Doc a) : Doc a := styled (.RShape .Italic) d

/-- Underlined. -/
def underlined (d : Doc a) : Doc a := styled (.RUnderline .ULSingle) d

/-- Struck out. -/
def strikeout (d : Doc a) : Doc a := styled (.RStrikeout .Struck) d

/-- The eight ANSI colours (alias of `Color8`). -/
abbrev Color := Color8

/-- Set the foreground colour. -/
def fg (c : Color) (d : Doc a) : Doc a := styled (.RForeground (.FG c)) d

/-- Set the background colour. -/
def bg (c : Color) (d : Doc a) : Doc a := styled (.RBackground (.BG c)) d

def black : Color := .Black
def red : Color := .Red
def green : Color := .Green
def yellow : Color := .Yellow
def blue : Color := .Blue
def magenta : Color := .Magenta
def cyan : Color := .Cyan
def white : Color := .White

/-- Attach a hyperlink target. -/
def link (target : String) (d : Doc a) : Doc a := .Linked target d

/- ── Render helpers ────────────────────────────────────────────────── -/

/-- The top font of the style stack (or `baseFont`). -/
def peekFont (st : RenderState a) : Font :=
  match st.fontStack with
  | [] => baseFont
  | f :: _ => f

/-- Drop trailing whitespace from a string. -/
private def dropTrailingSpace (s : String) : String :=
  String.ofList (s.toList.reverse.dropWhile Char.isWhitespace).reverse

/-- Install a new prefix, caching its attributed / trimmed / width forms. -/
def setPrefix (p : String) (st : RenderState a) : RenderState a :=
  let pa : a := IsString.fromString p
  { st with
    pfx := p
    prefixA := pa
    prefixTrimmedA := IsString.fromString (dropTrailingSpace p)
    prefixLen := realLength pa }

/-- Is this flat atom a block? -/
def isBlock : FlatDoc a → Bool
  | .FBlock .. => true
  | .FVFill .. => true
  | _ => false

/-- Does this flat atom introduce a break? -/
def isBreakable : FlatDoc a → Bool
  | .FBreakingSpace => true
  | .FCarriageReturn => true
  | .FNewLine => true
  | .FBlankLines _ => true
  | _ => false

/-- Is this flat atom printable (not a style/link marker)? -/
def isPrintable : FlatDoc a → Bool
  | .FLinkOpen _ => false
  | .FLinkClose => false
  | .FStyleOpen _ => false
  | .FStyleClose => false
  | _ => true

/-- The horizontal offset contributed by a flat atom. -/
def offsetOf : FlatDoc a → Int
  | .FText o _ => o
  | .FBlock w _ => w
  | .FVFill w _ => w
  | .FCookedText w _ => w
  | .FBreakingSpace => 1
  | _ => 0

/-- Does a string-like value start blank (empty or leading whitespace)? -/
def startsBlank' {b : Type} [IsString b] [Append b] [Inhabited b] [HasChars b]
    (x : b) : Bool :=
  match HasChars.foldrChar (fun c _ => some c) none x with
  | none => true
  | some c => c.isWhitespace

/-- Does a flat atom start blank? -/
def startsBlank : FlatDoc a → Bool
  | .FText _ t => startsBlank' t
  | .FCookedText _ t => startsBlank' t
  | .FBlock n ls => n > 0 && ls.all startsBlank'
  | .FVFill n t => n > 0 && startsBlank' t
  | .FBeforeNonBlank (x :: _) => startsBlank x
  | .FPrefixed _ (x :: _) => startsBlank x
  | .FFlush (x :: _) => startsBlank x
  | .FBreakingSpace => true
  | .FAfterBreak (t :: _) => startsBlank t
  | .FCarriageReturn => true
  | .FNewLine => true
  | .FBlankLines _ => true
  | .FStyleOpen _ => true
  | .FLinkOpen _ => true
  | .FStyleClose => true
  | .FLinkClose => true
  | _ => true

/- ── `flatten` and `normalize` ─────────────────────────────────────── -/

/-- The flat form of a `literal`-built string (used by `flatten`'s
`AfterBreak` case, which upstream defines as `flatten . fromString`).  Inlined
here so `flatten` stays structurally recursive. -/
def literalFlat (x : a) : List (FlatDoc a) :=
  List.intercalate [.FNewLine] <|
    (HasChars.splitLines x).map fun s =>
      if HasChars.isNull s then ([] : List (FlatDoc a))
      else [.FText (realLength s) s]

/-- Flatten a `Doc` into a list of flat atoms. -/
def flatten : Doc a → List (FlatDoc a)
  | .Text n s => [.FText n s]
  | .Block n ls => [.FBlock n ls]
  | .VFill n s => [.FVFill n s]
  | .CookedText n s => [.FCookedText n s]
  | .Prefixed p d => let f := flatten d; if f.isEmpty then [] else [.FPrefixed p f]
  | .BeforeNonBlank d => let f := flatten d; if f.isEmpty then [] else [.FBeforeNonBlank f]
  | .Flush d => let f := flatten d; if f.isEmpty then [] else [.FFlush f]
  | .BreakingSpace => [.FBreakingSpace]
  | .CarriageReturn => [.FCarriageReturn]
  | .AfterBreak t =>
      let f := literalFlat (IsString.fromString t : a)
      if f.isEmpty then [] else [.FAfterBreak f]
  | .NewLine => [.FNewLine]
  | .BlankLines n => [.FBlankLines n]
  | .Empty => []
  | .Concat x y => flatten x ++ flatten y
  | .Linked l x => .FLinkOpen l :: flatten x ++ [.FLinkClose]
  | .Styled s x => .FStyleOpen s :: flatten x ++ [.FStyleClose]

/-- Collapse adjacent breaks/blank-lines to a canonical form.  Strictly
length-decreasing on every recursive call (the two singleton rewrites are
inlined), so it terminates. -/
def normalize : List (FlatDoc a) → List (FlatDoc a)
  | [] => []
  | [.FNewLine] => [.FCarriageReturn]
  | [.FBlankLines _] => [.FCarriageReturn]
  | [.FBreakingSpace] => []
  | .FBlankLines m :: .FBlankLines n :: xs => normalize (.FBlankLines (max m n) :: xs)
  | .FBlankLines num :: .FBreakingSpace :: xs => normalize (.FBlankLines num :: xs)
  | .FBlankLines m :: .FCarriageReturn :: xs => normalize (.FBlankLines m :: xs)
  | .FBlankLines m :: .FNewLine :: xs => normalize (.FBlankLines m :: xs)
  | .FNewLine :: .FBlankLines m :: xs => normalize (.FBlankLines m :: xs)
  | .FNewLine :: .FBreakingSpace :: xs => normalize (.FNewLine :: xs)
  | .FNewLine :: .FCarriageReturn :: xs => normalize (.FNewLine :: xs)
  | .FCarriageReturn :: .FCarriageReturn :: xs => normalize (.FCarriageReturn :: xs)
  | .FCarriageReturn :: .FBlankLines m :: xs => normalize (.FBlankLines m :: xs)
  | .FCarriageReturn :: .FBreakingSpace :: xs => normalize (.FCarriageReturn :: xs)
  | .FBreakingSpace :: .FCarriageReturn :: xs => normalize (.FCarriageReturn :: xs)
  | .FBreakingSpace :: .FNewLine :: xs => normalize (.FNewLine :: xs)
  | .FBreakingSpace :: .FBlankLines n :: xs => normalize (.FBlankLines n :: xs)
  | .FBreakingSpace :: .FBreakingSpace :: xs => normalize (.FBreakingSpace :: xs)
  | x :: xs => x :: normalize xs
  termination_by l => l.length
  decreasing_by all_goals (simp_wf <;> omega)

/- ── Cooking and block merging ─────────────────────────────────────── -/

/-- Turn a non-empty attributed run into a cooked flat atom. -/
def cook (x : Attributed a) : Option (FlatDoc a) :=
  if HasChars.isNull x then none else some (.FCookedText (realLength x) x)

/-- Merge two column-blocks (each `(width, lines)`) side by side, padding to a
common height `h`. -/
def mergeBlocks (h : Int)
    (b1 b2 : Int × List (Int × Attributed a)) : Int × List (Int × Attributed a) :=
  let (w1, lns1) := b1
  let (w2, lns2) := b2
  let hN := h.toNat
  let len1 := (lns1.take hN).length
  let len2 := (lns2.take hN).length
  let empty : Attributed a := default
  let lns1' := if len1 < hN then lns1 ++ List.replicate (hN - len1) (0, empty) else lns1.take hN
  let lns2' := if len2 < hN then lns2 ++ List.replicate (hN - len2) (0, empty) else lns2.take hN
  let pad (n len : Int) (s : Attributed a) : Attributed a :=
    s ++ HasChars.replicateChar (n - len).toNat ' '
  let merge : (Int × Attributed a) → (Int × Attributed a) → (Int × Attributed a) :=
    fun (len1a, l1) (len2a, l2) => (w1 + len2a, pad w1 len1a l1 ++ l2)
  (w1 + w2, List.zipWith merge lns1' lns2')

/-- Break a string-like value into lines no wider than `n` columns. -/
def chop (n : Int) (x : a) : List a :=
  let withLen := (HasChars.splitLines x).map fun l => (realLength l, l)
  let removeFinalEmpty (xs : List (Int × a)) : List (Int × a) :=
    match xs.getLast? with
    | some (0, _) => xs.dropLast
    | _ => xs
  let chopLine : Int × a → List a := fun (len, l) =>
    if len ≤ n then [l]
    else
      (HasChars.foldrChar (fun c ls =>
        let clen := charWidth c
        let cs : a := HasChars.replicateChar 1 c
        match ls with
        | (len', l') :: rest =>
            if len' + clen > n then (clen, cs) :: (len', l') :: rest
            else (len' + clen, cs ++ l') :: rest
        | [] => [(clen, cs)]) ([] : List (Int × a)) l).map (·.2)
  ((removeFinalEmpty withLen).map chopLine).flatten

/- ── The render worker ─────────────────────────────────────────────── -/

/-- Emit `k` newlines. -/
def doNewlines : Nat → StateM (RenderState a) Unit
  | 0 => pure ()
  | k + 1 => do
      let st ← get
      let nl : Attr a := ⟨none, baseFont, IsString.fromString "\n"⟩
      if st.column == 0 && st.usePrefix && !st.pfx.isEmpty then
        set { st with
              output := nl :: ⟨none, baseFont, st.prefixTrimmedA⟩ :: st.output
              column := 0, newlines := st.newlines + 1 }
      else
        set { st with output := nl :: st.output, column := 0, newlines := st.newlines + 1 }
      doNewlines k

/-- Emit a single newline. -/
def newline : StateM (RenderState a) Unit := doNewlines 1

/-- Emit `off`-wide text `s`, prepending the prefix at column 0. -/
def outp (off : Int) (s : a) : StateM (RenderState a) Unit := do
  let st ← get
  let pref : a := if st.usePrefix then st.prefixA else IsString.fromString ""
  let font := peekFont st
  if st.column == 0 && !(HasChars.isNull pref && font == baseFont) then
    set { st with
      output := ⟨st.linkTarget, font, s⟩ :: ⟨none, baseFont, pref⟩ :: st.output,
      column := st.prefixLen + off, newlines := 0 }
  else
    set { st with
      output := ⟨st.linkTarget, font, s⟩ :: st.output
      column := st.column + off, newlines := 0 }

/-- Consume the flat-atom list, threading the render state.  Terminates over
the nested-`FlatDoc` measure (see the module note); written `unsafe`. -/
unsafe def renderList : List (FlatDoc a) → StateM (RenderState a) Unit
  | [] => pure ()
  | .FText off s :: xs => do outp off s; renderList xs
  | .FCookedText off s :: xs => do
      let st ← get
      let pref : a := if st.usePrefix then st.prefixA else IsString.fromString ""
      let elems := s.chunks.reverse
      if st.column == 0 && !HasChars.isNull pref then
        set { st with
              output := elems ++ ((⟨none, baseFont, pref⟩ : Attr a) :: st.output)
              column := st.prefixLen + off, newlines := 0 }
      else
        set { st with output := elems ++ st.output, column := st.column + off, newlines := 0 }
      renderList xs
  | .FStyleOpen style :: xs => do
      let st ← get
      modify fun s => { s with fontStack := (peekFont st ~> style) :: s.fontStack }
      renderList xs
  | .FStyleClose :: xs => do
      modify fun s => { s with fontStack := s.fontStack.drop 1 }
      renderList xs
  | .FLinkOpen target :: xs => do
      let st ← get
      match st.linkTarget with
      | none => do
          modify fun s => { s with linkTarget := some target }
          renderList xs
      | _ =>
          let (next, rest) := xs.span (fun d => match d with | .FLinkClose => false | _ => true)
          renderList (next ++ rest.drop 1)
  | .FLinkClose :: xs => do
      modify fun s => { s with linkTarget := none }
      renderList xs
  | .FPrefixed pref d :: xs => do
      let st ← get
      let oldPref := st.pfx
      let oldPrefixA := st.prefixA
      let oldPrefixTrimmedA := st.prefixTrimmedA
      let oldPrefixLen := st.prefixLen
      set (setPrefix (st.pfx ++ pref) st)
      renderList (normalize d)
      modify fun s => { s with
        pfx := oldPref, prefixA := oldPrefixA
        prefixTrimmedA := oldPrefixTrimmedA, prefixLen := oldPrefixLen }
      renderList xs
  | .FFlush d :: xs => do
      let st ← get
      let oldUsePrefix := st.usePrefix
      set { st with usePrefix := false }
      renderList (normalize d)
      modify fun s => { s with usePrefix := oldUsePrefix }
      renderList xs
  | .FBeforeNonBlank d :: xs => do
      match xs.dropWhile (fun x => !isPrintable x) with
      | x :: _ =>
          if startsBlank x then renderList xs
          else do renderList (normalize d); renderList xs
      | [] => renderList xs
  | .FBlankLines num :: xs => do
      let st ← get
      if st.newlines > num then pure ()
      else doNewlines (1 + num - st.newlines).toNat
      renderList xs
  | .FCarriageReturn :: xs => do
      let st ← get
      if st.newlines > 0 then renderList xs
      else do newline; renderList xs
  | .FNewLine :: xs => do newline; renderList xs
  | .FBreakingSpace :: xs => do
      let xs' := xs.dropWhile (fun d => match d with | .FBreakingSpace => true | _ => false)
      let next := xs'.takeWhile (fun d => !isBreakable d)
      let st ← get
      let off := next.foldl (fun tot t => tot + offsetOf t) 0
      let sp : a := IsString.fromString " "
      (match st.lineLength with
       | some l => if st.column + 1 + off > l then newline
                   else if st.column > 0 then outp 1 sp else pure ()
       | none => if st.column > 0 then outp 1 sp else pure ())
      renderList xs'
  | .FAfterBreak t :: xs => do
      let st ← get
      if st.newlines > 0 then renderList (t ++ xs) else renderList xs
  | b :: xs => do
      -- `FBlock`/`FVFill`: lay out side-by-side column blocks.
      let st ← get
      let font := peekFont st
      let (bs, rest) := xs.span isBlock
      let blocks := b :: bs
      let heightOf : FlatDoc a → Int := fun d => match d with
        | .FBlock _ ls => Int.ofNat ls.length
        | _ => 1
      let maxheight := (blocks.map heightOf).foldl max 0
      let toBlockSpec : FlatDoc a → Int × List (Int × Attributed a) := fun d => match d with
        | .FBlock w ls => (w, ls.map fun l => (realLength l, l))
        | .FVFill w t => (w, (List.replicate maxheight.toNat t).map fun tt =>
            let l : Attributed a := ⟨[⟨st.linkTarget, font, tt⟩]⟩
            (realLength l, l))
        | _ => (0, [])
      let merged := (bs.map toBlockSpec).foldl (mergeBlocks maxheight) (toBlockSpec b)
      let lns' := merged.2
      let oldPref := st.pfx
      let oldPrefixA := st.prefixA
      let oldPrefixTrimmedA := st.prefixTrimmedA
      let oldPrefixLen := st.prefixLen
      let n := st.column - oldPrefixLen
      (if n > 0 then
        modify fun s => { s with
          pfx := oldPref ++ String.ofList (List.replicate n.toNat ' ')
          prefixA := s.prefixA ++ HasChars.replicateChar n.toNat ' '
          prefixLen := s.prefixLen + n }
       else pure ())
      renderList (List.intersperse .FCarriageReturn (lns'.filterMap fun p => cook p.2))
      modify fun s => { s with
        pfx := oldPref, prefixA := oldPrefixA
        prefixTrimmedA := oldPrefixTrimmedA, prefixLen := oldPrefixLen }
      renderList rest

/-- Flatten, normalize, then render a document. -/
unsafe def renderDoc (d : Doc a) : StateM (RenderState a) Unit :=
  renderList (normalize (flatten d))

/-- Render to an attributed run under an optional line length. -/
unsafe def prerender (linelen : Option Int) (doc : Doc a) : Attributed a :=
  let startingState : RenderState a :=
    { output := [], pfx := "", prefixA := IsString.fromString ""
      prefixTrimmedA := IsString.fromString "", prefixLen := 0, usePrefix := true
      lineLength := linelen, column := 0, newlines := 2, fontStack := [], linkTarget := none }
  let st := (renderDoc doc |>.run startingState).2
  ⟨st.output.reverse⟩

/- ── Renderers ─────────────────────────────────────────────────────── -/

/-- Strip attributes from a chunk (dropping empty chunks). -/
def attrStrip (c : Attr a) : a :=
  if HasChars.isNull c.value then IsString.fromString "" else c.value

/-- Emit ANSI escapes for a chunk, tracking the current link/font. -/
def attrRender (acc : Option String × Font × String) (c : Attr a) :
    Option String × Font × String :=
  let (l, f, out) := acc
  if HasChars.isNull c.value then (l, f, out)
  else
    let newFont := if f == c.font then "" else renderFont c.font
    let newLink := if l == c.link then "" else renderOSC8 c.link
    (c.link, c.font, out ++ newFont ++ newLink ++ HasChars.build c.value)

/-- Render a document to plain `a`, wrapping at the optional line length. -/
unsafe def renderPlain (n : Option Int) (d : Doc a) : a :=
  (prerender n d).chunks.foldl (fun acc c => acc ++ attrStrip c) (IsString.fromString "")

/-- `render` is a synonym for `renderPlain`. -/
unsafe def render (n : Option Int) (d : Doc a) : a := renderPlain n d

/-- Render a document to an ANSI-escaped `String`. -/
unsafe def renderANSI (n : Option Int) (d : Doc a) : String :=
  let (_, _, out) := (prerender n d).chunks.foldl attrRender (none, baseFont, "")
  out ++ renderFont baseFont ++ renderOSC8 none

/- ── Blocks ────────────────────────────────────────────────────────── -/

/-- Build a block of the given width, applying `filler` to each chopped line. -/
unsafe def block (filler : Attributed a → Attributed a) (width : Int) (d : Doc a) : Doc a :=
  let w := if width < 1 && !(isEmpty d) then 1 else width
  let reboxed := chop w (prerender (some w) d)
  .Block w (reboxed.map filler)

/-- Left-aligned block of width `w`. -/
unsafe def lblock (w : Int) (d : Doc a) : Doc a := block id w d

/-- Right-aligned block of width `w`. -/
unsafe def rblock (w : Int) (d : Doc a) : Doc a :=
  block (fun s => HasChars.replicateChar (w - realLength s).toNat ' ' ++ s) w d

/-- Centred block of width `w`. -/
unsafe def cblock (w : Int) (d : Doc a) : Doc a :=
  block (fun s => HasChars.replicateChar ((w - realLength s) / 2).toNat ' ' ++ s) w d

/-- A vertically expandable fill of text `t`. -/
def vfill (t : a) : Doc a := .VFill (realLength t) t

/- ── Queries ───────────────────────────────────────────────────────── -/

/-- Does a document start with non-blank content?  (Reconstructed; upstream's
`isNonBlank` guards the `getOffset` `BeforeNonBlank` optimisation.) -/
def isNonBlank : Doc a → Bool
  | .Text _ s => !HasChars.isNull s
  | .CookedText _ s => !HasChars.isNull s
  | .Block n _ => n > 0
  | .VFill n _ => n > 0
  | .Concat x y => isNonBlank x || isNonBlank y
  | .Styled _ d => isNonBlank d
  | .Linked _ d => isNonBlank d
  | .Prefixed _ d => isNonBlank d
  | .Flush d => isNonBlank d
  | .BeforeNonBlank d => isNonBlank d
  | _ => false

/-- Compute `(maxLineLength, finalColumn)` under a break predicate.  Recurses
over re-associated `Concat` spines (see module note); written `unsafe`. -/
unsafe def getOffset (breakWhen : Int → Bool) : (Int × Int) → Doc a → (Int × Int)
  | (l, c), x => match x with
    | .Text n _ => (l, c + n)
    | .Block n _ => (l, c + n)
    | .VFill n _ => (l, c + n)
    | .CookedText n _ => (l, c + n)
    | .Empty => (l, c)
    | .Styled _ d => getOffset breakWhen (l, c) d
    | .Linked _ d => getOffset breakWhen (l, c) d
    | .CarriageReturn => (max l c, 0)
    | .NewLine => (max l c, 0)
    | .BlankLines _ => (max l c, 0)
    | .Prefixed t d =>
        let (l', c') := getOffset breakWhen (0, 0) d
        let tl := realLength (IsString.fromString t : a)
        (max l (l' + tl), c' + tl)
    | .BeforeNonBlank _ => (l, c)
    | .Flush d => getOffset breakWhen (l, c) d
    | .BreakingSpace => if breakWhen c then (max l c, 0) else (l, c + 1)
    | .AfterBreak t =>
        if c == 0 then (l, c + realLength (IsString.fromString t : a)) else (l, c)
    | .Concat (.Concat d y) z => getOffset breakWhen (l, c) (.Concat d (.Concat y z))
    | .Concat (.BeforeNonBlank d) y =>
        if isNonBlank y then getOffset breakWhen (l, c) (.Concat d y)
        else getOffset breakWhen (l, c) y
    | .Concat d y => let (l', c') := getOffset breakWhen (l, c) d; getOffset breakWhen (l', c') y

/-- The document's rendered width (longest line, no breaking). -/
unsafe def offset (d : Doc a) : Int :=
  let (l, c) := getOffset (fun _ => false) (0, 0) d; max l c

/-- The minimal width the document needs (breaking at every space). -/
unsafe def minOffset (d : Doc a) : Int :=
  let (l, c) := getOffset (fun n => n > 0) (0, 0) d; max l c

/-- The column reached from starting column `k`. -/
unsafe def updateColumn (d : Doc a) (k : Int) : Int :=
  (getOffset (fun _ => false) (0, k) d).2

/-- The document's height in rendered lines. -/
unsafe def height (d : Doc a) : Int :=
  Int.ofNat (HasChars.splitLines (render none d)).length

/- ── Trimming combinators ──────────────────────────────────────────── -/

/-- Drop trailing breaks/blank space from a document.  Recurses over
re-associated `Concat` spines (see module note); written `unsafe`. -/
unsafe def chomp : Doc a → Doc a
  | .BlankLines _ => .Empty
  | .NewLine => .Empty
  | .CarriageReturn => .Empty
  | .BreakingSpace => .Empty
  | .Prefixed s d => .Prefixed s (chomp d)
  | .Concat (.Concat x y) z => chomp (.Concat x (.Concat y z))
  | .Concat x y =>
      match chomp y with
      | .Empty => chomp x
      | z => x ++ z
  | d => d

/-- Drop leading breaks/blank space from a document.  Recurses over
re-associated `Concat` spines (see module note); written `unsafe`. -/
unsafe def nestle : Doc a → Doc a
  | .BlankLines _ => .Empty
  | .NewLine => .Empty
  | .Concat (.Concat x y) z => nestle (.Concat x (.Concat y z))
  | .Concat (.BlankLines _) x => nestle x
  | .Concat .NewLine x => nestle x
  | .Concat .CarriageReturn x => nestle x
  | d => d

end Text.DocLayout
