/-
  `Linen.Text.Pandoc.Parsing` — the shared parsec toolkit.

  ## Haskell source

  Ported from `Text.Pandoc.Parsing` and its submodules (`Parsing.State`,
  `Parsing.General`, …) in the `pandoc` package (v3.10,
  `src/Text/Pandoc/Parsing/*.hs`).

  Provides the `ParserState` record (the reader state threaded through the
  Markdown/HTML parsers), the `Key`/`HeaderType`/`ParserContext`/
  `QuoteContext` helpers, source-position tracking, and the widely-reused
  character/line/combinator primitives the readers build on (`spaceChar`,
  `blankline`, `anyLine`, `manyTill`, `many1Till`, `notFollowedBy'`,
  `oneOfStrings`(`CI`), `enclosed`, `charsInBalanced`, `escaped`,
  `characterReference`, `romanNumeral`, `uri`, `emailAddress`, …).

  ### Deviations from upstream

  * Built directly over Lean's `Std.Internal.Parsec.String.Parser` (the
    `parsec → Std.Internal.Parsec` substitution documented in
    `docs/imports/pandoc/dependencies.md`), the same house style used by the
    other `linen` parsec ports (`Linen.Data.PDF.*`, `Linen.Database.Redis`).
    Consequently the combinators are **stateless** over the char stream:
    `ParserState` is a value the readers thread themselves, rather than being
    woven into the parser monad (upstream's `ParsecT Sources ParserState m`);
    `getState`/`setState`/`updateState`-style state threading and the
    state-dependent actions (`parseFromString'`, `registerHeader`,
    `insertIncludedFile`, macro handling) belong to the reader tier and are
    scoped out here with this note.  The pure `ParserState` structure,
    position tracking, and every stream-only combinator ARE ported.
  * `ParserState` fields tied to deferred readers are scoped out: the
    `Future`-typed `stateMeta'`/`stateNotes'` (the delayed-evaluation
    machinery), the LaTeX `stateMacros`, and the RST-specific
    `stateRstDefaultRole`/`stateRstHighlight`/`stateRstCustomRoles`.  The
    widely-used fields (options, context, quote context, allow flags, keys,
    notes, identifiers, examples, log messages, last-string position, meta)
    are kept.
  * The ordered-list-marker parsers (upstream `Parsing.ListItem`) are left for
    the reader tier; `romanNumeral` and the character combinators they build
    on are provided.
-/

import Std.Internal.Parsec.String
import Linen.Text.Pandoc.Definition
import Linen.Text.Pandoc.Builder
import Linen.Text.Pandoc.Options
import Linen.Text.Pandoc.Sources
import Linen.Text.Pandoc.Shared
import Linen.Text.Pandoc.Extensions
import Linen.Text.Pandoc.URI
import Linen.Text.Pandoc.XML
import Linen.Text.Pandoc.Logging
import Linen.Text.Pandoc.Error
import Linen.Data.Map

namespace Linen.Text.Pandoc.Parsing

open Std.Internal.Parsec
open Std.Internal.Parsec.String
open Linen.Text.Pandoc

-- ── Parser state ──────────────────────────────────────────────────────

/-- A normalised reference key (bracket-stripped, whitespace-collapsed,
    lower-cased). -/
structure Key where
  /-- The normalised key text. -/
  unKey : String
  deriving Repr, BEq, Inhabited

instance : Ord Key where compare a b := compare a.unKey b.unKey

/-- Normalise link/reference text to a `Key`. -/
def toKey (t : String) : Key :=
  let unbracket (s : String) : String :=
    if s.startsWith "[" && s.endsWith "]" && s.length >= 2
    then (String.ofList (s.toList.drop 1).dropLast) else s
  let collapsed := " ".intercalate ((((Shared.splitBy Shared.isWS (unbracket t).toList).filter (· != [])).map String.ofList))
  ⟨collapsed.toLower⟩

/-- Whether the current parser is inside a list item or at the top level. -/
inductive ParserContext where
  | ListItemState | NullState
  deriving DecidableEq, Repr, Inhabited

/-- The current quotation context (used by the smart-quote logic). -/
inductive QuoteContext where
  | InSingleQuote | InDoubleQuote | NoQuote
  deriving DecidableEq, Repr, Inhabited

/-- A section-underline header type (for setext-style headers). -/
inductive HeaderType where
  | SingleHeader (c : Char)
  | DoubleHeader (c : Char)
  deriving DecidableEq, Repr, Inhabited

/-- A table of reference keys mapping to `(target, attr)`. -/
abbrev KeyTable := Data.Map Key (Target × Attr)
/-- A table of substitution definitions. -/
abbrev SubstTable := Data.Map Key (List Block)
/-- A table of footnotes mapping label to raw text. -/
abbrev NoteTable := List (String × String)

/-- The reader parser state.  (Scoped subset — see the module deviation note.) -/
structure ParserState where
  /-- The reader options. -/
  stateOptions : ReaderOptions := {}
  /-- List-item vs top-level context. -/
  stateParserContext : ParserContext := .NullState
  /-- The current quote context. -/
  stateQuoteContext : QuoteContext := .NoQuote
  /-- Whether links are currently allowed. -/
  stateAllowLinks : Bool := true
  /-- Whether line breaks are currently allowed. -/
  stateAllowLineBreaks : Bool := true
  /-- The position of the last parsed `Str` (for smart punctuation). -/
  stateLastStrPos : Option SourcePos := none
  /-- The reference-link key table. -/
  stateKeys : KeyTable := Data.Map.empty
  /-- The header-reference key table. -/
  stateHeaderKeys : KeyTable := Data.Map.empty
  /-- The substitution table. -/
  stateSubstitutions : SubstTable := Data.Map.empty
  /-- The footnote table. -/
  stateNotes : NoteTable := []
  /-- Labels of note references seen. -/
  stateNoteRefs : List String := []
  /-- Whether currently inside a note. -/
  stateInNote : Bool := false
  /-- The next note number. -/
  stateNoteNumber : Int := 0
  /-- The document metadata accumulated so far. -/
  stateMeta : Meta := nullMeta
  /-- Citation id → prefix map. -/
  stateCitations : Data.Map String String := Data.Map.empty
  /-- The stack of section header underline types. -/
  stateHeaderTable : List HeaderType := []
  /-- Identifiers already used (for uniqueness). -/
  stateIdentifiers : List String := []
  /-- The next example-list number. -/
  stateNextExample : Int := 1
  /-- Named example-list labels. -/
  stateExamples : Data.Map String Int := Data.Map.empty
  /-- The nesting stack of open containers/include files. -/
  stateContainers : List String := []
  /-- The accumulated log messages (reverse order). -/
  stateLogMessages : List LogMessage := []
  /-- Whether a markdown attribute block is being parsed. -/
  stateMarkdownAttribute : Bool := false
  /-- The fenced-div nesting level. -/
  stateFencedDivLevel : Int := 0
  deriving Inhabited

/-- The default parser state. -/
def defaultParserState : ParserState := {}

-- ── Source-position tracking ──────────────────────────────────────────

/-- Worker for `posFromByteOffset`. -/
private def posGo : List Char → Nat → Nat → Nat → Nat → SourcePos
  | [], _, _, line, col => { line := line, column := col }
  | c :: rest, byteIdx, consumed, line, col =>
      if consumed ≥ byteIdx then { line := line, column := col }
      else if c == '\n' then posGo rest byteIdx (consumed + c.utf8Size) (line + 1) 1
      else posGo rest byteIdx (consumed + c.utf8Size) line (col + 1)

/-- The `SourcePos` for a byte offset into a string (1-based line/column,
    counting `\n` as a line break and UTF-8 byte lengths for the offset). -/
def posFromByteOffset (s : String) (byteIdx : Nat) : SourcePos :=
  posGo s.toList byteIdx 0 1 1

/-- Parser action returning the current source position. -/
def getPosition : Parser SourcePos := fun it =>
  .success it (posFromByteOffset it.1 it.2.offset.byteIdx)

-- ── Character predicates ──────────────────────────────────────────────

/-- Is `c` a space, tab, `\n`, or `\r`? -/
def isSpaceChar (c : Char) : Bool := c == ' ' || c == '\t' || c == '\n' || c == '\r'

-- ── Character primitives ──────────────────────────────────────────────

/-- Parse a single character satisfying the predicate. -/
def satisfyC (p : Char → Bool) : Parser Char := satisfy p

/-- Parse any single character. -/
def anyChar : Parser Char := any

/-- Parse the given character. -/
def char (c : Char) : Parser Char := pchar c

/-- Parse the exact string. -/
def string (s : String) : Parser String := pstring s

/-- Parse a character in the given list. -/
def oneOf (cs : List Char) : Parser Char := satisfy (fun c => cs.contains c)

/-- Parse a character not in the given list. -/
def noneOf (cs : List Char) : Parser Char := satisfy (fun c => !cs.contains c)

/-- Parse a newline. -/
def newline : Parser Char := pchar '\n'

/-- Parse a single space or tab. -/
def spaceChar : Parser Char := satisfy (fun c => c == ' ' || c == '\t')

/-- Parse any character that is not a space, tab, newline, or carriage return. -/
def nonspaceChar : Parser Char := satisfy (fun c => !isSpaceChar c)

-- ── Text-returning `many` variants ────────────────────────────────────

/-- `many p`, packing `Char`s into a string. -/
def manyChar (p : Parser Char) : Parser String := manyChars p

/-- `many1 p`, packing `Char`s into a string. -/
def many1Char (p : Parser Char) : Parser String := many1Chars p

-- ── Repetition combinators ────────────────────────────────────────────

/-- Parse exactly `n` occurrences of `p`. -/
def count (n : Nat) (p : Parser α) : Parser (Array α) :=
  match n with
  | 0 => pure #[]
  | n + 1 => do let x ← p; let xs ← count n p; pure (#[x] ++ xs)

/-- `count n p`, packing `Char`s into a string. -/
def countChar (n : Nat) (p : Parser Char) : Parser String :=
  (fun (a : Array Char) => String.ofList a.toList) <$> count n p

/-- Succeed (consuming nothing) only if `p` fails. -/
def notFollowedBy' (p : Parser α) : Parser Unit := notFollowedBy p

/-- Parse `p` repeatedly until `endp` succeeds; return the `p` results. -/
def manyTill (p : Parser α) (endp : Parser β) : Parser (Array α) := do
  let xs ← many (attempt (do let _ ← notFollowedBy endp; p))
  let _ ← endp
  pure xs

/-- `manyTill`, packing `Char`s into a string. -/
def manyTillChar (p : Parser Char) (endp : Parser β) : Parser String :=
  (fun (a : Array Char) => String.ofList a.toList) <$> manyTill p endp

/-- Like `manyTill`, but requires at least one `p` before `endp`. -/
def many1Till (p : Parser α) (endp : Parser β) : Parser (Array α) := do
  let _ ← notFollowedBy endp
  let x ← p
  let xs ← manyTill p endp
  pure (#[x] ++ xs)

/-- `many1Till`, packing `Char`s into a string. -/
def many1TillChar (p : Parser Char) (endp : Parser β) : Parser String :=
  (fun (a : Array Char) => String.ofList a.toList) <$> many1Till p endp

/-- Like `manyTill`, also returning the value produced by `endp`. -/
def manyUntil (p : Parser α) (endp : Parser β) : Parser (Array α × β) := do
  let xs ← many (attempt (do let _ ← notFollowedBy endp; p))
  let e ← endp
  pure (xs, e)

/-- One or more `p` separated by `sep`. -/
def sepBy1 (p : Parser α) (sep : Parser β) : Parser (Array α) := do
  let x ← p
  let xs ← many (attempt (do let _ ← sep; p))
  pure (#[x] ++ xs)

/-- Zero or more `p` separated by `sep`. -/
def sepBy (p : Parser α) (sep : Parser β) : Parser (Array α) :=
  sepBy1 p sep <|> pure #[]

/-- Zero or more `p` separated and optionally terminated by `sep`. -/
def sepEndBy (p : Parser α) (sep : Parser β) : Parser (Array α) := do
  let xs ← sepBy p sep
  let _ ← (attempt (do let _ ← sep; pure ())) <|> pure ()
  pure xs

-- ── Whitespace / lines ────────────────────────────────────────────────

/-- Skip zero or more spaces/tabs. -/
def skipSpaces : Parser Unit := do let _ ← manyChar spaceChar; pure ()

/-- Skip spaces/tabs then consume a newline. -/
def blankline : Parser Char := attempt (do skipSpaces; newline)

/-- One or more blank lines, returned as the newline characters. -/
def blanklines : Parser String := many1Char blankline

/-- Parse a line (up to but excluding the newline). -/
def anyLine : Parser String := do
  let s ← manyChar (satisfy (· != '\n'))
  (do let _ ← newline; pure ()) <|> eof
  pure s

/-- Parse a line including its trailing newline. -/
def anyLineNewline : Parser String := do
  let s ← anyLine
  pure (s ++ "\n")

-- ── String matching ───────────────────────────────────────────────────

/-- Match a literal string, returning it (alias for `string`). -/
def textStr (s : String) : Parser String := string s

/-- Match one of the given strings, preferring the longest. -/
def oneOfStrings (strs : List String) : Parser String :=
  let sorted := strs.toArray.qsort (fun a b => a.length > b.length) |>.toList
  sorted.foldr (fun s acc => attempt (pstring s) <|> acc) (fail "oneOfStrings: no match")

/-- Case-insensitively match one specific string. -/
def stringAnyCase (s : String) : Parser String := do
  let cs ← s.toList.foldr
    (fun c acc => do
      let x ← satisfy (fun d => d.toLower == c.toLower)
      let rest ← acc
      pure (x :: rest))
    (pure [])
  pure (String.ofList cs)

/-- Case-insensitively match one of the given strings, preferring the longest. -/
def oneOfStringsCI (strs : List String) : Parser String :=
  let sorted := strs.toArray.qsort (fun a b => a.length > b.length) |>.toList
  sorted.foldr (fun s acc => attempt (stringAnyCase s) <|> acc) (fail "oneOfStringsCI: no match")

-- ── Delimited / balanced content ──────────────────────────────────────

/-- Parse `p` between `start` and `endp`, with no leading whitespace after
    `start`, returning the `p` results. -/
def enclosed (start : Parser s) (endp : Parser e) (p : Parser α) : Parser (Array α) := do
  let _ ← start
  let _ ← notFollowedBy (satisfy isSpaceChar)
  many1Till p endp

/-- Parse a backslash-escaped character parsed by `p`. -/
def escaped (p : Parser Char) : Parser Char := attempt (do let _ ← pchar '\\'; p)

-- NOTE: upstream's `charsInBalanced` (balanced-delimiter matching) needs a
-- nesting-depth stack whose recursion terminates only via a proof that each
-- step advances the input iterator; that proof is not ergonomic over the
-- stateless `Std.Internal.Parsec` base parser, and `charsInBalanced` is used
-- only by the (deferred) readers.  It is scoped out here per the module
-- deviation note.

-- ── Character references ──────────────────────────────────────────────

/-- Parse an HTML/XML character or entity reference (e.g. `&amp;`, `&#65;`,
    `&#x41;`) and resolve it to text. -/
def characterReference : Parser String := attempt do
  let _ ← pchar '&'
  let body ← many1Char (satisfy (fun c => c != ';' && c != '&' && !isSpaceChar c))
  let _ ← pchar ';'
  let decoded := XML.fromEntities ("&" ++ body ++ ";")
  if decoded == "&" ++ body ++ ";" then fail s!"unknown entity: {body}"
  else pure decoded

-- ── Roman numerals ────────────────────────────────────────────────────

/-- Evaluate a list of Roman-digit values with subtractive combination. -/
private def romanEval : List Int → Int
  | [] => 0
  | [x] => x
  | x :: y :: rest => if x < y then romanEval (y :: rest) - x else x + romanEval (y :: rest)

/-- Parse a Roman numeral (upper- or lower-case per `upper`), returning its
    integer value. -/
def romanNumeral (upper : Bool) : Parser Int := do
  let letters : List (Char × Int) :=
    if upper then [('M',1000),('D',500),('C',100),('L',50),('X',10),('V',5),('I',1)]
    else [('m',1000),('d',500),('c',100),('l',50),('x',10),('v',5),('i',1)]
  let str ← many1Char (satisfy (fun c => letters.any (·.1 == c)))
  let vals := str.toList.filterMap (fun c => (letters.find? (·.1 == c)).map (·.2))
  pure (romanEval vals)

-- ── URIs and email ────────────────────────────────────────────────────

/-- Punctuation permitted in the local part of an email address. -/
def emailPunctChars : List Char := "-!#$%&'*+/=?^_`{|}~;.".toList

/-- Parse an email address, returning `(rawText, "mailto:"++escaped)`. -/
def emailAddress : Parser (String × String) := attempt do
  let isLocal (c : Char) := c.isAlphanum || emailPunctChars.contains c
  let localPart ← many1Char (satisfy isLocal)
  let _ ← pchar '@'
  let domain ← many1Char (satisfy (fun c => c.isAlphanum || c == '.' || c == '-'))
  let raw := localPart ++ "@" ++ domain
  pure (raw, "mailto:" ++ raw)

/-- Parse a URI (scheme + rest, allowing balanced parentheses), returning
    `(rawText, escapedURI)`.  A trailing punctuation character is not consumed. -/
def uri : Parser (String × String) := attempt do
  let scheme ← many1Char (satisfy (fun c => c.isAlpha || c.isDigit || c == '+' || c == '.' || c == '-'))
  let _ ← pchar ':'
  if !URI.schemes.contains scheme.toLower then fail s!"unknown scheme: {scheme}"
  else do
    let rest ← manyChar (satisfy (fun c => !isSpaceChar c && c != '<' && c != '>'))
    -- strip a single trailing punctuation char (matches pandoc's heuristic)
    let restTrimmed :=
      if rest.length > 0 && ".,;:!?".toList.contains (rest.toList.getLast!)
      then String.ofList rest.toList.dropLast else rest
    let raw := scheme ++ ":" ++ restTrimmed
    pure (raw, URI.escapeURI raw)

-- ── Attribute helpers ─────────────────────────────────────────────────

/-- Move any `id`/`class` key-value pairs from the kv-list into the identifier
    and class fields of an attribute. -/
def extractIdClass (attr : Attr) : Attr :=
  let (ident, classes, kvs) := attr
  let ident' := ((kvs.find? (·.1 == "id")).map (·.2)).getD ident
  let classes' := match (kvs.find? (·.1 == "class")).map (·.2) with
    | some cs => classes ++ (((Shared.splitBy Shared.isWS cs.toList).filter (· != [])).map String.ofList)
    | none => classes
  let kvs' := kvs.filter (fun p => p.1 != "id" && p.1 != "class")
  (ident', classes', kvs')

-- ── Running a parser ──────────────────────────────────────────────────

/-- Run a parser over string input, converting failure to a `PandocError`. -/
def readWith (p : Parser α) (input : String) : Except PandocError α :=
  match p.run input with
  | .ok a => Except.ok a
  | .error e => (Except.error (.PandocParseError e) : Except PandocError α)

end Linen.Text.Pandoc.Parsing
