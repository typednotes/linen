/-
  Linen.Data.Yaml — YAML 1.2 core schema

  Reads the YAML that configuration files are actually written in: block
  mappings and sequences, flow collections, plain and quoted scalars, literal
  and folded block scalars, comments, and multi-document streams.

  ## Provenance
  Modelled on Hackage's `HsYAML` for the value model and the core schema's
  scalar resolution rules (YAML 1.2 §10.2).

  ## Supported

  * block mappings (`key: value`) and block sequences (`- item`), nested by
    indentation, including the compact `- key: value` form
  * flow collections — `{a: 1, b: [2, 3]}` — nested arbitrarily
  * plain, single-quoted and double-quoted scalars, with the usual escapes in
    the double-quoted form
  * literal (`|`) and folded (`>`) block scalars
  * comments, and multi-document streams separated by `---` / `...`
  * core-schema resolution: `null`/`~`, booleans, integers (decimal, `0x`,
    `0o`), floats including `.inf`/`.nan`, everything else a string

  ## Deliberately rejected

  Anchors (`&a`), aliases (`*a`), merge keys (`<<:`) and custom tags (`!!t`)
  produce an **error** rather than a wrong parse. Anchors would make the result
  a graph rather than a tree and need cycle detection; silently treating `*a`
  as the string `"*a"` would be worse than refusing it. Non-scalar mapping keys
  are likewise rejected: the value model uses `String` keys, which is what
  configuration files use.

  ## Structure
  Nothing here recurses on "the rest of the input after a sub-parse", so there
  is no termination argument anywhere. Four passes, each structural:

  1. `scanLines`  — strip comments and measure indentation, per line.
  2. `build`      — assemble blocks using an explicit stack keyed by indent.
  3. `flowTokens` — a character state machine for flow collections.
  4. `flowBuild`  — assemble flow values using an explicit stack.

  Nesting depth lives in the stacks, exactly as in `Linen.Text.XML`.
-/

import Linen.Data.Float

namespace Data.Yaml

-- ══════════════════════════════════════════════════════════════
-- Values
-- ══════════════════════════════════════════════════════════════

/-- A YAML node under the core schema. Mapping keys are `String`: see the
    module note on rejected constructs. -/
inductive Value where
  | null
  | bool  (b : Bool)
  | int   (i : Int)
  | float (f : Float)
  | str   (s : String)
  | seq   (xs : List Value)
  | map   (kvs : List (String × Value))
  deriving Repr, Inhabited, BEq

namespace Value

/-- The value at a key, for a mapping. -/
def get? : Value → String → Option Value
  | .map kvs, k => (kvs.find? (·.1 == k)).map (·.2)
  | _,        _ => none

/-- A scalar as text. Numbers and booleans render back to their source form,
    so a config reader can treat everything as a string when it wants to. -/
def asString? : Value → Option String
  | .str s   => some s
  | .int i   => some (toString i)
  | .bool b  => some (if b then "true" else "false")
  | .float f => some (toString f)
  | _        => none

def asInt? : Value → Option Int
  | .int i => some i
  | _      => none

def asBool? : Value → Option Bool
  | .bool b => some b
  | _       => none

def asSeq? : Value → Option (List Value)
  | .seq xs => some xs
  | _       => none

/-- The keys of a mapping, in document order. -/
def keys : Value → List String
  | .map kvs => kvs.map (·.1)
  | _        => []

/-- Look up a dotted path, e.g. `"a.b.c"`. -/
def path? (v : Value) (dotted : String) : Option Value :=
  (dotted.splitOn ".").foldl (fun acc k => acc.bind (·.get? k)) (some v)

end Value

-- ══════════════════════════════════════════════════════════════
-- Scalar resolution (YAML 1.2 §10.2, core schema)
-- ══════════════════════════════════════════════════════════════

private def isDigit (c : Char) : Bool := c ≥ '0' && c ≤ '9'

/-- Parse a signed integer in decimal, `0x` hex or `0o` octal. -/
private def parseInt? (s : String) : Option Int :=
  let (neg, body) :=
    if s.startsWith "-" then (true, s.drop 1 |>.toString)
    else if s.startsWith "+" then (false, s.drop 1 |>.toString)
    else (false, s)
  if body.isEmpty then none
  else
    let digits (base : Nat) (ds : List Char) : Option Nat :=
      if ds.isEmpty then none
      else ds.foldl (fun acc c => do
        let a ← acc
        let d ←
          if isDigit c then some (c.toNat - 48)
          else if c ≥ 'a' && c ≤ 'f' then some (c.toNat - 87)
          else if c ≥ 'A' && c ≤ 'F' then some (c.toNat - 55)
          else none
        if d < base then some (a * base + d) else none) (some 0)
    let mag :=
      if body.startsWith "0x" || body.startsWith "0X" then digits 16 (body.drop 2).toString.toList
      else if body.startsWith "0o" || body.startsWith "0O" then digits 8 (body.drop 2).toString.toList
      else digits 10 body.toList
    mag.map fun m => if neg then -(m : Int) else (m : Int)

/-- Whether the text looks like a core-schema float. Kept deliberately strict:
    anything ambiguous stays a string, which is the safer default for config. -/
private def looksLikeFloat (s : String) : Bool :=
  let body := if s.startsWith "-" || s.startsWith "+" then (s.drop 1).toString else s
  let cs := body.toList
  !cs.isEmpty
    && cs.all (fun c => isDigit c || c == '.' || c == 'e' || c == 'E' || c == '-' || c == '+')
    && cs.any (fun c => c == '.' || c == 'e' || c == 'E')
    && cs.any isDigit

/-- Resolve a *plain* (unquoted) scalar to a typed value. Quoted scalars never
    come here: they are always strings. -/
def resolveScalar (raw : String) : Value :=
  let s := raw.trimAscii.toString
  if s.isEmpty || s == "~" || s == "null" || s == "Null" || s == "NULL" then .null
  else if s == "true" || s == "True" || s == "TRUE" then .bool true
  else if s == "false" || s == "False" || s == "FALSE" then .bool false
  else if s == ".inf" || s == ".Inf" || s == ".INF" || s == "+.inf" then .float (1.0 / 0.0)
  else if s == "-.inf" || s == "-.Inf" || s == "-.INF" then .float (-1.0 / 0.0)
  else if s == ".nan" || s == ".NaN" || s == ".NAN" then .float (0.0 / 0.0)
  else match parseInt? s with
    | some i => .int i
    | none   => if looksLikeFloat s then
                  match Data.Float.parseFloat? s with
                  | some f => .float f
                  | none   => .str s
                else .str s

-- ══════════════════════════════════════════════════════════════
-- Rejected constructs
-- ══════════════════════════════════════════════════════════════

/-- Reject the constructs this port deliberately does not implement, naming
    them, so a document using them fails loudly instead of being mis-read. -/
def checkUnsupported (s : String) : Except String Unit :=
  let t := s.trimAscii.toString
  if t.startsWith "&" then .error s!"anchors are not supported: {t}"
  else if t.startsWith "*" then .error s!"aliases are not supported: {t}"
  else if t.startsWith "!!" || t.startsWith "!" then .error s!"tags are not supported: {t}"
  else .ok ()

-- ══════════════════════════════════════════════════════════════
-- Quoted and flow scalars
-- ══════════════════════════════════════════════════════════════

/-- Expand escapes inside a double-quoted scalar. Structural: a fold over the
    characters with a one-character "escaped" flag. -/
private def unescapeDouble (cs : List Char) : Except String String :=
  let step (st : Bool × List Char) (c : Char) : Except String (Bool × List Char) :=
    if st.1 then
      let out := match c with
        | 'n'  => '\n' | 't' => '\t' | 'r' => '\r' | '0' => '\x00'
        | '\\' => '\\' | '"' => '"'  | '/' => '/'  | 'a' => '\x07'
        | 'b'  => '\x08' | 'f' => '\x0c' | 'v' => '\x0b' | ' ' => ' '
        | other => other
      .ok (false, out :: st.2)
    else if c == '\\' then .ok (true, st.2)
    else .ok (false, c :: st.2)
  do
    let (pending, out) ← cs.foldlM step (false, [])
    if pending then .error "trailing backslash in double-quoted scalar"
    else .ok (String.ofList out.reverse)

-- ══════════════════════════════════════════════════════════════
-- Flow collections
-- ══════════════════════════════════════════════════════════════

/-- A lexical item inside a flow collection. -/
private inductive FlowTok where
  | lbrace | rbrace | lbrack | rbrack | comma | colon
  | scalar (v : Value)
  deriving Repr

/-- What the flow tokenizer is in the middle of. -/
private inductive FMode where
  | plain (acc : List Char)
  | single (acc : List Char)
  | double (acc : List Char) (escaped : Bool)

/-- Flush a plain run as a scalar token, dropping it when blank. -/
private def flushPlain (acc : List Char) (out : List FlowTok) : Except String (List FlowTok) :=
  let s := (String.ofList acc.reverse).trimAscii.toString
  if s.isEmpty then .ok out
  else do
    checkUnsupported s
    .ok (.scalar (resolveScalar s) :: out)

/-- One character of flow tokenization. No branch recurses. -/
private def fStep (st : FMode × List FlowTok) (c : Char) : Except String (FMode × List FlowTok) :=
  match st.1 with
  | .plain acc =>
    let punct (t : FlowTok) : Except String (FMode × List FlowTok) := do
      let out ← flushPlain acc st.2
      .ok (.plain [], t :: out)
    if c == '{' then punct .lbrace
    else if c == '}' then punct .rbrace
    else if c == '[' then punct .lbrack
    else if c == ']' then punct .rbrack
    else if c == ',' then punct .comma
    else if c == ':' then punct .colon
    -- A quote opens a scalar when nothing but whitespace precedes it: after
    -- `{a: ` the accumulator holds the separating space, not content.
    else if c == '\'' && acc.all (· == ' ') then .ok (.single [], st.2)
    else if c == '"' && acc.all (· == ' ') then .ok (.double [] false, st.2)
    else .ok (.plain (c :: acc), st.2)
  | .single acc =>
    if c == '\'' then .ok (.plain [], .scalar (.str (String.ofList acc.reverse)) :: st.2)
    else .ok (.single (c :: acc), st.2)
  | .double acc escaped =>
    if escaped then .ok (.double (c :: '\\' :: acc) false, st.2)
    else if c == '\\' then .ok (.double acc true, st.2)
    else if c == '"' then do
      let s ← unescapeDouble acc.reverse
      .ok (.plain [], .scalar (.str s) :: st.2)
    else .ok (.double (c :: acc) escaped, st.2)

private def flowTokens (s : String) : Except String (List FlowTok) := do
  let (mode, out) ← s.toList.foldlM fStep (FMode.plain [], [])
  match mode with
  | .plain acc  => return (← flushPlain acc out).reverse
  | .single _   => .error "unterminated single-quoted scalar"
  | .double _ _ => .error "unterminated double-quoted scalar"

/-- A partially built flow collection. -/
private structure FFrame where
  isSeq   : Bool
  items   : List Value := []
  entries : List (String × Value) := []
  key     : Option String := none

private def FFrame.close (f : FFrame) : Value :=
  if f.isSeq then .seq f.items.reverse else .map f.entries.reverse

/-- Attach a completed value to the innermost open frame, or return it when
    the stack is empty. -/
private def attach (stack : List FFrame) (v : Value) :
    Except String (List FFrame × Option Value) :=
  match stack with
  | [] => .ok ([], some v)
  | f :: rest =>
    if f.isSeq then .ok ({ f with items := v :: f.items } :: rest, none)
    else match f.key with
      | some k => .ok ({ f with entries := (k, v) :: f.entries, key := none } :: rest, none)
      | none   =>
        match v.asString? with
        | some k => .ok ({ f with key := some k } :: rest, none)
        | none   => .error "mapping key must be a scalar"

/-- Fold flow tokens into a value with an explicit stack: structural on the
    token list, so nesting needs no termination argument. -/
private def flowBuild : List FlowTok → List FFrame → Option Value →
    Except String Value
  | [], [], some v => .ok v
  | [], [], none   => .error "empty flow collection"
  | [], _,  _      => .error "unterminated flow collection"
  | .lbrace :: ts, stack, done => flowBuild ts ({ isSeq := false } :: stack) done
  | .lbrack :: ts, stack, done => flowBuild ts ({ isSeq := true } :: stack) done
  | .rbrace :: ts, stack, _ =>
    match stack with
    | f :: rest =>
      if f.isSeq then .error "expected ']' but found '}'"
      else do let (s, d) ← attach rest f.close; flowBuild ts s d
    | [] => .error "unmatched '}'"
  | .rbrack :: ts, stack, _ =>
    match stack with
    | f :: rest =>
      if !f.isSeq then .error "expected '}' but found ']'"
      else do let (s, d) ← attach rest f.close; flowBuild ts s d
    | [] => .error "unmatched ']'"
  | .comma :: ts, stack, done => flowBuild ts stack done
  | .colon :: ts, stack, done => flowBuild ts stack done
  | .scalar v :: ts, stack, _ => do
    let (s, d) ← attach stack v
    flowBuild ts s d

/-- Whether a line's content starts a flow collection. -/
private def isFlow (s : String) : Bool :=
  let t := s.trimAscii.toString
  t.startsWith "{" || t.startsWith "["

/-- Parse a flow collection or a scalar. -/
private def parseInline (s : String) : Except String Value := do
  let t := s.trimAscii.toString
  if isFlow t then flowBuild (← flowTokens t) [] none
  else if t.startsWith "'" && t.endsWith "'" && t.length ≥ 2 then
    .ok (.str (String.ofList ((t.drop 1).dropEnd 1).toString.toList))
  else if t.startsWith "\"" && t.endsWith "\"" && t.length ≥ 2 then
    .ok (.str (← unescapeDouble ((t.drop 1).dropEnd 1).toString.toList))
  else do
    checkUnsupported t
    .ok (resolveScalar t)

-- ══════════════════════════════════════════════════════════════
-- Line scanning
-- ══════════════════════════════════════════════════════════════

/-- One significant line: how deeply it is indented, and what it says with any
    trailing comment removed. `raw` keeps the original text, which block
    scalars need verbatim. -/
structure Line where
  indent  : Nat
  content : String
  raw     : String
  deriving Repr, DecidableEq, BEq

/-- Strip a trailing `#` comment.

    A `#` only starts a comment at the start of the line or after whitespace,
    and never inside quotes — so `url: http://h/#frag` and `x: "a # b"` keep
    their `#`. A fold over characters with a small quoting state. -/
private def stripComment (s : String) : String :=
  let step (st : Option Char × Bool × List Char) (c : Char) : Option Char × Bool × List Char :=
    let (quote, done, acc) := st
    if done then (quote, true, acc)
    else match quote with
      | some q => if c == q then (none, false, c :: acc) else (quote, false, c :: acc)
      | none =>
        if c == '\'' || c == '"' then (some c, false, c :: acc)
        else if c == '#' && (acc.isEmpty || acc.head? == some ' ' || acc.head? == some '\t')
        then (none, true, acc)
        else (none, false, c :: acc)
  let (_, _, acc) := s.toList.foldl step (none, false, [])
  String.ofList acc.reverse

/-- Count leading spaces. Tabs are rejected: YAML forbids them for indentation
    and silently accepting one would misplace a whole subtree. -/
private def measureIndent (s : String) : Except String Nat :=
  let cs := s.toList
  let spaces := (cs.takeWhile (· == ' ')).length
  if (cs.drop spaces).head? == some '\t' then
    .error "tabs are not permitted for indentation"
  else .ok spaces

/-- Split into significant lines, dropping blanks and comment-only lines. -/
def scanLines (input : String) : Except String (List Line) := do
  let raws := input.splitOn "\n"
  let mut out : List Line := []
  for raw in raws do
    let stripped := stripComment raw
    let trimmed := stripped.trimAscii.toString
    if !trimmed.isEmpty then
      let ind ← measureIndent stripped
      out := { indent := ind, content := trimmed, raw := raw } :: out
  return out.reverse

-- ══════════════════════════════════════════════════════════════
-- Block structure
-- ══════════════════════════════════════════════════════════════

/-- A container being filled, and the indent that owns it. -/
private structure BFrame where
  indent  : Nat
  isSeq   : Bool
  items   : List Value := []
  entries : List (String × Value) := []
  /-- For a mapping, the key whose value is still to come from deeper lines. -/
  key     : Option String := none
  /-- For a sequence, whether a `-` is awaiting its value from deeper lines. -/
  pending : Bool := false

/-- Close a frame. A key or a `-` still awaiting a value gets `null`, which is
    what `key:` on a line of its own means — dropping it instead would make the
    key silently vanish. -/
private def BFrame.close (f : BFrame) : Value :=
  if f.isSeq then
    .seq (if f.pending then f.items.reverse ++ [Value.null] else f.items.reverse)
  else
    .map (match f.key with
          | some k => f.entries.reverse ++ [(k, Value.null)]
          | none   => f.entries.reverse)

/-- Place a finished value into the innermost frame. -/
private def place (stack : List BFrame) (v : Value) : Except String (List BFrame × Option Value) :=
  match stack with
  | [] => .ok ([], some v)
  | f :: rest =>
    if f.isSeq then .ok ({ f with items := v :: f.items, pending := false } :: rest, none)
    else match f.key with
      | some k => .ok ({ f with entries := (k, v) :: f.entries, key := none } :: rest, none)
      | none   => .error "value with no key"

/-- Close every frame indented deeper than `ind`, folding each into its
    parent. Structural on the stack. -/
private def unwind : List BFrame → Option Value → Nat →
    Except String (List BFrame × Option Value)
  | [],        done, _   => .ok ([], done)
  | f :: rest, done, ind =>
    if f.indent > ind then
      let v := f.close
      -- `place`'s cases are inlined so the recursive call is visibly on a
      -- stack one frame shorter, which is what makes the measure work.
      match rest with
      | [] => .ok ([], some v)
      | g :: gs =>
        if g.isSeq then
          unwind ({ g with items := v :: g.items, pending := false } :: gs) done ind
        else match g.key with
          | some k => unwind ({ g with entries := (k, v) :: g.entries, key := none } :: gs) done ind
          | none   => .error "value with no key"
    else .ok (f :: rest, done)
termination_by stack => stack.length

/-- Additionally close a sequence sitting at exactly this indent.

    `key:` followed by `- a` puts the sequence at the *same* column as the
    mapping, so a later key at that column must end the sequence — which
    `unwind` alone, comparing strictly, would not do. -/
private def closeSeqAt (stack : List BFrame) (done : Option Value) (ind : Nat) :
    Except String (List BFrame × Option Value) :=
  match stack with
  | f :: rest => if f.isSeq && f.indent == ind then place rest f.close else .ok (stack, done)
  | []        => .ok ([], done)

/-- Split `key: rest` at the first `:` that ends a key.

    A `:` only separates when followed by a space or end of line, so
    `url: http://h` and `time: 12:30` keep their colons. Quotes are respected
    so `"a: b": v` works. -/
private def splitKey (s : String) : Option (String × String) :=
  let cs := s.toList
  let rec find (i : Nat) (quote : Option Char) (rest : List Char) : Option Nat :=
    match rest with
    | [] => none
    | c :: more =>
      match quote with
      | some q => if c == q then find (i + 1) none more else find (i + 1) quote more
      | none =>
        if c == '\'' || c == '"' then find (i + 1) (some c) more
        else if c == ':' && (more.isEmpty || more.head? == some ' ') then some i
        else find (i + 1) none more
  (find 0 none cs).map fun i =>
    (String.ofList (cs.take i) |>.trimAscii.toString,
     String.ofList (cs.drop (i + 1)) |>.trimAscii.toString)

/-- Peel every leading `- ` from a line, reporting the column each dash sits
    at and what is left over.

    `- - a` is two nested sequences on one line, so the dashes are peeled all
    at once rather than one per pass. Terminates on the shrinking character
    list: each step drops at least the dash and its space. -/
private def peelDashes : Nat → List Char → (List Nat × Nat × String)
  | col, '-' :: ' ' :: rest =>
    let skip := (rest.takeWhile (· == ' ')).length
    let (cols, c, s) := peelDashes (col + 2 + skip) (rest.drop skip)
    (col :: cols, c, s)
  | col, ['-'] => ([col], col + 2, "")
  | col, cs    => ([], col, (String.ofList cs).trimAscii.toString)
termination_by _ cs => cs.length
decreasing_by
  simp_wf
  omega

-- ══════════════════════════════════════════════════════════════
-- Building a document
-- ══════════════════════════════════════════════════════════════

/-- How a block scalar folds its lines back together. -/
private inductive BlockStyle where
  | literal   -- `|` : newlines kept
  | folded    -- `>` : newlines become spaces

/-- The fold's state: the container stack, a finished top-level value, and any
    block scalar currently being collected. -/
private structure BState where
  stack : List BFrame := []
  done  : Option Value := none
  block : Option (Nat × BlockStyle × Bool × List String) := none
  --      indent, style, chomp-trailing-newline, lines (reversed)

/-- Recognise `|`, `>` and their chomping indicators. -/
private def blockHeader (s : String) : Option (BlockStyle × Bool) :=
  if s == "|" then some (.literal, false)
  else if s == "|-" then some (.literal, true)
  else if s == "|+" then some (.literal, false)
  else if s == ">" then some (.folded, false)
  else if s == ">-" then some (.folded, true)
  else if s == ">+" then some (.folded, false)
  else none

/-- Join a block scalar's collected lines. -/
private def renderBlock (style : BlockStyle) (chomp : Bool) (lines : List String) : String :=
  let body := match style with
    | .literal => "\n".intercalate lines
    | .folded  => " ".intercalate (lines.map (·.trimAscii.toString))
  if chomp then body else body ++ "\n"

/-- Finish the block scalar in progress, if any, and place it. -/
private def finishBlock (st : BState) : Except String BState :=
  match st.block with
  | none => .ok st
  | some (_, style, chomp, lines) =>
    let ordered := lines.reverse
    -- YAML takes a block's indentation from its first non-empty line, not from
    -- the key's column, so measure it rather than assuming.
    let blockIndent := match ordered.find? (fun l => !l.trimAscii.toString.isEmpty) with
      | some l => (l.toList.takeWhile (· == ' ')).length
      | none   => 0
    let stripped := ordered.map fun l =>
      if l.length ≥ blockIndent then (l.drop blockIndent).toString else l.trimAscii.toString
    do
      let (s, d) ← place st.stack (.str (renderBlock style chomp stripped))
      .ok { stack := s, done := d, block := none }

/-- Ensure the innermost frame is a sequence at this indent.

    A `-` left pending by the previous line never got a deeper block, so its
    item is `null`. -/
private def ensureSeq (stack : List BFrame) (ind : Nat) : List BFrame :=
  match stack with
  | f :: rest =>
    if f.isSeq && f.indent == ind then
      if f.pending then { f with items := Value.null :: f.items, pending := false } :: rest
      else stack
    else { indent := ind, isSeq := true } :: stack
  | [] => [{ indent := ind, isSeq := true }]

/-- Ensure the innermost frame is a mapping at this indent.

    A key left pending by the previous line never got a deeper block, so its
    value is `null` — and crucially the *same* frame continues, rather than a
    second mapping being opened at the same column. -/
private def ensureMap (stack : List BFrame) (ind : Nat) : List BFrame :=
  match stack with
  | f :: rest =>
    if !f.isSeq && f.indent == ind then
      match f.key with
      | some k => { f with entries := (k, Value.null) :: f.entries, key := none } :: rest
      | none   => stack
    else { indent := ind, isSeq := false } :: stack
  | [] => [{ indent := ind, isSeq := false }]

/-- Handle the non-dash part of a line at a known column. -/
private def handleContent (st : BState) (ind : Nat) (content : String) :
    Except String BState := do
  if content.isEmpty then return st
  match splitKey content with
  | some (k, v) =>
    checkUnsupported k
    if k.startsWith "<<" then .error "merge keys (<<) are not supported"
    let key := if (k.startsWith "\"" && k.endsWith "\"") || (k.startsWith "'" && k.endsWith "'")
               then ((k.drop 1).dropEnd 1).toString else k
    let stack := ensureMap st.stack ind
    let stack := match stack with
      | f :: rest => { f with key := some key } :: rest
      | []        => []
    if v.isEmpty then
      return { st with stack }
    else match blockHeader v with
      | some (style, chomp) =>
        return { st with stack, block := some (ind + 1, style, chomp, []) }
      | none =>
        let val ← parseInline v
        let (s, d) ← place stack val
        return { st with stack := s, done := d }
  | none =>
    let val ← parseInline content
    let (s, d) ← place st.stack val
    return { st with stack := s, done := d }

/-- Handle one significant line, block scalars aside. -/
private def handleLine (st : BState) (line : Line) : Except String BState := do
  let (cols, col, rest) := peelDashes line.indent line.content.toList
  if cols.isEmpty then
    -- A plain line: close anything deeper, and any sequence at this column.
    let (s, d) ← unwind st.stack st.done line.indent
    let (s, d) ← closeSeqAt s d line.indent
    handleContent { st with stack := s, done := d } line.indent rest
  else
    -- One or more `-`: open a sequence per dash, then handle what follows.
    let (s, d) ← unwind st.stack st.done line.indent
    let mut stack := s
    for c in cols do
      stack := ensureSeq stack c
    if rest.isEmpty then
      -- `-` alone: the item's value comes from deeper lines, or is null.
      stack := match stack with
        | f :: more => { f with pending := true } :: more
        | []        => []
      return { st with stack, done := d }
    handleContent { st with stack, done := d } col rest

/-- Consume one line: either it extends a block scalar, or it ends one and is
    then handled normally. Neither branch recurses. -/
private def stepLine (st : BState) (line : Line) : Except String BState := do
  match st.block with
  | some (ind, style, chomp, acc) =>
    if line.indent ≥ ind then
      return { st with block := some (ind, style, chomp, line.raw :: acc) }
    else
      handleLine (← finishBlock st) line
  | none => handleLine st line

/-- Close every remaining frame, innermost first, folding each into its
    parent. Structural on the stack, like `unwind`. -/
private def closeAll : List BFrame → Option Value → Except String (Option Value)
  | [],        done => .ok done
  | f :: rest, done =>
    let v := f.close
    match rest with
    | [] => .ok (some v)
    | g :: gs =>
      if g.isSeq then
        closeAll ({ g with items := v :: g.items, pending := false } :: gs) done
      else match g.key with
        | some k => closeAll ({ g with entries := (k, v) :: g.entries, key := none } :: gs) done
        | none   => .error "value with no key"
termination_by stack => stack.length

/-- Build one document's value from its lines. -/
def build (lines : List Line) : Except String Value := do
  let st ← lines.foldlM stepLine ({} : BState)
  let st ← finishBlock st
  return (← closeAll st.stack st.done).getD .null

-- ══════════════════════════════════════════════════════════════
-- Entry points
-- ══════════════════════════════════════════════════════════════

/-- Whether a line separates documents. -/
private def isDocStart (l : Line) : Bool := l.content == "---" || l.content.startsWith "--- "
private def isDocEnd (l : Line) : Bool := l.content == "..."

/-- Parse a multi-document stream. -/
def parseDocuments (input : String) : Except String (List Value) := do
  let lines ← scanLines input
  -- Split on `---` / `...`. A fold, so no recursion: each line either starts a
  -- new group or joins the current one.
  let (groups, current) := lines.foldl
    (fun (acc : List (List Line) × List Line) l =>
      if isDocStart l then (acc.1 ++ [acc.2], [])
      else if isDocEnd l then (acc.1 ++ [acc.2], [])
      else (acc.1, acc.2 ++ [l]))
    ([], [])
  let all := (groups ++ [current]).filter (!·.isEmpty)
  all.mapM build

/-- Parse a single-document stream. A stream with several documents is an
    error here; use `parseDocuments` for those. -/
def parse (input : String) : Except String Value := do
  match ← parseDocuments input with
  | []  => return .null
  | [v] => return v
  | _   => .error "expected a single document; use parseDocuments"

end Data.Yaml
