/-
  Linen.Text.XML — XML parsing

  A parser for the XML that wire protocols actually emit: elements,
  attributes, character data, comments, CDATA sections, and the five
  predefined entities plus numeric character references.

  ## Provenance
  Ported in spirit from Hackage's `xml` package (`Text.XML.Light`), which is
  likewise a hand-written recursive parser rather than a combinator one.
  Linen previously had only `Linen.Text.Pandoc.XML`, which *escapes* XML but
  cannot read it, so nothing here could consume an XML response.

  ## Why a state machine
  `AGENTS.md` forbids `partial` and forbids fuel. A recursive-descent parser
  over nested elements has no structural measure — the recursion is on "the
  rest of the input after a sub-parse", which Lean cannot see as decreasing
  without a hand-proved bound. Linen's own `Data.Parser` is no help: its
  driver is `unsafe` and it has no monadic bind (a documented universe wall).

  So parsing is split in two, and both halves are structurally recursive:

  1. `tokenize` — a character-at-a-time state machine, folded over the input.
     Each step consumes exactly one character, so the recursion is on the
     character list itself.
  2. `build` — assembles the flat token list into a tree using an explicit
     stack of open elements. The recursion is on the token list.

  Nesting depth lives in the stack, not in the call graph, so no termination
  argument is needed anywhere.

  ## Not supported
  DTD internal subsets beyond being skipped, external entities, entity
  declarations, and namespace *resolution* (prefixes are parsed and exposed,
  but not mapped to URIs — `xmlns` attributes are returned as ordinary
  attributes). Processing instructions and `<!DOCTYPE …>` are skipped rather
  than reported.
-/

namespace Text.XML

-- ══════════════════════════════════════════════════════════════
-- The tree
-- ══════════════════════════════════════════════════════════════

/-- A possibly namespace-prefixed name. `prefix` is the part before the colon,
    when there is one; no attempt is made to resolve it to a URI. -/
structure QName where
  /-- The part after the colon, or the whole name when unprefixed. -/
  local' : String
  /-- The part before the colon, if any. -/
  prefix' : Option String := none
  deriving Repr, DecidableEq, BEq, Inhabited

/-- The name as written, prefix and all. -/
def QName.render (q : QName) : String :=
  match q.prefix' with
  | some p => p ++ ":" ++ q.local'
  | none   => q.local'

/-- Split `prefix:local` into a `QName`. A name with no colon, or with an empty
    prefix or local part, is taken whole. -/
def QName.parse (s : String) : QName :=
  match s.splitOn ":" with
  | [p, l] => if p.isEmpty || l.isEmpty then { local' := s } else { local' := l, prefix' := some p }
  | _      => { local' := s }

mutual

/-- An element: a name, its attributes, and its children in document order. -/
structure Element where
  name     : QName
  attrs    : List (QName × String) := []
  children : List Content := []
  deriving Repr, Inhabited

/-- Anything that can appear inside an element. -/
inductive Content where
  | element (e : Element)
  | text (s : String)
  | comment (s : String)
  deriving Repr, Inhabited

end

-- ══════════════════════════════════════════════════════════════
-- Entities
-- ══════════════════════════════════════════════════════════════

/-- The value of a numeric character reference body (`#123` or `#x7b`). -/
private def numericEntity (body : List Char) : Option Char :=
  let hexVal (c : Char) : Option Nat :=
    if c ≥ '0' && c ≤ '9' then some (c.toNat - 48)
    else if c ≥ 'a' && c ≤ 'f' then some (c.toNat - 87)
    else if c ≥ 'A' && c ≤ 'F' then some (c.toNat - 55)
    else none
  let fold (base : Nat) (ds : List Char) : Option Nat :=
    ds.foldl (fun acc c => do
      let a ← acc
      let d ← hexVal c
      if d < base then some (a * base + d) else none) (some 0)
  match body with
  | '#' :: 'x' :: ds => if ds.isEmpty then none else (fold 16 ds).map Char.ofNat
  | '#' :: 'X' :: ds => if ds.isEmpty then none else (fold 16 ds).map Char.ofNat
  | '#' :: ds        => if ds.isEmpty then none else (fold 10 ds).map Char.ofNat
  | _                => none

/-- Resolve one entity name (without `&` or `;`). -/
private def namedEntity (name : List Char) : Option Char :=
  match String.ofList name with
  | "lt"   => some '<'
  | "gt"   => some '>'
  | "amp"  => some '&'
  | "apos" => some '\''
  | "quot" => some '"'
  | _      => (numericEntity name)

/-- Whether entity expansion is mid-reference, and what has been read of it. -/
private inductive EMode where
  | plain
  | entity (acc : List Char)

/-- The longest entity name worth waiting for. Bounding this turns a stray `&`
    into a prompt error instead of swallowing the rest of the document. -/
private def maxEntityLength : Nat := 12

/-- One character of entity expansion. Like the tokenizer's `step`, no branch
    recurses, so folding it over the input is plainly structural. -/
private def eStep (st : EMode × List Char) (c : Char) : Except String (EMode × List Char) :=
  match st.1 with
  | .plain =>
    if c == '&' then .ok (.entity [], st.2) else .ok (.plain, c :: st.2)
  | .entity acc =>
    if c == ';' then
      match namedEntity acc.reverse with
      | some ch => .ok (.plain, ch :: st.2)
      | none    => .error s!"unknown entity: &{String.ofList acc.reverse};"
    else if c == '&' || acc.length ≥ maxEntityLength then
      .error "unterminated entity reference"
    else .ok (.entity (c :: acc), st.2)

/-- Expand entity references in a character list.

    An unterminated or unknown reference is an error rather than being passed
    through, so a typo cannot silently become literal text. -/
private def unescapeAux (cs : List Char) : Except String (List Char) := do
  let (mode, out) ← cs.foldlM eStep (EMode.plain, [])
  match mode with
  | .plain    => .ok out.reverse
  | .entity _ => .error "unterminated entity reference"

/-- Expand entity references, or report the first bad one. -/
def unescape (s : String) : Except String String :=
  (unescapeAux s.toList).map String.ofList

-- ══════════════════════════════════════════════════════════════
-- Tokens
-- ══════════════════════════════════════════════════════════════

/-- A flat lexical item. Self-closing tags become `empty`, so `build` never has
    to look ahead. -/
inductive Token where
  | start   (name : String) (attrs : List (String × String))
  | end'    (name : String)
  | empty   (name : String) (attrs : List (String × String))
  | text    (s : String)
  | comment (s : String)
  deriving Repr, DecidableEq, BEq

-- ══════════════════════════════════════════════════════════════
-- The tokenizer
-- ══════════════════════════════════════════════════════════════

/-- What the tokenizer is in the middle of. Character lists are accumulated
    reversed and flipped on completion. -/
private inductive Mode where
  | text       (acc : List Char)
  | afterLt
  | endName    (acc : List Char)
  | startName  (acc : List Char)
  | inTag      (name : String) (attrs : List (String × String))
  | afterSlash (name : String) (attrs : List (String × String))
  | attrName   (name : String) (attrs : List (String × String)) (acc : List Char)
  | afterName  (name : String) (attrs : List (String × String)) (an : String)
  | afterEq    (name : String) (attrs : List (String × String)) (an : String)
  | attrValue  (name : String) (attrs : List (String × String)) (an : String)
                (quote : Char) (acc : List Char)
  | bang       (acc : List Char)
  | comment    (acc : List Char)
  | cdata      (acc : List Char)
  | pi         (acc : List Char)
  | doctype    (depth : Nat)

private def isSpace (c : Char) : Bool := c == ' ' || c == '\t' || c == '\n' || c == '\r'

/-- Whether the reversed accumulator begins with the reversal of `pat` — i.e.
    whether the text so far *ends* with `pat`. -/
private def endsWith (acc : List Char) (pat : List Char) : Bool :=
  pat.reverse.isPrefixOf acc

/-- Emit accumulated character data, dropping it when it is empty so that
    whitespace between elements does not produce empty text nodes. -/
private def flushText (acc : List Char) (out : List Token) : Except String (List Token) :=
  if acc.isEmpty then .ok out
  else do
    let s ← unescapeAux acc.reverse
    .ok (.text (String.ofList s) :: out)

/-- The state carried through the fold: current mode and emitted tokens
    (reversed). -/
private structure TState where
  mode : Mode
  out  : List Token

/-- Consume exactly one character. Every branch either stays in the current
    mode or moves to another; none recurses, so the fold over the input is
    plainly structural. -/
private def step (st : TState) (c : Char) : Except String TState := do
  match st.mode with
  | .text acc =>
    if c == '<' then
      let out ← flushText acc st.out
      .ok { mode := .afterLt, out }
    else .ok { st with mode := .text (c :: acc) }

  | .afterLt =>
    if c == '/' then .ok { st with mode := .endName [] }
    else if c == '!' then .ok { st with mode := .bang [] }
    else if c == '?' then .ok { st with mode := .pi [] }
    else if isSpace c then .error "whitespace after '<'"
    else .ok { st with mode := .startName [c] }

  | .startName acc =>
    if isSpace c then .ok { st with mode := .inTag (String.ofList acc.reverse) [] }
    else if c == '>' then
      .ok { mode := .text [], out := .start (String.ofList acc.reverse) [] :: st.out }
    else if c == '/' then .ok { st with mode := .afterSlash (String.ofList acc.reverse) [] }
    else .ok { st with mode := .startName (c :: acc) }

  | .endName acc =>
    if c == '>' then
      .ok { mode := .text [], out := .end' (String.ofList acc.reverse) :: st.out }
    else if isSpace c then .ok { st with mode := .endName acc }
    else .ok { st with mode := .endName (c :: acc) }

  | .inTag name attrs =>
    if isSpace c then .ok st
    else if c == '>' then .ok { mode := .text [], out := .start name attrs.reverse :: st.out }
    else if c == '/' then .ok { st with mode := .afterSlash name attrs }
    else .ok { st with mode := .attrName name attrs [c] }

  | .afterSlash name attrs =>
    if c == '>' then .ok { mode := .text [], out := .empty name attrs.reverse :: st.out }
    else .error "expected '>' after '/'"

  | .attrName name attrs acc =>
    if c == '=' then .ok { st with mode := .afterEq name attrs (String.ofList acc.reverse) }
    else if isSpace c then .ok { st with mode := .afterName name attrs (String.ofList acc.reverse) }
    else if c == '>' || c == '/' then .error "attribute without a value"
    else .ok { st with mode := .attrName name attrs (c :: acc) }

  | .afterName name attrs an =>
    if isSpace c then .ok st
    else if c == '=' then .ok { st with mode := .afterEq name attrs an }
    else .error s!"expected '=' after attribute {an}"

  | .afterEq name attrs an =>
    if isSpace c then .ok st
    else if c == '"' || c == '\'' then .ok { st with mode := .attrValue name attrs an c [] }
    else .error s!"attribute {an} value must be quoted"

  | .attrValue name attrs an quote acc =>
    if c == quote then do
      let v ← unescapeAux acc.reverse
      .ok { st with mode := .inTag name ((an, String.ofList v) :: attrs) }
    else .ok { st with mode := .attrValue name attrs an quote (c :: acc) }

  | .bang acc =>
    let acc' := c :: acc
    let seen := acc'.reverse
    if seen == "--".toList then .ok { st with mode := .comment [] }
    else if seen == "[CDATA[".toList then .ok { st with mode := .cdata [] }
    else if "DOCTYPE".toList.isPrefixOf seen then .ok { st with mode := .doctype 0 }
    else if seen.length > 7 then .error "unrecognised '<!' declaration"
    else .ok { st with mode := .bang acc' }

  | .comment acc =>
    let acc' := c :: acc
    if endsWith acc' "-->".toList then
      .ok { mode := .text [],
            out := .comment (String.ofList (acc'.reverse.dropLast.dropLast.dropLast)) :: st.out }
    else .ok { st with mode := .comment acc' }

  | .cdata acc =>
    let acc' := c :: acc
    if endsWith acc' "]]>".toList then
      -- CDATA is literal: no entity expansion, so it becomes text directly.
      .ok { mode := .text [],
            out := .text (String.ofList (acc'.reverse.dropLast.dropLast.dropLast)) :: st.out }
    else .ok { st with mode := .cdata acc' }

  | .pi acc =>
    let acc' := c :: acc
    if endsWith acc' "?>".toList then .ok { mode := .text [], out := st.out }
    else .ok { st with mode := .pi acc' }

  | .doctype depth =>
    if c == '[' then .ok { st with mode := .doctype (depth + 1) }
    else if c == ']' then .ok { st with mode := .doctype (depth - 1) }
    else if c == '>' && depth == 0 then .ok { mode := .text [], out := st.out }
    else .ok st

/-- Split input into tokens. Errors name what went wrong rather than returning
    a partial result. -/
def tokenize (s : String) : Except String (List Token) := do
  let final ← s.toList.foldlM step { mode := .text [], out := [] }
  match final.mode with
  | .text acc => return (← flushText acc final.out).reverse
  | _         => .error "unexpected end of input"

-- ══════════════════════════════════════════════════════════════
-- Building the tree
-- ══════════════════════════════════════════════════════════════

/-- One open element on the stack: its name, attributes, and the children
    gathered so far (reversed). -/
private structure Frame where
  name     : String
  attrs    : List (String × String)
  children : List Content

private def frameToElement (f : Frame) : Element :=
  { name := QName.parse f.name
    attrs := f.attrs.map fun (k, v) => (QName.parse k, v)
    children := f.children.reverse }

/-- Fold tokens into content, carrying a stack of open elements.

    Structural on the token list; nesting depth lives in `stack`, so no
    termination argument is needed. `top` accumulates the content of the
    innermost open element, or of the document when the stack is empty. -/
private def build : List Token → List Frame → List Content →
    Except String (List Content)
  | [], [], top => .ok top.reverse
  | [], f :: _, _ => .error s!"unclosed element <{f.name}>"
  | .text s :: ts, stack, top => build ts stack (.text s :: top)
  | .comment s :: ts, stack, top => build ts stack (.comment s :: top)
  | .empty n as :: ts, stack, top =>
    build ts stack (.element (frameToElement ⟨n, as, []⟩) :: top)
  | .start n as :: ts, stack, top =>
    build ts (⟨n, as, top⟩ :: stack) []
  | .end' n :: ts, stack, top =>
    match stack with
    | [] => .error s!"closing tag </{n}> with no open element"
    | f :: rest =>
      if f.name != n then
        .error s!"closing tag </{n}> does not match open <{f.name}>"
      else
        -- `f.children` holds the enclosing content gathered before `f` opened.
        build ts rest (.element (frameToElement ⟨f.name, f.attrs, top⟩) :: f.children)

/-- Parse a document's content: the elements, text and comments at top level. -/
def parseContent (s : String) : Except String (List Content) := do
  build (← tokenize s) [] []

/-- Parse a document and return its single root element.

    Whitespace-only text around the root is ignored, as the specification
    requires; anything else at top level is an error. -/
def parse (s : String) : Except String Element := do
  let content ← parseContent s
  let significant := content.filter fun
    | .text t => !(t.all isSpace)
    | .comment _ => false
    | .element _ => true
  match significant with
  | [.element e] => .ok e
  | []           => .error "no root element"
  | _            => .error "expected exactly one root element"

-- ══════════════════════════════════════════════════════════════
-- Reading a tree
-- ══════════════════════════════════════════════════════════════

namespace Element

/-- The element's immediate children that are elements. -/
def elements (e : Element) : List Element :=
  e.children.filterMap fun | .element c => some c | _ => none

/-- Immediate child elements with this local name, ignoring any namespace
    prefix — which is what a consumer of a namespaced protocol document
    almost always wants. -/
def named (e : Element) (n : String) : List Element :=
  e.elements.filter (·.name.local' == n)

/-- The first immediate child element with this local name. -/
def child (e : Element) (n : String) : Option Element := (e.named n).head?

/-- All character data directly inside this element, concatenated. Child
    elements' text is not included. -/
def text (e : Element) : String :=
  String.join (e.children.filterMap fun | .text t => some t | _ => none)

/-- The text of the first child element with this local name. -/
def childText (e : Element) (n : String) : Option String := (e.child n).map text

/-- An attribute's value by local name. -/
def attr (e : Element) (n : String) : Option String :=
  (e.attrs.find? (·.1.local' == n)).map (·.2)

end Element

end Text.XML
