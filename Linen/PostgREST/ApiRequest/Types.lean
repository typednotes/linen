/-
  PostgREST.ApiRequest.Types — API request types

  Core types representing a parsed PostgREST API request: the action
  (read/mutate/invoke/info), select items, filters, ordering, logic trees,
  payloads, and the target resource.

  ## Haskell source
  - `PostgREST.ApiRequest.Types` (postgrest package)

  ## Design
  - `Action` captures the HTTP method semantics (read, mutate, invoke, info, inspect)
  - `Filter` and `LogicTree` represent the WHERE clause tree built from
    query parameters
  - `SelectItem` is a recursive tree of projected columns and embedded
    relationships
  - All types derive `BEq`, `Repr` for diagnostics
-/

import Linen.PostgREST.SchemaCache.Identifiers

namespace PostgREST.ApiRequest

-- ────────────────────────────────────────────────────────────────────
-- Actions
-- ────────────────────────────────────────────────────────────────────

/-- A mutation action: insert, update, single upsert, or delete.
    $$\text{Mutation} \in \{\text{insert}, \text{update},
      \text{singleUpsert}, \text{delete}\}$$ -/
inductive Mutation where
  | insert
  | update
  | singleUpsert
  | delete
  deriving BEq, Repr

instance : ToString Mutation where
  toString
    | .insert => "INSERT"
    | .update => "UPDATE"
    | .singleUpsert => "UPSERT"
    | .delete => "DELETE"

/-- The invocation method for RPC endpoints.
    $$\text{InvokeMethod} \in \{\text{invGet}, \text{invHead}, \text{invPost}\}$$ -/
inductive InvokeMethod where
  | invGet
  | invHead
  | invPost
  deriving BEq, Repr

instance : ToString InvokeMethod where
  toString
    | .invGet => "GET"
    | .invHead => "HEAD"
    | .invPost => "POST"

/-- The action a request performs.
    $$\text{Action} = \text{actionRead}\ \text{Bool} + \text{actionMutate}\ \text{Mutation}
      + \text{actionInvoke}\ \text{InvokeMethod} + \text{actionInfo}
      + \text{actionInspect}\ \text{Bool}$$ -/
inductive Action where
  | actionRead (headersOnly : Bool)
  | actionMutate (mutation : Mutation)
  | actionInvoke (method : InvokeMethod)
  | actionInfo
  | actionInspect (headersOnly : Bool)
  deriving BEq, Repr

instance : ToString Action where
  toString
    | .actionRead true => "HEAD (read)"
    | .actionRead false => "GET (read)"
    | .actionMutate m => s!"MUTATE ({m})"
    | .actionInvoke m => s!"INVOKE ({m})"
    | .actionInfo => "OPTIONS (info)"
    | .actionInspect true => "HEAD (inspect)"
    | .actionInspect false => "GET (inspect)"

-- ────────────────────────────────────────────────────────────────────
-- JSON operations for column access
-- ────────────────────────────────────────────────────────────────────

/-- JSON path operators for accessing nested fields.
    $$\text{JsonOperation} \in \{\texttt{->}\ k, \texttt{->>}\ k\}$$ -/
inductive JsonOperation where
  | arrowRight (key : String)
  | arrowRightRight (key : String)
  deriving BEq, Repr

instance : ToString JsonOperation where
  toString
    | .arrowRight key => s!"->{key}"
    | .arrowRightRight key => s!"->>{key}"

-- ────────────────────────────────────────────────────────────────────
-- Filter operators
-- ────────────────────────────────────────────────────────────────────

/-- Simple comparison and containment operators.
    $$\text{SimpleOperator} \in \{\texttt{eq}, \texttt{neq}, \texttt{gt},
      \texttt{gte}, \texttt{lt}, \texttt{lte}, \ldots\}$$ -/
inductive SimpleOperator where
  | opEqual
  | opNotEqual
  | opGreaterThan
  | opGreaterThanEqual
  | opLessThan
  | opLessThanEqual
  | opLike
  | opILike
  | opIn
  | opIs
  | opIsDistinct
  | opContains
  | opContainedIn
  | opOverlap
  | opMatch
  | opIMatch
  deriving BEq, Repr

instance : ToString SimpleOperator where
  toString
    | .opEqual => "eq"
    | .opNotEqual => "neq"
    | .opGreaterThan => "gt"
    | .opGreaterThanEqual => "gte"
    | .opLessThan => "lt"
    | .opLessThanEqual => "lte"
    | .opLike => "like"
    | .opILike => "ilike"
    | .opIn => "in"
    | .opIs => "is"
    | .opIsDistinct => "isdistinct"
    | .opContains => "cs"
    | .opContainedIn => "cd"
    | .opOverlap => "ov"
    | .opMatch => "match"
    | .opIMatch => "imatch"

/-- Full-text search operators with optional language configuration.
    $$\text{FtsOperator} \in \{\text{fts}, \text{plfts}, \text{phfts}, \text{wfts}\}
      \times \text{Option}\ \text{String}$$ -/
inductive FtsOperator where
  | fts (lang : Option String)
  | plfts (lang : Option String)
  | phfts (lang : Option String)
  | wfts (lang : Option String)
  deriving BEq, Repr

instance : ToString FtsOperator where
  toString
    | .fts lang => s!"fts{langSuffix lang}"
    | .plfts lang => s!"plfts{langSuffix lang}"
    | .phfts lang => s!"phfts{langSuffix lang}"
    | .wfts lang => s!"wfts{langSuffix lang}"
  where
    langSuffix : Option String → String
      | some l => s!"({l})"
      | none => ""

/-- Quantifier for combining operators: `any` or `all`.
    $$\text{QuantOperator} \in \{\text{any}, \text{all}\}$$ -/
inductive QuantOperator where
  | any
  | all
  deriving BEq, Repr

instance : ToString QuantOperator where
  toString
    | .any => "any"
    | .all => "all"

/-- A filter operator: simple, full-text search, or quantified.
    $$\text{FilterOperator} = \text{simple}\ \text{SimpleOperator}
      + \text{fts}\ \text{FtsOperator}
      + \text{quantified}\ \text{QuantOperator}\ \text{SimpleOperator}$$ -/
inductive FilterOperator where
  | simple (op : SimpleOperator)
  | fts (op : FtsOperator)
  | quantified (quant : QuantOperator) (op : SimpleOperator)
  deriving BEq, Repr

instance : ToString FilterOperator where
  toString
    | .simple op => toString op
    | .fts op => toString op
    | .quantified quant op => s!"{quant}.{op}"

-- ────────────────────────────────────────────────────────────────────
-- Filter
-- ────────────────────────────────────────────────────────────────────

/-- A single filter predicate on a field.
    $$\text{Filter} = \langle \text{field}, \text{jsonPath}, \text{operator},
      \text{value} \rangle$$ -/
structure Filter where
  field : String
  jsonPath : List JsonOperation := []
  operator : FilterOperator
  value : String
  deriving BEq, Repr

instance : ToString Filter where
  toString f :=
    let jp := String.join (f.jsonPath.map toString)
    s!"{f.field}{jp}.{f.operator}.{f.value}"

-- ────────────────────────────────────────────────────────────────────
-- Logic tree
-- ────────────────────────────────────────────────────────────────────

/-- Logical connectives for combining filters.
    $$\text{LogicOperator} \in \{\text{and}, \text{or}\}$$ -/
inductive LogicOperator where
  | and_
  | or_
  deriving BEq, Repr

instance : ToString LogicOperator where
  toString
    | .and_ => "and"
    | .or_ => "or"

/-- A logic tree combining filters with `and`/`or` and optional negation.
    $$\text{LogicTree} = \text{stmnt}\ \text{Filter}
      + \text{expr}\ \text{Bool}\ \text{LogicOperator}\ (\text{Array}\ \text{LogicTree})$$ -/
inductive LogicTree where
  | stmnt (filter : Filter)
  | expr (negated : Bool) (op : LogicOperator) (children : Array LogicTree)
  deriving BEq, Repr

/-- Render a `LogicTree` as a string (recursive). -/
def LogicTree.toString : LogicTree → String
  | .stmnt f => ToString.toString f
  | .expr negated op children =>
    let pfx := if negated then "not." else ""
    let childStrs := children.map LogicTree.toString
    let inner := ", ".intercalate childStrs.toList
    s!"{pfx}{op}({inner})"

instance : ToString LogicTree where
  toString := LogicTree.toString

-- ────────────────────────────────────────────────────────────────────
-- Ordering
-- ────────────────────────────────────────────────────────────────────

/-- Sort direction.
    $$\text{OrderDirection} \in \{\text{asc}, \text{desc}\}$$ -/
inductive OrderDirection where
  | asc
  | desc
  deriving BEq, Repr

instance : ToString OrderDirection where
  toString
    | .asc => "asc"
    | .desc => "desc"

/-- Null ordering preference.
    $$\text{OrderNulls} \in \{\text{nullsFirst}, \text{nullsLast}\}$$ -/
inductive OrderNulls where
  | nullsFirst
  | nullsLast
  deriving BEq, Repr

instance : ToString OrderNulls where
  toString
    | .nullsFirst => "nullsfirst"
    | .nullsLast => "nullslast"

/-- A single ordering term: column, direction, and null placement.
    $$\text{OrderTerm} = \langle \text{otTerm}, \text{otDirection},
      \text{otNulls}? \rangle$$ -/
structure OrderTerm where
  otTerm : String
  otDirection : OrderDirection := .asc
  otNulls : Option OrderNulls := none
  deriving BEq, Repr

instance : ToString OrderTerm where
  toString t :=
    let base := s!"{t.otTerm}.{t.otDirection}"
    match t.otNulls with
    | some n => s!"{base}.{n}"
    | none => base

-- ────────────────────────────────────────────────────────────────────
-- Select items
-- ────────────────────────────────────────────────────────────────────

/-- A projected item in the `select` query parameter.
    $$\text{SelectItem} = \text{star} + \text{field} + \text{spread}
      + \text{computed} + \text{relationship}$$ -/
inductive SelectItem where
  | star
  | field (name : String) (alias_ : Option String) (cast : Option String)
          (jsonPath : List JsonOperation)
  | spread (name : String) (selects : Array SelectItem)
  | computed (expr : String) (alias_ : Option String)
  | relationship (name : String) (alias_ : Option String) (hint : Option String)
                  (isInner : Bool) (selects : Array SelectItem)
  deriving BEq, Repr

/-- Render a `SelectItem` as a string (recursive). -/
def SelectItem.toString : SelectItem → String
  | .star => "*"
  | .field name alias_ cast jsonPath =>
    let aliasStr := match alias_ with | some a => s!"{a}:" | none => ""
    let castStr := match cast with | some c => s!"::{c}" | none => ""
    let jpStr := String.join (jsonPath.map ToString.toString)
    s!"{aliasStr}{name}{jpStr}{castStr}"
  | .spread name selects =>
    let sels := ", ".intercalate (selects.map SelectItem.toString).toList
    s!"...{name}({sels})"
  | .computed expr alias_ =>
    let aliasStr := match alias_ with | some a => s!"{a}:" | none => ""
    s!"{aliasStr}{expr}"
  | .relationship name alias_ hint isInner selects =>
    let aliasStr := match alias_ with | some a => s!"{a}:" | none => ""
    let hintStr := match hint with | some h => s!"!{h}" | none => ""
    let joinStr := if isInner then "!inner" else ""
    let sels := ", ".intercalate (selects.map SelectItem.toString).toList
    s!"{aliasStr}{name}{hintStr}{joinStr}({sels})"

instance : ToString SelectItem where
  toString := SelectItem.toString

-- ────────────────────────────────────────────────────────────────────
-- Payload
-- ────────────────────────────────────────────────────────────────────

/-- The request body payload in various formats.
    $$\text{Payload} = \text{jsonPayload}\ \text{String}
      + \text{urlEncodedPayload}\ (\text{List}\ (\text{String} \times \text{String}))
      + \text{rawPayload}\ \text{ByteArray}\ \text{String}$$ -/
inductive Payload where
  | jsonPayload (raw : String)
  | urlEncodedPayload (pairs : List (String × String))
  | rawPayload (bytes : ByteArray) (mediaType : String)

instance : Repr Payload where
  reprPrec p _ := match p with
    | .jsonPayload raw => .text s!"Payload.jsonPayload {repr raw}"
    | .urlEncodedPayload pairs => .text s!"Payload.urlEncodedPayload {repr pairs}"
    | .rawPayload bytes mediaType =>
      .text s!"Payload.rawPayload <{bytes.size} bytes> {repr mediaType}"

instance : BEq Payload where
  beq a b := match a, b with
    | .jsonPayload r1, .jsonPayload r2 => r1 == r2
    | .urlEncodedPayload p1, .urlEncodedPayload p2 => p1 == p2
    | .rawPayload b1 m1, .rawPayload b2 m2 => b1 == b2 && m1 == m2
    | _, _ => false

instance : ToString Payload where
  toString
    | .jsonPayload raw => s!"JSON({raw.length} chars)"
    | .urlEncodedPayload pairs => s!"URLEncoded({pairs.length} pairs)"
    | .rawPayload bytes mediaType => s!"Raw({bytes.size} bytes, {mediaType})"

-- ────────────────────────────────────────────────────────────────────
-- IS values
-- ────────────────────────────────────────────────────────────────────

/-- Values that can appear with the `IS` operator.
    $$\text{IsVal} \in \{\text{null}, \text{notNull}, \text{true}, \text{false},
      \text{unknown}\}$$ -/
inductive IsVal where
  | null_
  | notNull
  | true_
  | false_
  | unknown_
  deriving BEq, Repr

instance : ToString IsVal where
  toString
    | .null_ => "null"
    | .notNull => "not null"
    | .true_ => "true"
    | .false_ => "false"
    | .unknown_ => "unknown"

-- ────────────────────────────────────────────────────────────────────
-- Target
-- ────────────────────────────────────────────────────────────────────

/-- The target resource of an API request: a table/view or an RPC function.
    $$\text{Target} = \text{table}\ \text{QualifiedIdentifier}
      + \text{routine}\ \text{QualifiedIdentifier}$$ -/
inductive Target where
  | table (qi : PostgREST.SchemaCache.Identifiers.QualifiedIdentifier)
  | routine (qi : PostgREST.SchemaCache.Identifiers.QualifiedIdentifier)
  deriving BEq, Repr

instance : ToString Target where
  toString
    | .table qi => s!"table {qi}"
    | .routine qi => s!"routine {qi}"

end PostgREST.ApiRequest
