/-
JSON Types
-/

namespace Data.Json

-- JSON value AST
inductive Value where
  | object (fields : List (String × Value))
  | array (elements : Array Value)
  | string (s : String)
  | number (n : Float)
  | bool (b : Bool)
  | null
  deriving Repr, Inhabited

/-- Convenience alias for JSON object fields. -/
abbrev Object := List (String × Value)

namespace Value

-- ── Predicates ────────────────────────────────────────────────────────

/-- $$\text{isNull} : \text{Value} \to \text{Bool}$$ -/
def isNull : Value → Bool | .null => true | _ => false

/-- $$\text{isString} : \text{Value} \to \text{Bool}$$ -/
def isString : Value → Bool | .string _ => true | _ => false

/-- $$\text{isNumber} : \text{Value} \to \text{Bool}$$ -/
def isNumber : Value → Bool | .number _ => true | _ => false

/-- $$\text{isBool} : \text{Value} \to \text{Bool}$$ -/
def isBool : Value → Bool | .bool _ => true | _ => false

/-- $$\text{isObject} : \text{Value} \to \text{Bool}$$ -/
def isObject : Value → Bool | .object _ => true | _ => false

/-- $$\text{isArray} : \text{Value} \to \text{Bool}$$ -/
def isArray : Value → Bool | .array _ => true | _ => false

-- ── Accessors ─────────────────────────────────────────────────────────

/-- $$\text{asString} : \text{Value} \to \text{Option String}$$ -/
def asString : Value → Option String | .string s => some s | _ => none

/-- $$\text{asNumber} : \text{Value} \to \text{Option Float}$$ -/
def asNumber : Value → Option Float | .number n => some n | _ => none

/-- $$\text{asBool} : \text{Value} \to \text{Option Bool}$$ -/
def asBool : Value → Option Bool | .bool b => some b | _ => none

/-- $$\text{asObject} : \text{Value} \to \text{Option Object}$$ -/
def asObject : Value → Option Object | .object o => some o | _ => none

/-- $$\text{asArray} : \text{Value} \to \text{Option (Array Value)}$$ -/
def asArray : Value → Option (Array Value) | .array a => some a | _ => none

-- ── Object field access ───────────────────────────────────────────────

/-- Look up a key in a JSON object.
    $$\text{lookup} : \text{String} \to \text{Value} \to \text{Option Value}$$ -/
def lookup (key : String) : Value → Option Value
  | .object fields => fields.lookup key
  | _ => none

/-- Required field access, analogous to Haskell's `(.:)`.
    $$\text{getField} : \text{Value} \to \text{String} \to \text{Except String Value}$$ -/
def getField (obj : Value) (key : String) : Except String Value :=
  match obj.lookup key with
  | some v => .ok v
  | none => .error s!"key '{key}' not found"

/-- Optional field access, analogous to Haskell's `(.:?)`.
    Returns `none` for missing keys and explicit nulls.
    $$\text{getFieldOpt} : \text{Value} \to \text{String} \to \text{Except String (Option Value)}$$ -/
def getFieldOpt (obj : Value) (key : String) : Except String (Option Value) :=
  match obj.lookup key with
  | some .null => .ok none
  | some v => .ok (some v)
  | none => .ok none

/-- A successful `List.lookup` witnesses membership of the `(key, value)` pair. -/
private theorem _list_lookup_mem {α : Type} {key : String} {fields : List (String × α)} {v : α}
    (h : fields.lookup key = some v) : (key, v) ∈ fields := by
  induction fields with
  | nil => simp at h
  | cons hd tl ih =>
    obtain ⟨k, val⟩ := hd
    simp only [List.lookup] at h
    split at h
    · simp only [Option.some.injEq] at h
      subst h
      rename_i heq
      simp only [beq_iff_eq] at heq
      subst heq
      exact List.mem_cons_self
    · exact List.mem_cons_of_mem _ (ih h)

/-- A successful `getField` always returns a strictly structurally-smaller
    `Value` than the object it was looked up in. Lets a self-referential JSON
    record type (one with a field of its own list-of-self type, e.g. a tree —
    `CDP.Domains.Media.PlayerError.cause` is one example) write a plain
    recursive `FromJSON` instance and discharge its `termination_by`/
    `decreasing_by` obligations with this lemma, instead of needing to
    re-derive a `sizeOf` argument through `getField` from scratch. -/
theorem getField_sizeOf_lt {obj : Value} {key : String} {v : Value}
    (h : obj.getField key = .ok v) : sizeOf v < sizeOf obj := by
  cases obj with
  | object fields =>
    simp only [getField, lookup] at h
    cases hl : fields.lookup key with
    | none => rw [hl] at h; simp at h
    | some v' =>
      rw [hl] at h
      simp only [Except.ok.injEq] at h
      subst h
      have hmem := _list_lookup_mem hl
      have h1 := List.sizeOf_lt_of_mem hmem
      simp only [Prod.mk.sizeOf_spec] at h1
      show sizeOf v' < sizeOf (Value.object fields)
      simp only [Value.object.sizeOf_spec]
      omega
  | _ => simp [getField, lookup] at h

/-- Like `getField_sizeOf_lt`, but for a field accessed via plain `lookup`
    (e.g. an optional self-referential field, where the caller wants to treat
    a missing key or explicit `null` as `none` directly rather than going
    through `getFieldOpt`). -/
theorem lookup_sizeOf_lt {obj : Value} {key : String} {v : Value}
    (h : obj.lookup key = some v) : sizeOf v < sizeOf obj := by
  cases obj with
  | object fields =>
    simp only [lookup] at h
    have hmem := _list_lookup_mem h
    have h1 := List.sizeOf_lt_of_mem hmem
    simp only [Prod.mk.sizeOf_spec] at h1
    show sizeOf v < sizeOf (Value.object fields)
    simp only [Value.object.sizeOf_spec]
    omega
  | _ => simp [lookup] at h

end Value

-- ── DecidableEq for Float ────────────────────────────────────────────

/-- Two IEEE 754 floats with identical bit representations are equal.
    This is a sound axiom: `Float.toBits` is the 64-bit representation. -/
axiom Float.eq_of_toBits_eq : ∀ (a b : Float), a.toBits = b.toBits → a = b

instance : DecidableEq Float := fun a b =>
  if h : a.toBits = b.toBits then
    isTrue (Float.eq_of_toBits_eq a b h)
  else
    isFalse (fun hab => h (congrArg Float.toBits hab))

-- ── DecidableEq for Value ───────────────────────────────────────────

private def Value.decEq : (a b : Value) → Decidable (a = b)
  | .null, .null => isTrue rfl
  | .bool a, .bool b =>
    if h : a = b then isTrue (congrArg Value.bool h) else isFalse (h ∘ Value.bool.inj)
  | .string a, .string b =>
    if h : a = b then isTrue (congrArg Value.string h) else isFalse (h ∘ Value.string.inj)
  | .number a, .number b =>
    if h : a = b then isTrue (congrArg Value.number h) else isFalse (h ∘ Value.number.inj)
  | .array a, .array b =>
    match decEqValueList a.toList b.toList with
    | isTrue h => isTrue (congrArg Value.array (congrArg Array.mk h))
    | isFalse h => isFalse (h ∘ congrArg Array.toList ∘ Value.array.inj)
  | .object a, .object b =>
    match decEqFieldList a b with
    | isTrue h => isTrue (congrArg Value.object h)
    | isFalse h => isFalse (h ∘ Value.object.inj)
  | .null, .bool _ | .null, .string _ | .null, .number _ | .null, .array _ | .null, .object _
  | .bool _, .null | .bool _, .string _ | .bool _, .number _ | .bool _, .array _ | .bool _, .object _
  | .string _, .null | .string _, .bool _ | .string _, .number _ | .string _, .array _ | .string _, .object _
  | .number _, .null | .number _, .bool _ | .number _, .string _ | .number _, .array _ | .number _, .object _
  | .array _, .null | .array _, .bool _ | .array _, .string _ | .array _, .number _ | .array _, .object _
  | .object _, .null | .object _, .bool _ | .object _, .string _ | .object _, .number _ | .object _, .array _
    => isFalse nofun
where
  decEqValueList : (a b : List Value) → Decidable (a = b)
    | [], [] => isTrue rfl
    | [], _ :: _ | _ :: _, [] => isFalse nofun
    | x :: xs, y :: ys =>
      match Value.decEq x y, decEqValueList xs ys with
      | isTrue hx, isTrue hxs => isTrue (hx ▸ hxs ▸ rfl)
      | isFalse hx, _ => isFalse (fun h => by cases h; exact hx rfl)
      | _, isFalse hxs => isFalse (fun h => by cases h; exact hxs rfl)
  decEqFieldList : (a b : List (String × Value)) → Decidable (a = b)
    | [], [] => isTrue rfl
    | [], _ :: _ | _ :: _, [] => isFalse nofun
    | (k1, v1) :: rest1, (k2, v2) :: rest2 =>
      if hk : k1 = k2 then
        match Value.decEq v1 v2 with
        | isTrue hv =>
          match decEqFieldList rest1 rest2 with
          | isTrue hr => isTrue (hk ▸ hv ▸ hr ▸ rfl)
          | isFalse hr => isFalse (fun h => by cases h; exact hr rfl)
        | isFalse hv => isFalse (fun h => by cases h; exact hv rfl)
      else
        isFalse (fun h => by cases h; exact hk rfl)

instance : DecidableEq Value := Value.decEq

-- ── Typeclasses ───────────────────────────────────────────────────────

/-- Typeclass for serializing a value to JSON.
    $$\text{ToJSON}\ \alpha : \alpha \to \text{Value}$$ -/
class ToJSON (α : Type) where
  toJSON : α → Value

/-- Typeclass for deserializing a value from JSON.
    $$\text{FromJSON}\ \alpha : \text{Value} \to \text{Except String}\ \alpha$$ -/
class FromJSON (α : Type) where
  parseJSON : Value → Except String α

-- ── Basic ToJSON instances ────────────────────────────────────────────

instance : ToJSON String where toJSON := .string
instance : ToJSON Int where toJSON n := .number (Float.ofInt n)
instance : ToJSON Nat where toJSON n := .number (Float.ofNat n)
instance : ToJSON Float where toJSON := .number
instance : ToJSON Bool where toJSON := .bool
instance : ToJSON Value where toJSON := id

-- ── Helpers ──────────────────────────────────────────────────────────

/-- Convert a `Float` to `Int` by truncating toward zero.
    Uses the string representation to avoid depending on Float.toUInt64. -/
private def floatToInt (f : Float) : Int :=
  -- Render as string, split at '.', parse integer part
  let s := toString f
  let intPart := match s.splitOn "." with
    | [whole] => whole
    | [whole, _] => whole
    | _ => s
  intPart.toInt!  -- safe: Float.toString always produces a valid number string

-- ── Basic FromJSON instances ──────────────────────────────────────────

instance : FromJSON String where
  parseJSON
    | .string s => .ok s
    | v => .error s!"expected string, got {repr v}"

instance : FromJSON Int where
  parseJSON
    | .number n => .ok (floatToInt n)
    | v => .error s!"expected number, got {repr v}"

instance : FromJSON Nat where
  parseJSON
    | .number n =>
      let i := floatToInt n
      if i >= 0 then .ok i.toNat
      else .error s!"expected non-negative number, got {repr n}"
    | v => .error s!"expected number, got {repr v}"

instance : FromJSON Float where
  parseJSON
    | .number n => .ok n
    | v => .error s!"expected number, got {repr v}"

instance : FromJSON Bool where
  parseJSON
    | .bool b => .ok b
    | v => .error s!"expected bool, got {repr v}"

/-- The identity decoding, for callers that want the raw JSON tree of an
    open-ended field (symmetric with the `ToJSON Value` instance above). -/
instance : FromJSON Value where parseJSON := .ok

-- ── Option instances ──────────────────────────────────────────────────

instance [FromJSON α] : FromJSON (Option α) where
  parseJSON
    | .null => .ok none
    | v => (FromJSON.parseJSON v).map some

instance [ToJSON α] : ToJSON (Option α) where
  toJSON
    | some a => ToJSON.toJSON a
    | none => .null

-- ── Collection instances ──────────────────────────────────────────────

instance [ToJSON α] : ToJSON (Array α) where
  toJSON arr := .array (arr.map ToJSON.toJSON)

instance [FromJSON α] : FromJSON (Array α) where
  parseJSON
    | .array arr => arr.foldlM (init := #[]) fun acc v => do
        let a ← FromJSON.parseJSON v
        return acc.push a
    | v => .error s!"expected array, got {repr v}"

instance [ToJSON α] : ToJSON (List α) where
  toJSON l := .array (l.map ToJSON.toJSON |>.toArray)

instance [FromJSON α] : FromJSON (List α) where
  parseJSON
    | .array arr => arr.toList.mapM FromJSON.parseJSON
    | v => .error s!"expected array, got {repr v}"

/-- A pair is encoded as a 2-element JSON array, matching Haskell's `(a, b)`
    `ToJSON`/`FromJSON` instances. -/
instance [ToJSON α] [ToJSON β] : ToJSON (α × β) where
  toJSON p := .array #[ToJSON.toJSON p.1, ToJSON.toJSON p.2]

instance [FromJSON α] [FromJSON β] : FromJSON (α × β) where
  parseJSON
    | .array #[a, b] => return (← FromJSON.parseJSON a, ← FromJSON.parseJSON b)
    | v => .error s!"expected a 2-element array, got {repr v}"

-- ── Construction helpers ──────────────────────────────────────────────

/-- Construct a JSON object from key-value pairs.
    $$\text{object} : \text{List (String × Value)} \to \text{Value}$$ -/
def object (pairs : List (String × Value)) : Value := .object pairs

/-- The empty JSON object `{}`. -/
def emptyObject : Value := .object []

/-- The empty JSON array `[]`. -/
def emptyArray : Value := .array #[]

/-- Build a key-value pair for JSON object construction,
    analogous to Haskell's `(.=)` operator.
    $$\text{pair} : \text{String} \to \alpha \to (\text{String} \times \text{Value})$$ -/
def pair (key : String) [ToJSON α] (val : α) : String × Value :=
  (key, ToJSON.toJSON val)

-- ── Value predicate proofs ──────────────────────────────────────────

/-- `null` is null.
    $$\text{isNull}(\text{null}) = \text{true}$$ -/
theorem Value.isNull_null : Value.isNull .null = true := rfl

/-- `string s` is a string.
    $$\text{isString}(\text{string}(s)) = \text{true}$$ -/
theorem Value.isString_string (s : String) : Value.isString (.string s) = true := rfl

/-- `bool b` is a bool.
    $$\text{isBool}(\text{bool}(b)) = \text{true}$$ -/
theorem Value.isBool_bool (b : Bool) : Value.isBool (.bool b) = true := rfl

/-- `number n` is a number.
    $$\text{isNumber}(\text{number}(n)) = \text{true}$$ -/
theorem Value.isNumber_number (n : Float) : Value.isNumber (.number n) = true := rfl

/-- `object fs` is an object.
    $$\text{isObject}(\text{object}(fs)) = \text{true}$$ -/
theorem Value.isObject_object (fs : Object) : Value.isObject (.object fs) = true := rfl

/-- `array elems` is an array.
    $$\text{isArray}(\text{array}(elems)) = \text{true}$$ -/
theorem Value.isArray_array (elems : Array Value) : Value.isArray (.array elems) = true := rfl

-- ── ToJSON / FromJSON roundtrip proofs ──────────────────────────────

/-- `String` roundtrips through `ToJSON`/`FromJSON`.
    $$\text{parseJSON}(\text{toJSON}(s)) = \text{ok}(s)$$ -/
theorem roundtrip_string (s : String) : FromJSON.parseJSON (ToJSON.toJSON s) = .ok s := by
  simp [ToJSON.toJSON, FromJSON.parseJSON]

/-- `Bool` roundtrips through `ToJSON`/`FromJSON`.
    $$\text{parseJSON}(\text{toJSON}(b)) = \text{ok}(b)$$ -/
theorem roundtrip_bool (b : Bool) : FromJSON.parseJSON (ToJSON.toJSON b) = .ok b := by
  simp [ToJSON.toJSON, FromJSON.parseJSON]

/-- `Float` roundtrips through `ToJSON`/`FromJSON`.
    $$\text{parseJSON}(\text{toJSON}(n)) = \text{ok}(n)$$ -/
theorem roundtrip_float (n : Float) : FromJSON.parseJSON (ToJSON.toJSON n) = .ok n := by
  simp [ToJSON.toJSON, FromJSON.parseJSON]

/-- `Value` roundtrips through `ToJSON`/`FromJSON` (identity).
    $$\text{toJSON}(v) = v$$ -/
theorem toJSON_value_id (v : Value) : ToJSON.toJSON v = v := rfl

/-- `Option none` roundtrips through `ToJSON`/`FromJSON`.
    $$\text{parseJSON}(\text{toJSON}(\text{none})) = \text{ok}(\text{none})$$ -/
theorem roundtrip_option_none [FromJSON α] [ToJSON α] :
    @FromJSON.parseJSON (Option α) _ (ToJSON.toJSON (none : Option α)) = .ok none := by
  simp [ToJSON.toJSON, FromJSON.parseJSON]

end Data.Json
