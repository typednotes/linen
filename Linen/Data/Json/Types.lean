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

end Value

-- ── BEq instance ──────────────────────────────────────────────────────

/-- Structural equality for JSON values. -/
private partial def Value.beqImpl : Value → Value → Bool
  | .null, .null => true
  | .bool a, .bool b => a == b
  | .string a, .string b => a == b
  | .number a, .number b => a == b
  | .array a, .array b =>
      a.size == b.size &&
      let pairs := a.zip b
      pairs.all fun (x, y) => Value.beqImpl x y
  | .object a, .object b =>
      a.length == b.length &&
      a.all fun (k, v) =>
        match b.lookup k with
        | some v' => Value.beqImpl v v'
        | none => false
  | _, _ => false

instance : BEq Value where
  beq := Value.beqImpl

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
  simp [FromJSON.parseJSON]

/-- `Bool` roundtrips through `ToJSON`/`FromJSON`.
    $$\text{parseJSON}(\text{toJSON}(b)) = \text{ok}(b)$$ -/
theorem roundtrip_bool (b : Bool) : FromJSON.parseJSON (ToJSON.toJSON b) = .ok b := by
  simp [FromJSON.parseJSON]

/-- `Float` roundtrips through `ToJSON`/`FromJSON`.
    $$\text{parseJSON}(\text{toJSON}(n)) = \text{ok}(n)$$ -/
theorem roundtrip_float (n : Float) : FromJSON.parseJSON (ToJSON.toJSON n) = .ok n := by
  simp [FromJSON.parseJSON]

/-- `Value` roundtrips through `ToJSON`/`FromJSON` (identity).
    $$\text{toJSON}(v) = v$$ -/
theorem toJSON_value_id (v : Value) : ToJSON.toJSON v = v := rfl

/-- `Option none` roundtrips through `ToJSON`/`FromJSON`.
    $$\text{parseJSON}(\text{toJSON}(\text{none})) = \text{ok}(\text{none})$$ -/
theorem roundtrip_option_none [FromJSON α] [ToJSON α] :
    @FromJSON.parseJSON (Option α) _ (ToJSON.toJSON (none : Option α)) = .ok none := by
  simp [FromJSON.parseJSON]

end Data.Json
