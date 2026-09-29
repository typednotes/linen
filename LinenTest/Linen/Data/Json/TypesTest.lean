/-
  Tests for `Linen.Data.Json.Types`.

  Covers the JSON value AST, predicates, accessors, object field access,
  the `ToJSON`/`FromJSON` typeclasses and their instances, the construction
  helpers, and `DecidableEq Value`.

  Correctness checks use `#guard`, which evaluates the (decidable) proposition
  with the compiled interpreter and fails the build if it is not `true`.
-/
import Linen.Data.Json

open Data.Json

namespace Tests.Json.Types

-- ── Predicates ────────────────────────────────────────────────────────

#guard Value.isNull .null
#guard !Value.isNull (.bool true)

#guard Value.isString (.string "hi")
#guard !Value.isString .null

#guard Value.isNumber (.number 3)
#guard !Value.isNumber (.bool false)

#guard Value.isBool (.bool true)
#guard !Value.isBool (.number 0)

#guard Value.isObject (.object [])
#guard !Value.isObject (.array #[])

#guard Value.isArray (.array #[])
#guard !Value.isArray (.object [])

-- ── Accessors ─────────────────────────────────────────────────────────

#guard Value.asString (.string "x") = some "x"
#guard Value.asString .null = none

#guard Value.asNumber (.number 3.5) = some 3.5
#guard Value.asNumber (.bool true) = none

#guard Value.asBool (.bool false) = some false
#guard Value.asBool (.number 1) = none

#guard Value.asObject (.object [("k", .null)]) = some [("k", .null)]
#guard Value.asObject (.array #[]) = none

#guard Value.asArray (.array #[.number 1]) = some #[.number 1]
#guard Value.asArray (.string "no") = none

-- ── Object field access ───────────────────────────────────────────────

private def sample : Value :=
  .object [("name", .string "linen"), ("n", .number 1), ("flag", .null)]

#guard Value.lookup "name" sample = some (.string "linen")
#guard Value.lookup "missing" sample = none
#guard Value.lookup "x" (.array #[]) = none  -- lookup on a non-object is `none`

#guard sample.getField "n" = .ok (.number 1)
#guard (sample.getField "absent").toOption = none  -- required field: error when missing

#guard sample.getFieldOpt "name" = .ok (some (.string "linen"))
#guard sample.getFieldOpt "absent" = .ok none      -- optional field: missing → none
#guard sample.getFieldOpt "flag" = .ok none        -- optional field: explicit null → none

-- ── ToJSON instances ──────────────────────────────────────────────────

#guard ToJSON.toJSON "hello" = Value.string "hello"
#guard ToJSON.toJSON (42 : Int) = Value.number 42
#guard ToJSON.toJSON (-7 : Int) = Value.number (-7)
#guard ToJSON.toJSON (5 : Nat) = Value.number 5
#guard ToJSON.toJSON (3.25 : Float) = Value.number 3.25
#guard ToJSON.toJSON true = Value.bool true
#guard ToJSON.toJSON (Value.null) = Value.null  -- ToJSON Value is the identity
#guard FromJSON.parseJSON (α := Value) (Value.bool true) = .ok (Value.bool true)  -- likewise FromJSON

-- Pairs encode as 2-element JSON arrays, matching Haskell's `(a, b)` instances.
#guard ToJSON.toJSON (("k", "v") : String × String) = Value.array #[Value.string "k", Value.string "v"]
#guard FromJSON.parseJSON (α := String × String) (Value.array #[Value.string "k", Value.string "v"])
  = .ok ("k", "v")

#guard ToJSON.toJSON (some (9 : Int)) = Value.number 9
#guard ToJSON.toJSON (none : Option Int) = Value.null

#guard ToJSON.toJSON #[(1 : Int), 2, 3] = Value.array #[.number 1, .number 2, .number 3]
#guard ToJSON.toJSON [(1 : Int), 2] = Value.array #[.number 1, .number 2]

-- ── FromJSON instances ────────────────────────────────────────────────

#guard (FromJSON.parseJSON (.string "hi") : Except String String) = .ok "hi"
#guard ((FromJSON.parseJSON (.number 5) : Except String Int)) = .ok 5
#guard ((FromJSON.parseJSON (.number 3.9) : Except String Int)) = .ok 3  -- truncation toward zero
#guard ((FromJSON.parseJSON (.number 6) : Except String Nat)) = .ok 6
#guard ((FromJSON.parseJSON (.number (-2)) : Except String Nat).toOption = none)  -- negative → error
#guard ((FromJSON.parseJSON (.number 2.5) : Except String Float)) = .ok 2.5
#guard ((FromJSON.parseJSON (.bool true) : Except String Bool)) = .ok true

-- type-mismatch produces an error
#guard ((FromJSON.parseJSON (.number 1) : Except String String).toOption = none)

#guard ((FromJSON.parseJSON .null : Except String (Option Int)) = .ok none)
#guard ((FromJSON.parseJSON (.number 7) : Except String (Option Int)) = .ok (some 7))

#guard ((FromJSON.parseJSON (.array #[.number 1, .number 2]) : Except String (List Int)) = .ok [1, 2])
#guard ((FromJSON.parseJSON (.array #[.number 1, .number 2]) : Except String (Array Int)) = .ok #[1, 2])
#guard ((FromJSON.parseJSON (.array #[.number 1, .string "x"]) : Except String (List Int)).toOption = none)

-- ── Object field update ───────────────────────────────────────────────

-- Every other field survives, in its order: the property a read-modify-write
-- of a whole policy object rests on.
#guard (Value.object [("a", .string "1"), ("b", .string "2"), ("c", .null)]).setField "b" (.string "9")
     = .object [("a", .string "1"), ("b", .string "9"), ("c", .null)]
-- Absent: appended at the end.
#guard (Value.object [("a", .string "1")]).setField "b" (.string "2")
     = .object [("a", .string "1"), ("b", .string "2")]
-- Not an object: unchanged.
#guard (Value.array #[.null]).setField "b" (.string "2") = .array #[.null]
-- A rewritten field is what `lookup` then reads.
#guard ((Value.object [("k", .number 1)]).setField "k" (.number 2)).lookup "k" = some (.number 2)

-- ── Lenient scalar reads ──────────────────────────────────────────────

#guard (Value.object [("id", .string "x")]).lookupText "id" = some "x"
#guard (Value.object [("id", .bool true)]).lookupText "id" = some "true"
#guard ((Value.object [("id", .number 3)]).lookupText "id").isSome
#guard (Value.object [("id", .null)]).lookupText "id" = none
#guard (Value.object []).lookupText "id" = none
#guard (Value.object [("n", .number 3)]).lookupNat "n" = some 3
#guard (Value.object [("n", .string "3")]).lookupNat "n" = some 3
#guard (Value.object [("n", .string "x")]).lookupNat "n" = none
#guard (Value.object [("b", .string "true")]).lookupBool "b" = some true
#guard (Value.object [("b", .bool false)]).lookupBool "b" = some false
#guard (Value.object [("b", .string "yes")]).lookupBool "b" = none

-- ── Construction helpers ──────────────────────────────────────────────

#guard object [("a", .number 1)] = Value.object [("a", .number 1)]
#guard emptyObject = Value.object []
#guard emptyArray = Value.array #[]
#guard pair "k" (5 : Int) = ("k", Value.number 5)
#guard pair "s" "v" = ("s", Value.string "v")

-- ── DecidableEq Value ─────────────────────────────────────────────────

#guard (Value.null = Value.null)
#guard (Value.number 1 = Value.number 1)
#guard (Value.number 1 ≠ Value.number 2)
#guard (Value.string "a" ≠ Value.string "b")
#guard (Value.bool true ≠ Value.bool false)
#guard (Value.array #[.number 1, .null] = Value.array #[.number 1, .null])
#guard (Value.array #[.number 1] ≠ Value.array #[.number 2])
#guard (Value.object [("a", .number 1)] = Value.object [("a", .number 1)])
#guard (Value.object [("a", .number 1)] ≠ Value.object [("a", .number 2)])
#guard (Value.null ≠ Value.bool false)  -- distinct constructors

end Tests.Json.Types
