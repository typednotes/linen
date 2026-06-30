/-
  Linen.Database.SQL.Encoders — Parameter encoders

  Composable encoders for converting Lean values into SQL parameter
  arrays suitable for `LibPQ.execParams`.  Each encoder serializes one or
  more values as `Option String` (where `none` represents SQL NULL).

  ## Haskell source
  - `Hasql.Encoders` (hasql package)
-/

namespace Database.SQL.Encoders

-- ════════════════════════════════════════════════════════════════════
-- Param encoder
-- ════════════════════════════════════════════════════════════════════

/-- A parameter encoder converts a typed value to an array of SQL parameter strings. -/
structure Params (α : Type) where
  encode : α → Array (Option String)
  width : Nat

namespace Params

/-- Encode no parameters. -/
def none : Params Unit :=
  { encode := fun () => #[], width := 0 }

/-- Encode a non-nullable text parameter. -/
def text : Params String :=
  { encode := fun s => #[some s], width := 1 }

/-- Encode a non-nullable integer parameter. -/
def int : Params Int :=
  { encode := fun n => #[some (toString n)], width := 1 }

/-- Encode a non-nullable natural number parameter. -/
def nat : Params Nat :=
  { encode := fun n => #[some (toString n)], width := 1 }

/-- Encode a non-nullable float parameter. -/
def float : Params Float :=
  { encode := fun f => #[some (toString f)], width := 1 }

/-- Encode a non-nullable boolean parameter. -/
def bool : Params Bool :=
  { encode := fun b => #[some (if b then "t" else "f")], width := 1 }

/-- Make any encoder nullable. `none` encodes as SQL NULL. -/
def nullable (inner : Params α) : Params (Option α) :=
  { encode := fun
    | Option.some a => inner.encode a
    | Option.none => Array.replicate inner.width Option.none
    width := inner.width }

/-- Contramap the input of an encoder. -/
def contramap (f : β → α) (enc : Params α) : Params β :=
  { encode := fun b => enc.encode (f b), width := enc.width }

/-- Combine two encoders for a pair. -/
def pair (ea : Params α) (eb : Params β) : Params (α × β) :=
  { encode := fun (a, b) => ea.encode a ++ eb.encode b
    width := ea.width + eb.width }

/-- Combine three encoders for a triple. -/
def triple (ea : Params α) (eb : Params β) (ec : Params γ) : Params (α × β × γ) :=
  { encode := fun (a, b, c) => ea.encode a ++ eb.encode b ++ ec.encode c
    width := ea.width + eb.width + ec.width }

/-- Encode a value using its `ToString` instance. -/
def ofToString [ToString α] : Params α :=
  { encode := fun a => #[some (toString a)], width := 1 }

/-- `none` encoder has width 0. -/
theorem none_width : Params.none.width = 0 := rfl

/-- `text` encoder has width 1. -/
theorem text_width : Params.text.width = 1 := rfl

/-- `int` encoder has width 1. -/
theorem int_width : Params.int.width = 1 := rfl

/-- `nat` encoder has width 1. -/
theorem nat_width : Params.nat.width = 1 := rfl

/-- `bool` encoder has width 1. -/
theorem bool_width : Params.bool.width = 1 := rfl

/-- `nullable` preserves width. -/
theorem nullable_width (inner : Params α) : (Params.nullable inner).width = inner.width := rfl

/-- `contramap` preserves width. -/
theorem contramap_width (f : β → α) (enc : Params α) : (Params.contramap f enc).width = enc.width := rfl

/-- `pair` width is the sum of its constituents' widths. -/
theorem pair_width (ea : Params α) (eb : Params β) : (Params.pair ea eb).width = ea.width + eb.width := rfl

/-- `triple` width is the sum of all three widths. -/
theorem triple_width (ea : Params α) (eb : Params β) (ec : Params γ) :
    (Params.triple ea eb ec).width = ea.width + eb.width + ec.width := rfl

end Params
end Database.SQL.Encoders
