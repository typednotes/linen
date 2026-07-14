/-
  Linen.Data.Text.Lens — `packed`, `unpacked`, and `Cons`/`Snoc`/`Ixed`
  instances for `Linen.Data.Text`

  Port of Hackage's `lens-5.3.6`'s `Data.Text.Lens` (`Data.Text.Strict.Lens`
  upstream; fetched and read via Hackage's rendered source). Upstream's real
  content:

  ```
  packed   :: Iso' String Text
  unpacked :: Iso' Text String

  instance Cons Text Text Char Char
  instance Snoc Text Text Char Char
  instance Ixed Text  -- Index = Int, IxValue = Char
  ```

  translated against `Linen.Data.Text`'s `Text` (`abbrev Text := String`).

  **Deviation (`packed`/`unpacked`, the identity `Iso`).** Since `linen`'s
  `Text` *is* `String` (a transparent `abbrev`, unlike GHC's own packed
  `Text` representation), `packed`/`unpacked` are both literally `iso id
  id` here — a faithful, if degenerate, port: upstream's own `Iso'` still
  states the identical isomorphism, just between two representationally
  distinct types.

  **Scope note (`AsEmpty Text`).** Not repeated here: `Linen.Control.Lens.
  Empty`'s `instAsEmptyString` already covers `Text` directly, since `Text`
  unfolds to `String` (the same instance-sharing this module's `packed`/
  `unpacked` above already rely on). -/

import Linen.Control.Lens.At
import Linen.Control.Lens.Cons
import Linen.Control.Lens.Iso
import Linen.Data.Text

namespace Control.Lens

open Data (Text)

-- ── packed / unpacked ────────────────────────────

/-- `packed :: Iso' String Text` — the identity, since `Text := String`
    (see the module doc comment). -/
@[inline] def packed : Iso' String Text := iso id id

/-- `unpacked :: Iso' Text String` — `from packed`, again the identity. -/
@[inline] def unpacked : Iso' Text String := iso id id

-- ── Cons / Snoc ──────────────────────────────────

/-- `instance Cons Text Text Char Char` — `Data.Text.uncons`/prepending a
    `Char` via `String.cons` (Lean's `String` has no dedicated "cons"; a
    single-`Char` `String` concatenated on the left is Lean's own idiom). -/
instance instConsText : Cons Text Char Char Text where
  _Cons := prism (fun p => p.1.toString ++ p.2) (fun t =>
    match Data.Text.uncons t with
    | some (c, rest) => .inr (c, rest)
    | none => .inl "")

/-- `instance Snoc Text Text Char Char` — `Data.Text.unsnoc`/appending a
    `Char` via `String.push`. -/
instance instSnocText : Snoc Text Char Char Text where
  _Snoc := prism (fun p => p.1.push p.2) (fun t =>
    match Data.Text.unsnoc t with
    | some (rest, c) => .inr (rest, c)
    | none => .inl "")

-- ── Ixed ─────────────────────────────────────────

/-- `instance Ixed Text` (`Index Text = Int`, `IxValue Text = Char`,
    narrowed to `Nat`) — reads via `Data.Text.index`; writing at an in-range
    position rebuilds the string from its `Char` list with that position
    replaced, leaving `t` untouched when `i` is out of range. -/
instance instIxedText : Ixed Text Nat Char where
  ix i := fun {F} [Applicative F] (f : Char → F Char) (t : Text) =>
    match Data.Text.index t i with
    | some c =>
      (fun c' =>
        String.ofList (t.toList.set i c'))
        <$> f c
    | none => pure t

end Control.Lens
