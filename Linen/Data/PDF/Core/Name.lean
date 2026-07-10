/-
  Data.PDF.Core.Name — atomic PDF name objects

  Ports `Pdf.Core.Name` from Hackage's `pdf-toolbox-core`
  (https://github.com/Yuras/pdf-toolbox, `core/lib/Pdf/Core/Name.hs`), the
  first module of the `pdf-toolbox-core` import documented in
  `docs/imports/PdfToolboxCore/dependencies.md`.

  A PDF *name* (PDF32000-1:2008 §7.3.5) is an atomic symbol, most often used
  as a dictionary key. Names are just raw bytes with one well-formedness
  rule: byte `0x00` may never appear inside one.

  ## Design

  Wraps `linen`'s own `Data.ByteString` (rather than re-deriving a bespoke
  byte container) per the stdlib-substitution rule. `BEq`/`Ord`/`Repr`/
  `Hashable` are all derived structurally from the wrapped field, mirroring
  upstream's `deriving (Eq, Show, Ord, Monoid, Semigroup, Hashable)` — a later
  `Object` module puts `Name` to work as a `Std.HashMap` key precisely
  because of this `Hashable`/`BEq` pair. `linen` has no general `Semigroup`/
  `Monoid` type classes, so upstream's derived `Monoid`/`Semigroup` instances
  (name concatenation, with the empty name as identity) are exposed directly
  as ordinary functions instead, matching this project's usual substitute for
  those instances elsewhere.

  The constructor is private, so every `Name` outside this module is built
  through `make` — mirroring upstream's export list, which omits the `Name`
  data constructor and exports only `Name, make, toByteString`. Upstream's
  `IsString` instance (a *partial* function — it calls Haskell's `error` on
  an embedded `0x00`) is not ported: `linen` has no string-literal-overloading
  mechanism to hang it on, and the total `make` below is the safe substitute
  everywhere a literal would have been used.
-/
import Linen.Data.ByteString

namespace Data.PDF.Core.Name

/-- An atomic PDF name: raw bytes with no embedded `0x00`.
    Two `Name`s are equal exactly when their underlying bytes are equal. -/
structure Name where
  private mk ::
  /-- The raw bytes (never containing a `0x00` byte). -/
  bytes : Data.ByteString
deriving BEq, Ord, Repr, Hashable

namespace Name

-- ── Construction ──

/-- Build a name from raw bytes. Fails if the bytes contain a `0x00` byte —
    disallowed inside PDF names (PDF32000-1:2008 §7.3.5). -/
def make (bs : Data.ByteString) : Except String Name :=
  if Data.ByteString.any (· == 0) bs then
    .error "Name.make: 0 byte is not allowed"
  else
    .ok ⟨bs⟩

-- ── Destruction ──

/-- Unwrap a name to its raw bytes. -/
def toByteString (n : Name) : Data.ByteString := n.bytes

-- ── Substitutes for upstream's `Semigroup`/`Monoid` instances ──

/-- The empty name (upstream's `Monoid` identity, `mempty`). -/
def empty : Name := ⟨Data.ByteString.empty⟩

/-- Concatenate two names' underlying bytes (upstream's `Semigroup`/`Monoid`
    `(<>)`). The result is always well-formed: neither operand contained a
    `0x00` byte, so neither does their concatenation. -/
def append (a b : Name) : Name := ⟨Data.ByteString.append a.bytes b.bytes⟩

instance : Append Name := ⟨append⟩

end Name
end Data.PDF.Core.Name
