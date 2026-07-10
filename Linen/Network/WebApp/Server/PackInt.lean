/-
  Linen.Network.WebApp.Server.PackInt — Integer to ByteArray rendering
  $$\text{packInt} : \mathbb{N} \to \text{String}$$
-/
namespace Network.WebApp.Server

/-- Render a natural number as a decimal string.
    This is used for Content-Length headers and chunk sizes. -/
@[inline] def packInt (n : Nat) : String := toString n

/-- Render a natural number as a hex string (lowercase).
    Used for HTTP chunked transfer encoding. Built from core `Nat.toDigits 16`
    (which is total and already returns `['0']` for zero) rather than the
    upstream's hand-rolled recursive digit accumulator — no `partial` needed. -/
@[inline] def packHex (n : Nat) : String :=
  String.ofList (Nat.toDigits 16 n)

end Network.WebApp.Server
