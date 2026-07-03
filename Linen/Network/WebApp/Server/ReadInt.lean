/-
  Linen.Network.WebApp.Server.ReadInt — Fast integer parsing

  Performance-critical: used to parse Content-Length headers.
  $$\text{readInt} : \text{String} \to \text{Nat}$$
-/
namespace Network.WebApp.Server

/-- Parse a non-negative integer from the leading digits of a string.
    Stops at the first non-digit character. Returns 0 for empty/non-numeric input.
    $$\text{readInt}(s) = \text{foldl}(\lambda\, i\, c.\; i \times 10 + (c - \text{'0'}),\; 0,\; \text{takeWhile isDigit}\; s)$$ -/
@[inline] def readInt (s : String) : Nat :=
  s.foldl (init := (0, true)) (fun (acc, active) c =>
    if active && c.isDigit then (acc * 10 + (c.toNat - '0'.toNat), true)
    else (acc, false)) |>.1

/-- Parse a non-negative integer from a ByteArray (ASCII digits).
    $$\text{readIntBytes} : \text{ByteArray} \to \text{Nat}$$ -/
def readIntBytes (bs : ByteArray) : Nat :=
  go 0 0
where
  go (i acc : Nat) : Nat :=
    if h : i < bs.size then
      let b := bs[i]
      if b >= 0x30 && b <= 0x39 then  -- '0' to '9'
        go (i + 1) (acc * 10 + (b.toNat - 0x30))
      else acc
    else acc
  termination_by bs.size - i

end Network.WebApp.Server
