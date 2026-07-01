/-
  Linen.Data.IP — IP addresses and CIDR ranges

  Provides IPv4/IPv6 address types, CIDR range matching, and routing
  table lookup (longest prefix match).

  ## Design

  Mirrors Haskell's `Data.IP` from the `iproute` package.
  IPv4 is stored as UInt32, IPv6 as a pair of UInt64s.

  ## Typing

  - CIDR mask length is bounded by address width (0..32 for v4, 0..128 for v6)
  - `isMatchedTo` is decidable
-/

namespace Data.IP

/-- An IPv4 address stored as a 32-bit unsigned integer.
    $$\text{IPv4} = \{ \text{addr} : \text{UInt32} \}$$ -/
structure IPv4 where
  addr : UInt32
deriving BEq, Hashable, Repr, Inhabited

namespace IPv4

/-- Create an IPv4 from four octets: `a.b.c.d`
    $$\text{ofOctets}(a, b, c, d) = (a \ll 24) \lor (b \ll 16) \lor (c \ll 8) \lor d$$ -/
@[inline] def ofOctets (a b c d : UInt8) : IPv4 :=
  ⟨(a.toUInt32 <<< 24) ||| (b.toUInt32 <<< 16) ||| (c.toUInt32 <<< 8) ||| d.toUInt32⟩

/-- Extract the four octets. -/
def toOctets (ip : IPv4) : UInt8 × UInt8 × UInt8 × UInt8 :=
  ( (ip.addr >>> 24).toUInt8
  , (ip.addr >>> 16).toUInt8
  , (ip.addr >>> 8).toUInt8
  , ip.addr.toUInt8 )

instance : ToString IPv4 where
  toString ip :=
    let (a, b, c, d) := ip.toOctets
    s!"{a}.{b}.{c}.{d}"

instance : Ord IPv4 where
  compare a b := compare a.addr b.addr

/-- The loopback address 127.0.0.1. -/
def loopback : IPv4 := ofOctets 127 0 0 1

/-- The any address 0.0.0.0. -/
def any : IPv4 := ⟨0⟩

/-- The broadcast address 255.255.255.255. -/
def broadcast : IPv4 := ⟨0xFFFFFFFF⟩

end IPv4

/-- An IPv6 address stored as two 64-bit unsigned integers (high, low).
    $$\text{IPv6} = \{ \text{hi} : \text{UInt64},\; \text{lo} : \text{UInt64} \}$$ -/
structure IPv6 where
  hi : UInt64
  lo : UInt64
deriving BEq, Hashable, Repr, Inhabited

namespace IPv6

/-- The loopback address ::1. -/
def loopback : IPv6 := ⟨0, 1⟩

/-- The any address ::. -/
def any : IPv6 := ⟨0, 0⟩

instance : Ord IPv6 where
  compare a b :=
    match compare a.hi b.hi with
    | .eq => compare a.lo b.lo
    | ord => ord

instance : ToString IPv6 where
  toString ip := s!"{ip.hi}:{ip.lo}"  -- simplified display

end IPv6

/-- A generic IP address (IPv4 or IPv6). -/
inductive IP where
  | v4 : IPv4 → IP
  | v6 : IPv6 → IP
deriving BEq, Repr

namespace IP

instance : ToString IP where
  toString
    | .v4 a => toString a
    | .v6 a => toString a

end IP

/-- An IPv4 CIDR range with bounded mask length.
    $$\text{AddrRange4} = \{ \text{base} : \text{IPv4},\; \text{mask} : \{n : \mathbb{N} \mid n \leq 32\} \}$$ -/
structure AddrRange4 where
  /-- The network base address. -/
  base : IPv4
  /-- The prefix length (0..32). -/
  maskLen : Nat
  /-- Proof that mask length is valid. -/
  valid : maskLen ≤ 32

namespace AddrRange4

/-- Compute the network mask from the prefix length.
    $$\text{mask}(n) = \text{0xFFFFFFFF} \ll (32 - n)$$ -/
@[inline] def mask (r : AddrRange4) : UInt32 :=
  if r.maskLen == 0 then 0
  else 0xFFFFFFFF <<< (32 - r.maskLen).toUInt32

/-- Check if an address falls within this CIDR range.
    $$\text{isMatchedTo}(ip, r) \iff (ip \land \text{mask}) = (\text{base} \land \text{mask})$$ -/
@[inline] def isMatchedTo (ip : IPv4) (r : AddrRange4) : Bool :=
  (ip.addr &&& r.mask) == (r.base.addr &&& r.mask)

instance : ToString AddrRange4 where
  toString r := s!"{r.base}/{r.maskLen}"

end AddrRange4

/-- An IPv6 CIDR range with bounded mask length.
    $$\text{AddrRange6} = \{ \text{base} : \text{IPv6},\; \text{mask} : \{n : \mathbb{N} \mid n \leq 128\} \}$$ -/
structure AddrRange6 where
  base : IPv6
  maskLen : Nat
  valid : maskLen ≤ 128

namespace AddrRange6

instance : ToString AddrRange6 where
  toString r := s!"{r.base}/{r.maskLen}"

end AddrRange6

/-- A generic CIDR range. -/
inductive AddrRange where
  | v4 : AddrRange4 → AddrRange
  | v6 : AddrRange6 → AddrRange

namespace AddrRange

instance : ToString AddrRange where
  toString
    | .v4 r => toString r
    | .v6 r => toString r

end AddrRange

/-- Parse an IPv4 address from dotted-decimal string.
    Returns `none` on invalid input. -/
def parseIPv4 (s : String) : Option IPv4 := do
  let parts := s.splitOn "."
  if parts.length != 4 then none
  else do
    let a ← parts[0]!.toNat?
    let b ← parts[1]!.toNat?
    let c ← parts[2]!.toNat?
    let d ← parts[3]!.toNat?
    if a > 255 || b > 255 || c > 255 || d > 255 then none
    else some (IPv4.ofOctets a.toUInt8 b.toUInt8 c.toUInt8 d.toUInt8)

/-- Parse a CIDR range like "192.168.1.0/24". -/
def parseCIDR4 (s : String) : Option AddrRange4 := do
  let parts := s.splitOn "/"
  if parts.length != 2 then none
  else do
    let ip ← parseIPv4 parts[0]!
    let mask ← parts[1]!.toNat?
    if h : mask ≤ 32 then some ⟨ip, mask, h⟩
    else none

end Data.IP
