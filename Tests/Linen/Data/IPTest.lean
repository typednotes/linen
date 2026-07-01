/-
  Tests for `Linen.Data.IP`.

  IP address types, CIDR matching, and the parsers are pure, so behaviour is
  checked with `#guard`.
-/
import Linen.Data.IP

open Data.IP

namespace Tests.Data.IP

/-! ### IPv4 octets / display / constants -/

#guard (IPv4.ofOctets 192 168 1 1).addr == 0xC0A80101
#guard (IPv4.ofOctets 10 0 0 1).toOctets == (10, 0, 0, 1)
#guard toString (IPv4.ofOctets 192 168 1 1) == "192.168.1.1"
#guard toString IPv4.loopback == "127.0.0.1"
#guard IPv4.any.addr == 0
#guard IPv4.broadcast.addr == 0xFFFFFFFF
#guard compare (IPv4.ofOctets 1 0 0 0) (IPv4.ofOctets 2 0 0 0) == Ordering.lt

/-! ### IPv6 / IP -/

#guard IPv6.loopback == (⟨0, 1⟩ : IPv6)
#guard compare (IPv6.mk 0 1) (IPv6.mk 0 2) == Ordering.lt
#guard compare (IPv6.mk 1 0) (IPv6.mk 0 9) == Ordering.gt   -- hi dominates
#guard toString (IP.v4 IPv4.loopback) == "127.0.0.1"

/-! ### CIDR mask + matching -/

#guard (⟨IPv4.any, 24, by omega⟩ : AddrRange4).mask == 0xFFFFFF00
#guard (⟨IPv4.any, 0, by omega⟩ : AddrRange4).mask == 0
#guard (⟨IPv4.any, 32, by omega⟩ : AddrRange4).mask == 0xFFFFFFFF

def net24 : AddrRange4 := ⟨IPv4.ofOctets 192 168 1 0, 24, by omega⟩

#guard AddrRange4.isMatchedTo (IPv4.ofOctets 192 168 1 50) net24 == true
#guard AddrRange4.isMatchedTo (IPv4.ofOctets 192 168 1 255) net24 == true
#guard AddrRange4.isMatchedTo (IPv4.ofOctets 192 168 2 1) net24 == false
#guard AddrRange4.isMatchedTo (IPv4.ofOctets 10 0 0 1) net24 == false
#guard toString net24 == "192.168.1.0/24"

/-! ### Parsing -/

#guard parseIPv4 "192.168.1.1" == some (IPv4.ofOctets 192 168 1 1)
#guard parseIPv4 "0.0.0.0" == some IPv4.any
#guard parseIPv4 "256.0.0.1" == none          -- octet out of range
#guard parseIPv4 "1.2.3" == none              -- too few parts
#guard parseIPv4 "a.b.c.d" == none            -- non-numeric

#guard (parseCIDR4 "192.168.1.0/24").map (·.maskLen) == some 24
#guard (parseCIDR4 "192.168.1.0/24").map (·.base) == some (IPv4.ofOctets 192 168 1 0)
#guard (parseCIDR4 "10.0.0.0/33").isNone      -- mask > 32
#guard (parseCIDR4 "192.168.1.0").isNone      -- no mask

end Tests.Data.IP
