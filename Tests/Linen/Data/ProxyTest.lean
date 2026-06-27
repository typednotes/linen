/-
  Tests for `Linen.Data.Proxy`.

  The data-free `Proxy` and its instances/laws. Since `Proxy α` has a single
  value, every operation collapses to `Proxy.mk`.
-/
import Linen.Data.Proxy

open Data

namespace Tests.Data.Proxy

/-! ### Instances -/

#guard (Proxy.mk : Proxy Nat) == Proxy.mk
#guard compare (Proxy.mk : Proxy Nat) Proxy.mk == Ordering.eq
#guard toString (Proxy.mk : Proxy Nat) == "Proxy"
#guard hash (Proxy.mk : Proxy Nat) == 0

/-! ### Functor / Monad collapse to the single value -/

#guard ((· + 1) <$> (Proxy.mk : Proxy Nat)) == Proxy.mk
#guard (pure 7 : Proxy Nat) == Proxy.mk
#guard ((Proxy.mk : Proxy Nat) >>= fun _ => (Proxy.mk : Proxy String)) == Proxy.mk

/-! ### Laws (compile-time) -/

example (p : Proxy Nat) : Functor.map id p = p := Proxy.map_id p
example (a : Nat) (f : Nat → Proxy String) : bind (pure a) f = f a := Proxy.pure_bind a f
example (p : Proxy Nat) : bind p pure = p := Proxy.bind_pure p

end Tests.Data.Proxy
