/-
  Tests for `Linen.PostgREST.Cache.Sieve`.
-/
import Linen.PostgREST.Cache.Sieve

open PostgREST.Cache

namespace Tests.PostgREST.Cache.Sieve

def c0 : SieveCache Nat String := SieveCache.create 2

#guard c0.size == 0
#guard (SieveCache.lookup c0 1).1 == none

def c1 := SieveCache.insert c0 1 "a"
def c2 := SieveCache.insert c1 2 "b"

#guard c2.size == 2
#guard (SieveCache.lookup c2 1).1 == some "a"
#guard (SieveCache.lookup c2 2).1 == some "b"
#guard (SieveCache.lookup c2 3).1 == none

/-! ### Overwriting an existing key does not grow the cache -/

def c2' := SieveCache.insert c2 1 "a2"

#guard c2'.size == 2
#guard (SieveCache.lookup c2' 1).1 == some "a2"

/-! ### Eviction: unvisited entries are evicted first -/

-- c2 has keys 1, 2 both unvisited (never looked up). Inserting a third key
-- into a full cache evicts the unvisited entry at the hand (key 1).
def c3 := SieveCache.insert c2 3 "c"

#guard c3.size == 2
#guard (SieveCache.lookup c3 1).1 == none
#guard (SieveCache.lookup c3 3).1 == some "c"

/-! ### Eviction: visited entries survive one sweep -/

-- Mark key 1 as visited via lookup, then insert a third key: the sweep
-- clears key 1's visited flag and evicts the still-unvisited key 2 instead.
def cVisited := (SieveCache.lookup c2 1).2
def c4 := SieveCache.insert cVisited 3 "c"

#guard (SieveCache.lookup c4 1).1 == some "a"
#guard (SieveCache.lookup c4 2).1 == none
#guard (SieveCache.lookup c4 3).1 == some "c"

/-! ### `remove` -/

def c5 := SieveCache.remove c2 1

#guard c5.size == 1
#guard (SieveCache.lookup c5 1).1 == none
#guard (SieveCache.lookup c5 2).1 == some "b"

end Tests.PostgREST.Cache.Sieve
