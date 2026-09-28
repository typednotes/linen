import Linen.Network.WebApp.Extra.Middleware.Push.Referer.LRU

/-! ### Tests for `Linen.Network.WebApp.Extra.Middleware.Push.Referer.LRU`

    Coverage: `lookup` on a miss/hit, `insert` moves an entry to the front and
    evicts the least-recently-used entry once past `maxSize`. -/

open Network.WebApp.Extra.Middleware.Push.Referer

namespace Tests.Network.WebApp.Extra.Middleware.Push.Referer.LRU

#guard (LRU.empty (α := String) (β := Nat) 3).size == 0
#guard (LRU.lookup "a" (LRU.empty (α := String) (β := Nat) 3)).1 == none

def three : LRU String Nat :=
  (LRU.empty 3).insert "a" 1 |>.insert "b" 2 |>.insert "c" 3

#guard three.size == 3
#guard (LRU.lookup "a" three).1 == some 1
#guard (LRU.lookup "z" three).1 == none

-- inserting a 4th entry evicts "a" (least recently used, since "b"/"c" were
-- inserted after it and "a" was never re-looked-up)
def four : LRU String Nat := three.insert "d" 4

#guard four.size == 3
#guard (LRU.lookup "a" four).1 == none
#guard (LRU.lookup "d" four).1 == some 4

-- a lookup refreshes recency, protecting the entry from the next eviction
def refreshed : LRU String Nat :=
  let (_, c) := LRU.lookup "a" three
  c.insert "d" 4

#guard (LRU.lookup "a" refreshed).1 == some 1
#guard (LRU.lookup "b" refreshed).1 == none

end Tests.Network.WebApp.Extra.Middleware.Push.Referer.LRU
