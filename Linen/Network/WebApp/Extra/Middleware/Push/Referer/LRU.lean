/-
  Linen.Network.WebApp.Extra.Middleware.Push.Referer.LRU — LRU cache for push
  predictions

  Ports Hale's `Network.Wai.Middleware.Push.Referer.LRU`. Simple LRU cache
  backed by a list. Evicts the least recently used entry when the cache is
  full.

  ## Performance
  - Lookup: O(n) — acceptable for small caches
  - Insert: O(n) — moves to front
  - Eviction: O(1) — drops from back
-/
namespace Network.WebApp.Extra.Middleware.Push.Referer

/-- A simple LRU cache mapping keys to values.
    Items are ordered most-recently-used first. -/
structure LRU (α β : Type) [BEq α] where
  /-- Maximum cache size. -/
  maxSize : Nat
  /-- Entries ordered by access time (most recent first). -/
  entries : List (α × β)
deriving Repr

namespace LRU

/-- Create an empty LRU cache. -/
def empty [BEq α] (maxSize : Nat) : LRU α β :=
  ⟨maxSize, []⟩

/-- Look up a key, moving it to the front if found. -/
def lookup [BEq α] (key : α) (cache : LRU α β) : Option β × LRU α β :=
  match cache.entries.find? (fun (k, _) => k == key) with
  | some (_, v) =>
    let entries' := (key, v) :: cache.entries.filter (fun (k, _) => k != key)
    (some v, { cache with entries := entries' })
  | none => (none, cache)

/-- Insert a key-value pair, evicting the LRU entry if at capacity. -/
def insert [BEq α] (key : α) (val : β) (cache : LRU α β) : LRU α β :=
  let entries' := (key, val) :: cache.entries.filter (fun (k, _) => k != key)
  let entries'' := if entries'.length > cache.maxSize then
    entries'.take cache.maxSize
  else entries'
  { cache with entries := entries'' }

/-- Get the current size. -/
def size [BEq α] (cache : LRU α β) : Nat :=
  cache.entries.length

end LRU

end Network.WebApp.Extra.Middleware.Push.Referer
