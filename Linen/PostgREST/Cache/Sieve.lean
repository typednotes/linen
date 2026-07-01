/-
  `PostgREST.Cache.Sieve` — SIEVE cache eviction algorithm

  Implements the SIEVE cache replacement policy, which provides
  scan-resistant caching similar to ARC but simpler. Mirrors PostgREST's
  `PostgREST.Cache.Sieve` module.

  SIEVE maintains a circular buffer with a "hand" pointer. On cache hit, the
  entry is marked as visited. On eviction, the hand sweeps entries: visited
  entries have their flag cleared, unvisited entries are evicted.
-/

namespace PostgREST.Cache

/-- A SIEVE cache entry. -/
structure SieveEntry (α : Type) (β : Type) where
  key : α
  value : β
  visited : Bool := false
  deriving Repr, Inhabited

/-- A SIEVE cache with a fixed maximum size.
    $$\text{SieveCache}\ \alpha\ \beta = \{ \text{entries}, \text{hand}, \text{maxSize} \}$$ -/
structure SieveCache (α : Type) (β : Type) [BEq α] where
  entries : Array (SieveEntry α β)
  hand : Nat
  maxSize : Nat
  deriving Repr

namespace SieveCache

/-- Create an empty SIEVE cache with the given maximum size. -/
def create [BEq α] (maxSize : Nat) : SieveCache α β :=
  { entries := #[], hand := 0, maxSize }

/-- Look up a key in the cache, marking it as visited on hit. -/
def lookup [BEq α] [Inhabited α] [Inhabited β] (cache : SieveCache α β) (key : α)
    : Option β × SieveCache α β :=
  match cache.entries.findIdx? (·.key == key) with
  | none => (none, cache)
  | some idx =>
    let entry := cache.entries[idx]!
    let entries' := cache.entries.set! idx { entry with visited := true }
    (some entry.value, { cache with entries := entries' })

/-- Evict an entry and insert a new one. -/
private def evictAndInsert [BEq α] [Inhabited α] [Inhabited β]
    (cache : SieveCache α β) (key : α) (value : β)
    : SieveCache α β :=
  if cache.entries.isEmpty then cache
  else Id.run do
    let mut entries := cache.entries
    let mut hand := cache.hand % entries.size
    -- Sweep up to entries.size times to find an eviction candidate
    for _ in List.range entries.size do
      let entry := entries[hand]!
      if entry.visited then
        -- Clear visited flag and move on
        entries := entries.set! hand { entry with visited := false }
        hand := (hand + 1) % entries.size
      else
        -- Found unvisited entry: evict it
        entries := entries.set! hand { key, value, visited := false }
        return { entries, hand := (hand + 1) % entries.size, maxSize := cache.maxSize }
    -- All entries were visited: evict current hand position
    entries := entries.set! hand { key, value, visited := false }
    { entries, hand := (hand + 1) % entries.size, maxSize := cache.maxSize }

/-- Insert a key-value pair, evicting if necessary. -/
def insert [BEq α] [Inhabited α] [Inhabited β]
    (cache : SieveCache α β) (key : α) (value : β)
    : SieveCache α β :=
  -- Check if key already exists
  match cache.entries.findIdx? (·.key == key) with
  | some idx =>
    let entries' := cache.entries.set! idx { key, value, visited := true }
    { cache with entries := entries' }
  | none =>
    if cache.entries.size < cache.maxSize then
      -- Space available: just append
      { cache with entries := cache.entries.push { key, value, visited := false } }
    else
      -- Need to evict: sweep from hand
      evictAndInsert cache key value

/-- Remove a key from the cache. -/
def remove [BEq α] (cache : SieveCache α β) (key : α)
    : SieveCache α β :=
  { cache with entries := cache.entries.filter (·.key != key) }

/-- Return the number of entries in the cache. -/
def size [BEq α] (cache : SieveCache α β) : Nat :=
  cache.entries.size

end SieveCache
end PostgREST.Cache
