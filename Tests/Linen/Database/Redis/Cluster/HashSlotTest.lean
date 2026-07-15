import Linen.Database.Redis.Cluster.HashSlot

/-!
  Tests for `Linen.Database.Redis.Cluster.HashSlot`.

  The expected slot numbers below are Redis Cluster's well-known published
  test vectors (e.g. from the Redis Cluster specification and the reference
  `redis-cli --cluster` / `CLUSTER KEYSLOT` behaviour).
-/

open Database.Redis.Cluster.HashSlot

-- `numHashSlots` is fixed at 16384 = 2^14.
#guard numHashSlots == 16384

-- No hash tag: the whole key is hashed.
#guard (keyToSlot "123456789".toUTF8).toUInt16 == 12739

-- A key with a hash tag hashes only the tagged substring.
#guard (keyToSlot "foo{bar}".toUTF8).toUInt16 == (keyToSlot "bar".toUTF8).toUInt16

-- Two keys sharing a hash tag land on the same slot.
#guard (keyToSlot "user1000{tag}".toUTF8).toUInt16 == (keyToSlot "user2000{tag}".toUTF8).toUInt16

-- An empty hash tag (`{}`) falls back to hashing the whole key.
#guard (keyToSlot "foo{}".toUTF8).toUInt16 != (keyToSlot "foo".toUTF8).toUInt16

-- Every slot is within range.
#guard (keyToSlot "anything".toUTF8).toUInt16 < 16384

-- `findSubKey` extracts exactly the hash-tagged substring.
#guard findSubKey "foo{bar}baz".toUTF8 == "bar".toUTF8

-- `findSubKey` with no braces returns the whole key.
#guard findSubKey "nobraces".toUTF8 == "nobraces".toUTF8
