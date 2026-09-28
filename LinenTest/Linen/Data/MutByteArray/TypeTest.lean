import Linen.Data.MutByteArray.Type

open Data Data.MutByteArray

-- `new` allocates the requested length, unpinned, zero-filled.
#guard (MutByteArray.new 8).length == 8
#guard (MutByteArray.new 8).isPinned == false
#guard (MutByteArray.new 8).bytes.get! 3 == 0

-- `newPinned` allocates pinned.
#guard (MutByteArray.newPinned 4).isPinned == true
#guard (MutByteArray.newPinned 4).length == 4

-- `pin`/`unpin` flip the pinned flag.
#guard ((MutByteArray.new 2).pin).isPinned == true
#guard ((MutByteArray.newPinned 2).unpin).isPinned == false

-- `empty` is the zero-length array.
#guard MutByteArray.empty.length == 0
