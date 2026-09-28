/-
  Tests for `Linen.Database.Redis.PubSub.Types` — the pure Pub/Sub value
  types and the subscription-change algebra. All pure, checked with `#guard`.
-/
import Linen.Database.Redis.PubSub.Types

open Database.Redis.PubSub

namespace Tests.Database.Redis.PubSub.Types

/-! ### Message accessors -/

private def m1 : Message := .message "chan".toUTF8 "hello".toUTF8
private def m2 : Message := .pmessage "ch.*".toUTF8 "chan".toUTF8 "hi".toUTF8

#guard m1.msgChannel == "chan".toUTF8
#guard m1.msgMessage == "hello".toUTF8
#guard m1.msgPattern == none
#guard m2.msgChannel == "chan".toUTF8
#guard m2.msgMessage == "hi".toUTF8
#guard m2.msgPattern == some "ch.*".toUTF8

/-! ### Smart constructors -/

#guard subscribe [] == PubSub.empty
#guard (subscribe ["a".toUTF8]).subs == Cmd.cmd ["a".toUTF8]
#guard psubscribe [] == PubSub.empty
#guard (psubscribe ["p".toUTF8]).psubs == Cmd.cmd ["p".toUTF8]
-- `unsubscribe []` is NOT empty: an empty list means "unsubscribe all".
#guard (unsubscribe []).unsubs == Cmd.cmd []
#guard unsubscribe [] != PubSub.empty
-- `unsubscribe1 []` IS a no-op.
#guard unsubscribe1 [] == PubSub.empty
#guard (unsubscribe1 ["a".toUTF8]).unsubs == Cmd.cmd ["a".toUTF8]
#guard punsubscribe1 [] == PubSub.empty
#guard (punsubscribe []).punsubs == Cmd.cmd []

/-! ### Cmd append (subscribe vs. unsubscribe monoids) -/

-- Subscribe append: identity + concatenation.
#guard Cmd.appendSub Cmd.doNothing (Cmd.cmd ["a".toUTF8]) == Cmd.cmd ["a".toUTF8]
#guard Cmd.appendSub (Cmd.cmd ["a".toUTF8]) Cmd.doNothing == Cmd.cmd ["a".toUTF8]
#guard Cmd.appendSub (Cmd.cmd ["a".toUTF8]) (Cmd.cmd ["b".toUTF8]) == Cmd.cmd ["a".toUTF8, "b".toUTF8]

-- Unsubscribe append: an empty list absorbs its neighbour ("unsubscribe all").
#guard Cmd.appendUnsub (Cmd.cmd []) (Cmd.cmd ["a".toUTF8]) == Cmd.cmd []
#guard Cmd.appendUnsub (Cmd.cmd ["a".toUTF8]) (Cmd.cmd []) == Cmd.cmd []
#guard Cmd.appendUnsub (Cmd.cmd ["a".toUTF8]) (Cmd.cmd ["b".toUTF8]) == Cmd.cmd ["a".toUTF8, "b".toUTF8]
#guard Cmd.appendUnsub Cmd.doNothing (Cmd.cmd ["a".toUTF8]) == Cmd.cmd ["a".toUTF8]

/-! ### PubSub append (`· ++ ·`) -/

-- Combining two subscribe batches concatenates the subscribe fields.
#guard (subscribe ["a".toUTF8] ++ subscribe ["b".toUTF8]).subs == Cmd.cmd ["a".toUTF8, "b".toUTF8]
-- Combining subscribe with psubscribe keeps them in their own fields.
#guard (subscribe ["a".toUTF8] ++ psubscribe ["p".toUTF8]).subs == Cmd.cmd ["a".toUTF8]
#guard (subscribe ["a".toUTF8] ++ psubscribe ["p".toUTF8]).psubs == Cmd.cmd ["p".toUTF8]
-- Empty is a left/right identity.
#guard (PubSub.empty ++ subscribe ["a".toUTF8]) == subscribe ["a".toUTF8]
#guard (subscribe ["a".toUTF8] ++ PubSub.empty) == subscribe ["a".toUTF8]

/-! ### Cmd.changes -/

#guard Cmd.doNothing.changes == ([] : List ByteArray)
#guard (Cmd.cmd ["x".toUTF8]).changes == ["x".toUTF8]

end Tests.Database.Redis.PubSub.Types
