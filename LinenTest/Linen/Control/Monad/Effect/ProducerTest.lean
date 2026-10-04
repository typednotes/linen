/- Illustrative pure producer scripts and caller-owned scheduling. -/
import Linen.Control.Monad.Effect.Producer

open Control.Monad.Effect

namespace Tests.Control.Monad.Effect.Producer

/-- Branches and loop-local values are reconstructed across multiple waits. -/
def paced (xs : List Nat) := Producer.run do
  Producer.yieldAll (xs.take 2)
  Producer.wait 2000
  let mut total := 0
  for x in xs.drop 2 do
    total := total + x
    if x % 2 == 0 then
      Producer.yield total
      Producer.wait 1000
  Producer.yield (total + 100)

def first := Eff.run (paced [1, 2, 3, 4, 5, 6] 1000 none)
#guard first.1 == [1, 2] && first.2.2 == some 3000
def second := Eff.run (paced [1, 2, 3, 4, 5, 6] 3000 (some first.2.1))
#guard second.1 == [7] && second.2.2 == some 4000
def third := Eff.run (paced [1, 2, 3, 4, 5, 6] 4000 (some second.2.1))
#guard third.1 == [18] && third.2.2 == some 5000
def fourth := Eff.run (paced [1, 2, 3, 4, 5, 6] 5000 (some third.2.1))
#guard fourth.1 == [118] && fourth.2.2 == none
#guard (Eff.run (paced [1, 2, 3, 4, 5, 6] 6000 (some fourth.2.1))).1 == []

-- A whole list is one element; yieldAll expands a list into element emissions.
#guard (Eff.run (Producer.run (Producer.yield [1, 2, 3]) 0 none)).1 == [[1, 2, 3]]
#guard (Eff.run (Producer.run (Producer.yieldAll [1, 2, 3]) 0 none)).1 == [1, 2, 3]
#guard (Eff.run (Producer.run (Producer.yieldAll [2, 2, 1, 2]) 0 none)).1 == [2, 2, 1, 2]

-- Empty batches and zero waits continue in the same step, preserving order.
#guard Eff.run (Producer.run (do
    Producer.yieldAll ([] : List Nat)
    Producer.wait 0
    Producer.yield 1
    Producer.yieldAll [2, 3]) 10 none) == ([1, 2, 3], ⟨4⟩, none)

-- A wait before any emission suspends silently; waits use the actual call time.
def silent := Producer.run do
  Producer.wait 5000
  Producer.yield (9 : Nat)
  Producer.wait 2000
  Producer.yield 10
#guard Eff.run (silent 1000 none) == ([], ⟨1⟩, some 6000)
#guard Eff.run (silent 9000 (some ⟨1⟩)) == ([9], ⟨3⟩, some 11000)
#guard Eff.run (silent 11000 (some ⟨3⟩)) == ([10], ⟨4⟩, none)

-- Nested loops, branches, continue and break are ordinary Lean control flow.
def nested := Producer.run do
  for x in [1, 2, 3] do
    if x == 2 then continue
    for y in [10, 20, 30] do
      if y == 30 then break
      Producer.yield (x + y)
      Producer.wait 1000
#guard Eff.run (nested 0 none) == ([11], ⟨2⟩, some 1000)
#guard Eff.run (nested 1000 (some ⟨2⟩)) == ([21], ⟨4⟩, some 2000)
#guard Eff.run (nested 2000 (some ⟨4⟩)) == ([13], ⟨6⟩, some 3000)
#guard Eff.run (nested 3000 (some ⟨6⟩)) == ([23], ⟨8⟩, some 4000)
#guard Eff.run (nested 4000 (some ⟨8⟩)) == ([], ⟨8⟩, none)

-- JSON round trips suffice to resume; the state contains no closure.
#guard (Lean.fromJson? (Lean.toJson second.2.1) : Except String Producer.Cursor).toOption == some second.2.1
#guard (Lean.fromJson? (Lean.toJson ({cycle := 7, cursor := ⟨2⟩} : Producer.CycleCursor)) :
  Except String Producer.CycleCursor).toOption == some {cycle := 7, cursor := ⟨2⟩}

/-- An unbounded logical source made of finite, separately resumable cycles. -/
def ticker := Producer.every 5000 fun n => Producer.yield n
#guard Eff.run (ticker 1000 none) == ([0], ⟨0, ⟨2⟩⟩, some 6000)
#guard Eff.run (ticker 6000 (some ⟨0, ⟨2⟩⟩)) == ([1], ⟨1, ⟨2⟩⟩, some 11000)
#guard Eff.run (ticker 90000 (some ⟨1, ⟨2⟩⟩)) == ([2], ⟨2, ⟨2⟩⟩, some 95000)
#guard Eff.run (ticker 0 (some ⟨1000000, ⟨2⟩⟩)) == ([1000001], ⟨1000001, ⟨2⟩⟩, some 5000)

-- An internal wait resumes the same cycle; only the trailing wait starts anew.
def repeated := Producer.every 5000 fun n => do
  Producer.yield (n * 10)
  Producer.wait 2000
  Producer.yield (n * 10 + 1)
#guard Eff.run (repeated 1000 none) == ([0], ⟨0, ⟨2⟩⟩, some 3000)
#guard Eff.run (repeated 3000 (some ⟨0, ⟨2⟩⟩)) == ([1], ⟨0, ⟨4⟩⟩, some 8000)
#guard Eff.run (repeated 8000 (some ⟨0, ⟨4⟩⟩)) == ([10], ⟨1, ⟨2⟩⟩, some 10000)
#guard Eff.run ((Producer.every 0 fun n => Producer.yield n) 1000 none) ==
  ([0], ⟨0, ⟨2⟩⟩, some 1001)

end Tests.Control.Monad.Effect.Producer
