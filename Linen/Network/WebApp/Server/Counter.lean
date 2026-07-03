/-
  Linen.Network.WebApp.Server.Counter — Atomic connection counter

  Thread-safe counter for tracking active connections.
  Used for graceful shutdown: wait until all connections are closed.

  ## Guarantees
  - Thread-safe via IO.Ref atomicity
  - `waitForZero` blocks until count reaches 0
-/
namespace Network.WebApp.Server

/-- Atomic counter for tracking active connections.
    $$\text{Counter} = \text{IO.Ref}\ \mathbb{N}$$ -/
structure Counter where
  ref : IO.Ref Nat

/-- Create a new counter initialized to 0. -/
def Counter.new : IO Counter :=
  Counter.mk <$> IO.mkRef 0

/-- Increment the counter (new connection accepted). -/
def Counter.increase (c : Counter) : IO Unit :=
  c.ref.modify (· + 1)

/-- Decrement the counter (connection closed). -/
def Counter.decrease (c : Counter) : IO Unit :=
  c.ref.modify fun n => if n > 0 then n - 1 else 0

/-- Get the current count. -/
def Counter.getCount (c : Counter) : IO Nat :=
  c.ref.get

/-- Wait (polling) until the counter reaches zero.
    Used for graceful shutdown. Expressed as a `while` loop over the polled
    count rather than self-recursion, so no `partial` is needed. -/
def Counter.waitForZero (c : Counter) : IO Unit := do
  let mut n ← c.ref.get
  while n > 0 do
    IO.sleep 10  -- 10ms polling interval
    n ← c.ref.get

end Network.WebApp.Server
