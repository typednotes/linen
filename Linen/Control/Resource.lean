module

public import Init

public section

/-! # Indexed resource programs

Resource implementations use `Program` to describe the input world and the
world on successful return. An abstract proposition gates construction and
interpretation of primitive actions. A resource module keeps its proof
private and exports only operations that implement its protocol.
-/

namespace Control.Resource

/-- An effectful transition whose output world can depend on its result.
The private authorization proof gates primitive steps and interpretation. -/
structure ProgramT (m : Type → Type) (access : Prop) {σ : Type} (world : σ → Type)
    (pre : σ) (α : Type) (post : α → σ) where
  private mk ::
  private exec : access → world pre → m ((value : α) × world (post value))

/-- An IO program whose output state can depend on its result. -/
abbrev Program := ProgramT IO

/-- A resource program whose final state is independent of its result. -/
abbrev ActionT (m : Type → Type) (access : Prop) {σ : Type} (world : σ → Type)
    (pre post : σ) (α : Type) :=
  ProgramT m access world pre α (fun _ => post)

/-- An IO action with a fixed final state. -/
abbrev Action (access : Prop) {σ : Type} (world : σ → Type) (pre post : σ) (α : Type) :=
  ActionT IO access world pre post α

namespace ProgramT

/-- Define a primitive transition at the resource implementation boundary. -/
def step (_ : access) (f : world pre → m ((value : α) × world (post value))) :
    ProgramT m access world pre α post :=
  ⟨fun _ => f⟩

/-- Interpret a program at the resource implementation boundary. -/
def run (key : access) (program : ProgramT m access world pre α post) (initial : world pre) :
    m ((value : α) × world (post value)) :=
  program.exec key initial

/-- Return a value without changing resource state. -/
def pure [Pure m] (value : α) : ActionT m access world state state α :=
  ⟨fun _ current => Pure.pure ⟨value, current⟩⟩

/-- Lift an action that has no access to the program's private resource. -/
def lift [Monad m] (action : m α) : ActionT m access world state state α :=
  ⟨fun _ current => do return ⟨← action, current⟩⟩

/-- Sequence dependent transitions. Each continuation starts in the state
selected by the previous result. All branches agree on the final contract. -/
def bind [Monad m] (first : ProgramT m access world pre α mid)
    (next : (a : α) → ProgramT m access world (mid a) β post) :
    ProgramT m access world pre β post :=
  ⟨fun key current => do
    let ⟨value, current⟩ ← first.exec key current
    (next value).exec key current⟩

/-- Sequence actions with a fixed intermediate and final state. -/
def ibind [Monad m] (first : ActionT m access world pre mid α)
    (next : α → ActionT m access world mid post β) : ActionT m access world pre post β :=
  bind first next

/-- Map a result while preserving the fixed state transition. -/
def map [Monad m] (f : α → β) (action : ActionT m access world pre post α) :
    ActionT m access world pre post β :=
  ibind action fun a => pure (f a)

/-- State preserving actions support ordinary `do` notation. Use
`Program.ibind` or `Program.bind` to move between states. -/
instance [Monad m] : Monad (ActionT m access world state state) where
  pure := pure
  bind := ibind
  map := map

end ProgramT

namespace Program
export ProgramT (step run pure lift bind ibind map)
end Program

/-- Availability of a resource with a single usable phase. -/
inductive Lifetime where
  | open_
  | closed
  deriving DecidableEq

/-- The owner cell is shared by the interpreter and its cleanup handler.
Primitive operations have access only through the corresponding proof. -/
abbrev Owned (access : Prop) (handle : Type) :=
  Action access (fun _ : Lifetime => IO.Ref (Option handle))

namespace Owned

/-- Define an operation on the live resource. Implementations must keep its
handle private, including in the result and in callbacks. -/
def use (key : access) (f : handle → IO α) : Owned access handle .open_ .open_ α :=
  Program.step key fun cell => do
    let some raw ← cell.get | throw (IO.userError "resource owner is empty")
    return ⟨← f raw, cell⟩

private def releaseOnce (cell : IO.Ref (Option handle)) (release : handle → IO Unit) : IO Unit := do
  if let some raw ← cell.swap none then
    release raw

/-- Release the resource and consume its open state. Release is terminal even
if it reports an exception. Implementations must supply the runner's release
operation here. -/
def close (key : access) (release : handle → IO Unit) : Owned access handle .open_ .closed Unit :=
  Program.step key fun cell => do
    releaseOnce cell release
    return ⟨(), cell⟩

/-- Run a program with a newly acquired resource. Release runs at most once,
including on an exception or when the program returns with the resource open.
If both the program and cleanup fail, propagate the program's error. -/
def run (key : access) (acquire : IO handle) (release : handle → IO Unit)
    (action : Owned access handle .open_ post α) : IO α := do
  let cell ← IO.mkRef (some (← acquire))
  let result ← try
    (·.1) <$> Program.run key action cell
  catch error =>
    try releaseOnce cell release catch _ => pure ()
    throw error
  releaseOnce cell release
  pure result

end Owned
end Control.Resource
