/-
  Tests for `Cloud.Page`.

  The property under test is the one that distinguishes a bound which is part of
  the specification from fuel that hides a missing termination argument: **a
  bounded read must be distinguishable from a complete one.** So every case
  below checks `complete` alongside the items, and the counting stub reports how
  many requests were actually issued so that the bound can be shown to be
  respected rather than merely survived.
-/
import Linen.Cloud.Page

open Cloud

namespace Tests.Cloud.Page

-- ── A stub provider ─────────────────────────────────────────────────────────

/-- A listing of `total` single-item pages, numbered from zero.

    The cursor is the next page's index as a string — exactly the kind of thing
    a real provider sends and a caller must not interpret. `counter` records
    how many requests were issued, which is how the bound is checked. -/
def stub (total : Nat) (counter : IO.Ref Nat) :
    Option Cursor → IO (Except Error (Page Nat)) := fun cursor => do
  counter.modify (· + 1)
  let index := match cursor with
    | none   => 0
    | some c => c.token.toNat?.getD 0
  if index >= total then
    return .ok Page.empty
  return .ok
    { items := [index]
    , next := if index + 1 < total then some ⟨toString (index + 1)⟩ else none }

/-- A provider that fails on the `failAt`-th request, to check that an error
    part way through a multi-page read is propagated rather than yielding a
    short listing that looks complete. -/
def failingStub (total failAt : Nat) (counter : IO.Ref Nat) :
    Option Cursor → IO (Except Error (Page Nat)) := fun cursor => do
  let n ← counter.modifyGet (fun n => (n + 1, n + 1))
  if n == failAt then
    return .error { klass := .throttled, status := 503, code := "SlowDown" }
  stub total counter cursor

-- ── Reading a listing that fits ─────────────────────────────────────────────

/- Three pages, a budget of ten: the provider ends it, so the listing is
    complete and only three requests were issued. -/
/-- info: ([0, 1, 2], true, 3, 3) -/
#guard_msgs in
#eval show IO (List Nat × Bool × Nat × Nat) from do
  let counter ← IO.mkRef 0
  match ← paginate 10 (stub 3 counter) with
  | .error _ => return ([], false, 0, 0)
  | .ok l => return (l.items, l.complete, l.pagesRead, ← counter.get)

/- A single-page listing. `next = none` on the first page is the provider
    saying it is finished, which is the only source of a completeness claim. -/
/-- info: ([0], true, 1) -/
#guard_msgs in
#eval show IO (List Nat × Bool × Nat) from do
  let counter ← IO.mkRef 0
  match ← paginate 10 (stub 1 counter) with
  | .error _ => return ([], false, 0)
  | .ok l => return (l.items, l.complete, l.pagesRead)

-- ── Reading a listing that does not fit ─────────────────────────────────────

/- **The case the whole module is shaped around.** Five pages, a budget of two:
   the items are the first two, and `complete` is `false`. A caller that reads
   this as exhaustive would conclude three objects do not exist. -/
/-- info: ([0, 1], false, 2, 2, some "2") -/
#guard_msgs in
#eval show IO (List Nat × Bool × Nat × Nat × Option String) from do
  let counter ← IO.mkRef 0
  match ← paginate 2 (stub 5 counter) with
  | .error _ => return ([], true, 0, 0, none)
  | .ok l => return (l.items, l.complete, l.pagesRead, ← counter.get, l.next.map (·.token))

/- The bound is respected exactly: a budget of two issues two requests, not
   three. A bound that is checked after the fetch would issue one too many, and
   on a throttled API that is the difference between working and not. -/
/-- info: 4 -/
#guard_msgs in
#eval show IO Nat from do
  let counter ← IO.mkRef 0
  let _ ← paginate 4 (stub 100 counter)
  counter.get

/- A budget of zero issues **no** request and reports incomplete. This is the
   case that makes deriving completeness from `next` unworkable: `next` is
   `none` here because there is nothing to resume from yet, not because the
   listing ended. -/
/-- info: ([], false, 0, 0, none) -/
#guard_msgs in
#eval show IO (List Nat × Bool × Nat × Nat × Option String) from do
  let counter ← IO.mkRef 0
  match ← paginate 0 (stub 5 counter) with
  | .error _ => return ([], true, 0, 0, none)
  | .ok l => return (l.items, l.complete, l.pagesRead, ← counter.get, l.next.map (·.token))

/- Exactly at the boundary: three pages with a budget of three. The provider
   ended it on the last allowed request, so this *is* complete — the bound was
   reached but not exceeded. -/
/-- info: ([0, 1, 2], true) -/
#guard_msgs in
#eval show IO (List Nat × Bool) from do
  let counter ← IO.mkRef 0
  match ← paginate 3 (stub 3 counter) with
  | .error _ => return ([], false)
  | .ok l => return (l.items, l.complete)

-- ── Order is preserved ──────────────────────────────────────────────────────

/- Items come back in provider order across page boundaries. The accumulator is
   built reversed and flipped once, so this is the test that the flip happens
   exactly once. -/
/-- info: [0, 1, 2, 3, 4, 5, 6] -/
#guard_msgs in
#eval show IO (List Nat) from do
  let counter ← IO.mkRef 0
  match ← paginate 20 (stub 7 counter) with
  | .error _ => return []
  | .ok l => return l.items

-- ── Errors part way through ─────────────────────────────────────────────────

/- A failure on the third request is propagated, not turned into a two-page
   listing that claims to be truncated. Those are different situations: one is
   "there is more, ask again", the other is "the provider is unhappy". -/
/-- info: (Cloud.Class.throttled, true) -/
#guard_msgs in
#eval show IO (Class × Bool) from do
  let counter ← IO.mkRef 0
  match ← paginate 10 (failingStub 5 3 counter) with
  | .error e => return (e.klass, e.retryable)
  | .ok _ => return (.protocol, false)

/- A failure on the very first request is propagated too. -/
/-- info: Cloud.Class.throttled -/
#guard_msgs in
#eval show IO Class from do
  let counter ← IO.mkRef 0
  match ← paginate 10 (failingStub 5 1 counter) with
  | .error e => return e.klass
  | .ok _ => return .protocol

-- ── Resuming ────────────────────────────────────────────────────────────────

/- A truncated listing resumes from its cursor, and the two halves join in
   order. `pagesRead` counts only the resuming call's requests. -/
/-- info: ([0, 1, 2, 3, 4], true, 3) -/
#guard_msgs in
#eval show IO (List Nat × Bool × Nat) from do
  let counter ← IO.mkRef 0
  match ← paginate 2 (stub 5 counter) with
  | .error _ => return ([], false, 0)
  | .ok first =>
    match ← first.resume 10 (stub 5 counter) with
    | .error _ => return ([], false, 0)
    | .ok whole => return (whole.items, whole.complete, whole.pagesRead)

/- Resuming a listing that is already complete issues no request and changes
   nothing. -/
/-- info: ([0, 1, 2], true, 3) -/
#guard_msgs in
#eval show IO (List Nat × Bool × Nat) from do
  let counter ← IO.mkRef 0
  match ← paginate 10 (stub 3 counter) with
  | .error _ => return ([], false, 0)
  | .ok l =>
    let before ← counter.get
    match ← l.resume 10 (stub 3 counter) with
    | .error _ => return ([], false, 0)
    | .ok again => return (again.items, again.complete, before + (← counter.get) - before)

/- Resuming can itself be truncated, so a caller can walk a long listing in
   fixed-size bites without ever being told it is finished when it is not. -/
/-- info: ([0, 1, 2, 3], false) -/
#guard_msgs in
#eval show IO (List Nat × Bool) from do
  let counter ← IO.mkRef 0
  match ← paginate 2 (stub 9 counter) with
  | .error _ => return ([], true)
  | .ok first =>
    match ← first.resume 2 (stub 9 counter) with
    | .error _ => return ([], true)
    | .ok more => return (more.items, more.complete)

-- ── The small pure facts ────────────────────────────────────────────────────

#guard (Page.empty : Page Nat).isLast
#guard (Page.empty : Page Nat).items == []
#guard (Listing.empty : Listing Nat).complete
#guard (Listing.empty : Listing Nat).pagesRead == 0

/- A page with a cursor is not the last, however few items it carries — the
   distinction a provider makes and a client must not smooth over. -/
#guard !({ items := [1], next := some ⟨"tok"⟩ } : Page Nat).isLast
#guard !({ items := ([] : List Nat), next := some ⟨"tok"⟩ } : Page Nat).isLast

#guard defaultMaxPages == 1000

end Tests.Cloud.Page
