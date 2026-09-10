/-
  `Cloud.Page` — reading a listing that the provider hands back in pieces

  ## Why a bound here is a specification and not a dodge

  `AGENTS.md` forbids `partial def` and forbids leaning on a fuel parameter to
  dodge a termination argument. Listing objects, queues or secrets sits exactly
  on that line, so it is worth being precise about which side of it this module
  is on.

  A remote peer may answer every request with another continuation token,
  forever. There is therefore **no total function that returns a complete
  listing** — not because the proof is hard, but because the thing being
  described does not terminate. "Read at most $n$ pages" is a different
  operation from "read all pages", and it is the one that exists.

  So `maxPages` is not fuel standing in for a missing measure: the recursion is
  structural on it, and it is a *parameter of the specification*. What a bound
  cannot do is promise completeness — and that is where a bound turns into a
  lie, so this module reports completeness rather than assuming it.

  ## The mistake this module is shaped to prevent

  The sibling `typednotes/infra` reads GCS and Pub/Sub listings with a
  hard-coded 50-page cap, warns on standard error, and returns a plain `List`
  that is **indistinguishable from a complete one**. Its own comments say why
  that is dangerous: a truncated listing read as complete makes its planner
  propose creating resources that already exist.

  `Listing` therefore carries `truncated`, and `complete` is derived from it. A
  caller cannot accidentally treat a bounded read as exhaustive, because the
  type does not offer a way to.

  `next` is *where to resume*, which is a separate question from *whether there
  is more*: a budget that runs out before the first fetch has nothing to resume
  from and yet is certainly not complete. Two fields, two facts.

  ## Cursors are opaque

  A `Cursor` wraps whatever the provider sent — S3's `NextContinuationToken`,
  GCS's and Pub/Sub's `nextPageToken` — and is never inspected, compared for
  ordering, or constructed by a caller. Providers document these as opaque and
  change their shape without notice.
-/
import Linen.Cloud.Error

namespace Cloud

-- ── Cursors ─────────────────────────────────────────────────────────────────

/-- An opaque provider continuation token.

    Wraps S3's `NextContinuationToken`, GCS's `nextPageToken`, or Pub/Sub's
    equivalent. Never parsed and never built by a caller: providers document
    these as opaque and reserve the right to change them. -/
structure Cursor where
  /-- The token exactly as the provider sent it. -/
  token : String
  deriving Repr, DecidableEq, BEq

-- ── One page ────────────────────────────────────────────────────────────────

/-- Exactly one page of a listing, with the cursor to ask for the next.

    `next = none` is the provider saying there is no more — the only source of
    that claim, which is why `paginate` never manufactures it. -/
structure Page (α : Type) where
  /-- The items on this page, in the order the provider returned them. -/
  items : List α
  /-- The cursor for the following page, or `none` if this is the last. -/
  next  : Option Cursor := none
  deriving Repr

/-- The empty final page. What a backend answers for a prefix with nothing
    under it. -/
def Page.empty {α : Type} : Page α := { items := [] }

/-- Whether this is the provider's last page. -/
def Page.isLast {α : Type} (p : Page α) : Bool := p.next.isNone

-- ── Several pages ───────────────────────────────────────────────────────────

/-- The result of reading up to a bounded number of pages.

    The two facts that must not be conflated:

    - `truncated` — whether the page budget ran out *before* the provider said
      there was no more. This is the one a caller must not get wrong.
    - `next` — where to resume, if it did. `none` here means "from the
      beginning", which is why it cannot double as the completeness flag. -/
structure Listing (α : Type) where
  /-- Every item read, in provider order, across all pages read. -/
  items     : List α
  /-- The cursor to resume from, meaningful only when `truncated`. -/
  next      : Option Cursor := none
  /-- How many requests were actually issued. -/
  pagesRead : Nat := 0
  /-- Whether the bound was reached before the listing ended. -/
  truncated : Bool := false
  deriving Repr

/-- Whether the whole listing was read.

    Derived from `truncated` rather than from `next`, so that a budget
    exhausted before the first fetch reports incomplete rather than empty and
    complete. -/
def Listing.complete {α : Type} (l : Listing α) : Bool := !l.truncated

/-- A complete listing of nothing. -/
def Listing.empty {α : Type} : Listing α := { items := [] }

-- ── Reading pages ───────────────────────────────────────────────────────────

/-- Read pages until the provider says it is done or `remaining` runs out.

    Structurally recursive on `remaining`; the accumulator is reversed once at
    the end rather than appended to, so the cost is linear in the number of
    items rather than quadratic. -/
private def paginateGo {α : Type} (fetch : Option Cursor → IO (Except Error (Page α))) :
    (remaining : Nat) → Option Cursor → (read : Nat) → List α →
    IO (Except Error (Listing α))
  | 0, cursor, read, acc =>
    -- The budget is spent and the provider never said it was finished, so this
    -- is truncated whatever `cursor` happens to be.
    return .ok { items := acc.reverse, next := cursor, pagesRead := read, truncated := true }
  | remaining + 1, cursor, read, acc => do
    match ← fetch cursor with
    | .error e => return .error e
    | .ok page =>
      let acc := page.items.reverse ++ acc
      match page.next with
      | none   =>
        -- The provider said this was the last page. The only way to be
        -- complete.
        return .ok { items := acc.reverse, pagesRead := read + 1 }
      | some c => paginateGo fetch remaining (some c) (read + 1) acc

/-- Read at most `maxPages` pages of a listing.

    `fetch` is called with `none` for the first page and with the previous
    page's cursor thereafter. The result is complete only if the provider
    itself said so; if the bound was reached first, `truncated` says so and
    `next` says where to resume.

    `maxPages := 0` issues no request at all and reports an incomplete, empty
    listing — the honest answer to "read nothing", and the case that makes
    deriving completeness from `next` unworkable. -/
def paginate {α : Type} (maxPages : Nat)
    (fetch : Option Cursor → IO (Except Error (Page α))) :
    IO (Except Error (Listing α)) :=
  paginateGo fetch maxPages none 0 []

/-- The page budget `listAll`-style convenience functions use when the caller
    does not choose one.

    Deliberately large enough that an ordinary listing completes and small
    enough that a runaway peer is bounded. A caller that needs a different
    answer passes one; a caller that needs *no* bound cannot have one, because
    no such function exists. -/
def defaultMaxPages : Nat := 1000

/-- Resume a truncated listing, reading up to `maxPages` more pages.

    Answers a `Listing` whose `pagesRead` counts only this call's requests. A
    complete listing resumes as itself, with no request issued. -/
def Listing.resume {α : Type} (l : Listing α) (maxPages : Nat)
    (fetch : Option Cursor → IO (Except Error (Page α))) :
    IO (Except Error (Listing α)) := do
  if l.complete then return .ok { l with pagesRead := 0 }
  match ← paginateGo fetch maxPages l.next 0 [] with
  | .error e => return .error e
  | .ok more =>
    return .ok {
        items := l.items ++ more.items
      , next := more.next
      , pagesRead := more.pagesRead
      , truncated := more.truncated }

/-- How many items a page of an **in-memory** backend holds.

    Small on purpose. A local backend that answered everything in one page
    would never exercise a caller's pagination, which is exactly the bug a
    local backend should catch before an account is involved. -/
def inMemoryPageSize : Nat := 3

-- ── Self-checks ─────────────────────────────────────────────────────────────

#guard (Page.empty : Page Nat).isLast
#guard (Listing.empty : Listing Nat).complete
#guard !({ items := ([] : List Nat), truncated := true } : Listing Nat).complete

-- A page carrying a cursor is not the last, however few items it holds.
#guard !({ items := [1], next := some ⟨"tok"⟩ } : Page Nat).isLast

end Cloud
