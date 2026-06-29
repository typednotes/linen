/-
  Linen.Data.ByteString.Lazy — lazy (chunked) byte strings

  A `LazyByteString` is a linked list of **non-empty** strict `ByteString`
  chunks whose tail is deferred via `Thunk`, emulating Haskell's lazy
  `Data.ByteString.Lazy` in Lean's strict setting. The non-empty-chunk
  invariant is carried by the `chunk` constructor's `h : bs.len > 0`.

  Every value is finite (no `partial`/corecursion), so the chunk recursions —
  which Lean accepts structurally through `Thunk` — terminate.
-/

import Linen.Data.ByteString

namespace Data.ByteString.Lazy

/-- A lazy byte string: non-empty strict chunks with a `Thunk`-deferred tail.

    $$\text{LazyByteString} = \text{nil} \mid \text{chunk}(bs, \text{rest}),\quad |bs| > 0$$ -/
inductive LazyByteString where
  /-- The empty lazy byte string. -/
  | nil : LazyByteString
  /-- A non-empty chunk (`h : bs.len > 0`) followed by a lazily-evaluated tail. -/
  | chunk (bs : ByteString) (h : bs.len > 0) (rest : Thunk LazyByteString) : LazyByteString

namespace LazyByteString

/-- The empty lazy byte string. -/
@[inline] def empty : LazyByteString := .nil

instance : Inhabited LazyByteString := ⟨empty⟩

/-- Is this lazy byte string empty? -/
@[inline] def null : LazyByteString → Bool
  | .nil => true
  | .chunk .. => false

/-- Smart constructor that drops empty chunks. -/
def chunk' (bs : ByteString) (rest : Thunk LazyByteString) : LazyByteString :=
  if h : bs.len > 0 then .chunk bs h rest else rest.get

/-- Total length across all chunks (forces all thunks). -/
def length : LazyByteString → Nat
  | .nil => 0
  | .chunk bs _ rest => bs.len + rest.get.length

/-- Build a lazy byte string from strict chunks (empty chunks are dropped). -/
def fromChunks : List ByteString → LazyByteString
  | [] => .nil
  | c :: cs => chunk' c (Thunk.mk fun () => fromChunks cs)

/-- Materialise all chunks into a list (forces all thunks). -/
def toChunks : LazyByteString → List ByteString
  | .nil => []
  | .chunk bs _ rest => bs :: rest.get.toChunks

/-- A single-chunk lazy byte string from a strict one. O(1). -/
def fromStrict (bs : ByteString) : LazyByteString :=
  chunk' bs (Thunk.mk fun () => .nil)

/-- Force all chunks into one strict byte string. O(n). -/
def toStrict (lbs : LazyByteString) : ByteString :=
  ByteString.concat lbs.toChunks

/-- Append two lazy byte strings (O(1) at the call site; the tail is lazy). -/
def append : LazyByteString → LazyByteString → LazyByteString
  | .nil, ys => ys
  | .chunk bs h rest, ys => .chunk bs h (Thunk.mk fun () => rest.get.append ys)

instance : Append LazyByteString := ⟨LazyByteString.append⟩

/-- Cons a byte to the front. -/
def cons (w : UInt8) (lbs : LazyByteString) : LazyByteString :=
  chunk' (ByteString.singleton w) (Thunk.mk fun () => lbs)

/-- Snoc a byte to the end. -/
def snoc (lbs : LazyByteString) (w : UInt8) : LazyByteString :=
  lbs.append (fromStrict (ByteString.singleton w))

/-- The first byte, given non-emptiness. -/
def head : (lbs : LazyByteString) → lbs ≠ .nil → UInt8
  | .nil, h => absurd rfl h
  | .chunk bs hb _, _ => bs.head hb

/-- The first byte, or `none` if empty. -/
def head? : LazyByteString → Option UInt8
  | .nil => none
  | .chunk bs h _ => some (bs.head h)

/-- All bytes except the first, given non-emptiness. -/
def tail : (lbs : LazyByteString) → lbs ≠ .nil → LazyByteString
  | .nil, h => absurd rfl h
  | .chunk bs h rest, _ =>
    if _h2 : bs.len > 1 then chunk' (bs.tail h) rest else rest.get

/-- Decompose into head and tail. -/
def uncons : LazyByteString → Option (UInt8 × LazyByteString)
  | .nil => none
  | .chunk bs h rest =>
    let t := if _h2 : bs.len > 1 then chunk' (bs.tail h) rest else rest.get
    some (bs.head h, t)

/-- Left fold over all bytes, across all chunks. -/
def foldl (f : β → UInt8 → β) (init : β) : LazyByteString → β
  | .nil => init
  | .chunk bs _ rest => rest.get.foldl f (bs.foldl f init)

/-- Right fold over all bytes, across all chunks. -/
def foldr (f : UInt8 → β → β) (init : β) : LazyByteString → β
  | .nil => init
  | .chunk bs _ rest => bs.foldr f (rest.get.foldr f init)

/-- Fold over chunks (left). -/
def foldlChunks (f : β → ByteString → β) (init : β) : LazyByteString → β
  | .nil => init
  | .chunk bs _ rest => rest.get.foldlChunks f (f init bs)

/-- Fold over chunks (right). -/
def foldrChunks (f : ByteString → β → β) (init : β) : LazyByteString → β
  | .nil => init
  | .chunk bs _ rest => f bs (rest.get.foldrChunks f init)

/-- Map a function over all bytes. -/
def map (f : UInt8 → UInt8) : LazyByteString → LazyByteString
  | .nil => .nil
  | .chunk bs _ rest => chunk' (bs.map f) (Thunk.mk fun () => rest.get.map f)

/-- Filter bytes satisfying a predicate. -/
def filter (p : UInt8 → Bool) : LazyByteString → LazyByteString
  | .nil => .nil
  | .chunk bs _ rest => chunk' (bs.filter p) (Thunk.mk fun () => rest.get.filter p)

/-- Take the first `n` bytes. -/
def take (n : Nat) : LazyByteString → LazyByteString
  | .nil => .nil
  | .chunk bs h rest =>
    if n == 0 then .nil
    else if n ≥ bs.len then .chunk bs h (Thunk.mk fun () => rest.get.take (n - bs.len))
    else fromStrict (bs.take n)

/-- Drop the first `n` bytes. -/
def drop (n : Nat) : LazyByteString → LazyByteString
  | .nil => .nil
  | .chunk bs _ rest =>
    if n ≥ bs.len then rest.get.drop (n - bs.len)
    else chunk' (bs.drop n) rest

/-- Split at byte position `n`. -/
def splitAt (n : Nat) (lbs : LazyByteString) : LazyByteString × LazyByteString :=
  (lbs.take n, lbs.drop n)

/-- Reverse (forces all thunks). -/
def reverse (lbs : LazyByteString) : LazyByteString :=
  fromStrict lbs.toStrict.reverse

/-- Pack a list of bytes into a lazy byte string. -/
def pack (ws : List UInt8) : LazyByteString :=
  fromStrict (ByteString.pack ws)

/-- Unpack into a list of bytes (forces all thunks). -/
def unpack (lbs : LazyByteString) : List UInt8 :=
  lbs.toStrict.unpack

/-- True if any byte satisfies the predicate. -/
def any (p : UInt8 → Bool) : LazyByteString → Bool
  | .nil => false
  | .chunk bs _ rest => bs.any p || rest.get.any p

/-- True if all bytes satisfy the predicate. -/
def all (p : UInt8 → Bool) : LazyByteString → Bool
  | .nil => true
  | .chunk bs _ rest => bs.all p && rest.get.all p

/-- Does a byte occur in the lazy byte string? -/
def elem (w : UInt8) (lbs : LazyByteString) : Bool := lbs.any (· == w)

/-- Concatenate a list of lazy byte strings. -/
def concat (lbss : List LazyByteString) : LazyByteString :=
  lbss.foldl (· ++ ·) empty

/-! ── Instances (compare by the forced strict form) ── -/

instance : BEq LazyByteString := ⟨fun a b => a.toStrict == b.toStrict⟩
instance : Ord LazyByteString := ⟨fun a b => compare a.toStrict b.toStrict⟩
instance : ToString LazyByteString := ⟨fun lbs => toString lbs.toStrict⟩
instance : Repr LazyByteString where
  reprPrec lbs _ := Std.Format.text (toString lbs)
instance : Hashable LazyByteString := ⟨fun lbs => hash lbs.toStrict⟩

end LazyByteString
end Data.ByteString.Lazy
