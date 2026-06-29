/-
  Linen.Data.ByteString — slice-based strict byte strings

  A `ByteString` is a slice into a core `ByteArray` carrying an offset, a length,
  and a proof `off + len ≤ data.size`. This gives **O(1) `take`/`drop`/`splitAt`**
  (just pointer arithmetic), the value core's `ByteArray` lacks (its `extract`
  copies). Mirrors Haskell's `Data.ByteString`.

  All loops are structural recursion on a `remaining` byte counter (no `partial`,
  no fuel); `groupBy` uses a proven well-founded measure.
-/

namespace Data

/-- A slice into a `ByteArray`, enabling O(1) `take`/`drop`/`splitAt`.

    $$\text{ByteString} = \{ d : \text{ByteArray},\; o : \mathbb{N},\; l : \mathbb{N} \mid o + l \leq |d| \}$$

    The bytes are `d[o], d[o+1], …, d[o+l-1]`. -/
structure ByteString where
  /-- The underlying byte array (shared; not copied on slice). -/
  data : ByteArray
  /-- Offset into `data` where this slice begins. -/
  off : Nat
  /-- Number of bytes in this slice. -/
  len : Nat
  /-- Proof that the slice is within bounds. -/
  valid : off + len ≤ data.size

namespace ByteString

/-- Construct a `ByteString` viewing a whole fresh `ByteArray` (off = 0). -/
@[inline] private def ofByteArray (arr : ByteArray) : ByteString :=
  ⟨arr, 0, arr.size, by omega⟩

/-! ── Construction ── -/

/-- The empty `ByteString`. O(1). -/
@[inline] def empty : ByteString :=
  ⟨ByteArray.empty, 0, 0, by omega⟩

instance : Inhabited ByteString := ⟨empty⟩

/-- A single-byte `ByteString`. -/
@[inline] def singleton (w : UInt8) : ByteString :=
  ofByteArray (ByteArray.empty.push w)

/-- Pack a list of bytes into a `ByteString`. -/
def pack (ws : List UInt8) : ByteString :=
  ofByteArray (ws.foldl (fun a w => a.push w) ByteArray.empty)

/-- Unpack a `ByteString` into a list of bytes. -/
def unpack (bs : ByteString) : List UInt8 :=
  go bs.off bs.len []
where
  go (i : Nat) (remaining : Nat) (acc : List UInt8) : List UInt8 :=
    match remaining with
    | 0 => acc.reverse
    | n + 1 => go (i + 1) n (bs.data.get! i :: acc)

/-- A `ByteString` of `n` copies of byte `w`. -/
def replicate (n : Nat) (w : UInt8) : ByteString :=
  ofByteArray ((List.replicate n w).foldl (fun a b => a.push b) ByteArray.empty)

/-- Force a copy of the slice into a fresh `ByteArray`. O(n); breaks sharing. -/
def copy (bs : ByteString) : ByteString :=
  ofByteArray (bs.data.extract bs.off (bs.off + bs.len))

/-! ── Basic interface ── -/

/-- Is this `ByteString` empty? -/
@[inline] def null (bs : ByteString) : Bool := bs.len == 0

/-- The number of bytes. -/
@[inline] def length (bs : ByteString) : Nat := bs.len

/-- Cons a byte to the front. O(n). -/
def cons (w : UInt8) (bs : ByteString) : ByteString :=
  ofByteArray (ByteArray.empty.push w |>.append (bs.data.extract bs.off (bs.off + bs.len)))

/-- Snoc a byte to the end. O(n). -/
def snoc (bs : ByteString) (w : UInt8) : ByteString :=
  ofByteArray ((bs.data.extract bs.off (bs.off + bs.len)).push w)

/-- The first byte, with proof of non-emptiness. -/
def head (bs : ByteString) (h : bs.len > 0) : UInt8 :=
  have hv := bs.valid
  bs.data[bs.off]'(by omega)

/-- The first byte, or `none` if empty. -/
def head? (bs : ByteString) : Option UInt8 :=
  if h : bs.len > 0 then some (bs.head h) else none

/-- All bytes except the first. O(1) slicing. -/
def tail (bs : ByteString) (h : bs.len > 0) : ByteString :=
  have hv := bs.valid
  ⟨bs.data, bs.off + 1, bs.len - 1, by omega⟩

/-- The last byte, with proof of non-emptiness. -/
def last (bs : ByteString) (h : bs.len > 0) : UInt8 :=
  have hv := bs.valid
  bs.data[bs.off + bs.len - 1]'(by omega)

/-- All bytes except the last. O(1) slicing. -/
def init (bs : ByteString) (h : bs.len > 0) : ByteString :=
  have hv := bs.valid
  ⟨bs.data, bs.off, bs.len - 1, by omega⟩

/-- Decompose into head and tail, or `none` if empty. -/
def uncons (bs : ByteString) : Option (UInt8 × ByteString) :=
  if h : bs.len > 0 then some (bs.head h, bs.tail h) else none

/-- Decompose into init and last, or `none` if empty. -/
def unsnoc (bs : ByteString) : Option (ByteString × UInt8) :=
  if h : bs.len > 0 then some (bs.init h, bs.last h) else none

/-! ── Append ── -/

/-- Concatenate two `ByteString`s. O(m + n). -/
def append (a b : ByteString) : ByteString :=
  if a.null then b.copy
  else if b.null then a.copy
  else
    let arrA := a.data.extract a.off (a.off + a.len)
    ofByteArray (arrA.append (b.data.extract b.off (b.off + b.len)))

instance : Append ByteString := ⟨ByteString.append⟩

/-- Concatenate a list of `ByteString`s. -/
def concat (bss : List ByteString) : ByteString :=
  bss.foldl (· ++ ·) empty

/-- Intercalate a separator between `ByteString`s. -/
def intercalate (sep : ByteString) : List ByteString → ByteString
  | [] => empty
  | [x] => x.copy
  | x :: xs => xs.foldl (fun acc b => acc ++ sep ++ b) x

/-! ── Transform ── -/

/-- Map a function over every byte. O(n). -/
def map (f : UInt8 → UInt8) (bs : ByteString) : ByteString :=
  ofByteArray (go bs.off bs.len ByteArray.empty)
where
  go (i : Nat) (remaining : Nat) (acc : ByteArray) : ByteArray :=
    match remaining with
    | 0 => acc
    | n + 1 => go (i + 1) n (acc.push (f (bs.data.get! i)))

/-- Reverse the bytes. O(n). -/
def reverse (bs : ByteString) : ByteString :=
  ofByteArray (go (bs.off + bs.len) bs.len ByteArray.empty)
where
  go (i : Nat) (remaining : Nat) (acc : ByteArray) : ByteArray :=
    match remaining with
    | 0 => acc
    | n + 1 => go (i - 1) n (acc.push (bs.data.get! (i - 1)))

/-- Intersperse a byte between each element. O(n). -/
def intersperse (w : UInt8) (bs : ByteString) : ByteString :=
  if bs.len ≤ 1 then bs.copy
  else ofByteArray (go (bs.off + 1) (bs.len - 1) (ByteArray.empty.push (bs.data.get! bs.off)))
where
  go (i : Nat) (remaining : Nat) (acc : ByteArray) : ByteArray :=
    match remaining with
    | 0 => acc
    | n + 1 => go (i + 1) n (acc.push w |>.push (bs.data.get! i))

/-- Transpose rows and columns of a list of `ByteString`s. -/
def transpose (bss : List ByteString) : List ByteString :=
  let maxLen := bss.foldl (fun m bs => Nat.max m bs.len) 0
  List.range maxLen |>.map fun col =>
    pack (bss.filterMap fun bs =>
      if col < bs.len then some (bs.data.get! (bs.off + col)) else none)

/-! ── Folds ── -/

/-- Left fold over bytes. O(n). -/
def foldl (f : β → UInt8 → β) (init : β) (bs : ByteString) : β :=
  go bs.off bs.len init
where
  go (i : Nat) (remaining : Nat) (acc : β) : β :=
    match remaining with
    | 0 => acc
    | n + 1 => go (i + 1) n (f acc (bs.data.get! i))

/-- Right fold over bytes. O(n). -/
def foldr (f : UInt8 → β → β) (init : β) (bs : ByteString) : β :=
  go (bs.off + bs.len) bs.len init
where
  go (i : Nat) (remaining : Nat) (acc : β) : β :=
    match remaining with
    | 0 => acc
    | n + 1 => go (i - 1) n (f (bs.data.get! (i - 1)) acc)

/-- Left fold on a non-empty `ByteString`, seeded with the first byte. -/
def foldl1 (f : UInt8 → UInt8 → UInt8) (bs : ByteString) (h : bs.len > 0) : UInt8 :=
  (bs.tail h).foldl f (bs.head h)

/-- Right fold on a non-empty `ByteString`, seeded with the last byte. -/
def foldr1 (f : UInt8 → UInt8 → UInt8) (bs : ByteString) (h : bs.len > 0) : UInt8 :=
  (bs.init h).foldr f (bs.last h)

/-- Map with accumulator, left-to-right. O(n). -/
def mapAccumL (f : σ → UInt8 → σ × UInt8) (init : σ) (bs : ByteString) : σ × ByteString :=
  let (s, arr) := go bs.off bs.len init ByteArray.empty
  (s, ofByteArray arr)
where
  go (i : Nat) (remaining : Nat) (s : σ) (acc : ByteArray) : σ × ByteArray :=
    match remaining with
    | 0 => (s, acc)
    | n + 1 =>
      let (s', w') := f s (bs.data.get! i)
      go (i + 1) n s' (acc.push w')

/-- Map with accumulator, right-to-left. O(n). -/
def mapAccumR (f : σ → UInt8 → σ × UInt8) (init : σ) (bs : ByteString) : σ × ByteString :=
  let (s, ws) := go (bs.off + bs.len) bs.len init []
  (s, ofByteArray (ws.foldl (fun a w => a.push w) ByteArray.empty))
where
  go (i : Nat) (remaining : Nat) (s : σ) (acc : List UInt8) : σ × List UInt8 :=
    match remaining with
    | 0 => (s, acc)
    | n + 1 =>
      let (s', w') := f s (bs.data.get! (i - 1))
      go (i - 1) n s' (w' :: acc)

/-- Map a function over bytes and concatenate the results. -/
def concatMap (f : UInt8 → ByteString) (bs : ByteString) : ByteString :=
  concat (bs.foldl (fun acc w => acc ++ [f w]) [])

/-- True if any byte satisfies the predicate. -/
def any (p : UInt8 → Bool) (bs : ByteString) : Bool :=
  bs.foldl (fun acc w => acc || p w) false

/-- True if all bytes satisfy the predicate. -/
def all (p : UInt8 → Bool) (bs : ByteString) : Bool :=
  bs.foldl (fun acc w => acc && p w) true

/-- Maximum byte in a non-empty `ByteString`. -/
def maximum (bs : ByteString) (h : bs.len > 0) : UInt8 :=
  (bs.tail h).foldl (fun acc w => if w > acc then w else acc) (bs.head h)

/-- Minimum byte in a non-empty `ByteString`. -/
def minimum (bs : ByteString) (h : bs.len > 0) : UInt8 :=
  (bs.tail h).foldl (fun acc w => if w < acc then w else acc) (bs.head h)

/-! ── Scans ── -/

/-- Left scan. Result has length `n + 1`. -/
def scanl (f : UInt8 → UInt8 → UInt8) (z : UInt8) (bs : ByteString) : ByteString :=
  ofByteArray (go bs.off bs.len z (ByteArray.empty.push z))
where
  go (i : Nat) (remaining : Nat) (acc : UInt8) (arr : ByteArray) : ByteArray :=
    match remaining with
    | 0 => arr
    | n + 1 =>
      let acc' := f acc (bs.data.get! i)
      go (i + 1) n acc' (arr.push acc')

/-- Left scan on a non-empty `ByteString`, seeded with the first byte. -/
def scanl1 (f : UInt8 → UInt8 → UInt8) (bs : ByteString) (h : bs.len > 0) : ByteString :=
  scanl f (bs.head h) (bs.tail h)

/-- Right scan. Result has length `n + 1`. -/
def scanr (f : UInt8 → UInt8 → UInt8) (z : UInt8) (bs : ByteString) : ByteString :=
  pack (go (bs.off + bs.len) bs.len z [z])
where
  go (i : Nat) (remaining : Nat) (acc : UInt8) (result : List UInt8) : List UInt8 :=
    match remaining with
    | 0 => result
    | n + 1 =>
      let acc' := f (bs.data.get! (i - 1)) acc
      go (i - 1) n acc' (acc' :: result)

/-- Right scan on a non-empty `ByteString`, seeded with the last byte. -/
def scanr1 (f : UInt8 → UInt8 → UInt8) (bs : ByteString) (h : bs.len > 0) : ByteString :=
  scanr f (bs.last h) (bs.init h)

/-! ── O(1) substrings (the main value-add over `ByteArray`) ── -/

/-- Take the first `n` bytes. O(1) — no copying. -/
@[inline] def take (n : Nat) (bs : ByteString) : ByteString :=
  have hv := bs.valid
  ⟨bs.data, bs.off, min n bs.len, by omega⟩

/-- Drop the first `n` bytes. O(1) — no copying. -/
@[inline] def drop (n : Nat) (bs : ByteString) : ByteString :=
  have hv := bs.valid
  ⟨bs.data, bs.off + min n bs.len, bs.len - min n bs.len, by omega⟩

/-- Split at position `n`: `(take n bs, drop n bs)`. O(1). -/
@[inline] def splitAt (n : Nat) (bs : ByteString) : ByteString × ByteString :=
  (bs.take n, bs.drop n)

/-- Take bytes from the front while the predicate holds. -/
def takeWhile (p : UInt8 → Bool) (bs : ByteString) : ByteString :=
  have hv := bs.valid
  ⟨bs.data, bs.off, min (go bs.off bs.len 0) bs.len, by omega⟩
where
  go (i : Nat) (remaining : Nat) (count : Nat) : Nat :=
    match remaining with
    | 0 => count
    | n + 1 => if p (bs.data.get! i) then go (i + 1) n (count + 1) else count

/-- Drop bytes from the front while the predicate holds. -/
def dropWhile (p : UInt8 → Bool) (bs : ByteString) : ByteString :=
  bs.drop (bs.takeWhile p).len

/-- Split where the predicate first fails: `(takeWhile p, dropWhile p)`. -/
def span (p : UInt8 → Bool) (bs : ByteString) : ByteString × ByteString :=
  let tw := bs.takeWhile p
  (tw, bs.drop tw.len)

/-- Split where the predicate first holds: `span (not ∘ p)`. -/
def «break» (p : UInt8 → Bool) (bs : ByteString) : ByteString × ByteString :=
  bs.span (fun w => !p w)

/-- Span equal elements from the front of a list into an accumulator,
    returning `(group, remaining)` where `remaining` is a suffix of the input. -/
private def spanEqList (eq : UInt8 → UInt8 → Bool) (prev : UInt8) (ws : List UInt8)
    (acc : List UInt8) : List UInt8 × List UInt8 :=
  match ws with
  | [] => (acc, [])
  | w :: rest =>
    if eq prev w then spanEqList eq w rest (w :: acc)
    else (acc, w :: rest)

/-- `spanEqList` returns a remainder no longer than the input. -/
private theorem spanEqList_length_le (eq : UInt8 → UInt8 → Bool) (prev : UInt8)
    (ws : List UInt8) (acc : List UInt8) :
    (spanEqList eq prev ws acc).2.length ≤ ws.length := by
  induction ws generalizing prev acc with
  | nil => simp [spanEqList]
  | cons w rest ih =>
    simp only [spanEqList]
    split
    · exact Nat.le_succ_of_le (ih w (w :: acc))
    · simp

/-- Group consecutive bytes by an equivalence relation. -/
def groupBy (eq : UInt8 → UInt8 → Bool) (bs : ByteString) : List ByteString :=
  if bs.null then [] else go bs.unpack []
where
  go (ws : List UInt8) (acc : List ByteString) : List ByteString :=
    match ws with
    | [] => acc.reverse
    | w :: rest =>
      let result := spanEqList eq w rest [w]
      go result.2 (pack result.1.reverse :: acc)
    termination_by ws.length
    decreasing_by exact Nat.lt_succ_of_le (spanEqList_length_le eq w rest [w])

/-- Group consecutive equal bytes. -/
def group (bs : ByteString) : List ByteString :=
  groupBy (· == ·) bs

/-- All initial segments (prefixes). -/
def inits (bs : ByteString) : List ByteString :=
  List.range (bs.len + 1) |>.map (fun n => bs.take n)

/-- All final segments (suffixes). -/
def tails (bs : ByteString) : List ByteString :=
  List.range (bs.len + 1) |>.map (fun n => bs.drop n)

/-- Is `pfx` a prefix of `bs`? -/
def isPrefixOf (pfx bs : ByteString) : Bool :=
  if pfx.len > bs.len then false else go 0 pfx.len
where
  go (i : Nat) (remaining : Nat) : Bool :=
    match remaining with
    | 0 => true
    | n + 1 =>
      if bs.data.get! (bs.off + i) == pfx.data.get! (pfx.off + i)
      then go (i + 1) n else false

/-- Is `sfx` a suffix of `bs`? -/
def isSuffixOf (sfx bs : ByteString) : Bool :=
  if sfx.len > bs.len then false
  else go 0 sfx.len (bs.len - sfx.len)
where
  go (i : Nat) (remaining : Nat) (bsStart : Nat) : Bool :=
    match remaining with
    | 0 => true
    | n + 1 =>
      if bs.data.get! (bs.off + bsStart + i) == sfx.data.get! (sfx.off + i)
      then go (i + 1) n bsStart else false

/-- Is `needle` an infix (substring) of `bs`? -/
def isInfixOf (needle bs : ByteString) : Bool :=
  if needle.null then true
  else if needle.len > bs.len then false
  else (List.range (bs.len - needle.len + 1)).any fun start =>
    needle.isPrefixOf (bs.drop start)

/-- Strip a prefix, returning `none` if not a prefix. -/
def stripPrefix (pfx bs : ByteString) : Option ByteString :=
  if pfx.isPrefixOf bs then some (bs.drop pfx.len) else none

/-- Strip a suffix, returning `none` if not a suffix. -/
def stripSuffix (sfx bs : ByteString) : Option ByteString :=
  if sfx.isSuffixOf bs then some (bs.take (bs.len - sfx.len)) else none

/-! ── Search ── -/

/-- Does the byte occur in the `ByteString`? -/
def elem (w : UInt8) (bs : ByteString) : Bool :=
  bs.any (· == w)

/-- Does the byte NOT occur? -/
def notElem (w : UInt8) (bs : ByteString) : Bool :=
  !bs.elem w

/-- Find the first byte satisfying a predicate. -/
def find (p : UInt8 → Bool) (bs : ByteString) : Option UInt8 :=
  go bs.off bs.len
where
  go (i : Nat) (remaining : Nat) : Option UInt8 :=
    match remaining with
    | 0 => none
    | n + 1 =>
      let w := bs.data.get! i
      if p w then some w else go (i + 1) n

/-- Filter bytes satisfying a predicate. O(n). -/
def filter (p : UInt8 → Bool) (bs : ByteString) : ByteString :=
  ofByteArray (bs.foldl (fun acc w => if p w then acc.push w else acc) ByteArray.empty)

/-- Partition into `(satisfying, not-satisfying)`. O(n). -/
def partition (p : UInt8 → Bool) (bs : ByteString) : ByteString × ByteString :=
  let (yes, no) := bs.foldl (fun (y, n) w =>
    if p w then (y.push w, n) else (y, n.push w)) (ByteArray.empty, ByteArray.empty)
  (ofByteArray yes, ofByteArray no)

/-- Index into the `ByteString` with a bounds proof. -/
@[inline] def index (bs : ByteString) (i : Nat) (h : i < bs.len) : UInt8 :=
  have hv := bs.valid
  bs.data[bs.off + i]'(by omega)

/-- Index of the first byte satisfying a predicate. -/
def findIndex (p : UInt8 → Bool) (bs : ByteString) : Option Nat :=
  go bs.off bs.len 0
where
  go (i : Nat) (remaining : Nat) (idx : Nat) : Option Nat :=
    match remaining with
    | 0 => none
    | n + 1 => if p (bs.data.get! i) then some idx else go (i + 1) n (idx + 1)

/-- All indices where the predicate holds. -/
def findIndices (p : UInt8 → Bool) (bs : ByteString) : List Nat :=
  go bs.off bs.len 0 []
where
  go (i : Nat) (remaining : Nat) (idx : Nat) (acc : List Nat) : List Nat :=
    match remaining with
    | 0 => acc.reverse
    | n + 1 =>
      let acc' := if p (bs.data.get! i) then idx :: acc else acc
      go (i + 1) n (idx + 1) acc'

/-- Index of the first occurrence of a byte. -/
def elemIndex (w : UInt8) (bs : ByteString) : Option Nat :=
  findIndex (· == w) bs

/-- All indices where a byte occurs. -/
def elemIndices (w : UInt8) (bs : ByteString) : List Nat :=
  findIndices (· == w) bs

/-- Count occurrences of a byte. O(n). -/
def count (w : UInt8) (bs : ByteString) : Nat :=
  bs.foldl (fun acc b => if b == w then acc + 1 else acc) 0

/-! ── I/O ── -/

/-- Read a file into a `ByteString` (`IO.FS.readBinFile`). -/
def readFile (path : System.FilePath) : IO ByteString := do
  return ofByteArray (← IO.FS.readBinFile path)

/-- Write a `ByteString` to a file (`IO.FS.writeBinFile`). -/
def writeFile (path : System.FilePath) (bs : ByteString) : IO Unit :=
  IO.FS.writeBinFile path (bs.data.extract bs.off (bs.off + bs.len))

/-- Append a `ByteString` to a file. -/
def appendFile (path : System.FilePath) (bs : ByteString) : IO Unit := do
  let h ← IO.FS.Handle.mk path .append
  h.write (bs.data.extract bs.off (bs.off + bs.len))

/-- Read `n` bytes from a handle (`IO.FS.Handle.read`). -/
def hGet (h : IO.FS.Handle) (n : USize) : IO ByteString := do
  return ofByteArray (← h.read n)

/-- Write a `ByteString` to a handle (`IO.FS.Handle.write`). -/
def hPut (h : IO.FS.Handle) (bs : ByteString) : IO Unit :=
  h.write (bs.data.extract bs.off (bs.off + bs.len))

/-! ── Instances ── -/

private def beqAux (a b : ByteString) : Bool :=
  if a.len != b.len then false else go 0 a.len
where
  go (i : Nat) (remaining : Nat) : Bool :=
    match remaining with
    | 0 => true
    | n + 1 =>
      if a.data.get! (a.off + i) == b.data.get! (b.off + i)
      then go (i + 1) n else false

instance : BEq ByteString := ⟨beqAux⟩

private def compareAux (a b : ByteString) : Ordering :=
  go 0 (min a.len b.len)
where
  go (i : Nat) (remaining : Nat) : Ordering :=
    match remaining with
    | 0 => compare a.len b.len
    | n + 1 =>
      match compare (a.data.get! (a.off + i)) (b.data.get! (b.off + i)) with
      | .eq => go (i + 1) n
      | ord => ord

instance : Ord ByteString := ⟨compareAux⟩

instance : ToString ByteString where
  toString bs := "[" ++ String.intercalate ", " (bs.unpack.map (fun w => toString w.toNat)) ++ "]"

instance : Repr ByteString where
  reprPrec bs _ := Std.Format.text (toString bs)

instance : Hashable ByteString where
  hash bs := bs.foldl (fun h w => mixHash h (hash w)) 7

/-! ── Proofs ── -/

/-- `take` preserves the bounds invariant (by construction). -/
theorem take_valid (n : Nat) (bs : ByteString) :
    (bs.take n).off + (bs.take n).len ≤ (bs.take n).data.size :=
  (bs.take n).valid

/-- `drop` preserves the bounds invariant (by construction). -/
theorem drop_valid (n : Nat) (bs : ByteString) :
    (bs.drop n).off + (bs.drop n).len ≤ (bs.drop n).data.size :=
  (bs.drop n).valid

/-- `null` iff `length` is zero. -/
theorem null_iff_length_zero (bs : ByteString) :
    bs.null = true ↔ bs.length = 0 := by
  simp [null, length, BEq.beq]

/-- Length of `take` is `min n bs.len`. -/
theorem take_length (n : Nat) (bs : ByteString) :
    (bs.take n).len = min n bs.len := by
  simp [take]

/-- Length of `drop` is `bs.len - min n bs.len`. -/
theorem drop_length (n : Nat) (bs : ByteString) :
    (bs.drop n).len = bs.len - min n bs.len := by
  simp [drop]

end ByteString
end Data
