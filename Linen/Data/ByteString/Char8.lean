/-
  Linen.Data.ByteString.Char8 — character-oriented strict byte strings

  A `Char`-oriented view of `Data.ByteString`: each byte is a Latin-1 character
  (`c2w c = c.toNat % 256`, `w2c w = Char.ofNat w.toNat`). No new types — every
  function operates on `ByteString` with `Char ↔ UInt8` conversions. Mirrors
  Haskell's `Data.ByteString.Char8`.

  `lines`/`words` are structural recursions over `List UInt8` (no `partial`, no
  fuel).
-/

import Linen.Data.ByteString

namespace Data.ByteString.Char8

/-- Convert a `Char` to a byte (Latin-1 truncation): $\text{c2w}(c) = c \bmod 256$. -/
@[inline] private def c2w (c : Char) : UInt8 := c.toNat.toUInt8

/-- Convert a byte to a `Char` (Latin-1): $\text{w2c}(w) = \text{Char.ofNat}(w)$. -/
@[inline] private def w2c (w : UInt8) : Char := Char.ofNat w.toNat

/-! ── Pack / unpack ── -/

/-- Pack a `String` into a `ByteString` (Latin-1 truncation). -/
def pack (s : String) : ByteString :=
  ByteString.pack (s.toList.map c2w)

/-- Unpack a `ByteString` into a `String` (Latin-1 interpretation). -/
def unpack (bs : ByteString) : String :=
  String.ofList (bs.unpack.map w2c)

/-! ── Character-oriented wrappers ── -/

/-- Cons a character to the front. -/
def cons (c : Char) (bs : ByteString) : ByteString :=
  ByteString.cons (c2w c) bs

/-- Snoc a character to the end. -/
def snoc (bs : ByteString) (c : Char) : ByteString :=
  ByteString.snoc bs (c2w c)

/-- The first character, with proof of non-emptiness. -/
def head (bs : ByteString) (h : bs.len > 0) : Char :=
  w2c (bs.head h)

/-- The first character, or `none` if empty. -/
def head? (bs : ByteString) : Option Char :=
  bs.head?.map w2c

/-- The last character, with proof of non-emptiness. -/
def last (bs : ByteString) (h : bs.len > 0) : Char :=
  w2c (bs.last h)

/-- Map a character function over every byte. -/
def map (f : Char → Char) (bs : ByteString) : ByteString :=
  ByteString.map (fun w => c2w (f (w2c w))) bs

/-- Filter bytes whose character satisfies the predicate. -/
def filter (p : Char → Bool) (bs : ByteString) : ByteString :=
  ByteString.filter (fun w => p (w2c w)) bs

/-- Left fold with characters. -/
def foldl (f : β → Char → β) (init : β) (bs : ByteString) : β :=
  ByteString.foldl (fun acc w => f acc (w2c w)) init bs

/-- Right fold with characters. -/
def foldr (f : Char → β → β) (init : β) (bs : ByteString) : β :=
  ByteString.foldr (fun w acc => f (w2c w) acc) init bs

/-- Take characters while the predicate holds. -/
def takeWhile (p : Char → Bool) (bs : ByteString) : ByteString :=
  ByteString.takeWhile (fun w => p (w2c w)) bs

/-- Drop characters while the predicate holds. -/
def dropWhile (p : Char → Bool) (bs : ByteString) : ByteString :=
  ByteString.dropWhile (fun w => p (w2c w)) bs

/-- Split where the character predicate first holds. -/
def «break» (p : Char → Bool) (bs : ByteString) : ByteString × ByteString :=
  ByteString.«break» (fun w => p (w2c w)) bs

/-- Split where the character predicate first fails. -/
def span (p : Char → Bool) (bs : ByteString) : ByteString × ByteString :=
  ByteString.span (fun w => p (w2c w)) bs

/-- Does a character occur in the `ByteString`? -/
def elem (c : Char) (bs : ByteString) : Bool :=
  ByteString.elem (c2w c) bs

/-- Find the first character satisfying a predicate. -/
def find (p : Char → Bool) (bs : ByteString) : Option Char :=
  (ByteString.find (fun w => p (w2c w)) bs).map w2c

/-! ── Lines & words (structural over the byte list) ── -/

/-- Split a byte list into lines at `'\n'` (byte `10`). A trailing newline does
    not produce a trailing empty line (Haskell's `lines`). Structural on the
    input list. -/
private def linesAux : List UInt8 → List UInt8 → List (List UInt8)
  | [], cur => if cur.isEmpty then [] else [cur.reverse]
  | w :: rest, cur =>
    if w == 10 then cur.reverse :: linesAux rest []
    else linesAux rest (w :: cur)

/-- Split a `ByteString` into lines on `'\n'`. -/
def lines (bs : ByteString) : List ByteString :=
  (linesAux bs.unpack []).map ByteString.pack

/-- Is the byte ASCII whitespace (space, tab, newline, carriage return)? -/
private def isSpaceByte (w : UInt8) : Bool := w == 32 || w == 9 || w == 10 || w == 13

/-- Split a byte list into words on whitespace runs, dropping empty fields
    (Haskell's `words`). Structural on the input list. -/
private def wordsAux : List UInt8 → List UInt8 → List (List UInt8)
  | [], cur => if cur.isEmpty then [] else [cur.reverse]
  | w :: rest, cur =>
    if isSpaceByte w then
      if cur.isEmpty then wordsAux rest [] else cur.reverse :: wordsAux rest []
    else wordsAux rest (w :: cur)

/-- Split a `ByteString` into words on whitespace. -/
def words (bs : ByteString) : List ByteString :=
  (wordsAux bs.unpack []).map ByteString.pack

/-- Join lines, appending a newline after each. -/
def unlines (bss : List ByteString) : ByteString :=
  ByteString.concat (bss.map (fun bs => ByteString.snoc bs 10))

/-- Join words with single spaces. -/
def unwords (bss : List ByteString) : ByteString :=
  ByteString.intercalate (ByteString.singleton 32) bss

end Data.ByteString.Char8
