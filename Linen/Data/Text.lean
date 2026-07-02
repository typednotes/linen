/-
  Linen.Data.Text — Unicode text type and operations

  Provides Haskell's `Data.Text` API on top of Lean's built-in `String`.

  ## Haskell equivalent
  `Data.Text` (https://hackage.haskell.org/package/text/docs/Data-Text.html)

  ## Design
  `Text` is `abbrev Text := String`. Lean's `String` is already a UTF-8 encoded
  Unicode string, so we reuse it directly and layer Haskell-compatible naming on top.

  $$\text{Text} \cong \text{String}$$

  ## Lean stdlib reuse
  Most operations delegate to `String` and `List Char` methods from Lean's stdlib.
  `words`/`unwords`/`unlines` delegate to `Linen.Data.String`, which already adds
  them directly on `String`, rather than re-implementing the tokenising logic here.

  Fuel-counter recursion is avoided throughout: `chunksOf` and `isInfixOf` use
  genuine structural/well-founded recursion on the list being consumed (matching
  `groupByAux` below), and `transpose` is reformulated as an index-driven `map`
  over `List.range`, needing no custom recursion at all.
-/
import Linen.Data.String

namespace Data

/-- Unicode text type. Lean's `String` is already UTF-8 encoded Unicode.
    $$\text{Text} := \text{String}$$ -/
abbrev Text := String

namespace Text

-- ── Construction ─────────────────────────────────

/-- Convert a list of characters to `Text`.
    $$\text{pack}([c_1, \ldots, c_n]) = \text{"c_1 \ldots c_n"}$$ -/
@[inline] def pack (cs : List Char) : Text := String.ofList cs

/-- Convert `Text` to a list of characters.
    $$\text{unpack}(t) = [t_0, t_1, \ldots, t_{n-1}]$$ -/
@[inline] def unpack (t : Text) : List Char := t.toList

/-- A `Text` containing a single character.
    $$\text{singleton}(c) = \text{"c"}$$ -/
@[inline] def singleton (c : Char) : Text := String.ofList [c]

/-- The empty `Text`.
    $$\text{empty} = \text{""}$$ -/
@[inline] def empty : Text := ""

-- ── Basic interface ──────────────────────────────

/-- Is the `Text` empty?
    $$\text{null}(t) \iff |t| = 0$$ -/
@[inline] def null (t : Text) : Bool := t.isEmpty

/-- The number of characters (code points) in the `Text`.
    $$\text{length}(t) = |t|$$ -/
@[inline] def length (t : Text) : Nat := t.toList.length

/-- Compare the length of a `Text` against a `Nat`.
    Returns `Ordering.lt`, `Ordering.eq`, or `Ordering.gt`.
    $$\text{compareLength}(t, n) = \text{compare}(|t|, n)$$ -/
def compareLength (t : Text) (n : Nat) : Ordering :=
  compare t.length n

-- ── Transformations ──────────────────────────────

/-- Apply a function to every character.
    $$\text{map}(f, t) = \text{pack}(\text{List.map}\ f\ (\text{unpack}\ t))$$ -/
def map (f : Char → Char) (t : Text) : Text :=
  pack (t.toList.map f)

/-- Insert a separator `Text` between elements and concatenate.
    $$\text{intercalate}(sep, [t_1, \ldots, t_k]) = t_1 \mathbin{+\!\!+} sep \mathbin{+\!\!+} \cdots \mathbin{+\!\!+} t_k$$ -/
def intercalate (sep : Text) (ts : List Text) : Text :=
  String.intercalate sep ts

/-- Insert a character between each character of the `Text`.
    $$\text{intersperse}(c, \text{"abc"}) = \text{"acbcc"}$$ -/
def intersperse (c : Char) (t : Text) : Text :=
  match t.toList with
  | [] => empty
  | [x] => singleton x
  | x :: xs => pack (x :: xs.flatMap (fun ch => [c, ch]))

/-- Transpose rows and columns of a list of `Text`s. Column `i` collects the
    `i`-th character of every row that has one, matching Haskell's ragged-row
    behaviour.
    $$\text{transpose}([\text{"ab"}, \text{"cd"}]) = [\text{"ac"}, \text{"bd"}]$$ -/
def transpose (ts : List Text) : List Text :=
  let css := ts.map (·.toList)
  let maxLen := css.foldl (fun acc l => Nat.max acc l.length) 0
  (List.range maxLen).map fun i => pack (css.filterMap (·[i]?))

/-- Reverse the characters.
    $$\text{reverse}(\text{"abc"}) = \text{"cba"}$$ -/
def reverse (t : Text) : Text :=
  pack t.toList.reverse

/-- Replace all non-overlapping occurrences of `needle` with `replacement`.
    $$\text{replace}(\text{needle}, \text{replacement}, t)$$ -/
def replace (needle replacement : Text) (t : Text) : Text :=
  if needle.isEmpty then t
  else
    let parts := t.splitOn needle
    String.intercalate replacement parts

-- ── Case conversion ──────────────────────────────

/-- Convert a character to lower case. -/
private def charToLower (c : Char) : Char := c.toLower

/-- Convert a character to upper case. -/
private def charToUpper (c : Char) : Char := c.toUpper

/-- Case-fold: convert to lower case for case-insensitive comparisons.
    $$\text{toCaseFold}(t) \approx \text{toLower}(t)$$
    Note: Full Unicode case folding is approximated by `toLower`. -/
def toCaseFold (t : Text) : Text :=
  map charToLower t

/-- Convert all characters to lower case.
    $$\text{toLower}(\text{"ABC"}) = \text{"abc"}$$ -/
def toLower (t : Text) : Text :=
  map charToLower t

/-- Convert all characters to upper case.
    $$\text{toUpper}(\text{"abc"}) = \text{"ABC"}$$ -/
def toUpper (t : Text) : Text :=
  map charToUpper t

/-- Title case: capitalize the first character, lowercase the rest.
    $$\text{toTitle}(\text{"hello world"}) = \text{"Hello world"}$$ -/
def toTitle (t : Text) : Text :=
  match t.toList with
  | [] => empty
  | c :: cs => pack (c.toUpper :: cs.map charToLower)

-- ── Justification ────────────────────────────────

/-- Left-justify to width `n` with fill character `c`.
    $$\text{justifyLeft}(n, c, t) = t \mathbin{+\!\!+} \text{replicate}(n - |t|, c)$$ -/
def justifyLeft (n : Nat) (c : Char) (t : Text) : Text :=
  let len := t.length
  if len >= n then t
  else t ++ pack (List.replicate (n - len) c)

/-- Right-justify to width `n` with fill character `c`.
    $$\text{justifyRight}(n, c, t) = \text{replicate}(n - |t|, c) \mathbin{+\!\!+} t$$ -/
def justifyRight (n : Nat) (c : Char) (t : Text) : Text :=
  let len := t.length
  if len >= n then t
  else pack (List.replicate (n - len) c) ++ t

/-- Center-justify to width `n` with fill character `c`.
    $$\text{center}(n, c, t)$$ pads equally on both sides. -/
def center (n : Nat) (c : Char) (t : Text) : Text :=
  let len := t.length
  if len >= n then t
  else
    let pad := n - len
    let lpad := pad / 2
    let rpad := pad - lpad
    pack (List.replicate lpad c) ++ t ++ pack (List.replicate rpad c)

-- ── Folds ────────────────────────────────────────

/-- Left fold over characters.
    $$\text{foldl}(f, z, t) = f(\ldots f(f(z, t_0), t_1) \ldots, t_{n-1})$$ -/
def foldl (f : α → Char → α) (z : α) (t : Text) : α :=
  t.toList.foldl f z

/-- Strict left fold (same as `foldl` in Lean since evaluation is strict).
    $$\text{foldl'}(f, z, t) = \text{foldl}(f, z, t)$$ -/
@[inline] def foldl' (f : α → Char → α) (z : α) (t : Text) : α :=
  foldl f z t

/-- Right fold over characters.
    $$\text{foldr}(f, z, t) = f(t_0, f(t_1, \ldots f(t_{n-1}, z)))$$ -/
def foldr (f : Char → α → α) (z : α) (t : Text) : α :=
  t.toList.foldr f z

-- ── Special folds ────────────────────────────────

/-- Concatenate a list of `Text`s.
    $$\text{concat}([t_1, \ldots, t_k]) = t_1 \mathbin{+\!\!+} \cdots \mathbin{+\!\!+} t_k$$ -/
def concat (ts : List Text) : Text :=
  String.join ts

/-- Map a function over characters and concatenate the results.
    $$\text{concatMap}(f, t) = \text{concat}(\text{map}(f, \text{unpack}(t)))$$ -/
def concatMap (f : Char → Text) (t : Text) : Text :=
  String.join (t.toList.map f)

/-- Does any character satisfy the predicate?
    $$\text{any}(p, t) = \exists c \in t.\; p(c)$$ -/
def any (p : Char → Bool) (t : Text) : Bool :=
  t.toList.any p

/-- Do all characters satisfy the predicate?
    $$\text{all}(p, t) = \forall c \in t.\; p(c)$$ -/
def all (p : Char → Bool) (t : Text) : Bool :=
  t.toList.all p

/-- Maximum character in a non-empty `Text`.
    $$\text{maximum}(t) = \max(t),\quad \text{requires } |t| > 0$$ -/
def maximum (t : Text) (h : t.toList.length > 0 := by simp) : Char :=
  match t.toList, h with
  | c :: cs, _ => cs.foldl (fun acc x => if x > acc then x else acc) c

/-- Minimum character in a non-empty `Text`.
    $$\text{minimum}(t) = \min(t),\quad \text{requires } |t| > 0$$ -/
def minimum (t : Text) (h : t.toList.length > 0 := by simp) : Char :=
  match t.toList, h with
  | c :: cs, _ => cs.foldl (fun acc x => if x < acc then x else acc) c

-- ── Substrings ───────────────────────────────────

/-- The first character of a non-empty `Text`.
    $$\text{head}(t) = t_0,\quad \text{requires } |t| > 0$$ -/
def head (t : Text) (h : t.toList.length > 0 := by simp) : Char :=
  match t.toList, h with
  | c :: _, _ => c

/-- The last character of a non-empty `Text`.
    $$\text{last}(t) = t_{|t|-1},\quad \text{requires } |t| > 0$$ -/
def last (t : Text) (h : t.toList.length > 0 := by simp) : Char :=
  match t.toList, h with
  | c :: cs, _ => cs.foldl (fun _ x => x) c

/-- All characters except the first.
    $$\text{tail}(t) = t[1..],\quad \text{requires } |t| > 0$$ -/
def tail (t : Text) (h : t.toList.length > 0 := by simp) : Text :=
  match t.toList, h with
  | _ :: cs, _ => pack cs

/-- All characters except the last.
    $$\text{init}(t) = t[..|t|-1],\quad \text{requires } |t| > 0$$ -/
def init (t : Text) (h : t.toList.length > 0 := by simp) : Text :=
  match t.toList, h with
  | cs, _ => pack cs.dropLast

/-- Prepend a character.
    $$\text{cons}(c, t) = c : t$$ -/
@[inline] def cons (c : Char) (t : Text) : Text :=
  pack (c :: t.toList)

/-- Append a character.
    $$\text{snoc}(t, c) = t \mathbin{+\!\!+} [c]$$ -/
@[inline] def snoc (t : Text) (c : Char) : Text :=
  pack (t.toList ++ [c])

/-- Append two `Text`s.
    $$\text{append}(s, t) = s \mathbin{+\!\!+} t$$ -/
@[inline] def append (s t : Text) : Text := s ++ t

/-- Decompose into head and tail, or `none` if empty.
    $$\text{uncons}(t) = \begin{cases} \text{some}(t_0, t[1..]) & |t| > 0 \\ \text{none} & |t| = 0 \end{cases}$$ -/
def uncons (t : Text) : Option (Char × Text) :=
  match t.toList with
  | [] => none
  | c :: cs => some (c, pack cs)

/-- Decompose into init and last, or `none` if empty.
    $$\text{unsnoc}(t) = \begin{cases} \text{some}(t[..|t|-1], t_{|t|-1}) & |t| > 0 \\ \text{none} & |t| = 0 \end{cases}$$ -/
def unsnoc (t : Text) : Option (Text × Char) :=
  match t.toList with
  | [] => none
  | c :: cs =>
    let all := c :: cs
    let lst := all.getLast (by simp)
    some (pack all.dropLast, lst)

-- ── Cutting ──────────────────────────────────────

/-- Take the first `n` characters.
    $$\text{take}(n, t) = t[0..n]$$ -/
def take (n : Nat) (t : Text) : Text :=
  pack (t.toList.take n)

/-- Drop the first `n` characters.
    $$\text{drop}(n, t) = t[n..]$$ -/
def drop (n : Nat) (t : Text) : Text :=
  pack (t.toList.drop n)

/-- Take characters from the front while the predicate holds.
    $$\text{takeWhile}(p, t) = t[0..k]$$ where $k$ is the first index where $p$ fails. -/
def takeWhile (p : Char → Bool) (t : Text) : Text :=
  pack (t.toList.takeWhile p)

/-- Drop characters from the front while the predicate holds.
    $$\text{dropWhile}(p, t) = t[k..]$$ where $k$ is the first index where $p$ fails. -/
def dropWhile (p : Char → Bool) (t : Text) : Text :=
  pack (t.toList.dropWhile p)

/-- Drop characters from the end while the predicate holds.
    $$\text{dropWhileEnd}(p, t)$$ -/
def dropWhileEnd (p : Char → Bool) (t : Text) : Text :=
  pack (t.toList.reverse.dropWhile p).reverse

/-- Drop characters from both ends while the predicate holds.
    $$\text{dropAround}(p, t) = \text{dropWhile}(p, \text{dropWhileEnd}(p, t))$$ -/
def dropAround (p : Char → Bool) (t : Text) : Text :=
  dropWhile p (dropWhileEnd p t)

/-- Strip leading and trailing whitespace.
    $$\text{strip}(t)$$ -/
def strip (t : Text) : Text :=
  dropAround Char.isWhitespace t

/-- Strip leading whitespace.
    $$\text{stripStart}(t)$$ -/
def stripStart (t : Text) : Text :=
  dropWhile Char.isWhitespace t

/-- Strip trailing whitespace.
    $$\text{stripEnd}(t)$$ -/
def stripEnd (t : Text) : Text :=
  dropWhileEnd Char.isWhitespace t

/-- Split at position `n`.
    $$\text{splitAt}(n, t) = (\text{take}(n, t), \text{drop}(n, t))$$ -/
def splitAt (n : Nat) (t : Text) : Text × Text :=
  let cs := t.toList
  (pack (cs.take n), pack (cs.drop n))

/-- Break a `Text` at the first occurrence of `needle`.
    Returns `(before, matchAndAfter)`. If not found, returns `(t, "")`.
    $$\text{breakOn}(\text{needle}, t) = (t[..i], t[i..])$$ -/
def breakOn (needle : Text) (t : Text) : Text × Text :=
  if needle.isEmpty then (empty, t)
  else
    let parts := t.splitOn needle
    match parts with
    | [] => (t, empty)
    | [_] => (t, empty)
    | p :: _ =>
      let afterPrefix := t.toList.drop (p.toList.length)
      (p, pack afterPrefix)

/-- Break a `Text` at the last occurrence of `needle`.
    Returns `(beforeAndMatch, after)`. If not found, returns `("", t)`.
    $$\text{breakOnEnd}(\text{needle}, t)$$ -/
def breakOnEnd (needle : Text) (t : Text) : Text × Text :=
  if needle.isEmpty then (t, empty)
  else
    let parts := t.splitOn needle
    match parts with
    | [] => (empty, t)
    | [_] => (empty, t)
    | _ =>
      let lastPart := parts.getLast!
      let prefixLen := t.toList.length - lastPart.toList.length
      (pack (t.toList.take prefixLen), lastPart)

/-- Break at the first character where the predicate holds.
    $$\text{break\_}(p, t) = (\text{takeWhile}(\neg p, t), \text{dropWhile}(\neg p, t))$$ -/
def break_ (p : Char → Bool) (t : Text) : Text × Text :=
  let cs := t.toList
  let pre := cs.takeWhile (fun c => !p c)
  (pack pre, pack (cs.drop pre.length))

/-- Span: take while predicate holds, then return both parts.
    $$\text{span}(p, t) = (\text{takeWhile}(p, t), \text{dropWhile}(p, t))$$ -/
def span (p : Char → Bool) (t : Text) : Text × Text :=
  let cs := t.toList
  let pre := cs.takeWhile p
  (pack pre, pack (cs.drop pre.length))

-- ── Grouping ─────────────────────────────────────

private def groupByAux (eq : Char → Char → Bool) : List Char → List Text → List Text
  | [], acc => acc.reverse
  | c :: cs, acc =>
    let same := cs.takeWhile (eq c)
    let rest := cs.drop same.length
    groupByAux eq rest (pack (c :: same) :: acc)
termination_by cs => cs.length
decreasing_by
  simp_all
  omega

/-- Group consecutive characters by a custom equality.
    $$\text{groupBy}(eq, t)$$ -/
def groupBy (eq : Char → Char → Bool) (t : Text) : List Text :=
  groupByAux eq t.toList []

/-- Group consecutive equal characters.
    $$\text{group}(\text{"aabbbca"}) = [\text{"aa"}, \text{"bbb"}, \text{"c"}, \text{"a"}]$$ -/
def group (t : Text) : List Text :=
  groupBy (· == ·) t

-- ── Prefixes/suffixes ────────────────────────────

/-- All initial segments.
    $$\text{inits}(\text{"abc"}) = [\text{""}, \text{"a"}, \text{"ab"}, \text{"abc"}]$$ -/
def inits (t : Text) : List Text :=
  let cs := t.toList
  List.range (cs.length + 1) |>.map (fun n => pack (cs.take n))

/-- All final segments.
    $$\text{tails}(\text{"abc"}) = [\text{"abc"}, \text{"bc"}, \text{"c"}, \text{""}]$$ -/
def tails (t : Text) : List Text :=
  let cs := t.toList
  List.range (cs.length + 1) |>.map (fun n => pack (cs.drop n))

-- ── Splitting ────────────────────────────────────

/-- Split on a separator string.
    $$\text{splitOn}(\text{","}, \text{"a,b,c"}) = [\text{"a"}, \text{"b"}, \text{"c"}]$$ -/
def splitOn (sep : Text) (t : Text) : List Text :=
  if sep.isEmpty then t.toList.map singleton
  else String.splitOn t sep

/-- Split on characters satisfying a predicate.
    $$\text{split}(p, t)$$ splits at every character where $p$ holds. -/
def split (p : Char → Bool) (t : Text) : List Text :=
  go t.toList [] []
where
  go : List Char → List Char → List Text → List Text
  | [], acc, result => (pack acc.reverse :: result).reverse
  | c :: cs, acc, result =>
    if p c then go cs [] (pack acc.reverse :: result)
    else go cs (c :: acc) result

/-- Split into chunks of at most `n` characters (empty `n` yields `[t]` unchanged).
    $$\text{chunksOf}(n, t)$$ -/
def chunksOf (n : Nat) (t : Text) : List Text :=
  if h : n = 0 then [t]
  else go (Nat.pos_of_ne_zero h) t.toList
where
  go (hn : 0 < n) : List Char → List Text
  | [] => []
  | c :: cs =>
    pack ((c :: cs).take n) :: go hn ((c :: cs).drop n)
  termination_by l => l.length
  decreasing_by
    simp only [List.length_drop, List.length_cons]
    omega

-- ── Lines and words ──────────────────────────────

/-- Split into lines.
    $$\text{lines}(\text{"a\\nb\\nc"}) = [\text{"a"}, \text{"b"}, \text{"c"}]$$ -/
def lines (t : Text) : List Text :=
  String.splitOn t "\n"

/-- Split into words (on whitespace). Delegates to `String.words`.
    $$\text{words}(\text{"hello world"}) = [\text{"hello"}, \text{"world"}]$$ -/
def words (t : Text) : List Text := String.words t

/-- Join lines with newlines. Delegates to `String.unlines`.
    $$\text{unlines}([\text{"a"}, \text{"b"}]) = \text{"a\\nb\\n"}$$ -/
def unlines (ts : List Text) : Text := String.unlines ts

/-- Join words with spaces. Delegates to `String.unwords`.
    $$\text{unwords}([\text{"hello"}, \text{"world"}]) = \text{"hello world"}$$ -/
def unwords (ts : List Text) : Text := String.unwords ts

-- ── Predicates ───────────────────────────────────

/-- Is `pfx` a prefix of `t`?
    $$\text{isPrefixOf}(\text{pfx}, t) \iff t \text{ starts with } \text{pfx}$$ -/
def isPrefixOf (pfx t : Text) : Bool :=
  t.startsWith pfx

/-- Is `sfx` a suffix of `t`?
    $$\text{isSuffixOf}(\text{sfx}, t) \iff t \text{ ends with } \text{sfx}$$ -/
def isSuffixOf (sfx t : Text) : Bool :=
  t.endsWith sfx

/-- Is `needle` contained in `t`?
    $$\text{isInfixOf}(\text{needle}, t) \iff \text{needle} \subseteq t$$ -/
def isInfixOf (needle t : Text) : Bool :=
  if needle.isEmpty then true
  else go needle.toList t.toList
where
  go (ncs : List Char) : List Char → Bool
  | [] => false
  | c :: cs =>
    if (c :: cs).take ncs.length == ncs then true
    else go ncs cs

/-- Strip a prefix, returning `none` if not a prefix.
    $$\text{stripPrefix}(\text{pfx}, t)$$ -/
def stripPrefix (pfx t : Text) : Option Text :=
  if t.startsWith pfx then some (pack (t.toList.drop pfx.toList.length))
  else none

/-- Strip a suffix, returning `none` if not a suffix.
    $$\text{stripSuffix}(\text{sfx}, t)$$ -/
def stripSuffix (sfx t : Text) : Option Text :=
  if t.endsWith sfx then some (pack (t.toList.take (t.toList.length - sfx.toList.length)))
  else none

-- ── Search ───────────────────────────────────────

/-- Does the character occur in the `Text`?
    $$\text{elem}(c, t) = \exists i.\; t_i = c$$ -/
def elem (c : Char) (t : Text) : Bool :=
  t.toList.elem c

/-- Find the first character satisfying a predicate.
    $$\text{find}(p, t)$$ -/
def find (p : Char → Bool) (t : Text) : Option Char :=
  t.toList.find? p

/-- Keep only characters satisfying a predicate.
    $$\text{filter}(p, t) = [c \in t \mid p(c)]$$ -/
def filter (p : Char → Bool) (t : Text) : Text :=
  pack (t.toList.filter p)

/-- Partition into (satisfying, not-satisfying).
    $$\text{partition}(p, t) = (\text{filter}(p, t), \text{filter}(\neg p, t))$$ -/
def partition (p : Char → Bool) (t : Text) : Text × Text :=
  let (yes, no) := t.toList.partition p
  (pack yes, pack no)

-- ── Indexing ─────────────────────────────────────

/-- Index into the `Text` with a bounds check.
    $$\text{index}(t, i) = t_i$$ -/
def index (t : Text) (i : Nat) : Option Char :=
  t.toList[i]?

/-- Count non-overlapping occurrences of a substring.
    $$\text{count}(\text{needle}, t)$$ -/
def count (needle : Text) (t : Text) : Nat :=
  if needle.isEmpty then t.toList.length + 1
  else
    let parts := String.splitOn t needle
    parts.length - 1

-- ── Zipping ──────────────────────────────────────

/-- Zip two `Text`s into a list of character pairs.
    $$\text{zip}(s, t) = [(s_0, t_0), (s_1, t_1), \ldots]$$ -/
def zip (s t : Text) : List (Char × Char) :=
  s.toList.zip t.toList

/-- Zip two `Text`s with a combining function.
    $$\text{zipWith}(f, s, t) = [f(s_0, t_0), f(s_1, t_1), \ldots]$$ -/
def zipWith (f : Char → Char → Char) (s t : Text) : Text :=
  pack (List.zipWith f s.toList t.toList)

-- ── Proofs ───────────────────────────────────────

/-- `pack` and `unpack` are inverses.
    $$\text{pack}(\text{unpack}(t)) = t$$ -/
theorem pack_unpack (t : Text) : pack (unpack t) = t := by
  simp [pack, unpack]

/-- `unpack` and `pack` are inverses.
    $$\text{unpack}(\text{pack}(cs)) = cs$$ -/
theorem unpack_pack (cs : List Char) : unpack (pack cs) = cs := by
  simp [pack, unpack]

/-- `empty` is null.
    $$\text{null}(\text{empty}) = \text{true}$$ -/
theorem null_empty : null empty = true := by
  simp [null, empty, String.isEmpty]

/-- `length` of `empty` is zero.
    $$\text{length}(\text{empty}) = 0$$ -/
theorem length_empty : length empty = 0 := by
  simp [length, empty]

/-- `singleton` produces a text of length 1.
    $$\text{length}(\text{singleton}(c)) = 1$$ -/
theorem length_singleton (c : Char) : length (singleton c) = 1 := by
  simp [length, singleton]

/-- `singleton` is not null.
    $$\text{null}(\text{singleton}(c)) = \text{false}$$ -/
theorem null_singleton (c : Char) : null (singleton c) = false := by
  simp [null, singleton, String.isEmpty]
  exact Nat.pos_iff_ne_zero.mp (Char.utf8Size_pos c)

/-- `cons` increases length by one.
    $$\text{length}(\text{cons}(c, t)) = \text{length}(t) + 1$$ -/
theorem length_cons (c : Char) (t : Text) : length (cons c t) = length t + 1 := by
  simp [length, cons, pack]

/-- `append` with `empty` on the left is identity.
    $$\text{append}(\text{empty}, t) = t$$ -/
theorem append_empty_left (t : Text) : append empty t = t := by
  simp [append, empty]

/-- `append` with `empty` on the right is identity.
    $$\text{append}(t, \text{empty}) = t$$ -/
theorem append_empty_right (t : Text) : append t empty = t := by
  simp [append, empty]

/-- `reverse` of `empty` is `empty`.
    $$\text{reverse}(\text{empty}) = \text{empty}$$ -/
theorem reverse_empty : reverse empty = empty := by
  simp [reverse, empty, pack]

/-- `reverse` is an involution.
    $$\text{reverse}(\text{reverse}(t)) = t$$ -/
theorem reverse_reverse (t : Text) : reverse (reverse t) = t := by
  simp [reverse, pack]

end Text
end Data
