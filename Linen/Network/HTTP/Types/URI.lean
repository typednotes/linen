/-
  Linen.Network.HTTP.Types.URI — Query string parsing and URL encoding
-/
import Linen.Network.URI

namespace Network.HTTP.Types

/-- A single query parameter. -/
abbrev QueryItem := String × Option String

/-- A parsed query string. -/
abbrev Query := List QueryItem

/-- Parse a query string (with or without leading '?').
    $$\text{parseQuery}(\texttt{"?a=1\&b=2"}) = [(\texttt{a}, \text{some}\ \texttt{1}), (\texttt{b}, \text{some}\ \texttt{2})]$$ -/
def parseQuery (s : String) : Query :=
  let s := if s.startsWith "?" then (s.drop 1).toString else s
  if s.isEmpty then []
  else
    let pairs := s.splitOn "&"
    pairs.map fun pair =>
      match pair.splitOn "=" with
      | [k]    => (k, none)
      | [k, v] => (k, some v)
      | _      => (pair, none)

/-- Render a query string with leading '?'.
    $$\text{renderQuery}(q) = \texttt{?} \cdot \text{join}(\texttt{\&}, [k_i\texttt{=}v_i])$$ -/
def renderQuery (q : Query) : String :=
  if q.isEmpty then ""
  else
    let parts := q.map fun (k, v) =>
      match v with
      | some val => s!"{k}={val}"
      | none => k
    "?" ++ "&".intercalate parts

/-- Simple percent-encoding for URLs. Encodes non-alphanumeric non-safe characters. -/
def urlEncode (s : String) : String :=
  let safe := "-._~"
  String.join (s.toList.map fun c =>
    if c.isAlphanum || safe.any (· == c) then s!"{c}"
    else
      let n := c.toNat
      let hi := n / 16
      let lo := n % 16
      let hexChar (x : Nat) : Char :=
        if x < 10 then Char.ofNat (48 + x) else Char.ofNat (55 + x)
      s!"%{hexChar hi}{hexChar lo}")

-- ── Canonical form ──

/-- Percent-encode one query component strictly, escaping everything outside
    RFC 3986 §2.3's unreserved set `A-Za-z0-9-_.~` with uppercase hex.

    Stricter than `urlEncode`, which leaves `+` and other characters alone and
    mishandles non-ASCII by encoding code points rather than UTF-8 bytes.
    Delegates to `Network.URI.escapeURIString`, which is UTF-8 correct. -/
def encodeQueryComponent (s : String) : String :=
  Network.URI.escapeURIString Network.URI.isUnreserved s

/-- Render a query in *canonical* form: every name and value strictly
    percent-encoded, entries sorted by encoded name and then by encoded value,
    joined with `&`, and no leading `?`. A valueless parameter renders as
    `name=`.

    Unlike `renderQuery`, this is deterministic — two queries differing only in
    parameter order produce the same string. Signing schemes require exactly
    that, AWS Signature Version 4 among them; `renderQuery` preserves the
    caller's order and so cannot be used to sign.

    $$\text{canonicalQuery}(q) =
      \text{join}(\texttt{\&}, \text{sort}\ [\,e(k_i)\texttt{=}e(v_i)\,])$$ -/
def canonicalQuery (q : Query) : String :=
  let encoded := q.map fun (k, v) =>
    (encodeQueryComponent k, encodeQueryComponent (v.getD ""))
  let sorted := encoded.mergeSort fun a b =>
    match compare a.1 b.1 with
    | .lt => true
    | .gt => false
    | .eq => compare a.2 b.2 != .gt
  "&".intercalate (sorted.map fun (k, v) => k ++ "=" ++ v)

/-- Simple percent-decoding for URLs. -/
def urlDecode (s : String) : String :=
  let rec go (chars : List Char) (acc : List Char) : List Char :=
    match chars with
    | [] => acc.reverse
    | '%' :: h :: l :: rest =>
      let hexVal (c : Char) : Nat :=
        if c.isDigit then c.toNat - 48
        else if c.toNat >= 65 && c.toNat <= 70 then c.toNat - 55
        else if c.toNat >= 97 && c.toNat <= 102 then c.toNat - 87
        else 0
      let v := hexVal h * 16 + hexVal l
      go rest (Char.ofNat v :: acc)
    | '+' :: rest => go rest (' ' :: acc)
    | c :: rest => go rest (c :: acc)
  String.ofList (go s.toList [])

end Network.HTTP.Types
