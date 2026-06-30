/-
  Linen.Network.HTTP.Types.URI — Query string parsing and URL encoding
-/

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
