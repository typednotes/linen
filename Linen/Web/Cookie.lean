/-
  Linen.Web.Cookie — HTTP cookie parsing and rendering

  Parses `Cookie:` and `Set-Cookie:` header values (RFC 6265). Names and values
  are trimmed; empty input parses to `[]`. The parsers/renderers are pure
  (no `Id.run`/`while`); attribute scanning is a `List.foldl`.
-/

namespace Web.Cookie

/-- A parsed cookie key-value pair. -/
abbrev CookiePair := String × String

/-- Trim leading/trailing ASCII whitespace. -/
private def trim (s : String) : String := s.trimAscii.toString

/-- Parse a `Cookie:` header value (`name1=value1; name2=value2`) into pairs. -/
def parseCookies (header : String) : List CookiePair :=
  (header.splitOn ";").filterMap fun pair =>
    match (trim pair).splitOn "=" with
    | name :: rest => some (trim name, trim ("=".intercalate rest))  -- value may contain '='
    | _ => none

/-- Render cookie pairs into a `Cookie:` header value. -/
def renderCookies (cookies : List CookiePair) : String :=
  "; ".intercalate (cookies.map fun (k, v) => k ++ "=" ++ v)

/-- The `SameSite` attribute. -/
inductive SameSite where
  | strict
  | lax
  | none_
deriving BEq, Repr

/-- A `Set-Cookie` configuration. -/
structure SetCookie where
  name : String
  value : String
  path : Option String := none
  domain : Option String := none
  maxAge : Option Nat := none
  secure : Bool := false
  httpOnly : Bool := false
  sameSite : Option SameSite := none

/-- Render a `SetCookie` as a `Set-Cookie:` header value. -/
def renderSetCookie (sc : SetCookie) : String :=
  let s := sc.name ++ "=" ++ sc.value
  let s := match sc.path with | some p => s ++ "; Path=" ++ p | none => s
  let s := match sc.domain with | some d => s ++ "; Domain=" ++ d | none => s
  let s := match sc.maxAge with | some a => s ++ "; Max-Age=" ++ toString a | none => s
  let s := if sc.secure then s ++ "; Secure" else s
  let s := if sc.httpOnly then s ++ "; HttpOnly" else s
  match sc.sameSite with
  | some ss => s ++ "; SameSite=" ++ (match ss with | .strict => "Strict" | .lax => "Lax" | .none_ => "None")
  | none => s

/-- Apply one `Set-Cookie` attribute segment to the accumulating cookie.
    The prefix is matched case-insensitively (on the lowercased `attr`), but
    values keep their original case (taken from `part`). -/
private def applyAttr (sc : SetCookie) (part : String) : SetCookie :=
  let attr := (trim part).toLower
  if attr.startsWith "path=" then { sc with path := some ((trim part).drop 5).toString }
  else if attr.startsWith "domain=" then { sc with domain := some ((trim part).drop 7).toString }
  else if attr.startsWith "max-age=" then { sc with maxAge := ((trim part).drop 8).toString.toNat? }
  else if attr == "secure" then { sc with secure := true }
  else if attr == "httponly" then { sc with httpOnly := true }
  else if attr.startsWith "samesite=" then
    let val := (attr.drop 9).toString
    { sc with sameSite :=
        if val == "strict" then some .strict
        else if val == "lax" then some .lax
        else if val == "none" then some .none_
        else none }
  else sc

/-- Parse a `Set-Cookie:` header value. The `name=value` is required; attributes
    are parsed best-effort. -/
def parseSetCookie (header : String) : Option SetCookie :=
  let parts := header.splitOn ";"
  match parts.head? with
  | none => none
  | some main =>
    match (trim main).splitOn "=" with
    | n :: rest =>
      let initial : SetCookie := { name := trim n, value := trim ("=".intercalate rest) }
      some ((parts.drop 1).foldl applyAttr initial)
    | _ => none

end Web.Cookie
