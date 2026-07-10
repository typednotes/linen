/-
  Linen.Network.URI — RFC 3986 URI parsing, rendering, and resolution

  Ports `network-uri`'s `Network.URI` (see
  `docs/imports/network-uri/dependencies.md`). The upstream parser is built on
  `parsec`; here the grammar is reimplemented directly as a structurally-recursive
  recursive-descent parser over `List Char` — Parsec is an implementation detail of
  the upstream parser, not part of its public API, so it is not itself ported.

  Simplification: the bracketed IP-literal host form (`[...]`, RFC 3986 §3.2.2,
  covering IPv6 addresses and `IPvFuture`) is accepted at the character-class level
  (hex digits/`:`/`.` for IPv6, `v<hex>.<unreserved|sub-delim|;>+` for IPvFuture)
  rather than fully validating RFC 3986's precise IPv6 group-count grammar (the
  eight-way case split upstream encodes via Parsec alternatives). Every other
  production (scheme, userinfo, reg-name/IPv4 host, port, path, query, fragment,
  percent-encoding, relative resolution, normalization) is ported at full fidelity.
-/

namespace Network.URI

-- ── Types ──

/-- The authority component of a URI: `[userinfo@]host[:port]`. -/
structure URIAuth where
  /-- e.g. `"anonymous@"` — includes the trailing `@`; empty if absent. -/
  uriUserInfo : String := ""
  /-- The host: a reg-name, dotted-decimal IPv4 address, or bracketed IP literal. -/
  uriRegName : String := ""
  /-- e.g. `":42"` — includes the leading `:`; empty if absent. -/
  uriPort : String := ""
  deriving Repr, BEq, Inhabited

/-- A parsed URI: `scheme:[//authority]path[?query][#fragment]`. -/
structure URI where
  /-- e.g. `"http:"` — includes the trailing `:`; empty for a relative reference. -/
  uriScheme : String := ""
  uriAuthority : Option URIAuth := none
  uriPath : String := ""
  /-- e.g. `"?q=1"` — includes the leading `?`; empty if absent. -/
  uriQuery : String := ""
  /-- e.g. `"#frag"` — includes the leading `#`; empty if absent. -/
  uriFragment : String := ""
  deriving Repr, BEq, Inhabited

/-- The empty URI. -/
def nullURI : URI := {}

/-- The empty authority. -/
def nullURIAuth : URIAuth := {}

/-- Add a prefix to a string, unless it already has it. -/
private def ensurePrefix (p s : String) : String := if s.startsWith p then s else p ++ s

/-- Add a suffix to a string, unless it already has it. -/
private def ensureSuffix (suf s : String) : String := if s.endsWith suf then s else s ++ suf

private def unlessEmpty (f : String → String) (s : String) : String := if s.isEmpty then s else f s

/-- Given a `URIAuth` in "nonstandard" form (lacking required separator characters),
    return one that is standard. -/
def rectifyAuth (a : URIAuth) : URIAuth :=
  { a with
    uriUserInfo := unlessEmpty (ensureSuffix "@") a.uriUserInfo
    uriPort := unlessEmpty (ensurePrefix ":") a.uriPort }

/-- Given a `URI` in "nonstandard" form (lacking required separator characters),
    return one that is standard. -/
def rectify (u : URI) : URI :=
  { u with
    uriScheme := ensureSuffix ":" u.uriScheme
    uriAuthority := u.uriAuthority.map rectifyAuth
    uriQuery := unlessEmpty (ensurePrefix "?") u.uriQuery
    uriFragment := unlessEmpty (ensurePrefix "#") u.uriFragment }

-- ── Character classes ──
--
-- Not Unicode-aware by design, matching upstream's restriction to RFC 3986's ABNF
-- (which is defined over ASCII).

def isAlphaChar (c : Char) : Bool := (c ≥ 'A' && c ≤ 'Z') || (c ≥ 'a' && c ≤ 'z')
def isDigitChar (c : Char) : Bool := c ≥ '0' && c ≤ '9'
def isAlphaNumChar (c : Char) : Bool := isAlphaChar c || isDigitChar c
def isHexDigitChar (c : Char) : Bool :=
  isDigitChar c || (c ≥ 'a' && c ≤ 'f') || (c ≥ 'A' && c ≤ 'F')
def isSchemeChar (c : Char) : Bool := isAlphaNumChar c || c == '+' || c == '-' || c == '.'

/-- RFC 3986 §2.2 `gen-delims`. -/
def isGenDelims (c : Char) : Bool :=
  match c with
  | ':' | '/' | '?' | '#' | '[' | ']' | '@' => true
  | _ => false

/-- RFC 3986 §2.2 `sub-delims`. -/
def isSubDelims (c : Char) : Bool :=
  match c with
  | '!' | '$' | '&' | '\'' | '(' | ')' | '*' | '+' | ',' | ';' | '=' => true
  | _ => false

/-- "Reserved" characters (RFC 3986 §2.2): must be escaped to appear literally in a
    URI component. -/
def isReserved (c : Char) : Bool := isGenDelims c || isSubDelims c

/-- "Unreserved" characters (RFC 3986 §2.3): never need escaping. -/
def isUnreserved (c : Char) : Bool :=
  isAlphaNumChar c || c == '-' || c == '_' || c == '.' || c == '~'

/-- Characters allowed anywhere in a finished URI (reserved, unreserved, or the `%`
    escape marker itself). -/
def isAllowedInURI (c : Char) : Bool := isReserved c || isUnreserved c || c == '%'

/-- Characters allowed unescaped in a finished URI (not `%`; probably not what you
    want for a single component — see `isUnescapedInURIComponent`). -/
def isUnescapedInURI (c : Char) : Bool := isReserved c || isUnreserved c

/-- Characters allowed unescaped inside a single URI *component* — also excludes
    reserved separators like `/`, `?`. Use with `escapeURIString` to percent-encode
    a value before inserting it into a URI field. -/
def isUnescapedInURIComponent (c : Char) : Bool := !isReserved c && isUnescapedInURI c

def isIpvFutureChar (c : Char) : Bool := isUnreserved c || isSubDelims c || c == ';'

-- ── Percent-encoding ──

/-- UTF-8 encode a Unicode codepoint as its constituent bytes. -/
def utf8EncodeChar (c : Char) : List Nat :=
  let oc := c.toNat
  if oc ≤ 0x7f then
    [oc]
  else if oc ≤ 0x7ff then
    [0xc0 + (oc >>> 6), 0x80 + (oc &&& 0x3f)]
  else if oc ≤ 0xffff then
    [0xe0 + (oc >>> 12), 0x80 + ((oc >>> 6) &&& 0x3f), 0x80 + (oc &&& 0x3f)]
  else
    [ 0xf0 + (oc >>> 18), 0x80 + ((oc >>> 12) &&& 0x3f)
    , 0x80 + ((oc >>> 6) &&& 0x3f), 0x80 + (oc &&& 0x3f) ]

private def hexDigitUpper (n : Nat) : Char :=
  if n < 10 then Char.ofNat (48 + n) else Char.ofNat (55 + n)

/-- Render a byte (0–255) as two uppercase hex digits. -/
def hexByte (n : Nat) : String :=
  String.ofList [hexDigitUpper (n / 16), hexDigitUpper (n % 16)]

/-- Escape `c` (percent-encoding its UTF-8 bytes) unless `p c` holds, in which case
    it is returned unchanged. -/
def escapeURIChar (p : Char → Bool) (c : Char) : String :=
  if p c then c.toString
  else String.join ((utf8EncodeChar c).map fun b => "%" ++ hexByte b)

/-- Percent-encode every character of `s` rejected by `p`. -/
def escapeURIString (p : Char → Bool) (s : String) : String :=
  String.join (s.toList.map (escapeURIChar p))

private def hexVal (c : Char) : Option Nat :=
  if isDigitChar c then some (c.toNat - 48)
  else if c ≥ 'a' && c ≤ 'f' then some (c.toNat - 87)
  else if c ≥ 'A' && c ≤ 'F' then some (c.toNat - 55)
  else none

private def replacementChar : Char := Char.ofNat 0xfffd

/-- Turn every `%XX` percent-escape in the string back into its literal byte
    value.

    Simplification: each valid `%XX` triple decodes to `Char.ofNat` of its raw
    byte value directly, rather than reassembling multi-byte UTF-8 sequences the
    way upstream's hand-rolled decoder does (relevant only to percent-encoded
    non-ASCII text, which nothing in this project currently needs — see also the
    IPv6 simplification noted in the module docstring). An invalid `%` escape
    (not followed by two hex digits) is passed through as a literal `%`. -/
def unEscapeString : List Char → List Char
  | [] => []
  | '%' :: h1 :: h2 :: rest =>
    match hexVal h1, hexVal h2 with
    | some v1, some v2 => Char.ofNat (v1 * 16 + v2) :: unEscapeString rest
    | _, _ => '%' :: unEscapeString (h1 :: h2 :: rest)
  | c :: cs => c :: unEscapeString cs

/-- Turn all percent-escape sequences in the string back into literal characters. -/
def unEscapeString' (s : String) : String := String.ofList (unEscapeString s.toList)

-- ── Recursive-descent grammar ──
--
-- Each production below is a structurally-recursive function over `List Char`
-- returning `Option (String × List Char)` (an outright failure) or, for productions
-- that can never fail (RFC 3986's `*(...)` "zero or more" forms), a plain
-- `String × List Char` pair. Percent-encoded triples (`%XX`) are kept verbatim in
-- the returned string, matching upstream's `URI` representation (component values
-- are stored still percent-encoded).

/-- A run of zero or more chars satisfying `extra`, unreserved, sub-delims, or a
    `%XX` escape triple — the common shape behind `pchar`, `segment`, `userinfo`,
    query and fragment characters (RFC 3986 §2.3, §3.3–3.5). -/
def manyUChars (extra : Char → Bool) : List Char → String × List Char
  | '%' :: h1 :: h2 :: rest =>
    if isHexDigitChar h1 && isHexDigitChar h2 then
      let (s, rest') := manyUChars extra rest
      ("%" ++ String.ofList [h1, h2] ++ s, rest')
    else
      ("", '%' :: h1 :: h2 :: rest)
  | c :: cs =>
    if isUnreserved c || isSubDelims c || extra c then
      let (s, rest) := manyUChars extra cs
      (c.toString ++ s, rest)
    else
      ("", c :: cs)
  | [] => ("", [])

private def uchars1 (extra : Char → Bool) (cs : List Char) : Option (String × List Char) :=
  match manyUChars extra cs with
  | ("", _) => none
  | r => some r

/-- RFC 3986 §3.3 `pchar` (without the trailing escape alternative, folded into
    `manyUChars` itself): `unreserved / pct-encoded / sub-delims / ":" / "@"`. -/
def pcharExtra (c : Char) : Bool := c == ':' || c == '@'

/-- RFC 3986 §3.2.1 `userinfo *( unreserved / pct-encoded / sub-delims / ":" ) "@"`. -/
def parseUserinfo (cs : List Char) : Option (String × List Char) :=
  let (body, rest) := manyUChars (· == ':') cs
  match rest with
  | '@' :: rest' => some (body ++ "@", rest')
  | _ => none

/-- RFC 3986 §3.2.3 `port = *DIGIT`, including the leading `:`. -/
def parsePort : List Char → Option (String × List Char)
  | ':' :: cs =>
    let rec go : List Char → String × List Char
      | c :: cs' => if isDigitChar c then let (s, r) := go cs'; (c.toString ++ s, r) else ("", c :: cs')
      | [] => ("", [])
    let (digits, rest) := go cs
    some (":" ++ digits, rest)
  | _ => none

/-- RFC 3986 §3.2.2 `dec-octet "." dec-octet "." dec-octet "." dec-octet`, each
    octet ≤ 255 and not immediately followed by another name character. -/
def parseIPv4Address (cs : List Char) : Option (String × List Char) :=
  let decOctet (cs : List Char) : Option (String × List Char) :=
    let rec take (n : Nat) : List Char → String × List Char
      | c :: cs' => if n > 0 && isDigitChar c then let (s, r) := take (n - 1) cs'; (c.toString ++ s, r) else ("", c :: cs')
      | [] => ("", [])
    match take 3 cs with
    | ("", _) => none
    | (digits, rest) => if digits.toNat! ≤ 255 then some (digits, rest) else none
  let isNameChar (c : Char) : Bool := isUnreserved c || isSubDelims c || c == '%'
  match decOctet cs with
  | none => none
  | some (a1, '.' :: r1) =>
    match decOctet r1 with
    | none => none
    | some (a2, '.' :: r2) =>
      match decOctet r2 with
      | none => none
      | some (a3, '.' :: r3) =>
        match decOctet r3 with
        | none => none
        | some (a4, rest) =>
          match rest with
          | c :: _ => if isNameChar c then none else some (a1 ++ "." ++ a2 ++ "." ++ a3 ++ "." ++ a4, rest)
          | [] => some (a1 ++ "." ++ a2 ++ "." ++ a3 ++ "." ++ a4, rest)
      | _ => none
    | _ => none
  | _ => none

/-- RFC 3986 §3.2.2 `reg-name = *( unreserved / pct-encoded / sub-delims )`. -/
def parseRegName (cs : List Char) : String × List Char := manyUChars (fun _ => false) cs

/-- A simplified bracketed IP-literal (`[...]`, RFC 3986 §3.2.2): accepts an IPv6
    address or `IPvFuture` at the character-class level rather than fully
    validating RFC 3986's precise group-count grammar (see the module docstring). -/
def parseIPLiteral : List Char → Option (String × List Char)
  | '[' :: cs =>
    let isIPv6Char (c : Char) : Bool := isHexDigitChar c || c == ':' || c == '.' || c == '%' || isAlphaNumChar c
    let rec runIPv6 : List Char → String × List Char
      | c :: cs' => if isIPv6Char c then let (s, r) := runIPv6 cs'; (c.toString ++ s, r) else ("", c :: cs')
      | [] => ("", [])
    match cs with
    | 'v' :: h :: '.' :: rest =>
      if isHexDigitChar h then
        match uchars1 isIpvFutureChar rest with
        | some (body, ']' :: rest') => some ("[v" ++ h.toString ++ "." ++ body ++ "]", rest')
        | _ => none
      else none
    | _ =>
      match runIPv6 cs with
      | ("", _) => none
      | (body, ']' :: rest) => some ("[" ++ body ++ "]", rest)
      | _ => none
  | _ => none

/-- RFC 3986 §3.2.2 `host = IP-literal / IPv4address / reg-name`. -/
def parseHost (cs : List Char) : String × List Char :=
  match parseIPLiteral cs with
  | some r => r
  | none =>
    match parseIPv4Address cs with
    | some r => r
    | none => parseRegName cs

/-- RFC 3986 §3.2 `authority = [ userinfo "@" ] host [ ":" port ]`. -/
def parseAuthority (cs : List Char) : URIAuth × List Char :=
  let (userInfo, cs') :=
    match parseUserinfo cs with
    | some r => r
    | none => ("", cs)
  let (regName, cs'') := parseHost cs'
  let (port, cs''') :=
    match parsePort cs'' with
    | some r => r
    | none => ("", cs'')
  ({ uriUserInfo := userInfo, uriRegName := regName, uriPort := port }, cs''')

/-- RFC 3986 §3.3 `segment = *pchar` (a single `/`-delimited path segment). -/
def parseSegment (cs : List Char) : String × List Char := manyUChars pcharExtra cs

/-- RFC 3986 §3.3 `segment-nz = 1*pchar`. -/
def parseSegmentNz (cs : List Char) : Option (String × List Char) := uchars1 pcharExtra cs

/-- RFC 3986 §3.3 `segment-nz-nc = 1*( unreserved / pct-encoded / sub-delims / "@" )`. -/
def parseSegmentNzc (cs : List Char) : Option (String × List Char) := uchars1 (· == '@') cs

/-- RFC 3986 §3.3 `path-abempty = *( "/" segment )` — equivalent to a single flat
    run of path/segment characters (`pchar`, plus the `/` separator itself), since
    segments may be empty (so consecutive/leading/trailing `/` are all allowed). -/
def parsePathAbEmpty (cs : List Char) : String × List Char :=
  manyUChars (fun c => pcharExtra c || c == '/') cs

/-- RFC 3986 §3.3 `path-rootless = segment-nz *( "/" segment )`. -/
def parsePathRootless (cs : List Char) : Option (String × List Char) :=
  match parseSegmentNz cs with
  | none => none
  | some (s1, rest) =>
    let (more, rest') := parsePathAbEmpty rest
    some (s1 ++ more, rest')

/-- RFC 3986 §3.3 `path-noscheme = segment-nz-nc *( "/" segment )`. -/
def parsePathNoScheme (cs : List Char) : Option (String × List Char) :=
  match parseSegmentNzc cs with
  | none => none
  | some (s1, rest) =>
    let (more, rest') := parsePathAbEmpty rest
    some (s1 ++ more, rest')

/-- RFC 3986 §3.3 `path-abs = "/" [ segment-nz *( "/" segment ) ]`. -/
def parsePathAbs : List Char → Option (String × List Char)
  | '/' :: cs =>
    match parsePathRootless cs with
    | some (s, rest) => some ("/" ++ s, rest)
    | none => some ("/", cs)
  | _ => none

/-- RFC 3986 §3 `hier-part`. -/
def parseHierPart (cs : List Char) : Option URIAuth × String × List Char :=
  match cs with
  | '/' :: '/' :: rest =>
    let (auth, rest') := parseAuthority rest
    let (path, rest'') := parsePathAbEmpty rest'
    (some auth, path, rest'')
  | _ =>
    match parsePathAbs cs with
    | some (p, rest) => (none, p, rest)
    | none =>
      match parsePathRootless cs with
      | some (p, rest) => (none, p, rest)
      | none => (none, "", cs)

/-- RFC 3986 §4.2 `relative-part`. -/
def parseRelativePart (cs : List Char) : Option URIAuth × String × List Char :=
  match cs with
  | '/' :: '/' :: rest =>
    let (auth, rest') := parseAuthority rest
    let (path, rest'') := parsePathAbEmpty rest'
    (some auth, path, rest'')
  | _ =>
    match parsePathAbs cs with
    | some (p, rest) => (none, p, rest)
    | none =>
      match parsePathNoScheme cs with
      | some (p, rest) => (none, p, rest)
      | none => (none, "", cs)

/-- RFC 3986 §3.1 `scheme = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )`, plus the
    trailing `:`. -/
def parseScheme : List Char → Option (String × List Char)
  | c :: cs =>
    if isAlphaChar c then
      let rec go : List Char → String × List Char
        | c' :: cs' => if isSchemeChar c' then let (s, r) := go cs'; (c'.toString ++ s, r) else ("", c' :: cs')
        | [] => ("", [])
      let (rest', tail) := go cs
      match tail with
      | ':' :: tail' => some (c.toString ++ rest' ++ ":", tail')
      | _ => none
    else none
  | [] => none

private def queryFragmentExtra (c : Char) : Bool := c == ':' || c == '@' || c == '/' || c == '?'

/-- RFC 3986 §3.4 `query`, including the leading `?`. -/
def parseQueryPart (cs : List Char) : String × List Char :=
  let (body, rest) := manyUChars queryFragmentExtra cs
  ("?" ++ body, rest)

/-- RFC 3986 §3.5 `fragment`, including the leading `#`. -/
def parseFragmentPart (cs : List Char) : String × List Char :=
  let (body, rest) := manyUChars queryFragmentExtra cs
  ("#" ++ body, rest)

private def parseQF (cs : List Char) : String × String × List Char :=
  match cs with
  | '?' :: cs' =>
    let (q, rest) := parseQueryPart cs'
    match rest with
    | '#' :: rest' => let (f, rest'') := parseFragmentPart rest'; (q, f, rest'')
    | _ => (q, "", rest)
  | '#' :: cs' => let (f, rest) := parseFragmentPart cs'; ("", f, rest)
  | _ => ("", "", cs)

/-- RFC 3986 §3 `URI = scheme ":" hier-part [ "?" query ] [ "#" fragment ]`. -/
def parseUriP (cs : List Char) : Option (URI × List Char) :=
  match parseScheme cs with
  | none => none
  | some (scheme, rest) =>
    let (auth, path, rest') := parseHierPart rest
    let (query, frag, rest'') := parseQF rest'
    some ({ uriScheme := scheme, uriAuthority := auth, uriPath := path, uriQuery := query, uriFragment := frag }, rest'')

/-- RFC 3986 §4.2 `relative-ref`. -/
def parseRelativeRefP (cs : List Char) : Option (URI × List Char) :=
  -- A scheme must not be parseable as a prefix (else this would be an absolute URI).
  match parseScheme cs with
  | some _ => none
  | none =>
    let (auth, path, rest) := parseRelativePart cs
    let (query, frag, rest') := parseQF rest
    some ({ uriScheme := "", uriAuthority := auth, uriPath := path, uriQuery := query, uriFragment := frag }, rest')

/-- RFC 3986 §4.1 `URI-reference = URI / relative-ref`. -/
def parseUriReferenceP (cs : List Char) : Option (URI × List Char) :=
  match parseUriP cs with
  | some r => some r
  | none => parseRelativeRefP cs

/-- RFC 3986 §4.3 `absolute-URI = scheme ":" hier-part [ "?" query ]` (no fragment). -/
def parseAbsoluteUriP (cs : List Char) : Option (URI × List Char) :=
  match parseScheme cs with
  | none => none
  | some (scheme, rest) =>
    let (auth, path, rest') := parseHierPart rest
    match rest' with
    | '?' :: rest'' =>
      let (query, restTail) := parseQueryPart rest''
      some ({ uriScheme := scheme, uriAuthority := auth, uriPath := path, uriQuery := query, uriFragment := "" }, restTail)
    | _ => some ({ uriScheme := scheme, uriAuthority := auth, uriPath := path, uriQuery := "", uriFragment := "" }, rest')

private def parseAll (p : List Char → Option (URI × List Char)) (s : String) : Option URI :=
  match p s.toList with
  | some (u, []) => some u
  | _ => none

-- ── Parsing entry points ──

/-- Parse a string as an absolute URI with optional fragment
    (`scheme ":" hier-part [ "?" query ] [ "#" fragment ]`). `none` if invalid. -/
def parseURI (s : String) : Option URI := parseAll parseUriP s

/-- Parse a string as a URI reference — absolute or relative, with optional
    fragment. `none` if invalid. -/
def parseURIReference (s : String) : Option URI := parseAll parseUriReferenceP s

/-- Parse a string as a relative reference (no scheme), with optional fragment.
    `none` if invalid. -/
def parseRelativeReference (s : String) : Option URI := parseAll parseRelativeRefP s

/-- Parse a string as an absolute URI without a fragment identifier. `none` if
    invalid. -/
def parseAbsoluteURI (s : String) : Option URI := parseAll parseAbsoluteUriP s

/-- Does the string contain a valid URI (absolute, with optional fragment)? -/
def isURI (s : String) : Bool := (parseURI s).isSome

/-- Does the string contain a valid URI reference (absolute or relative)? -/
def isURIReference (s : String) : Bool := (parseURIReference s).isSome

/-- Does the string contain a valid relative reference? -/
def isRelativeReference (s : String) : Bool := (parseRelativeReference s).isSome

/-- Does the string contain a valid absolute URI (no fragment)? -/
def isAbsoluteURI (s : String) : Bool := (parseAbsoluteURI s).isSome

/-- Does the string contain a valid IPv4 address? -/
def isIPv4address (s : String) : Bool :=
  match parseIPv4Address s.toList with
  | some (_, []) => true
  | _ => false

/-- Does the string contain a syntactically-valid IPv6 address (see the module
    docstring for the simplification versus RFC 3986's full grammar)? -/
def isIPv6address (s : String) : Bool :=
  match parseIPLiteral ("[" ++ s ++ "]").toList with
  | some (_, []) => true
  | _ => false

-- ── Predicates ──

def uriIsAbsolute (u : URI) : Bool := u.uriScheme != ""
def uriIsRelative (u : URI) : Bool := !uriIsAbsolute u

-- ── Rendering ──

/-- Render an authority back to a string (`//[userinfomap userinfo]host[port]`),
    given a transform for the user-info part (e.g. to hide a password). -/
def uriAuthToString (userInfoMap : String → String) : Option URIAuth → String
  | none => ""
  | some a => "//" ++ (if a.uriUserInfo.isEmpty then "" else userInfoMap a.uriUserInfo) ++ a.uriRegName ++ a.uriPort

/-- Render a `URI` back to a string, given a transform for the authority's
    user-info part. -/
def uriToString (userInfoMap : String → String) (u : URI) : String :=
  u.uriScheme ++ uriAuthToString userInfoMap u.uriAuthority ++ u.uriPath ++ u.uriQuery ++ u.uriFragment

/-- Masks any password-shaped suffix of a user-info string (`user:pass@` →
    `user:...@`), matching the default the upstream `Show` instance uses so a URI's
    string form doesn't leak a plaintext password. -/
def defaultUserInfoMap (uinf : String) : String :=
  match uinf.splitOn ":" with
  | [_] => uinf
  | user :: rest =>
    let pass := ":".intercalate rest
    if pass == "@" || pass == ":@" then user ++ pass else user ++ ":...@"
  | [] => uinf

instance : ToString URI where
  toString := uriToString defaultUserInfoMap

-- ── Path segments ──

/-- Split a `/`-delimited string into its list of segments, e.g. `"a/b/c"` ↦
    `["a", "b", "c"]`, `"/a/b"` ↦ `["", "a", "b"]`, `""` ↦ `[""]`. Structural over
    `List Char`. -/
def splitOnSlash (s : String) : List String :=
  let rec go : List Char → List String
    | [] => [""]
    | c :: cs =>
      let rest := go cs
      if c == '/' then
        "" :: rest
      else
        match rest with
        | seg :: more => (c.toString ++ seg) :: more
        | [] => [c.toString]     -- unreachable: `go` always returns a nonempty list
  go s.toList

/-- Join a list of segments back into a `/`-delimited string (inverse of
    `splitOnSlash`). -/
def joinSlash (segs : List String) : String := "/".intercalate segs

/-- Drop a single leading `""` (from a leading `/`), if present. -/
private def dropLeadingEmpty : List String → List String
  | "" :: rest => rest
  | segs => segs

/-- Drop a single trailing `""` (from a trailing `/`), if present. -/
private def dropTrailingEmpty (segs : List String) : List String :=
  match segs.reverse with
  | "" :: revInit => revInit.reverse
  | _ => segs

/-- The segments of a path, e.g. `"/foo/bar/baz"` ↦ `["foo", "bar", "baz"]`,
    `"/foo/bar/"` ↦ `["foo", "bar"]` (a single leading and/or trailing `/`
    contributes no empty segment, matching upstream's `pathSegments`). -/
def pathSegmentsOf (path : String) : List String :=
  dropTrailingEmpty (dropLeadingEmpty (splitOnSlash path))

/-- The segments of a URI's path component. -/
def pathSegments (u : URI) : List String := pathSegmentsOf u.uriPath

/-- Split the last (name) segment off a path, returning `(path, name)` — `path`
    keeps its trailing `/` (if the original had one), matching upstream (so that
    e.g. `mergePaths` can just concatenate `path` with what follows). -/
def splitLast (p : String) : String × String :=
  match (splitOnSlash p).reverse with
  | [] => ("", "")
  | name :: revInit =>
    (joinSlash revInit.reverse ++ (if revInit.isEmpty then "" else "/"), name)

/-- The next `/`-delimited segment (with its trailing `/`, if any) and the rest of
    the path. -/
def nextSegment (p : String) : String × String :=
  match splitOnSlash p with
  | [seg] => (seg, "")
  | seg :: rest => (seg ++ "/", joinSlash rest)
  | [] => ("", "")

/-- The segments of a "directory" path — one that is either empty or ends in `/`
    — without the trailing empty entry that split produces, e.g. `"a/b/"` ↦
    `["a", "b"]`, `""` ↦ `[]`. -/
private def dirSegments (p : String) : List String :=
  if p.isEmpty then [] else (splitOnSlash p).dropLast

-- ── Removing dot segments (RFC 3986 §5.2.4) ──

/-- Collapse `.`/`..` segments out of a list of `/`-delimited path segments
    (RFC 3986 §5.2.4), scanning left to right with a stack (`rev`, reversed) of
    segments kept so far: `.` contributes nothing, `..` pops the most recently
    kept segment (dropped entirely if there is nothing to pop). -/
private def collapseDotsAcc (rev : List String) : List String → List String
  | [] => rev.reverse
  | "." :: rest =>
    if rest.isEmpty then ("" :: rev).reverse else collapseDotsAcc rev rest
  | ".." :: rest =>
    let rev' := match rev with | _ :: prev => prev | [] => []
    if rest.isEmpty then ("" :: rev').reverse else collapseDotsAcc rev' rest
  | seg :: rest => collapseDotsAcc (seg :: rev) rest

/-- Remove `.`/`..` dot-segments from a path (RFC 3986 §5.2.4). -/
def removeDotSegments (path : String) : String := joinSlash (collapseDotsAcc [] (splitOnSlash path))

-- ── Relative resolution (RFC 3986 §5.2) ──

private def mergePaths (base ref : URI) : String :=
  if base.uriAuthority.isSome && base.uriPath.isEmpty then
    "/" ++ ref.uriPath
  else
    (splitLast base.uriPath).1 ++ ref.uriPath

/-- Resolve `ref` (interpreted as relative) against `base`, per RFC 3986 §5.2. -/
def relativeTo (ref base : URI) : URI :=
  let justSegments (u : URI) : URI := { u with uriPath := removeDotSegments u.uriPath }
  if !ref.uriScheme.isEmpty then
    justSegments ref
  else if ref.uriAuthority.isSome then
    justSegments { ref with uriScheme := base.uriScheme }
  else if !ref.uriPath.isEmpty then
    if ref.uriPath.startsWith "/" then
      justSegments { ref with uriScheme := base.uriScheme, uriAuthority := base.uriAuthority }
    else
      justSegments
        { ref with
          uriScheme := base.uriScheme, uriAuthority := base.uriAuthority
          uriPath := mergePaths base ref }
  else if !ref.uriQuery.isEmpty then
    justSegments { ref with uriScheme := base.uriScheme, uriAuthority := base.uriAuthority, uriPath := base.uriPath }
  else
    justSegments
      { ref with
        uriScheme := base.uriScheme, uriAuthority := base.uriAuthority
        uriPath := base.uriPath, uriQuery := base.uriQuery }

/-- Like `relativeTo`, but treats `ref` as relative even if it repeats `base`'s own
    scheme (e.g. `"http:foo" nonStrictRelativeTo "http://bar.org/"` = `"http://bar.org/foo"`). -/
def nonStrictRelativeTo (ref base : URI) : URI :=
  relativeTo (if ref.uriScheme == base.uriScheme then { ref with uriScheme := "" } else ref) base

/-- Discard the common leading segments of `target`/`base` (both directory
    segment lists), returning what remains of each. -/
private def commonPrefixDrop : List String → List String → List String × List String
  | a :: as, b :: bs => if a == b then commonPrefixDrop as bs else (a :: as, b :: bs)
  | as, bs => (as, bs)

/-- RFC 3986 §5.2.2's `relSegsFrom`/`difSegsFrom`, combined: the directory
    segments of the relative path from `base` to `target` — a `".."` for every
    segment left over in `base` after discarding the common prefix, followed by
    `target`'s own leftover segments. -/
private def relSegs (target base : List String) : List String :=
  let (targetRem, baseRem) := commonPrefixDrop target base
  (baseRem.map fun _ => "..") ++ targetRem

/-- Calculate the relative path from `base` to `pabs` (both directory paths with
    trailing names already split off), per RFC 3986 §5.2.2's reference algorithm. -/
def relPathFrom1 (pabs base : String) : String :=
  let (sa, na) := splitLast pabs
  let (sb, nb) := splitLast base
  let rp := relSegs (dirSegments sa) (dirSegments sb)
  let protect (s : String) : Bool := s.isEmpty || s.any (· == ':')
  if rp.isEmpty then
    if na == nb then "" else if protect na then "./" ++ na else na
  else
    joinSlash rp ++ "/" ++ na

/-- Calculate the path to `pabs` from `base` (both absolute paths). -/
def relPathFrom (pabs base : String) : String :=
  if pabs.isEmpty then "/"
  else if base.isEmpty then pabs
  else
    let (sa1, ra1) := nextSegment pabs
    let (sb1, rb1) := nextSegment base
    if sa1 == sb1 then
      if sa1 == "/" then
        let (sa2, ra2) := nextSegment ra1
        let (sb2, rb2) := nextSegment rb1
        if sa2 == sb2 then relPathFrom1 ra2 rb2 else pabs
      else
        relPathFrom1 ra1 rb1
    else
      pabs

/-- Returns a `URI` that represents `uabs`'s location relative to `base` — the
    (non-unique) inverse of `relativeTo`, per RFC 3986 §5.2's discussion. -/
def relativeFrom (uabs base : URI) : URI :=
  let removeBodyDotSegments (p : String) : String :=
    let (p1, p2) := splitLast p
    removeDotSegments p1 ++ p2
  if uabs.uriScheme != base.uriScheme then
    uabs
  else if uabs.uriAuthority != base.uriAuthority then
    { uabs with uriScheme := "" }
  else if uabs.uriPath != base.uriPath then
    { uabs with
      uriScheme := "", uriAuthority := none
      uriPath := relPathFrom (removeBodyDotSegments uabs.uriPath) (removeBodyDotSegments base.uriPath) }
  else if uabs.uriQuery != base.uriQuery then
    { uabs with uriScheme := "", uriAuthority := none, uriPath := "" }
  else
    { uabs with uriScheme := "", uriAuthority := none, uriPath := "", uriQuery := "" }

-- ── Other normalization functions (RFC 3986 §6.2.2) ──

/-- Case-normalize a URI string: lowercase the scheme and any hex digits in
    percent-escapes are upper-cased (RFC 3986 §6.2.2.1). Authority case
    normalization is not performed, matching upstream. -/
def normalizeCase (uristr : String) : String :=
  let rec ncEscape : List Char → List Char
    | '%' :: h1 :: h2 :: cs => '%' :: h1.toUpper :: h2.toUpper :: ncEscape cs
    | c :: cs => c :: ncEscape cs
    | [] => []
  let rec ncScheme : List Char → List Char
    | ':' :: cs => ':' :: ncEscape cs
    | c :: cs => if isSchemeChar c then c.toLower :: ncScheme cs else ncEscape uristr.toList
    | [] => ncEscape uristr.toList
  String.ofList (ncScheme uristr.toList)

/-- Encoding-normalize a URI string: percent-escapes of unreserved characters are
    replaced by the literal character (RFC 3986 §6.2.2.2). -/
def normalizeEscape (uristr : String) : String :=
  let rec go : List Char → List Char
    | '%' :: h1 :: h2 :: cs =>
      match hexVal h1, hexVal h2 with
      | some v1, some v2 =>
        let escVal := Char.ofNat (v1 * 16 + v2)
        if isUnreserved escVal then escVal :: go cs else '%' :: h1 :: h2 :: go cs
      | _, _ => '%' :: h1 :: h2 :: go cs
    | c :: cs => c :: go cs
    | [] => []
  String.ofList (go uristr.toList)

/-- Path-segment-normalize a URI string: parses it, removes dot-segments from the
    path, and renders it back (RFC 3986 §6.2.2.3). Returns the input unchanged if
    it doesn't parse as a `URI`. -/
def normalizePathSegments (uristr : String) : String :=
  match parseURI uristr with
  | none => uristr
  | some u => toString { u with uriPath := removeDotSegments u.uriPath }

end Network.URI
