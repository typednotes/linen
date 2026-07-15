/-
  `Linen.Text.Pandoc.XML` — XML entity escaping and decoding.

  ## Haskell source

  Ported from `Text.Pandoc.XML` in the `pandoc` package
  (v3.10, `src/Text/Pandoc/XML.hs`).

  Provides `escapeCharForXML`, `escapeStringForXML`, `escapeNCName`,
  `toEntities`, `toHtml5Entities`, `fromEntities`, and the HTML4/HTML5/RDFa
  attribute-name allowlists.

  ### Deviations from upstream

  * `Text` → `String`.
  * The `Doc`-valued tag builders (`inTags`, `selfClosingTag`,
    `inTagsSimple`, `inTagsIndented`) are omitted here: they render through
    `doclayout`'s `Doc`, the not-yet-ported prerequisite (index #82). They
    belong to the writer layer and will land with it.
  * `fromEntities` uses a local `lookupEntity` (numeric decimal/hex fully;
    named entities a common HTML/XML subset) in place of
    `Commonmark.Entity.lookupEntity`.
  * `toHtml5Entities`'s named-entity map and `html5Attributes`/
    `html4Attributes` are representative subsets (the full tables are
    ~2000/~190/~115 entries and feed the deferred HTML writer); `rdfaAttributes`
    is complete.
-/

namespace Linen.Text.Pandoc
namespace XML

-- ── Escaping ──────────────────────────────────────────────────────────

/-- Escape a single character for XML text. `'` is not escaped; other
    characters pass through unchanged. -/
def escapeCharForXML : Char → String
  | '&' => "&amp;"
  | '<' => "&lt;"
  | '>' => "&gt;"
  | '"' => "&quot;"
  | c   => String.singleton c

/-- Whether a character is a legal XML character. -/
def isLegalXMLChar (c : Char) : Bool :=
  let n := c.toNat
  c == '\t' || c == '\n' || c == '\r' ||
  (n ≥ 0x20 && n ≤ 0xD7FF) || (n ≥ 0xE000 && n ≤ 0xFFFD) ||
  (n ≥ 0x10000 && n ≤ 0x10FFFF)

/-- Escape a string for XML, first dropping illegal XML characters. -/
def escapeStringForXML (s : String) : String :=
  String.join ((s.toList.filter isLegalXMLChar).map escapeCharForXML)

-- ── Numeric entities ──────────────────────────────────────────────────

/-- Hex digit value, if valid. -/
private def hexDigit? (c : Char) : Option Nat :=
  if c.isDigit then some (c.toNat - '0'.toNat)
  else if c ≥ 'a' && c ≤ 'f' then some (c.toNat - 'a'.toNat + 10)
  else if c ≥ 'A' && c ≤ 'F' then some (c.toNat - 'A'.toNat + 10)
  else none

/-- Parse a hex string (list of chars) to a `Nat`. -/
private def hexToNat? (cs : List Char) : Option Nat :=
  if cs.isEmpty then none
  else cs.foldl (fun acc c =>
    match acc, hexDigit? c with
    | some n, some d => some (n * 16 + d)
    | _, _ => none) (some 0)

/-- Hex digit `0..15` as an uppercase character. -/
private def hexDigitChar (d : Nat) : Char :=
  if d < 10 then Char.ofNat ('0'.toNat + d) else Char.ofNat ('A'.toNat + d - 10)

/-- Render a `Nat` as uppercase hex (no leading zeros; `0` renders empty). -/
private def natToHexUpper (n : Nat) : String :=
  if n == 0 then ""
  else natToHexUpper (n / 16) ++ String.singleton (hexDigitChar (n % 16))
termination_by n
decreasing_by
  have hn : n ≠ 0 := by simp_all
  exact Nat.div_lt_self (Nat.pos_of_ne_zero hn) (by decide)

/-- Convert every non-ASCII character to a hex numeric entity `&#xHH;`. -/
def toEntities (s : String) : String :=
  String.join (s.toList.map fun c =>
    let n := c.toNat
    if n < 128 then String.singleton c
    else "&#x" ++ (if n == 0 then "0" else natToHexUpper n) ++ ";")

-- ── Named entities ────────────────────────────────────────────────────

/-- A common subset of named HTML/XML entities (name → replacement). -/
def namedEntities : List (String × String) :=
  [ ("amp", "&"), ("lt", "<"), ("gt", ">"), ("quot", "\""), ("apos", "'")
  , ("nbsp", " "), ("copy", "©"), ("reg", "®"), ("trade", "™")
  , ("hellip", "…"), ("mdash", "—"), ("ndash", "–")
  , ("lsquo", "‘"), ("rsquo", "’"), ("ldquo", "“"), ("rdquo", "”")
  , ("laquo", "«"), ("raquo", "»"), ("deg", "°"), ("plusmn", "±")
  , ("times", "×"), ("divide", "÷"), ("frac12", "½"), ("frac14", "¼")
  , ("frac34", "¾"), ("micro", "µ"), ("para", "¶"), ("sect", "§")
  , ("middot", "·"), ("bull", "•"), ("dagger", "†"), ("Dagger", "‡")
  , ("euro", "€"), ("pound", "£"), ("yen", "¥"), ("cent", "¢")
  , ("aacute", "á"), ("eacute", "é"), ("iacute", "í"), ("oacute", "ó")
  , ("uacute", "ú"), ("agrave", "à"), ("egrave", "è"), ("ntilde", "ñ")
  , ("uuml", "ü"), ("ouml", "ö"), ("auml", "ä"), ("szlig", "ß")
  , ("ccedil", "ç"), ("aring", "å"), ("oslash", "ø"), ("aelig", "æ") ]

/-- Look up an entity name (without `&`/`;`): decimal `#123`, hex `#xAB`, or
    a named entity. -/
def lookupEntity (name : List Char) : Option (List Char) :=
  match name with
  | '#' :: 'x' :: rest | '#' :: 'X' :: rest =>
      (hexToNat? rest).map (fun n => [Char.ofNat n])
  | '#' :: rest =>
      (String.ofList rest).toNat?.map (fun n => [Char.ofNat n])
  | _ => (namedEntities.lookup (String.ofList name)).map (·.toList)

/-- The decoder state machine. `buf` (reversed entity-name chars) is `some`
    while reading an entity started by `&`. Recurses structurally on the
    input list. -/
private def fromEntitiesGo : List Char → Option (List Char) → List Char
  | [], none => []
  | [], some buf =>
      -- entity unterminated at end of input: try to decode, else emit literally
      match lookupEntity buf.reverse with
      | some decoded => decoded
      | none => '&' :: buf.reverse
  | c :: rest, none =>
      if c == '&' then fromEntitiesGo rest (some [])
      else c :: fromEntitiesGo rest none
  | c :: rest, some buf =>
      if c == ';' then
        match lookupEntity buf.reverse with
        | some decoded => decoded ++ fromEntitiesGo rest none
        | none => '&' :: (buf.reverse ++ (';' :: fromEntitiesGo rest none))
      else if c.isWhitespace then
        -- terminated by whitespace without ';': emit the `&…` literally,
        -- keeping the whitespace char
        '&' :: (buf.reverse ++ (c :: fromEntitiesGo rest none))
      else fromEntitiesGo rest (some (c :: buf))

/-- Decode numeric and named entities in a string. Unrecognised `&…`
    sequences are left unchanged. -/
def fromEntities (t : String) : String :=
  String.ofList (fromEntitiesGo t.toList none)

-- ── HTML5 named entity map (for toHtml5Entities) ──────────────────────

/-- A common subset of the codepoint → HTML5-entity-name map. -/
def html5EntityMap : List (Char × String) :=
  [ ('©', "copy"), ('®', "reg"), ('™', "trade"), ('…', "hellip")
  , ('—', "mdash"), ('–', "ndash"), ('‘', "lsquo"), ('’', "rsquo")
  , ('“', "ldquo"), ('”', "rdquo"), ('«', "laquo"), ('»', "raquo")
  , ('°', "deg"), ('±', "plusmn"), ('×', "times"), ('÷', "divide")
  , ('§', "sect"), ('¶', "para"), ('•', "bull"), ('†', "dagger")
  , ('‡', "Dagger"), ('€', "euro"), ('£', "pound"), ('¥', "yen")
  , ('é', "eacute"), ('è', "egrave"), ('à', "agrave"), ('ñ', "ntilde")
  , ('ü', "uuml"), ('ö', "ouml"), ('ä', "auml"), ('ß', "szlig") ]

/-- Like `toEntities`, but prefer named HTML5 entities, falling back to
    decimal numeric entities. -/
def toHtml5Entities (s : String) : String :=
  String.join (s.toList.map fun c =>
    if c.toNat < 128 then String.singleton c
    else match html5EntityMap.lookup c with
      | some nm => "&" ++ nm ++ ";"
      | none => "&#" ++ toString c.toNat ++ ";")

-- ── NCName escaping ───────────────────────────────────────────────────

/-- Whether a character is a valid NCName start character (letter or `_`). -/
def isNCNameStart (c : Char) : Bool := c.isAlpha || c == '_'

/-- Whether a character is a valid NCName continuation character. -/
def isNCNameChar (c : Char) : Bool :=
  isNCNameStart c || c.isDigit || c == '-' || c == '.' || c == '·'

/-- Escape a codepoint disallowed in an NCName as `Uxxxx` (hex, `U`-prefixed). -/
private def escapeNC (c : Char) : String :=
  let n := c.toNat
  "U" ++ (if n == 0 then "0" else natToHexUpper n)

/-- Escape a string so it is a valid XML NCName. -/
def escapeNCName (s : String) : String :=
  match s.toList with
  | [] => ""
  | c :: cs =>
    let first := if isNCNameStart c then String.singleton c else escapeNC c
    let rest := String.join (cs.map fun c => if isNCNameChar c then String.singleton c else escapeNC c)
    first ++ rest

-- ── Attribute allowlists ──────────────────────────────────────────────

/-- RDFa attribute names (complete). -/
def rdfaAttributes : List String :=
  [ "about", "rel", "rev", "src", "href", "resource", "property"
  , "content", "datatype", "typeof", "vocab", "prefix" ]

/-- A representative subset of valid HTML5 attribute names. -/
def html5Attributes : List String :=
  [ "abbr", "accept", "accesskey", "action", "allow", "alt", "async"
  , "autocomplete", "autofocus", "autoplay", "charset", "checked", "cite"
  , "class", "cols", "colspan", "content", "contenteditable", "controls"
  , "coords", "crossorigin", "data", "datetime", "default", "defer", "dir"
  , "disabled", "download", "draggable", "enctype", "for", "form", "headers"
  , "height", "hidden", "high", "href", "hreflang", "id", "integrity"
  , "ismap", "kind", "label", "lang", "list", "loop", "low", "max"
  , "maxlength", "media", "method", "min", "multiple", "muted", "name"
  , "novalidate", "open", "optimum", "pattern", "placeholder", "poster"
  , "preload", "readonly", "rel", "required", "reversed", "role", "rows"
  , "rowspan", "sandbox", "scope", "selected", "shape", "size", "sizes"
  , "span", "spellcheck", "src", "srcset", "start", "step", "style"
  , "tabindex", "target", "title", "translate", "type", "usemap", "value"
  , "width", "wrap", "onclick", "onload", "onmouseover", "itemscope"
  , "itemtype", "itemprop" ]

/-- A representative subset of valid HTML4 attribute names. -/
def html4Attributes : List String :=
  [ "abbr", "accept", "accesskey", "action", "align", "alt", "axis"
  , "bgcolor", "border", "cellpadding", "cellspacing", "char", "charoff"
  , "charset", "checked", "cite", "class", "cols", "colspan", "content"
  , "coords", "datetime", "dir", "disabled", "for", "frameborder", "headers"
  , "height", "href", "hreflang", "hspace", "id", "ismap", "label", "lang"
  , "marginheight", "marginwidth", "maxlength", "media", "method", "multiple"
  , "name", "nohref", "noresize", "readonly", "rel", "rev", "rows", "rowspan"
  , "rules", "scope", "scrolling", "selected", "shape", "size", "span", "src"
  , "start", "style", "summary", "tabindex", "target", "title", "type"
  , "usemap", "valign", "value", "vspace", "width", "onclick", "onload"
  , "onmouseover" ]

end XML
end Linen.Text.Pandoc
