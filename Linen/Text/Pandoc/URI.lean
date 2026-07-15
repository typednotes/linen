/-
  `Linen.Text.Pandoc.URI` — URI escaping and validation.

  ## Haskell source

  Ported from `Text.Pandoc.URI` in the `pandoc` package
  (v3.10, `src/Text/Pandoc/URI.hs`).

  Provides `urlEncode`, `escapeURI`, `isURI`, the `schemes` set and
  `pBase64DataURI`, layered over the existing `Linen.Network.URI`
  (`network-uri`) and `Linen.Data.Base64` ports.

  ### Deviations from upstream

  * `Text` → `String`.
  * `schemes` is a representative subset of the IANA URI-scheme registry (plus
    pandoc's unofficial additions `doi`/`isbn`/`pmid`/`javascript`) — the
    common web/document schemes — rather than the full ~300-entry registry.
  * `pBase64DataURI` is written as a direct total parser over the string
    (returning `Option`) instead of an attoparsec combinator; base64 decoding
    is done via `Linen.Data.Base64` after normalising whitespace and padding.
-/

import Linen.Network.URI
import Linen.Network.HTTP.Types.URI
import Linen.Data.Base64

namespace Linen.Text.Pandoc
namespace URI

open _root_.Network.URI (escapeURIString parseURI)

/-- URL-encode a string (percent-encode all but unreserved characters). -/
def urlEncode (s : String) : String := _root_.Network.HTTP.Types.urlEncode s

/-- Escape whitespace and the punctuation set `<>|"{}[]^\`` in a URI, leaving
    everything else intact. -/
def escapeURI (s : String) : String :=
  let needsEscaping (c : Char) : Bool :=
    c.isWhitespace || "<>|\"{}[]^`".any (· == c)
  escapeURIString (fun c => !needsEscaping c) s

/-- Known URI schemes (lower-case). A representative subset of the IANA
    registry plus pandoc's unofficial additions. -/
def schemes : List String :=
  [ "http", "https", "ftp", "ftps", "sftp", "file", "mailto", "data"
  , "tel", "sms", "geo", "urn", "ws", "wss", "ssh", "git", "svn"
  , "irc", "ircs", "ldap", "ldaps", "news", "nntp", "gopher", "telnet"
  , "xmpp", "magnet", "bitcoin", "gemini", "chrome", "about", "javascript"
  , "doi", "isbn", "pmid", "view-source", "feed", "webcal", "mms", "rtsp"
  , "callto", "skype", "steam", "dns", "dav", "ni", "pkcs11", "s3" ]

/-- Parse a base64-encoded `data:` URI, returning the decoded bytes and MIME
    type. Returns `none` if the string is not a well-formed base64 data URI. -/
def pBase64DataURI (s : String) : Option (ByteArray × String) :=
  if !"data:".isPrefixOf s then none
  else
    let body := (s.drop "data:".length).toString
    match body.splitOn ";base64," with
    | header :: payloadParts@(_ :: _) =>
        let mimetype := (header.takeWhile (· != ';')).toString
        if !(mimetype.any (· == '/')) then none
        else
          let payload := ";base64,".intercalate payloadParts
          -- keep only base64 alphabet characters, then pad to a multiple of 4
          let b64chars := payload.toList.filter fun c =>
            c.isAlphanum || c == '+' || c == '/' || c == '='
          let cleaned := String.ofList (b64chars.filter (· != '='))
          let padLen := (4 - cleaned.length % 4) % 4
          let padded := cleaned ++ String.ofList (List.replicate padLen '=')
          (Data.Base64.decode padded).map (fun bytes => (bytes, mimetype))
    | _ => none

/-- Is the given string a valid URI (with a known scheme, or a base64 data URI)? -/
def isURI (t : String) : Bool :=
  if (pBase64DataURI t).isSome then true
  else
    let escaped := escapeURIString (fun c => c.toNat < 128) t
    match parseURI escaped with
    | some u =>
        let scheme := (u.uriScheme.toList.filter (· != ':') |> String.ofList).toLower
        schemes.contains scheme
    | none => false

end URI
end Linen.Text.Pandoc
