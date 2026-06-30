/-
  Linen.Network.HTTP.Types.Header — HTTP headers

  Header names are case-insensitive ByteStrings (CI ByteString).
  For simplicity, we use CI String here.
-/

import Linen.Data.CaseInsensitive

namespace Network.HTTP.Types

open Data

/-- A header name is a case-insensitive string.
    $$\text{HeaderName} = \text{CI}(\text{String})$$ -/
abbrev HeaderName := CI String

/-- A single HTTP header: name-value pair. -/
abbrev Header := HeaderName × String

/-- A list of HTTP headers. -/
abbrev RequestHeaders := List Header
abbrev ResponseHeaders := List Header

-- ── Standard header names ──

@[inline] def hAccept : HeaderName := CI.mk' "Accept"
@[inline] def hAcceptCharset : HeaderName := CI.mk' "Accept-Charset"
@[inline] def hAcceptEncoding : HeaderName := CI.mk' "Accept-Encoding"
@[inline] def hAcceptLanguage : HeaderName := CI.mk' "Accept-Language"
@[inline] def hAcceptRanges : HeaderName := CI.mk' "Accept-Ranges"
@[inline] def hAge : HeaderName := CI.mk' "Age"
@[inline] def hAllow : HeaderName := CI.mk' "Allow"
@[inline] def hAuthorization : HeaderName := CI.mk' "Authorization"
@[inline] def hCacheControl : HeaderName := CI.mk' "Cache-Control"
@[inline] def hConnection : HeaderName := CI.mk' "Connection"
@[inline] def hContentDisposition : HeaderName := CI.mk' "Content-Disposition"
@[inline] def hContentEncoding : HeaderName := CI.mk' "Content-Encoding"
@[inline] def hContentLanguage : HeaderName := CI.mk' "Content-Language"
@[inline] def hContentLength : HeaderName := CI.mk' "Content-Length"
@[inline] def hContentLocation : HeaderName := CI.mk' "Content-Location"
@[inline] def hContentRange : HeaderName := CI.mk' "Content-Range"
@[inline] def hContentType : HeaderName := CI.mk' "Content-Type"
@[inline] def hCookie : HeaderName := CI.mk' "Cookie"
@[inline] def hDate : HeaderName := CI.mk' "Date"
@[inline] def hETag : HeaderName := CI.mk' "ETag"
@[inline] def hExpect : HeaderName := CI.mk' "Expect"
@[inline] def hExpires : HeaderName := CI.mk' "Expires"
@[inline] def hFrom : HeaderName := CI.mk' "From"
@[inline] def hHost : HeaderName := CI.mk' "Host"
@[inline] def hIfMatch : HeaderName := CI.mk' "If-Match"
@[inline] def hIfModifiedSince : HeaderName := CI.mk' "If-Modified-Since"
@[inline] def hIfNoneMatch : HeaderName := CI.mk' "If-None-Match"
@[inline] def hIfRange : HeaderName := CI.mk' "If-Range"
@[inline] def hIfUnmodifiedSince : HeaderName := CI.mk' "If-Unmodified-Since"
@[inline] def hLastModified : HeaderName := CI.mk' "Last-Modified"
@[inline] def hLocation : HeaderName := CI.mk' "Location"
@[inline] def hMaxForwards : HeaderName := CI.mk' "Max-Forwards"
@[inline] def hOrigin : HeaderName := CI.mk' "Origin"
@[inline] def hPragma : HeaderName := CI.mk' "Pragma"
@[inline] def hProxyAuthenticate : HeaderName := CI.mk' "Proxy-Authenticate"
@[inline] def hProxyAuthorization : HeaderName := CI.mk' "Proxy-Authorization"
@[inline] def hRange : HeaderName := CI.mk' "Range"
@[inline] def hReferer : HeaderName := CI.mk' "Referer"
@[inline] def hRetryAfter : HeaderName := CI.mk' "Retry-After"
@[inline] def hServer : HeaderName := CI.mk' "Server"
@[inline] def hSetCookie : HeaderName := CI.mk' "Set-Cookie"
@[inline] def hTE : HeaderName := CI.mk' "TE"
@[inline] def hTrailer : HeaderName := CI.mk' "Trailer"
@[inline] def hTransferEncoding : HeaderName := CI.mk' "Transfer-Encoding"
@[inline] def hUpgrade : HeaderName := CI.mk' "Upgrade"
@[inline] def hUserAgent : HeaderName := CI.mk' "User-Agent"
@[inline] def hVary : HeaderName := CI.mk' "Vary"
@[inline] def hVia : HeaderName := CI.mk' "Via"
@[inline] def hWWWAuthenticate : HeaderName := CI.mk' "WWW-Authenticate"
@[inline] def hWarning : HeaderName := CI.mk' "Warning"

end Network.HTTP.Types
