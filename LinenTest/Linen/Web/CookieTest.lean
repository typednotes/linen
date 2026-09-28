/-
  Tests for `Linen.Web.Cookie` — RFC 6265 cookie parsing/rendering.
-/
import Linen.Web.Cookie

open Web.Cookie

namespace Tests.Web.Cookie

/-! ### parseCookies -/

#guard parseCookies "a=1; b=2" == [("a", "1"), ("b", "2")]
#guard parseCookies "  a = 1 ;  b=2 " == [("a", "1"), ("b", "2")]   -- whitespace trimmed
#guard parseCookies "x=a=b=c" == [("x", "a=b=c")]                    -- '=' kept in value
#guard parseCookies "" == [("", "")]                                -- single empty field
#guard parseCookies "sessionId=abc123" == [("sessionId", "abc123")]

/-! ### renderCookies / round-trip -/

#guard renderCookies [("a", "1"), ("b", "2")] == "a=1; b=2"
#guard parseCookies (renderCookies [("k", "v"), ("m", "n")]) == [("k", "v"), ("m", "n")]

/-! ### renderSetCookie -/

#guard renderSetCookie { name := "id", value := "42" } == "id=42"
#guard renderSetCookie { name := "id", value := "42", path := some "/", secure := true }
        == "id=42; Path=/; Secure"
#guard renderSetCookie { name := "s", value := "x", maxAge := some 3600, httpOnly := true }
        == "s=x; Max-Age=3600; HttpOnly"
#guard renderSetCookie { name := "s", value := "x", domain := some "example.com", sameSite := some .lax }
        == "s=x; Domain=example.com; SameSite=Lax"
#guard renderSetCookie { name := "s", value := "x", sameSite := some .strict } == "s=x; SameSite=Strict"
#guard renderSetCookie { name := "s", value := "x", sameSite := some .none_ } == "s=x; SameSite=None"

/-! ### parseSetCookie -/

#guard (parseSetCookie "id=42").map (·.name) == some "id"
#guard (parseSetCookie "id=42").map (·.value) == some "42"
#guard (parseSetCookie "id=42; Path=/admin; Secure; HttpOnly").bind (·.path) == some "/admin"
#guard (parseSetCookie "id=42; Path=/admin; Secure; HttpOnly").map (·.secure) == some true
#guard (parseSetCookie "id=42; Path=/admin; Secure; HttpOnly").map (·.httpOnly) == some true
#guard (parseSetCookie "id=42; Max-Age=7200").bind (·.maxAge) == some 7200
#guard (parseSetCookie "id=42; Domain=ex.com").bind (·.domain) == some "ex.com"
#guard (parseSetCookie "id=42; SameSite=Lax").bind (·.sameSite) == some SameSite.lax
#guard (parseSetCookie "id=42; SameSite=Strict").bind (·.sameSite) == some SameSite.strict
-- attribute keywords are case-insensitive; the value keeps its case
#guard (parseSetCookie "id=42; secure").map (·.secure) == some true
#guard (parseSetCookie "id=42; PATH=/X").bind (·.path) == some "/X"
-- a bare name (no '=') still parses, with empty value
#guard (parseSetCookie "flag").map (fun sc => (sc.name, sc.value)) == some ("flag", "")

/-! ### render → parse round-trip on Set-Cookie -/

#guard
  let sc : SetCookie := { name := "tok", value := "abc", path := some "/", maxAge := some 60, secure := true }
  ((parseSetCookie (renderSetCookie sc)).map (fun p => (p.name, p.value, p.path, p.maxAge, p.secure)))
    == some ("tok", "abc", some "/", some 60, true)

end Tests.Web.Cookie
