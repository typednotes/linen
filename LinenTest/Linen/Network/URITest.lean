/-
  Tests for `Linen.Network.URI`.

  Parsing/rendering/resolution are pure, so behaviour is checked with `#guard`,
  including several of RFC 3986's own worked examples (§1.1.2, §5.4).
-/
import Linen.Network.URI

open Network.URI

namespace Tests.Network.URI

/-! ### parseURI — the shape `cdp`'s `CDP.Endpoints` actually needs -/

#guard parseURI "http://127.0.0.1:9222/json/version" ==
  some { uriScheme := "http:"
       , uriAuthority := some { uriUserInfo := "", uriRegName := "127.0.0.1", uriPort := ":9222" }
       , uriPath := "/json/version", uriQuery := "", uriFragment := "" }

#guard ((parseURI "http://127.0.0.1:9222/json/version").bind (·.uriAuthority)).map (·.uriRegName)
  == some "127.0.0.1"
#guard ((parseURI "http://127.0.0.1:9222/json/version").bind (·.uriAuthority)).map (·.uriPort)
  == some ":9222"
#guard (parseURI "http://127.0.0.1:9222/json/version").map (·.uriPath) == some "/json/version"

-- RFC 3986 §1.1.2's own example.
#guard parseURI "foo://anonymous@www.haskell.org:42/ghc?query#frag" ==
  some { uriScheme := "foo:"
       , uriAuthority := some { uriUserInfo := "anonymous@", uriRegName := "www.haskell.org", uriPort := ":42" }
       , uriPath := "/ghc", uriQuery := "?query", uriFragment := "#frag" }

#guard parseURI "not a valid uri" == none
#guard parseURI "http://host/needs a scheme" == none   -- unescaped space in path

/-! ### parseURIReference / parseRelativeReference / parseAbsoluteURI -/

#guard parseURI "foo" == none                          -- no scheme ⇒ not an absolute URI
#guard (parseURIReference "foo").map (·.uriPath) == some "foo"
#guard (parseRelativeReference "foo").map (·.uriPath) == some "foo"
#guard parseRelativeReference "http://foo.org" == none  -- has a scheme ⇒ not a relative reference
#guard parseAbsoluteURI "http://foo.org?q#f" == none      -- a fragment isn't part of absolute-URI
#guard (parseAbsoluteURI "http://foo.org?q").map (·.uriQuery) == some "?q"

/-! ### isX classifiers -/

#guard isURI "http://example.org/"
#guard !isURI "example.org/"          -- no scheme
#guard isURIReference "example.org/"
#guard isRelativeReference "foo/bar"
#guard !isRelativeReference "http://foo.org"
#guard isAbsoluteURI "http://foo.org/x?y"
#guard isIPv4address "192.168.1.1"
#guard !isIPv4address "192.168.1.999"    -- octet > 255... wait 999 has 3 digits, still > 255
#guard !isIPv4address "1.2.3"             -- too few octets
#guard isIPv6address "::1"
#guard isIPv6address "2001:db8::1"

/-! ### uriIsAbsolute / uriIsRelative -/

#guard uriIsAbsolute { nullURI with uriScheme := "http:" }
#guard uriIsRelative nullURI

/-! ### Authority host forms -/

#guard ((parseURI "http://[::1]:8080/").bind (·.uriAuthority)).map (·.uriRegName) == some "[::1]"
#guard ((parseURI "http://user:pass@host/").bind (·.uriAuthority)).map (·.uriUserInfo)
  == some "user:pass@"

/-! ### Percent-encoding -/

#guard escapeURIString isUnescapedInURIComponent "a b/c" == "a%20b%2Fc"
#guard escapeURIString isUnescapedInURI "a b/c" == "a%20b/c"     -- '/' left alone (reserved, allowed)
#guard unEscapeString' "a%20b%2Fc" == "a b/c"
#guard unEscapeString' (escapeURIString isUnescapedInURIComponent "hello world!") == "hello world!"
#guard unEscapeString' "100%25" == "100%"
#guard unEscapeString' "%zz" == "%zz"   -- invalid escape passed through literally

/-! ### Rendering -/

#guard toString ({ nullURI with
    uriScheme := "http:"
    uriAuthority := some { uriUserInfo := "", uriRegName := "example.org", uriPort := "" }
    uriPath := "/a", uriQuery := "?q", uriFragment := "#f" } : URI)
  == "http://example.org/a?q#f"

-- The default `Show`-equivalent hides a password.
#guard toString ({ nullURI with
    uriScheme := "http:"
    uriAuthority := some { uriUserInfo := "user:secret@", uriRegName := "h", uriPort := "" } } : URI)
  == "http://user:...@h"

/-! ### pathSegments -/

#guard (parseURI "http://example.org/foo/bar/baz").map pathSegments == some ["foo", "bar", "baz"]
#guard (parseURI "http://example.org/").map pathSegments == some []

/-! ### removeDotSegments (RFC 3986 §5.2.4, both of its own worked examples) -/

#guard removeDotSegments "/a/b/c/./../../g" == "/a/g"
#guard removeDotSegments "mid/content=5/../6" == "mid/6"

/-! ### relativeTo (RFC 3986 §5.4's full worked example set, base
    `http://a/b/c/d;p?q`) -/

def base : URI := (parseURI "http://a/b/c/d;p?q").get!

#guard toString (relativeTo (parseURIReference "g:h" |>.get!) base) == "g:h"
#guard toString (relativeTo (parseURIReference "g" |>.get!) base) == "http://a/b/c/g"
#guard toString (relativeTo (parseURIReference "./g" |>.get!) base) == "http://a/b/c/g"
#guard toString (relativeTo (parseURIReference "g/" |>.get!) base) == "http://a/b/c/g/"
#guard toString (relativeTo (parseURIReference "/g" |>.get!) base) == "http://a/g"
#guard toString (relativeTo (parseURIReference "//g" |>.get!) base) == "http://g"
#guard toString (relativeTo (parseURIReference "?y" |>.get!) base) == "http://a/b/c/d;p?y"
#guard toString (relativeTo (parseURIReference "g?y" |>.get!) base) == "http://a/b/c/g?y"
#guard toString (relativeTo (parseURIReference "#s" |>.get!) base) == "http://a/b/c/d;p?q#s"
#guard toString (relativeTo (parseURIReference "g#s" |>.get!) base) == "http://a/b/c/g#s"
#guard toString (relativeTo (parseURIReference "g?y#s" |>.get!) base) == "http://a/b/c/g?y#s"
#guard toString (relativeTo (parseURIReference ";x" |>.get!) base) == "http://a/b/c/;x"
#guard toString (relativeTo (parseURIReference "g;x" |>.get!) base) == "http://a/b/c/g;x"
#guard toString (relativeTo (parseURIReference "g;x?y#s" |>.get!) base) == "http://a/b/c/g;x?y#s"
#guard toString (relativeTo (parseURIReference "" |>.get!) base) == "http://a/b/c/d;p?q"
#guard toString (relativeTo (parseURIReference "." |>.get!) base) == "http://a/b/c/"
#guard toString (relativeTo (parseURIReference "./" |>.get!) base) == "http://a/b/c/"
#guard toString (relativeTo (parseURIReference ".." |>.get!) base) == "http://a/b/"
#guard toString (relativeTo (parseURIReference "../" |>.get!) base) == "http://a/b/"
#guard toString (relativeTo (parseURIReference "../g" |>.get!) base) == "http://a/b/g"
#guard toString (relativeTo (parseURIReference "../.." |>.get!) base) == "http://a/"
#guard toString (relativeTo (parseURIReference "../../g" |>.get!) base) == "http://a/g"

/-! ### relativeFrom / relativeTo round-trip -/

#guard relativeTo (relativeFrom (parseURI "http://example.com/Root/sub1/name2" |>.get!) base) base
    == (parseURI "http://example.com/Root/sub1/name2" |>.get!)

/-! ### Normalization -/

#guard normalizeCase "HTTP://example.org/%3a" == "http://example.org/%3A"
#guard normalizeEscape "%7e" == "~"                           -- unreserved ⇒ literal
#guard normalizeEscape "%2f" == "%2f"                          -- reserved ⇒ left escaped
#guard normalizePathSegments "http://a/b/c/./../../g" == "http://a/g"

end Tests.Network.URI
