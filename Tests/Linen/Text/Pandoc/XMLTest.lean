/-
  Tests for `Linen.Text.Pandoc.XML`.
-/
import Linen.Text.Pandoc.XML

namespace Tests.Linen.Text.Pandoc.XML

open _root_.Linen.Text.Pandoc

-- ── escaping ──────────────────────────────────────────────────────────

#guard XML.escapeCharForXML '&' == "&amp;"
#guard XML.escapeCharForXML '<' == "&lt;"
#guard XML.escapeCharForXML '\'' == "'"   -- apostrophe is not escaped
#guard XML.escapeStringForXML "a < b & c > \"d\"" == "a &lt; b &amp; c &gt; &quot;d&quot;"

-- ── numeric entities ──────────────────────────────────────────────────

#guard XML.toEntities "aé" == "a&#xE9;"
#guard XML.toEntities "abc" == "abc"

-- ── fromEntities ──────────────────────────────────────────────────────

#guard XML.fromEntities "a &amp; b" == "a & b"
#guard XML.fromEntities "&lt;tag&gt;" == "<tag>"
#guard XML.fromEntities "&#65;&#66;&#67;" == "ABC"
#guard XML.fromEntities "&#xE9;" == "é"
#guard XML.fromEntities "&copy; 2024" == "© 2024"
-- unknown entity left unchanged
#guard XML.fromEntities "a &nope; b" == "a &nope; b"
-- literal ampersand not starting an entity
#guard XML.fromEntities "AT&T" == "AT&T"
-- round-trip with escaping
#guard XML.fromEntities (XML.escapeStringForXML "x < y & z") == "x < y & z"

-- ── escapeNCName ──────────────────────────────────────────────────────

#guard XML.escapeNCName "valid_name-1.2" == "valid_name-1.2"
#guard XML.escapeNCName "has space" == "hasU20space"

-- ── attribute allowlists ──────────────────────────────────────────────

#guard XML.rdfaAttributes.length == 12
#guard XML.html5Attributes.contains "href" == true
#guard XML.html4Attributes.contains "bgcolor" == true

end Tests.Linen.Text.Pandoc.XML
