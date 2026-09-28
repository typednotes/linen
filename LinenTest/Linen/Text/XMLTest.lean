/-
  Tests for `Linen.Text.XML`.

  Pure, so everything is checked with `#guard`. The two headline documents are
  real AWS S3 responses — a `ListAllMyBucketsResult` and an `Error` — since
  reading those is what the module was written for.

  `Element` has no `BEq` (it is a mutual inductive), so assertions compare
  extracted strings rather than whole trees. That reads better anyway: it says
  what a consumer would actually pull out.
-/
import Linen.Text.XML

open Text.XML

namespace Tests.Text.XML

/-- The name of a parse result's root, or the error. -/
private def rootName (s : String) : String :=
  match parse s with
  | .ok e    => e.name.render
  | .error m => s!"error: {m}"

/-- Whether parsing failed. -/
private def failed (s : String) : Bool := (parse s).toOption.isNone

/-! ### QName -/

#guard (QName.parse "Name").local' == "Name"
#guard (QName.parse "Name").prefix' == none
#guard (QName.parse "ns:Name").local' == "Name"
#guard (QName.parse "ns:Name").prefix' == some "ns"
#guard (QName.parse "ns:Name").render == "ns:Name"
#guard (QName.parse ":x").local' == ":x"          -- empty prefix: taken whole
#guard (QName.parse "a:b:c").local' == "a:b:c"    -- two colons: not a QName

/-! ### Entities -/

#guard (unescape "plain").toOption == some "plain"
#guard (unescape "a &lt; b").toOption == some "a < b"
#guard (unescape "&lt;&gt;&amp;&apos;&quot;").toOption == some "<>&'\""
#guard (unescape "&#65;&#66;").toOption == some "AB"      -- decimal
#guard (unescape "&#x41;&#x42;").toOption == some "AB"    -- hex, lowercase x
#guard (unescape "&#X41;").toOption == some "A"           -- hex, uppercase X
-- Rejected rather than passed through, so a typo cannot become literal text.
#guard (unescape "&bogus;").toOption == none
#guard (unescape "a & b").toOption == none                -- bare '&'
#guard (unescape "&lt").toOption == none                  -- unterminated
#guard (unescape "&#;").toOption == none                  -- no digits

/-! ### Minimal documents -/

#guard rootName "<a/>" == "a"
#guard rootName "<a></a>" == "a"
#guard rootName "<?xml version=\"1.0\"?><a/>" == "a"      -- prolog skipped
#guard rootName "<!DOCTYPE a><a/>" == "a"                 -- doctype skipped
#guard rootName "  <a/>  " == "a"                         -- surrounding space ignored
#guard rootName "<ns:a xmlns:ns='u'/>" == "ns:a"

/-! ### Attributes -/

private def attrs (s : String) : List (String × String) :=
  match parse s with
  | .ok e    => e.attrs.map fun (k, v) => (k.render, v)
  | .error _ => []

#guard attrs "<a x='1' y=\"2\"/>" == [("x", "1"), ("y", "2")]   -- both quote styles
#guard attrs "<a/>" == []
#guard attrs "<a  x = '1' />" == [("x", "1")]                    -- space around '='
#guard attrs "<a x='a &lt; b'/>" == [("x", "a < b")]             -- entities expanded
#guard attrs "<a x='has \"quote\"'/>" == [("x", "has \"quote\"")]
-- Malformed attributes are errors, not silently dropped.
#guard failed "<a x/>"
#guard failed "<a x=1/>"          -- unquoted value
#guard failed "<a x=/>"

/-! ### Text, comments and CDATA -/

private def rootText (s : String) : String :=
  match parse s with | .ok e => e.text | .error _ => "«error»"

#guard rootText "<a>hello</a>" == "hello"
#guard rootText "<a>a &amp; b</a>" == "a & b"
#guard rootText "<a><!-- gone -->kept</a>" == "kept"          -- comments are not text
#guard rootText "<a><![CDATA[<raw> & ]]></a>" == "<raw> & "   -- CDATA is literal
#guard rootText "<a><b>nested</b></a>" == ""                  -- child text is not own text
#guard rootText "<a>before<b/>after</a>" == "beforeafter"

/-! ### Structure and lookup -/

private def s3ListBuckets : String :=
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
<ListAllMyBucketsResult xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">\
<Owner><ID>abc123</ID><DisplayName>me</DisplayName></Owner>\
<Buckets>\
<Bucket><Name>assets</Name><CreationDate>2024-01-02T03:04:05.000Z</CreationDate></Bucket>\
<Bucket><Name>logs</Name><CreationDate>2024-02-03T04:05:06.000Z</CreationDate></Bucket>\
</Buckets></ListAllMyBucketsResult>"

private def bucketNames : List String :=
  match parse s3ListBuckets with
  | .error _ => []
  | .ok root =>
    match root.child "Buckets" with
    | some bs => (bs.named "Bucket").filterMap (·.childText "Name")
    | none    => []

#guard rootName s3ListBuckets == "ListAllMyBucketsResult"
#guard bucketNames == ["assets", "logs"]

private def s3Error : String :=
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
<Error><Code>NoSuchBucket</Code>\
<Message>The specified bucket does not exist</Message>\
<BucketName>nope</BucketName><RequestId>TX1</RequestId></Error>"

#guard (parse s3Error).toOption.bind (·.childText "Code") == some "NoSuchBucket"
#guard (parse s3Error).toOption.bind (·.childText "Message")
     == some "The specified bucket does not exist"
#guard (parse s3Error).toOption.bind (·.childText "Missing") == none

-- Lookup ignores the namespace prefix, which is what a consumer of a
-- namespaced protocol document wants.
#guard (parse "<ns:a xmlns:ns='u'><ns:b>v</ns:b></ns:a>").toOption.bind (·.childText "b")
     == some "v"

-- Deep nesting: depth lives in the builder's stack, not the call graph.
#guard rootText "<a><b><c><d>deep</d></c></b></a>" == ""
#guard ((((parse "<a><b><c><d>deep</d></c></b></a>").toOption.bind (·.child "b")).bind
          (·.child "c")).bind (·.childText "d")) == some "deep"

/-! ### Errors

  Every malformed case is reported, never silently accepted. -/

#guard failed "<a></b>"           -- mismatched close
#guard failed "<a>"               -- unclosed
#guard failed "</a>"              -- close with nothing open
#guard failed ""                  -- no root
#guard failed "<a/><b/>"          -- two roots
#guard failed "<a>&bogus;</a>"    -- unknown entity
#guard failed "< a/>"             -- space after '<'
#guard failed "<a/"               -- truncated mid-tag

-- A comment or text outside the root is tolerated; a second element is not.
#guard rootName "<!-- lead --><a/>" == "a"
#guard failed "<a/><!-- trail --><b/>"

/-! ### Tokenizer

  Checked directly so a tokenizer bug is distinguishable from a builder bug. -/

#guard (tokenize "<a/>").toOption == some [.empty "a" []]
#guard (tokenize "<a></a>").toOption == some [.start "a" [], .end' "a"]
#guard (tokenize "<a>t</a>").toOption == some [.start "a" [], .text "t", .end' "a"]
#guard (tokenize "<a x='1'/>").toOption == some [.empty "a" [("x", "1")]]
#guard (tokenize "<!-- c -->").toOption == some [.comment " c "]
#guard (tokenize "<?pi?>").toOption == some []                    -- processing instruction dropped
#guard (tokenize "<a></a  >").toOption == some [.start "a" [], .end' "a"]   -- space in close tag

/-! ### Signatures -/

example : String → Except String Element := parse
example : String → Except String (List Content) := parseContent
example : String → Except String (List Token) := tokenize
example : String → Except String String := unescape
example : Element → String → Option Element := Element.child
example : Element → String → Option String := Element.attr

end Tests.Text.XML
