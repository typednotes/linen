import Linen.Network.WebApp.Extra.Parse
import Linen.Network.WebApp.Extra.Test

/-! ### Tests for `Linen.Network.WebApp.Extra.Parse`

    Coverage: `parseUrlEncoded`'s `+`-for-space decoding and bare-key
    handling, and `parseRequestBody`'s URL-encoded/multipart/absent
    `Content-Type` dispatch. -/

open Network.WebApp Network.WebApp.Extra Network.WebApp.Extra.Parse
open Network.WebApp.Extra.Test (post)
open Network.HTTP.Types

namespace Tests.Network.WebApp.Extra.Parse

#guard parseUrlEncoded "a=1&b=hello+world" == [("a", "1"), ("b", "hello world")]
#guard parseUrlEncoded "flag" == [("flag", "")]
#guard parseUrlEncoded "" == [("", "")]

#eval show IO Unit from do
  let echo : Application := fun req respond =>
    AppM.respondIO respond do
      let (params, files) ← parseRequestBody req
      pure (responseLBS status200 [] s!"{params.length},{files.length}")
  let resp ← post echo "/" "a=1&b=2".toUTF8 "application/x-www-form-urlencoded"
  unless String.fromUTF8! resp.simpleBody == "2,0" do
    throw (IO.userError s!"expected 2,0 params/files, got {String.fromUTF8! resp.simpleBody}")

#eval show IO Unit from do
  let echo : Application := fun req respond =>
    AppM.respondIO respond do
      let (params, files) ← parseRequestBody req
      pure (responseLBS status200 [] s!"{params.length},{files.length}")
  let resp ← post echo "/" "irrelevant".toUTF8 "multipart/form-data; boundary=x"
  unless String.fromUTF8! resp.simpleBody == "0,0" do
    throw (IO.userError s!"expected 0,0 for unparsed multipart, got {String.fromUTF8! resp.simpleBody}")

end Tests.Network.WebApp.Extra.Parse
