/-
  Tests for `Linen.CDP.Domains.IO`.
-/
import Linen.CDP.Domains.IO

open CDP.Domains.IO
open CDP.Internal.Utils (Command)
open Data.Json (ToJSON FromJSON)
open Data.Json.Decode (decodeAs)
open Data.Json.Encode (encode)

namespace Tests.CDP.Domains.IO

#guard encode (ToJSON.toJSON ({ handle := "1" } : PClose)) = "{\"handle\":\"1\"}"
#guard Command.commandName ({ handle := "1" } : PClose) = "IO.close"

#guard encode (ToJSON.toJSON ({ handle := "1" } : PRead)) = "{\"handle\":\"1\"}"
#guard encode (ToJSON.toJSON ({ handle := "1", offset := some 4, size := some 8 } : PRead))
  = "{\"handle\":\"1\",\"offset\":4,\"size\":8}"
#guard Command.commandName ({ handle := "1" } : PRead) = "IO.read"

#guard decodeAs "{\"data\": \"abc\", \"eof\": false}" (α := Read)
  = .ok { base64Encoded := none, data := "abc", eof := false }
#guard decodeAs "{\"base64Encoded\": true, \"data\": \"YWJj\", \"eof\": true}" (α := Read)
  = .ok { base64Encoded := some true, data := "YWJj", eof := true }

#guard encode (ToJSON.toJSON ({ objectId := "obj-1" } : PResolveBlob)) = "{\"objectId\":\"obj-1\"}"
#guard Command.commandName ({ objectId := "obj-1" } : PResolveBlob) = "IO.resolveBlob"
#guard decodeAs "{\"uuid\": \"u\"}" (α := ResolveBlob) = .ok { uuid := "u" }

end Tests.CDP.Domains.IO
