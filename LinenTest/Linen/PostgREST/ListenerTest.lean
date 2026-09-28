/-
  Tests for `Linen.PostgREST.Listener`.
-/
import Linen.PostgREST.Listener

open PostgREST.Listener

namespace Tests.PostgREST.Listener

/-! ### Constants -/

#guard pgrstChannel == "pgrst"
#guard listenSql == "LISTEN pgrst"

/-! ### `parseNotification` -/

#guard parseNotification "reload schema" == .reload
#guard parseNotification "" == .reload
#guard parseNotification "  " == .reload
#guard parseNotification "reload config" == .configReload
#guard parseNotification "RELOAD SCHEMA" == .reload
#guard parseNotification "RELOAD CONFIG" == .configReload
#guard parseNotification "  reload config  " == .configReload
#guard parseNotification "something else" == .unknown "something else"

end Tests.PostgREST.Listener
