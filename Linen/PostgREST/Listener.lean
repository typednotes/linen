/-
  PostgREST.Listener — PostgreSQL LISTEN/NOTIFY

  Listens for `pgrst` channel notifications from PostgreSQL, triggering
  schema cache reloads and configuration refreshes.

  ## Haskell source
  - `PostgREST.Listener` (postgrest package)
-/

namespace PostgREST.Listener

/-- The PostgreSQL channel PostgREST listens on. -/
def pgrstChannel : String := "pgrst"

/-- LISTEN command SQL. -/
def listenSql : String := s!"LISTEN {pgrstChannel}"

/-- Notification payloads that trigger specific actions. -/
inductive NotificationAction where
  | reload       -- Full schema cache reload
  | configReload -- Configuration reload
  | unknown (payload : String)
  deriving BEq, Repr

/-- Parse a notification payload into an action. -/
def parseNotification (payload : String) : NotificationAction :=
  match payload.trimAscii.toString.toLower with
  | "reload schema" | "" => .reload
  | "reload config" => .configReload
  | p => .unknown p

end PostgREST.Listener
