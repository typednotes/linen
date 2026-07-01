/-
  PostgREST.Network — Server binding helpers

  ## Haskell source
  - `PostgREST.Network` (postgrest package)
-/

namespace PostgREST.Network

/-- Resolve a host string to a bindable address.
    `"!4"` means bind to all IPv4 interfaces,
    `"!6"` means bind to all IPv6 interfaces. -/
def resolveHost (host : String) : String :=
  match host with
  | "!4" => "0.0.0.0"
  | "!6" => "::"
  | "*"  => "0.0.0.0"
  | h    => h

end PostgREST.Network
