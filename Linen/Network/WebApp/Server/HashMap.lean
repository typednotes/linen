/-
  Linen.Network.WebApp.Server.HashMap — Specialized hash map

  Thin wrapper around Lean's HashMap for header storage.
-/
import Std.Data.HashMap

namespace Network.WebApp.Server

/-- A specialized hash map for string keys.
    Uses Lean's standard HashMap. -/
abbrev HeaderMap := Std.HashMap String String

namespace HeaderMap

/-- Create an empty header map. -/
@[inline] def empty : HeaderMap := {}

/-- Insert a key-value pair. -/
@[inline] def insert' (m : HeaderMap) (k v : String) : HeaderMap :=
  Std.HashMap.insert m k v

/-- Look up a value by key. -/
@[inline] def find? (m : HeaderMap) (k : String) : Option String :=
  Std.HashMap.get? m k

end HeaderMap

end Network.WebApp.Server
