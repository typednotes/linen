/-
  Linen.CDP.Definition — Chrome DevTools Protocol schema descriptor types

  Ports the data types (and `FromJSON` instances) of `CDP.Definition` (see
  `docs/imports/cdp/dependencies.md`) — the JSON shape of a
  `devtools-protocol`-style protocol specification, as returned live by a
  browser's `/json/protocol` endpoint (`CDP.Endpoints.EPCurrentProtocol`).

  Upstream's `CDP.Definition` also has a `parse : FilePath -> IO TopLevel`
  entry point reading a spec file from disk — that belongs to the excluded
  code generator (`CDP.Gen.*`, `gen/Main.hs`) and is not ported.
-/
import Linen.Data.Json

namespace CDP.Definition

open Data.Json (Value FromJSON)

/-- Optional field access with a default for a missing (or explicit `null`) key,
    analogous to Haskell's `(.:?) ... (.!=)`. -/
private def getFieldD [FromJSON α] (obj : Value) (key : String) (default : α) : Except String α := do
  match ← Value.getFieldOpt obj key with
  | some j => FromJSON.parseJSON j
  | none => .ok default

/-- Optional field access, analogous to Haskell's `(.:?)`. -/
private def getFieldOpt' [FromJSON α] (obj : Value) (key : String) : Except String (Option α) := do
  (← Value.getFieldOpt obj key).mapM FromJSON.parseJSON

/-- A `$ref`/array-element type descriptor (RFC-style `items` field). -/
structure Items where
  type : Option String := none
  ref : Option String := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON Items where
  parseJSON v := do
    .ok { type := ← getFieldOpt' v "type", ref := ← getFieldOpt' v "$ref" }

/-- A single property of a `TypeDef`, or a command/event parameter. -/
structure Property where
  items : Option Items := none
  experimental : Bool := false
  name : String
  type : Option String := none
  enum : Option (List String) := none
  optional : Bool := false
  ref : Option String := none
  description : Option String := none
  deprecated : Bool := false
  deriving Repr, BEq, DecidableEq

instance : FromJSON Property where
  parseJSON v := do
    .ok
      { items := ← getFieldOpt' v "items"
        experimental := ← getFieldD v "experimental" false
        name := ← Value.getField v "name" >>= FromJSON.parseJSON
        type := ← getFieldOpt' v "type"
        enum := ← getFieldOpt' v "enum"
        optional := ← getFieldD v "optional" false
        ref := ← getFieldOpt' v "$ref"
        description := ← getFieldOpt' v "description"
        deprecated := ← getFieldD v "deprecated" false }

/-- A command exposed by a `Domain`. -/
structure Command where
  experimental : Bool := false
  name : String
  returns : List Property := []
  parameters : List Property := []
  redirect : Option String := none
  description : Option String := none
  deprecated : Bool := false
  deriving Repr, BEq, DecidableEq

instance : FromJSON Command where
  parseJSON v := do
    .ok
      { experimental := ← getFieldD v "experimental" false
        name := ← Value.getField v "name" >>= FromJSON.parseJSON
        returns := ← getFieldD v "returns" []
        parameters := ← getFieldD v "parameters" []
        redirect := ← getFieldOpt' v "redirect"
        description := ← getFieldOpt' v "description"
        deprecated := ← getFieldD v "deprecated" false }

/-- A type defined by a `Domain`. -/
structure TypeDef where
  items : Option Items := none
  experimental : Bool := false
  id : String
  type : String
  enum : Option (List String) := none
  properties : Option (List Property) := none
  description : Option String := none
  deprecated : Bool := false
  deriving Repr, BEq, DecidableEq

instance : FromJSON TypeDef where
  parseJSON v := do
    .ok
      { items := ← getFieldOpt' v "items"
        experimental := ← getFieldD v "experimental" false
        id := ← Value.getField v "id" >>= FromJSON.parseJSON
        type := ← Value.getField v "type" >>= FromJSON.parseJSON
        enum := ← getFieldOpt' v "enum"
        properties := ← getFieldOpt' v "properties"
        description := ← getFieldOpt' v "description"
        deprecated := ← getFieldD v "deprecated" false }

/-- An event fired by a `Domain`. -/
structure Event where
  experimental : Bool := false
  name : String
  parameters : List Property := []
  description : Option String := none
  deprecated : Bool := false
  deriving Repr, BEq, DecidableEq

instance : FromJSON Event where
  parseJSON v := do
    .ok
      { experimental := ← getFieldD v "experimental" false
        name := ← Value.getField v "name" >>= FromJSON.parseJSON
        parameters := ← getFieldD v "parameters" []
        description := ← getFieldOpt' v "description"
        deprecated := ← getFieldD v "deprecated" false }

/-- A CDP domain (e.g. `"Page"`, `"Runtime"`): its commands, types, and events. -/
structure Domain where
  commands : List Command
  domain : String
  dependencies : List String := []
  experimental : Bool := false
  types : List TypeDef := []
  events : List Event := []
  description : Option String := none
  deprecated : Bool := false
  deriving Repr, BEq, DecidableEq

instance : FromJSON Domain where
  parseJSON v := do
    .ok
      { commands := ← Value.getField v "commands" >>= FromJSON.parseJSON
        domain := ← Value.getField v "domain" >>= FromJSON.parseJSON
        dependencies := ← getFieldD v "dependencies" []
        experimental := ← getFieldD v "experimental" false
        types := ← getFieldD v "types" []
        events := ← getFieldD v "events" []
        description := ← getFieldOpt' v "description"
        deprecated := ← getFieldD v "deprecated" false }

/-- The protocol version (`{"major": ..., "minor": ...}`). -/
structure Version where
  minor : String
  major : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON Version where
  parseJSON v := do
    .ok
      { minor := ← Value.getField v "minor" >>= FromJSON.parseJSON
        major := ← Value.getField v "major" >>= FromJSON.parseJSON }

/-- A full protocol specification, as returned by `/json/protocol`. -/
structure TopLevel where
  version : Version
  domains : List Domain
  deriving Repr, BEq, DecidableEq

instance : FromJSON TopLevel where
  parseJSON v := do
    .ok
      { version := ← Value.getField v "version" >>= FromJSON.parseJSON
        domains := ← Value.getField v "domains" >>= FromJSON.parseJSON }

end CDP.Definition
