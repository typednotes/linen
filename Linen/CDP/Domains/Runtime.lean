/-
  Linen.CDP.Domains.Runtime — the `Runtime` CDP domain

  Exposes the JavaScript runtime by means of remote evaluation and mirror
  objects. Evaluation results are returned as mirror objects that expose
  object type, string representation, and a unique identifier that can be
  used for further object reference. Ports `CDP.Domains.Runtime` (see
  `docs/imports/cdp/dependencies.md`); naming conventions as in
  `CDP.Domains.CacheStorage`'s docstring.

  Two consolidations versus upstream, which redeclares identical enums once
  per context (Haskell's single flat namespace forces distinct names even for
  structurally-identical types): `ObjType` (upstream's `RemoteObjectType` and
  `ObjectPreviewType`, which list the exact same 8 cases) and `Subtype`
  (upstream's `RemoteObjectSubtype`/`ObjectPreviewSubtype`/
  `PropertyPreviewSubtype`, the same 19 cases each time) are each declared
  once and reused. `PropertyPreviewType` keeps its own type (it has an extra
  `accessor` case the other two don't).

  `ObjectPreview`, `PropertyPreview`, and `EntryPreview` are mutually
  recursive (`ObjectPreview.properties : [PropertyPreview]`,
  `PropertyPreview.valuePreview : Maybe ObjectPreview`,
  `ObjectPreview.entries : Maybe [EntryPreview]`,
  `EntryPreview.value/key : ObjectPreview`/`Maybe ObjectPreview`), and
  `StackTrace` is self-referential via `parent`. Both need real termination
  proofs for their `FromJSON`/`ToJSON`, via `Data.Json.Value.getField_sizeOf_lt`
  / `lookup_sizeOf_lt` (see `CDP.Domains.Media.PlayerError` for the same
  technique applied to a simpler, singly-recursive case).
-/
import Linen.CDP.Internal.Utils

namespace CDP.Domains.Runtime

open Data.Json (Value ToJSON FromJSON)
open CDP.Internal.Utils (Command Event)

abbrev ScriptId := String

/-- The value serialized by the WebDriver BiDi specification
    (<https://w3c.github.io/webdriver-bidi>). -/
inductive WebDriverValueType where
  | undefined | null | string | number | boolean | bigint | regexp | date | symbol | array
  | object | function | map | set | weakmap | weakset | error | proxy | promise | typedarray
  | arraybuffer | node | window
  deriving Repr, BEq, DecidableEq

instance : FromJSON WebDriverValueType where
  parseJSON
    | .string "undefined" => .ok .undefined
    | .string "null" => .ok .null
    | .string "string" => .ok .string
    | .string "number" => .ok .number
    | .string "boolean" => .ok .boolean
    | .string "bigint" => .ok .bigint
    | .string "regexp" => .ok .regexp
    | .string "date" => .ok .date
    | .string "symbol" => .ok .symbol
    | .string "array" => .ok .array
    | .string "object" => .ok .object
    | .string "function" => .ok .function
    | .string "map" => .ok .map
    | .string "set" => .ok .set
    | .string "weakmap" => .ok .weakmap
    | .string "weakset" => .ok .weakset
    | .string "error" => .ok .error
    | .string "proxy" => .ok .proxy
    | .string "promise" => .ok .promise
    | .string "typedarray" => .ok .typedarray
    | .string "arraybuffer" => .ok .arraybuffer
    | .string "node" => .ok .node
    | .string "window" => .ok .window
    | v => .error s!"failed to parse WebDriverValueType: {repr v}"

instance : ToJSON WebDriverValueType where
  toJSON
    | .undefined => .string "undefined" | .null => .string "null" | .string => .string "string"
    | .number => .string "number" | .boolean => .string "boolean" | .bigint => .string "bigint"
    | .regexp => .string "regexp" | .date => .string "date" | .symbol => .string "symbol"
    | .array => .string "array" | .object => .string "object" | .function => .string "function"
    | .map => .string "map" | .set => .string "set" | .weakmap => .string "weakmap"
    | .weakset => .string "weakset" | .error => .string "error" | .proxy => .string "proxy"
    | .promise => .string "promise" | .typedarray => .string "typedarray"
    | .arraybuffer => .string "arraybuffer" | .node => .string "node" | .window => .string "window"

/-- Represents the value serialized by the WebDriver BiDi specification. -/
structure WebDriverValue where
  type : WebDriverValueType
  value : Option Value := none
  objectId : Option String := none
  deriving Repr, BEq

instance : FromJSON WebDriverValue where
  parseJSON v := do
    .ok
      { type := ← Value.getField v "type" >>= FromJSON.parseJSON
        value := ← (← Value.getFieldOpt v "value").mapM FromJSON.parseJSON
        objectId := ← (← Value.getFieldOpt v "objectId").mapM FromJSON.parseJSON }

instance : ToJSON WebDriverValue where
  toJSON p := Data.Json.object <|
    [("type", ToJSON.toJSON p.type)]
    ++ (p.value.map fun v => ("value", ToJSON.toJSON v)).toList
    ++ (p.objectId.map fun v => ("objectId", ToJSON.toJSON v)).toList

abbrev RemoteObjectId := String

/-- A primitive value which cannot be JSON-stringified. Includes values `-0`,
    `NaN`, `Infinity`, `-Infinity`, and bigint literals. -/
abbrev UnserializableValue := String

/-- The type of a mirror object (`RemoteObject`/`ObjectPreview`). -/
inductive ObjType where
  | object | function | undefined | string | number | boolean | symbol | bigint
  deriving Repr, BEq, DecidableEq

instance : FromJSON ObjType where
  parseJSON
    | .string "object" => .ok .object
    | .string "function" => .ok .function
    | .string "undefined" => .ok .undefined
    | .string "string" => .ok .string
    | .string "number" => .ok .number
    | .string "boolean" => .ok .boolean
    | .string "symbol" => .ok .symbol
    | .string "bigint" => .ok .bigint
    | v => .error s!"failed to parse ObjType: {repr v}"

instance : ToJSON ObjType where
  toJSON
    | .object => .string "object" | .function => .string "function"
    | .undefined => .string "undefined" | .string => .string "string" | .number => .string "number"
    | .boolean => .string "boolean" | .symbol => .string "symbol" | .bigint => .string "bigint"

/-- The subtype hint of a mirror object (`RemoteObject`/`ObjectPreview`/
    `PropertyPreview`), specified for `object`-type values only. -/
inductive Subtype where
  | array | null | node | regexp | date | map | set | weakmap | weakset | iterator | generator
  | error | proxy | promise | typedarray | arraybuffer | dataview | webassemblymemory | wasmvalue
  deriving Repr, BEq, DecidableEq

instance : FromJSON Subtype where
  parseJSON
    | .string "array" => .ok .array
    | .string "null" => .ok .null
    | .string "node" => .ok .node
    | .string "regexp" => .ok .regexp
    | .string "date" => .ok .date
    | .string "map" => .ok .map
    | .string "set" => .ok .set
    | .string "weakmap" => .ok .weakmap
    | .string "weakset" => .ok .weakset
    | .string "iterator" => .ok .iterator
    | .string "generator" => .ok .generator
    | .string "error" => .ok .error
    | .string "proxy" => .ok .proxy
    | .string "promise" => .ok .promise
    | .string "typedarray" => .ok .typedarray
    | .string "arraybuffer" => .ok .arraybuffer
    | .string "dataview" => .ok .dataview
    | .string "webassemblymemory" => .ok .webassemblymemory
    | .string "wasmvalue" => .ok .wasmvalue
    | v => .error s!"failed to parse Subtype: {repr v}"

instance : ToJSON Subtype where
  toJSON
    | .array => .string "array" | .null => .string "null" | .node => .string "node"
    | .regexp => .string "regexp" | .date => .string "date" | .map => .string "map"
    | .set => .string "set" | .weakmap => .string "weakmap" | .weakset => .string "weakset"
    | .iterator => .string "iterator" | .generator => .string "generator" | .error => .string "error"
    | .proxy => .string "proxy" | .promise => .string "promise" | .typedarray => .string "typedarray"
    | .arraybuffer => .string "arraybuffer" | .dataview => .string "dataview"
    | .webassemblymemory => .string "webassemblymemory" | .wasmvalue => .string "wasmvalue"

/-- The type of a `PropertyPreview` — like `ObjType`, plus `accessor` (the
    property itself is an accessor property). -/
inductive PropertyPreviewType where
  | object | function | undefined | string | number | boolean | symbol | accessor | bigint
  deriving Repr, BEq, DecidableEq

instance : FromJSON PropertyPreviewType where
  parseJSON
    | .string "object" => .ok .object
    | .string "function" => .ok .function
    | .string "undefined" => .ok .undefined
    | .string "string" => .ok .string
    | .string "number" => .ok .number
    | .string "boolean" => .ok .boolean
    | .string "symbol" => .ok .symbol
    | .string "accessor" => .ok .accessor
    | .string "bigint" => .ok .bigint
    | v => .error s!"failed to parse PropertyPreviewType: {repr v}"

instance : ToJSON PropertyPreviewType where
  toJSON
    | .object => .string "object" | .function => .string "function"
    | .undefined => .string "undefined" | .string => .string "string" | .number => .string "number"
    | .boolean => .string "boolean" | .symbol => .string "symbol" | .accessor => .string "accessor"
    | .bigint => .string "bigint"

mutual

/-- Object containing abbreviated remote object value. Mutually recursive with
    `PropertyPreview`/`EntryPreview` — see the module docstring. -/
structure ObjectPreview where
  type : ObjType
  subtype : Option Subtype := none
  description : Option String := none
  /-- `true` iff some of the properties or entries of the original object did
      not fit. -/
  overflow : Bool
  properties : List PropertyPreview
  /-- Specified for `map` and `set` subtype values only. -/
  entries : Option (List EntryPreview) := none
  deriving Repr, BEq

/-- One property of an `ObjectPreview`. -/
structure PropertyPreview where
  name : String
  /-- `accessor` means the property itself is an accessor property. -/
  type : PropertyPreviewType
  /-- User-friendly property value string. -/
  value : Option String := none
  valuePreview : Option ObjectPreview := none
  subtype : Option Subtype := none
  deriving Repr, BEq

/-- One `map`/`set`-like entry of an `ObjectPreview`. -/
structure EntryPreview where
  /-- Specified for map-like collection entries. -/
  key : Option ObjectPreview := none
  value : ObjectPreview
  deriving Repr, BEq

end

/-- Finish decoding an `ObjectPreview` given its already-decoded recursive
    fields (`properties`/`entries`) — an ordinary (non-recursive) helper, kept
    out of the `mutual` block below. -/
private def finishObjectPreview (v : Value) (properties : List PropertyPreview)
    (entries : Option (List EntryPreview)) : Except String ObjectPreview := do
  .ok
    { type := ← Value.getField v "type" >>= FromJSON.parseJSON
      subtype := ← (← Value.getFieldOpt v "subtype").mapM FromJSON.parseJSON
      description := ← (← Value.getFieldOpt v "description").mapM FromJSON.parseJSON
      overflow := ← Value.getField v "overflow" >>= FromJSON.parseJSON
      properties, entries }

/-- Finish decoding a `PropertyPreview` given its already-decoded
    `valuePreview`. -/
private def finishPropertyPreview (v : Value) (valuePreview : Option ObjectPreview) :
    Except String PropertyPreview := do
  .ok
    { name := ← Value.getField v "name" >>= FromJSON.parseJSON
      type := ← Value.getField v "type" >>= FromJSON.parseJSON
      value := ← (← Value.getFieldOpt v "value").mapM FromJSON.parseJSON
      valuePreview
      subtype := ← (← Value.getFieldOpt v "subtype").mapM FromJSON.parseJSON }

mutual

def parseObjectPreview (v : Value) : Except String ObjectPreview :=
  match h1 : Value.getField v "properties" with
  | .error e => .error e
  | .ok propsV =>
    match parsePropertyPreviewList propsV with
    | .error e => .error e
    | .ok properties =>
      match h2 : v.lookup "entries" with
      | none => finishObjectPreview v properties none
      | some .null => finishObjectPreview v properties none
      | some entriesV =>
        match parseEntryPreviewList entriesV with
        | .error e => .error e
        | .ok entries => finishObjectPreview v properties (some entries)
termination_by sizeOf v
decreasing_by
  all_goals first
    | exact Value.getField_sizeOf_lt h1
    | exact Value.lookup_sizeOf_lt h2

private def parsePropertyPreviewList (v : Value) : Except String (List PropertyPreview) :=
  match v with
  | .array arr => arr.attach.toList.mapM fun p => parsePropertyPreview p.1
  | v => .error s!"expected array, got {repr v}"
termination_by sizeOf v
decreasing_by
  simp_wf
  have := Array.sizeOf_lt_of_mem p.2
  omega

def parsePropertyPreview (v : Value) : Except String PropertyPreview :=
  match h : v.lookup "valuePreview" with
  | none => finishPropertyPreview v none
  | some .null => finishPropertyPreview v none
  | some vpV =>
    match parseObjectPreview vpV with
    | .error e => .error e
    | .ok vp => finishPropertyPreview v (some vp)
termination_by sizeOf v
decreasing_by exact Value.lookup_sizeOf_lt h

private def parseEntryPreviewList (v : Value) : Except String (List EntryPreview) :=
  match v with
  | .array arr => arr.attach.toList.mapM fun p => parseEntryPreview p.1
  | v => .error s!"expected array, got {repr v}"
termination_by sizeOf v
decreasing_by
  simp_wf
  have := Array.sizeOf_lt_of_mem p.2
  omega

def parseEntryPreview (v : Value) : Except String EntryPreview :=
  match h2 : Value.getField v "value" with
  | .error e => .error e
  | .ok valueV =>
    match parseObjectPreview valueV with
    | .error e => .error e
    | .ok value =>
      match h1 : v.lookup "key" with
      | none => .ok { key := none, value }
      | some .null => .ok { key := none, value }
      | some keyV =>
        match parseObjectPreview keyV with
        | .error e => .error e
        | .ok key => .ok { key := some key, value }
termination_by sizeOf v
decreasing_by
  all_goals first
    | exact Value.getField_sizeOf_lt h2
    | exact Value.lookup_sizeOf_lt h1

end

instance : FromJSON ObjectPreview where parseJSON := parseObjectPreview
instance : FromJSON PropertyPreview where parseJSON := parsePropertyPreview
instance : FromJSON EntryPreview where parseJSON := parseEntryPreview

private theorem ObjectPreview.entries_sizeOf_lt {p : ObjectPreview} {entries : List EntryPreview}
    (h : p.entries = some entries) : sizeOf entries < sizeOf p := by
  cases p with
  | mk type subtype description overflow properties entries' =>
    have h' : entries' = some entries := h
    subst h'
    simp only [ObjectPreview.mk.sizeOf_spec, Option.some.sizeOf_spec]
    omega

private theorem PropertyPreview.valuePreview_sizeOf_lt {p : PropertyPreview} {vp : ObjectPreview}
    (h : p.valuePreview = some vp) : sizeOf vp < sizeOf p := by
  cases p with
  | mk name type value valuePreview subtype =>
    have h' : valuePreview = some vp := h
    subst h'
    simp only [PropertyPreview.mk.sizeOf_spec, Option.some.sizeOf_spec]
    omega

private theorem EntryPreview.key_sizeOf_lt {e : EntryPreview} {k : ObjectPreview}
    (h : e.key = some k) : sizeOf k < sizeOf e := by
  cases e with
  | mk key value =>
    have h' : key = some k := h
    subst h'
    simp only [EntryPreview.mk.sizeOf_spec, Option.some.sizeOf_spec]
    omega

mutual

def encodeObjectPreview (p : ObjectPreview) : Value :=
  match h : p.entries with
  | none =>
    Data.Json.object <|
      [ ("type", ToJSON.toJSON p.type), ("overflow", ToJSON.toJSON p.overflow)
      , ("properties", encodePropertyPreviewList p.properties) ]
      ++ (p.subtype.map fun v => ("subtype", ToJSON.toJSON v)).toList
      ++ (p.description.map fun v => ("description", ToJSON.toJSON v)).toList
  | some entries =>
    Data.Json.object <|
      [ ("type", ToJSON.toJSON p.type), ("overflow", ToJSON.toJSON p.overflow)
      , ("properties", encodePropertyPreviewList p.properties)
      , ("entries", encodeEntryPreviewList entries) ]
      ++ (p.subtype.map fun v => ("subtype", ToJSON.toJSON v)).toList
      ++ (p.description.map fun v => ("description", ToJSON.toJSON v)).toList
termination_by sizeOf p
decreasing_by
  all_goals first
    | (cases p with
        | mk type subtype description overflow properties entries =>
          simp only [ObjectPreview.mk.sizeOf_spec]; omega)
    | exact ObjectPreview.entries_sizeOf_lt h

private def encodePropertyPreviewList (l : List PropertyPreview) : Value :=
  Value.array (l.map encodePropertyPreview).toArray
termination_by sizeOf l
decreasing_by
  rename_i hmem
  have := List.sizeOf_lt_of_mem hmem
  omega

def encodePropertyPreview (p : PropertyPreview) : Value :=
  match h : p.valuePreview with
  | none =>
    Data.Json.object <|
      [("name", ToJSON.toJSON p.name), ("type", ToJSON.toJSON p.type)]
      ++ (p.value.map fun v => ("value", ToJSON.toJSON v)).toList
      ++ (p.subtype.map fun v => ("subtype", ToJSON.toJSON v)).toList
  | some vp =>
    Data.Json.object <|
      [("name", ToJSON.toJSON p.name), ("type", ToJSON.toJSON p.type)]
      ++ (p.value.map fun v => ("value", ToJSON.toJSON v)).toList
      ++ [("valuePreview", encodeObjectPreview vp)]
      ++ (p.subtype.map fun v => ("subtype", ToJSON.toJSON v)).toList
termination_by sizeOf p
decreasing_by exact PropertyPreview.valuePreview_sizeOf_lt h

private def encodeEntryPreviewList (l : List EntryPreview) : Value :=
  Value.array (l.map encodeEntryPreview).toArray
termination_by sizeOf l
decreasing_by
  rename_i hmem
  have := List.sizeOf_lt_of_mem hmem
  omega

def encodeEntryPreview (e : EntryPreview) : Value :=
  match h : e.key with
  | none => Data.Json.object [("value", encodeObjectPreview e.value)]
  | some k =>
    Data.Json.object [("key", encodeObjectPreview k), ("value", encodeObjectPreview e.value)]
termination_by sizeOf e
decreasing_by
  all_goals first
    | (cases e with
        | mk key value => simp only [EntryPreview.mk.sizeOf_spec]; omega)
    | exact EntryPreview.key_sizeOf_lt h

end

instance : ToJSON ObjectPreview where toJSON := encodeObjectPreview
instance : ToJSON PropertyPreview where toJSON := encodePropertyPreview
instance : ToJSON EntryPreview where toJSON := encodeEntryPreview

/-- The client-side custom formatter's rendering of a `RemoteObject`. -/
structure CustomPreview where
  /-- The JSON-stringified result of `formatter.header(object, config)`. It
      contains a JsonML array that represents the `RemoteObject`. -/
  header : String
  /-- If the formatter returns `true` from `formatter.hasBody`, this is the
      `RemoteObjectId` for the function that returns the result of
      `formatter.body(object, config)` (also a JsonML array). -/
  bodyGetterId : Option RemoteObjectId := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON CustomPreview where
  parseJSON v := do
    .ok
      { header := ← Value.getField v "header" >>= FromJSON.parseJSON
        bodyGetterId := ← (← Value.getFieldOpt v "bodyGetterId").mapM FromJSON.parseJSON }

instance : ToJSON CustomPreview where
  toJSON p := Data.Json.object <|
    [("header", ToJSON.toJSON p.header)]
    ++ (p.bodyGetterId.map fun v => ("bodyGetterId", ToJSON.toJSON v)).toList

/-- Mirror object referencing an original JavaScript object. -/
structure RemoteObject where
  type : ObjType
  /-- Specified for `object` type values only. -/
  subtype : Option Subtype := none
  /-- Object class (constructor) name. Specified for `object` type values only. -/
  className : Option String := none
  /-- The value in case of primitive values or JSON values (if requested). -/
  value : Option Value := none
  /-- Set instead of `value` when the value can't be JSON-stringified. -/
  unserializableValue : Option UnserializableValue := none
  /-- String representation of the object. -/
  description : Option String := none
  webDriverValue : Option WebDriverValue := none
  /-- Unique object identifier (for non-primitive values). -/
  objectId : Option RemoteObjectId := none
  /-- Specified for `object` type values only. -/
  preview : Option ObjectPreview := none
  customPreview : Option CustomPreview := none
  deriving Repr, BEq

instance : FromJSON RemoteObject where
  parseJSON v := do
    .ok
      { type := ← Value.getField v "type" >>= FromJSON.parseJSON
        subtype := ← (← Value.getFieldOpt v "subtype").mapM FromJSON.parseJSON
        className := ← (← Value.getFieldOpt v "className").mapM FromJSON.parseJSON
        value := ← (← Value.getFieldOpt v "value").mapM FromJSON.parseJSON
        unserializableValue := ← (← Value.getFieldOpt v "unserializableValue").mapM FromJSON.parseJSON
        description := ← (← Value.getFieldOpt v "description").mapM FromJSON.parseJSON
        webDriverValue := ← (← Value.getFieldOpt v "webDriverValue").mapM FromJSON.parseJSON
        objectId := ← (← Value.getFieldOpt v "objectId").mapM FromJSON.parseJSON
        preview := ← (← Value.getFieldOpt v "preview").mapM FromJSON.parseJSON
        customPreview := ← (← Value.getFieldOpt v "customPreview").mapM FromJSON.parseJSON }

instance : ToJSON RemoteObject where
  toJSON p := Data.Json.object <|
    [("type", ToJSON.toJSON p.type)]
    ++ (p.subtype.map fun v => ("subtype", ToJSON.toJSON v)).toList
    ++ (p.className.map fun v => ("className", ToJSON.toJSON v)).toList
    ++ (p.value.map fun v => ("value", ToJSON.toJSON v)).toList
    ++ (p.unserializableValue.map fun v => ("unserializableValue", ToJSON.toJSON v)).toList
    ++ (p.description.map fun v => ("description", ToJSON.toJSON v)).toList
    ++ (p.webDriverValue.map fun v => ("webDriverValue", ToJSON.toJSON v)).toList
    ++ (p.objectId.map fun v => ("objectId", ToJSON.toJSON v)).toList
    ++ (p.preview.map fun v => ("preview", ToJSON.toJSON v)).toList
    ++ (p.customPreview.map fun v => ("customPreview", ToJSON.toJSON v)).toList

/-- An object property descriptor. -/
structure PropertyDescriptor where
  /-- Property name or symbol description. -/
  name : String
  value : Option RemoteObject := none
  /-- Whether the value may be changed (data descriptors only). -/
  writable : Option Bool := none
  /-- Getter function, or absent if there is no getter (accessor descriptors
      only). -/
  get : Option RemoteObject := none
  /-- Setter function, or absent if there is no setter (accessor descriptors
      only). -/
  set : Option RemoteObject := none
  /-- Whether the descriptor's type may be changed and the property may be
      deleted from the corresponding object. -/
  configurable : Bool
  /-- Whether this property shows up during enumeration of the properties on
      the corresponding object. -/
  enumerable : Bool
  /-- Whether the result was thrown during the evaluation. -/
  wasThrown : Option Bool := none
  /-- Whether the property is owned by the object. -/
  isOwn : Option Bool := none
  /-- Property symbol object, if the property is of the `symbol` type. -/
  symbol : Option RemoteObject := none
  deriving Repr, BEq

instance : FromJSON PropertyDescriptor where
  parseJSON v := do
    .ok
      { name := ← Value.getField v "name" >>= FromJSON.parseJSON
        value := ← (← Value.getFieldOpt v "value").mapM FromJSON.parseJSON
        writable := ← (← Value.getFieldOpt v "writable").mapM FromJSON.parseJSON
        get := ← (← Value.getFieldOpt v "get").mapM FromJSON.parseJSON
        set := ← (← Value.getFieldOpt v "set").mapM FromJSON.parseJSON
        configurable := ← Value.getField v "configurable" >>= FromJSON.parseJSON
        enumerable := ← Value.getField v "enumerable" >>= FromJSON.parseJSON
        wasThrown := ← (← Value.getFieldOpt v "wasThrown").mapM FromJSON.parseJSON
        isOwn := ← (← Value.getFieldOpt v "isOwn").mapM FromJSON.parseJSON
        symbol := ← (← Value.getFieldOpt v "symbol").mapM FromJSON.parseJSON }

instance : ToJSON PropertyDescriptor where
  toJSON p := Data.Json.object <|
    [("name", ToJSON.toJSON p.name)]
    ++ (p.value.map fun v => ("value", ToJSON.toJSON v)).toList
    ++ (p.writable.map fun v => ("writable", ToJSON.toJSON v)).toList
    ++ (p.get.map fun v => ("get", ToJSON.toJSON v)).toList
    ++ (p.set.map fun v => ("set", ToJSON.toJSON v)).toList
    ++ [("configurable", ToJSON.toJSON p.configurable), ("enumerable", ToJSON.toJSON p.enumerable)]
    ++ (p.wasThrown.map fun v => ("wasThrown", ToJSON.toJSON v)).toList
    ++ (p.isOwn.map fun v => ("isOwn", ToJSON.toJSON v)).toList
    ++ (p.symbol.map fun v => ("symbol", ToJSON.toJSON v)).toList

/-- An object internal property descriptor. Not normally visible in
    JavaScript code. -/
structure InternalPropertyDescriptor where
  /-- Conventional property name. -/
  name : String
  value : Option RemoteObject := none
  deriving Repr, BEq

instance : FromJSON InternalPropertyDescriptor where
  parseJSON v := do
    .ok
      { name := ← Value.getField v "name" >>= FromJSON.parseJSON
        value := ← (← Value.getFieldOpt v "value").mapM FromJSON.parseJSON }

instance : ToJSON InternalPropertyDescriptor where
  toJSON p := Data.Json.object <|
    [("name", ToJSON.toJSON p.name)] ++ (p.value.map fun v => ("value", ToJSON.toJSON v)).toList

/-- An object private field descriptor. -/
structure PrivatePropertyDescriptor where
  name : String
  value : Option RemoteObject := none
  /-- Getter, or absent if there is no getter (accessor descriptors only). -/
  get : Option RemoteObject := none
  /-- Setter, or absent if there is no setter (accessor descriptors only). -/
  set : Option RemoteObject := none
  deriving Repr, BEq

instance : FromJSON PrivatePropertyDescriptor where
  parseJSON v := do
    .ok
      { name := ← Value.getField v "name" >>= FromJSON.parseJSON
        value := ← (← Value.getFieldOpt v "value").mapM FromJSON.parseJSON
        get := ← (← Value.getFieldOpt v "get").mapM FromJSON.parseJSON
        set := ← (← Value.getFieldOpt v "set").mapM FromJSON.parseJSON }

instance : ToJSON PrivatePropertyDescriptor where
  toJSON p := Data.Json.object <|
    [("name", ToJSON.toJSON p.name)]
    ++ (p.value.map fun v => ("value", ToJSON.toJSON v)).toList
    ++ (p.get.map fun v => ("get", ToJSON.toJSON v)).toList
    ++ (p.set.map fun v => ("set", ToJSON.toJSON v)).toList

/-- A function call argument: either a remote object id (`objectId`), a
    primitive `value`, an unserializable primitive value, or none of these
    (for `undefined`). -/
structure CallArgument where
  /-- A primitive value or serializable JavaScript object. -/
  value : Option Value := none
  unserializableValue : Option UnserializableValue := none
  objectId : Option RemoteObjectId := none
  deriving Repr, BEq

instance : FromJSON CallArgument where
  parseJSON v := do
    .ok
      { value := ← (← Value.getFieldOpt v "value").mapM FromJSON.parseJSON
        unserializableValue := ← (← Value.getFieldOpt v "unserializableValue").mapM FromJSON.parseJSON
        objectId := ← (← Value.getFieldOpt v "objectId").mapM FromJSON.parseJSON }

instance : ToJSON CallArgument where
  toJSON p := Data.Json.object <|
    (p.value.map fun v => ("value", ToJSON.toJSON v)).toList
    ++ (p.unserializableValue.map fun v => ("unserializableValue", ToJSON.toJSON v)).toList
    ++ (p.objectId.map fun v => ("objectId", ToJSON.toJSON v)).toList

/-- Id of an execution context. -/
abbrev ExecutionContextId := Int

/-- Description of an isolated world. -/
structure ExecutionContextDescription where
  /-- Unique id of the execution context. -/
  id : ExecutionContextId
  origin : String
  /-- Human readable name describing the context. -/
  name : String
  /-- A system-unique execution context identifier: unlike `id`, unique across
      multiple processes, so it can reliably identify a specific context
      across a cross-process navigation. -/
  uniqueId : String
  /-- Embedder-specific auxiliary data. -/
  auxData : Option (List (String × String)) := none
  deriving Repr, BEq

instance : FromJSON ExecutionContextDescription where
  parseJSON v := do
    .ok
      { id := ← Value.getField v "id" >>= FromJSON.parseJSON
        origin := ← Value.getField v "origin" >>= FromJSON.parseJSON
        name := ← Value.getField v "name" >>= FromJSON.parseJSON
        uniqueId := ← Value.getField v "uniqueId" >>= FromJSON.parseJSON
        auxData := ← (← Value.getFieldOpt v "auxData").mapM FromJSON.parseJSON }

instance : ToJSON ExecutionContextDescription where
  toJSON p := Data.Json.object <|
    [ ("id", ToJSON.toJSON p.id), ("origin", ToJSON.toJSON p.origin), ("name", ToJSON.toJSON p.name)
    , ("uniqueId", ToJSON.toJSON p.uniqueId) ]
    ++ (p.auxData.map fun v => ("auxData", ToJSON.toJSON v)).toList

/-- Stack entry for runtime errors and assertions. -/
structure CallFrame where
  functionName : String
  scriptId : ScriptId
  /-- Script name or URL. -/
  url : String
  /-- 0-based. -/
  lineNumber : Int
  /-- 0-based. -/
  columnNumber : Int
  deriving Repr, BEq, DecidableEq

instance : FromJSON CallFrame where
  parseJSON v := do
    .ok
      { functionName := ← Value.getField v "functionName" >>= FromJSON.parseJSON
        scriptId := ← Value.getField v "scriptId" >>= FromJSON.parseJSON
        url := ← Value.getField v "url" >>= FromJSON.parseJSON
        lineNumber := ← Value.getField v "lineNumber" >>= FromJSON.parseJSON
        columnNumber := ← Value.getField v "columnNumber" >>= FromJSON.parseJSON }

instance : ToJSON CallFrame where
  toJSON p := Data.Json.object
    [ ("functionName", ToJSON.toJSON p.functionName), ("scriptId", ToJSON.toJSON p.scriptId)
    , ("url", ToJSON.toJSON p.url), ("lineNumber", ToJSON.toJSON p.lineNumber)
    , ("columnNumber", ToJSON.toJSON p.columnNumber) ]

abbrev UniqueDebuggerId := String

/-- If `debuggerId` is set, this stack trace comes from another debugger and
    can be resolved there — allowing cross-debugger calls to be tracked. -/
structure StackTraceId where
  id : String
  debuggerId : Option UniqueDebuggerId := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON StackTraceId where
  parseJSON v := do
    .ok
      { id := ← Value.getField v "id" >>= FromJSON.parseJSON
        debuggerId := ← (← Value.getFieldOpt v "debuggerId").mapM FromJSON.parseJSON }

instance : ToJSON StackTraceId where
  toJSON p := Data.Json.object <|
    [("id", ToJSON.toJSON p.id)] ++ (p.debuggerId.map fun v => ("debuggerId", ToJSON.toJSON v)).toList

/-- Call frames for assertions or error messages. Self-referential via
    `parent` — see the module docstring. -/
structure StackTrace where
  /-- For async traces, may be the name of the function that initiated the
      async call. -/
  description : Option String := none
  callFrames : List CallFrame
  /-- The asynchronous JavaScript stack trace that preceded this one, if
      available. -/
  parent : Option StackTrace := none
  /-- Ditto, but referenced by id (see `StackTraceId`'s own docstring). -/
  parentId : Option StackTraceId := none
  deriving Repr, BEq

private def finishStackTrace (v : Value) (parent : Option StackTrace) : Except String StackTrace := do
  .ok
    { description := ← (← Value.getFieldOpt v "description").mapM FromJSON.parseJSON
      callFrames := ← Value.getField v "callFrames" >>= FromJSON.parseJSON
      parent
      parentId := ← (← Value.getFieldOpt v "parentId").mapM FromJSON.parseJSON }

def parseStackTrace (v : Value) : Except String StackTrace :=
  match h : v.lookup "parent" with
  | none => finishStackTrace v none
  | some .null => finishStackTrace v none
  | some parentV =>
    match parseStackTrace parentV with
    | .error e => .error e
    | .ok parent => finishStackTrace v (some parent)
termination_by sizeOf v
decreasing_by exact Value.lookup_sizeOf_lt h

instance : FromJSON StackTrace where parseJSON := parseStackTrace

private theorem StackTrace.parent_sizeOf_lt {p : StackTrace} {parent : StackTrace}
    (h : p.parent = some parent) : sizeOf parent < sizeOf p := by
  cases p with
  | mk description callFrames parent' parentId =>
    have h' : parent' = some parent := h
    subst h'
    simp only [StackTrace.mk.sizeOf_spec, Option.some.sizeOf_spec]
    omega

def encodeStackTrace (p : StackTrace) : Value :=
  match h : p.parent with
  | none =>
    Data.Json.object <|
      (p.description.map fun v => ("description", ToJSON.toJSON v)).toList
      ++ [("callFrames", ToJSON.toJSON p.callFrames)]
      ++ (p.parentId.map fun v => ("parentId", ToJSON.toJSON v)).toList
  | some parent =>
    Data.Json.object <|
      (p.description.map fun v => ("description", ToJSON.toJSON v)).toList
      ++ [("callFrames", ToJSON.toJSON p.callFrames), ("parent", encodeStackTrace parent)]
      ++ (p.parentId.map fun v => ("parentId", ToJSON.toJSON v)).toList
termination_by sizeOf p
decreasing_by exact StackTrace.parent_sizeOf_lt h

instance : ToJSON StackTrace where toJSON := encodeStackTrace

/-- Detailed information about an exception (or error) thrown during script
    compilation or execution. -/
structure ExceptionDetails where
  exceptionId : Int
  /-- Should be used together with the exception object when available. -/
  text : String
  /-- 0-based. -/
  lineNumber : Int
  /-- 0-based. -/
  columnNumber : Int
  scriptId : Option ScriptId := none
  /-- To be used when the script was not reported. -/
  url : Option String := none
  stackTrace : Option StackTrace := none
  exception : Option RemoteObject := none
  /-- Identifier of the context where the exception happened. -/
  executionContextId : Option ExecutionContextId := none
  /-- Metadata the client associated with this exception, such as information
      about associated network requests, etc. -/
  exceptionMetaData : Option (List (String × String)) := none
  deriving Repr, BEq

instance : FromJSON ExceptionDetails where
  parseJSON v := do
    .ok
      { exceptionId := ← Value.getField v "exceptionId" >>= FromJSON.parseJSON
        text := ← Value.getField v "text" >>= FromJSON.parseJSON
        lineNumber := ← Value.getField v "lineNumber" >>= FromJSON.parseJSON
        columnNumber := ← Value.getField v "columnNumber" >>= FromJSON.parseJSON
        scriptId := ← (← Value.getFieldOpt v "scriptId").mapM FromJSON.parseJSON
        url := ← (← Value.getFieldOpt v "url").mapM FromJSON.parseJSON
        stackTrace := ← (← Value.getFieldOpt v "stackTrace").mapM FromJSON.parseJSON
        exception := ← (← Value.getFieldOpt v "exception").mapM FromJSON.parseJSON
        executionContextId := ← (← Value.getFieldOpt v "executionContextId").mapM FromJSON.parseJSON
        exceptionMetaData := ← (← Value.getFieldOpt v "exceptionMetaData").mapM FromJSON.parseJSON }

instance : ToJSON ExceptionDetails where
  toJSON p := Data.Json.object <|
    [ ("exceptionId", ToJSON.toJSON p.exceptionId), ("text", ToJSON.toJSON p.text)
    , ("lineNumber", ToJSON.toJSON p.lineNumber), ("columnNumber", ToJSON.toJSON p.columnNumber) ]
    ++ (p.scriptId.map fun v => ("scriptId", ToJSON.toJSON v)).toList
    ++ (p.url.map fun v => ("url", ToJSON.toJSON v)).toList
    ++ (p.stackTrace.map fun v => ("stackTrace", ToJSON.toJSON v)).toList
    ++ (p.exception.map fun v => ("exception", ToJSON.toJSON v)).toList
    ++ (p.executionContextId.map fun v => ("executionContextId", ToJSON.toJSON v)).toList
    ++ (p.exceptionMetaData.map fun v => ("exceptionMetaData", ToJSON.toJSON v)).toList

/-- Number of milliseconds since epoch. -/
abbrev Timestamp := Float

/-- Number of milliseconds. -/
abbrev TimeDelta := Float

/-- The `Runtime.bindingCalled` event. -/
structure BindingCalled where
  name : String
  payload : String
  /-- Identifier of the context where the call was made. -/
  executionContextId : ExecutionContextId
  deriving Repr, BEq, DecidableEq

instance : FromJSON BindingCalled where
  parseJSON v := do
    .ok
      { name := ← Value.getField v "name" >>= FromJSON.parseJSON
        payload := ← Value.getField v "payload" >>= FromJSON.parseJSON
        executionContextId := ← Value.getField v "executionContextId" >>= FromJSON.parseJSON }

instance : Event BindingCalled where
  eventName := "Runtime.bindingCalled"

/-- The kind of console API call (`Runtime.consoleAPICalled`). -/
inductive ConsoleAPICalledType where
  | log | debug | info | error | warning | dir | dirxml | table | trace | clear | startGroup
  | startGroupCollapsed | endGroup | assert | profile | profileEnd | count | timeEnd
  deriving Repr, BEq, DecidableEq

instance : FromJSON ConsoleAPICalledType where
  parseJSON
    | .string "log" => .ok .log
    | .string "debug" => .ok .debug
    | .string "info" => .ok .info
    | .string "error" => .ok .error
    | .string "warning" => .ok .warning
    | .string "dir" => .ok .dir
    | .string "dirxml" => .ok .dirxml
    | .string "table" => .ok .table
    | .string "trace" => .ok .trace
    | .string "clear" => .ok .clear
    | .string "startGroup" => .ok .startGroup
    | .string "startGroupCollapsed" => .ok .startGroupCollapsed
    | .string "endGroup" => .ok .endGroup
    | .string "assert" => .ok .assert
    | .string "profile" => .ok .profile
    | .string "profileEnd" => .ok .profileEnd
    | .string "count" => .ok .count
    | .string "timeEnd" => .ok .timeEnd
    | v => .error s!"failed to parse ConsoleAPICalledType: {repr v}"

instance : ToJSON ConsoleAPICalledType where
  toJSON
    | .log => .string "log" | .debug => .string "debug" | .info => .string "info"
    | .error => .string "error" | .warning => .string "warning" | .dir => .string "dir"
    | .dirxml => .string "dirxml" | .table => .string "table" | .trace => .string "trace"
    | .clear => .string "clear" | .startGroup => .string "startGroup"
    | .startGroupCollapsed => .string "startGroupCollapsed" | .endGroup => .string "endGroup"
    | .assert => .string "assert" | .profile => .string "profile" | .profileEnd => .string "profileEnd"
    | .count => .string "count" | .timeEnd => .string "timeEnd"

/-- The `Runtime.consoleAPICalled` event. -/
structure ConsoleAPICalled where
  type : ConsoleAPICalledType
  args : List RemoteObject
  /-- Identifier of the context where the call was made. -/
  executionContextId : ExecutionContextId
  timestamp : Timestamp
  /-- Automatically reported for `assert`/`error`/`trace`/`warning`; for other
      types, use `Debugger.getStackTrace` and `stackTrace.parentId`. -/
  stackTrace : Option StackTrace := none
  /-- `'anonymous#unique-logger-id'`/`'name#unique-logger-id'` for calls on a
      non-default console context. -/
  context : Option String := none
  deriving Repr, BEq

instance : FromJSON ConsoleAPICalled where
  parseJSON v := do
    .ok
      { type := ← Value.getField v "type" >>= FromJSON.parseJSON
        args := ← Value.getField v "args" >>= FromJSON.parseJSON
        executionContextId := ← Value.getField v "executionContextId" >>= FromJSON.parseJSON
        timestamp := ← Value.getField v "timestamp" >>= FromJSON.parseJSON
        stackTrace := ← (← Value.getFieldOpt v "stackTrace").mapM FromJSON.parseJSON
        context := ← (← Value.getFieldOpt v "context").mapM FromJSON.parseJSON }

instance : Event ConsoleAPICalled where
  eventName := "Runtime.consoleAPICalled"

/-- The `Runtime.exceptionRevoked` event. -/
structure ExceptionRevoked where
  /-- Why the exception was revoked. -/
  reason : String
  /-- The id of the revoked exception, as reported in `exceptionThrown`. -/
  exceptionId : Int
  deriving Repr, BEq, DecidableEq

instance : FromJSON ExceptionRevoked where
  parseJSON v := do
    .ok
      { reason := ← Value.getField v "reason" >>= FromJSON.parseJSON
        exceptionId := ← Value.getField v "exceptionId" >>= FromJSON.parseJSON }

instance : Event ExceptionRevoked where
  eventName := "Runtime.exceptionRevoked"

/-- The `Runtime.exceptionThrown` event. -/
structure ExceptionThrown where
  timestamp : Timestamp
  exceptionDetails : ExceptionDetails
  deriving Repr, BEq

instance : FromJSON ExceptionThrown where
  parseJSON v := do
    .ok
      { timestamp := ← Value.getField v "timestamp" >>= FromJSON.parseJSON
        exceptionDetails := ← Value.getField v "exceptionDetails" >>= FromJSON.parseJSON }

instance : Event ExceptionThrown where
  eventName := "Runtime.exceptionThrown"

/-- The `Runtime.executionContextCreated` event. -/
structure ExecutionContextCreated where
  context : ExecutionContextDescription
  deriving Repr, BEq

instance : FromJSON ExecutionContextCreated where
  parseJSON v := do .ok { context := ← Value.getField v "context" >>= FromJSON.parseJSON }

instance : Event ExecutionContextCreated where
  eventName := "Runtime.executionContextCreated"

/-- The `Runtime.executionContextDestroyed` event. -/
structure ExecutionContextDestroyed where
  executionContextId : ExecutionContextId
  deriving Repr, BEq, DecidableEq

instance : FromJSON ExecutionContextDestroyed where
  parseJSON v := do
    .ok { executionContextId := ← Value.getField v "executionContextId" >>= FromJSON.parseJSON }

instance : Event ExecutionContextDestroyed where
  eventName := "Runtime.executionContextDestroyed"

/-- The `Runtime.executionContextsCleared` event. -/
structure ExecutionContextsCleared where
  deriving Repr, BEq, DecidableEq

instance : FromJSON ExecutionContextsCleared where parseJSON _ := .ok {}

instance : Event ExecutionContextsCleared where
  eventName := "Runtime.executionContextsCleared"

/-- The `Runtime.inspectRequested` event. -/
structure InspectRequested where
  object : RemoteObject
  hints : List (String × String)
  /-- Identifier of the context where the call was made. -/
  executionContextId : Option ExecutionContextId := none
  deriving Repr, BEq

instance : FromJSON InspectRequested where
  parseJSON v := do
    .ok
      { object := ← Value.getField v "object" >>= FromJSON.parseJSON
        hints := ← Value.getField v "hints" >>= FromJSON.parseJSON
        executionContextId := ← (← Value.getFieldOpt v "executionContextId").mapM FromJSON.parseJSON }

instance : Event InspectRequested where
  eventName := "Runtime.inspectRequested"

/-- Parameters of the `Runtime.awaitPromise` command: adds a handler to a
    promise with the given promise object id. -/
structure PAwaitPromise where
  /-- Identifier of the promise. -/
  promiseObjectId : RemoteObjectId
  /-- Whether the result is expected to be a JSON object sent by value. -/
  returnByValue : Option Bool := none
  /-- Whether a preview should be generated for the result. -/
  generatePreview : Option Bool := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PAwaitPromise where
  toJSON p := Data.Json.object <|
    [("promiseObjectId", ToJSON.toJSON p.promiseObjectId)]
    ++ (p.returnByValue.map fun v => ("returnByValue", ToJSON.toJSON v)).toList
    ++ (p.generatePreview.map fun v => ("generatePreview", ToJSON.toJSON v)).toList

/-- Response of the `Runtime.awaitPromise` command. -/
structure AwaitPromise where
  /-- Contains the rejected value if the promise was rejected. -/
  result : RemoteObject
  /-- Present if a stack trace is available. -/
  exceptionDetails : Option ExceptionDetails := none
  deriving Repr, BEq

instance : FromJSON AwaitPromise where
  parseJSON v := do
    .ok
      { result := ← Value.getField v "result" >>= FromJSON.parseJSON
        exceptionDetails := ← (← Value.getFieldOpt v "exceptionDetails").mapM FromJSON.parseJSON }

instance : Command PAwaitPromise where
  Response := AwaitPromise
  commandName _ := "Runtime.awaitPromise"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Runtime.callFunctionOn` command: calls a function with
    the given declaration on the given object. The result's object group is
    inherited from the target object. -/
structure PCallFunctionOn where
  /-- Declaration of the function to call. -/
  functionDeclaration : String
  /-- Either `objectId` or `executionContextId` should be specified. -/
  objectId : Option RemoteObjectId := none
  /-- Must belong to the same JavaScript world as the target object. -/
  arguments : Option (List CallArgument) := none
  /-- Overrides `setPauseOnException` state: in silent mode, thrown exceptions
      are not reported and do not pause execution. -/
  silent : Option Bool := none
  returnByValue : Option Bool := none
  generatePreview : Option Bool := none
  /-- Whether execution should be treated as user-initiated. -/
  userGesture : Option Bool := none
  /-- Whether to `await` the resulting value if it's a promise. -/
  awaitPromise : Option Bool := none
  /-- Either `executionContextId` or `objectId` should be specified. -/
  executionContextId : Option ExecutionContextId := none
  /-- If unset and `objectId` is set, inherited from the object. -/
  objectGroup : Option String := none
  /-- Whether to throw an exception if a side effect can't be ruled out. -/
  throwOnSideEffect : Option Bool := none
  /-- Mutually exclusive with `returnByValue`; `objectId` is still provided. -/
  generateWebDriverValue : Option Bool := none
  deriving Repr, BEq

instance : ToJSON PCallFunctionOn where
  toJSON p := Data.Json.object <|
    [("functionDeclaration", ToJSON.toJSON p.functionDeclaration)]
    ++ (p.objectId.map fun v => ("objectId", ToJSON.toJSON v)).toList
    ++ (p.arguments.map fun v => ("arguments", ToJSON.toJSON v)).toList
    ++ (p.silent.map fun v => ("silent", ToJSON.toJSON v)).toList
    ++ (p.returnByValue.map fun v => ("returnByValue", ToJSON.toJSON v)).toList
    ++ (p.generatePreview.map fun v => ("generatePreview", ToJSON.toJSON v)).toList
    ++ (p.userGesture.map fun v => ("userGesture", ToJSON.toJSON v)).toList
    ++ (p.awaitPromise.map fun v => ("awaitPromise", ToJSON.toJSON v)).toList
    ++ (p.executionContextId.map fun v => ("executionContextId", ToJSON.toJSON v)).toList
    ++ (p.objectGroup.map fun v => ("objectGroup", ToJSON.toJSON v)).toList
    ++ (p.throwOnSideEffect.map fun v => ("throwOnSideEffect", ToJSON.toJSON v)).toList
    ++ (p.generateWebDriverValue.map fun v => ("generateWebDriverValue", ToJSON.toJSON v)).toList

/-- Response of the `Runtime.callFunctionOn` command. -/
structure CallFunctionOn where
  result : RemoteObject
  exceptionDetails : Option ExceptionDetails := none
  deriving Repr, BEq

instance : FromJSON CallFunctionOn where
  parseJSON v := do
    .ok
      { result := ← Value.getField v "result" >>= FromJSON.parseJSON
        exceptionDetails := ← (← Value.getFieldOpt v "exceptionDetails").mapM FromJSON.parseJSON }

instance : Command PCallFunctionOn where
  Response := CallFunctionOn
  commandName _ := "Runtime.callFunctionOn"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Runtime.compileScript` command: compiles an
    expression. -/
structure PCompileScript where
  /-- Expression to compile. -/
  expression : String
  /-- Source URL to set for the script. -/
  sourceURL : String
  /-- Whether the compiled script should be persisted. -/
  persistScript : Bool
  /-- If omitted, evaluation is performed in the context of the inspected
      page. -/
  executionContextId : Option ExecutionContextId := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PCompileScript where
  toJSON p := Data.Json.object <|
    [ ("expression", ToJSON.toJSON p.expression), ("sourceURL", ToJSON.toJSON p.sourceURL)
    , ("persistScript", ToJSON.toJSON p.persistScript) ]
    ++ (p.executionContextId.map fun v => ("executionContextId", ToJSON.toJSON v)).toList

/-- Response of the `Runtime.compileScript` command. -/
structure CompileScript where
  scriptId : Option ScriptId := none
  exceptionDetails : Option ExceptionDetails := none
  deriving Repr, BEq

instance : FromJSON CompileScript where
  parseJSON v := do
    .ok
      { scriptId := ← (← Value.getFieldOpt v "scriptId").mapM FromJSON.parseJSON
        exceptionDetails := ← (← Value.getFieldOpt v "exceptionDetails").mapM FromJSON.parseJSON }

instance : Command PCompileScript where
  Response := CompileScript
  commandName _ := "Runtime.compileScript"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Runtime.disable` command: disables reporting of
    execution context creation. -/
structure PDisable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PDisable where toJSON _ := .null

instance : Command PDisable where
  Response := Unit
  commandName _ := "Runtime.disable"
  decodeResponse _ := .ok ()

/-- Parameters of the `Runtime.discardConsoleEntries` command: discards
    collected exceptions and console API calls. -/
structure PDiscardConsoleEntries where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PDiscardConsoleEntries where toJSON _ := .null

instance : Command PDiscardConsoleEntries where
  Response := Unit
  commandName _ := "Runtime.discardConsoleEntries"
  decodeResponse _ := .ok ()

/-- Parameters of the `Runtime.enable` command: enables reporting of execution
    context creation via `executionContextCreated`. When enabled, the event
    fires immediately for each existing execution context. -/
structure PEnable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PEnable where toJSON _ := .null

instance : Command PEnable where
  Response := Unit
  commandName _ := "Runtime.enable"
  decodeResponse _ := .ok ()

/-- Parameters of the `Runtime.evaluate` command: evaluates an expression on
    the global object. -/
structure PEvaluate where
  /-- Expression to evaluate. -/
  expression : String
  objectGroup : Option String := none
  /-- Whether the Command Line API should be available during evaluation. -/
  includeCommandLineAPI : Option Bool := none
  /-- Overrides `setPauseOnException` state. -/
  silent : Option Bool := none
  /-- Mutually exclusive with `uniqueContextId`. If omitted, evaluation is
      performed in the context of the inspected page. -/
  contextId : Option ExecutionContextId := none
  returnByValue : Option Bool := none
  generatePreview : Option Bool := none
  userGesture : Option Bool := none
  awaitPromise : Option Bool := none
  /-- Implies `disableBreaks`. -/
  throwOnSideEffect : Option Bool := none
  /-- Milliseconds before terminating execution. -/
  timeout : Option TimeDelta := none
  disableBreaks : Option Bool := none
  /-- Enables `let` re-declaration and top-level `await`; re-declaration only
      works if the `let` originated from `replMode` itself. -/
  replMode : Option Bool := none
  /-- Bypasses CSP to allow `unsafe-eval` (`eval()`/`Function()`/
      `setTimeout()`/`setInterval()` with non-callable arguments). Defaults to
      `true`. -/
  allowUnsafeEvalBlockedByCSP : Option Bool := none
  /-- Mutually exclusive with `contextId`: guaranteed system-unique (unlike
      `contextId`, which may be reused across processes), preventing
      accidental evaluation in the wrong context after a cross-process
      navigation. -/
  uniqueContextId : Option String := none
  /-- Whether the result should be serialized per WebDriver BiDi. -/
  generateWebDriverValue : Option Bool := none
  deriving Repr, BEq

instance : ToJSON PEvaluate where
  toJSON p := Data.Json.object <|
    [("expression", ToJSON.toJSON p.expression)]
    ++ (p.objectGroup.map fun v => ("objectGroup", ToJSON.toJSON v)).toList
    ++ (p.includeCommandLineAPI.map fun v => ("includeCommandLineAPI", ToJSON.toJSON v)).toList
    ++ (p.silent.map fun v => ("silent", ToJSON.toJSON v)).toList
    ++ (p.contextId.map fun v => ("contextId", ToJSON.toJSON v)).toList
    ++ (p.returnByValue.map fun v => ("returnByValue", ToJSON.toJSON v)).toList
    ++ (p.generatePreview.map fun v => ("generatePreview", ToJSON.toJSON v)).toList
    ++ (p.userGesture.map fun v => ("userGesture", ToJSON.toJSON v)).toList
    ++ (p.awaitPromise.map fun v => ("awaitPromise", ToJSON.toJSON v)).toList
    ++ (p.throwOnSideEffect.map fun v => ("throwOnSideEffect", ToJSON.toJSON v)).toList
    ++ (p.timeout.map fun v => ("timeout", ToJSON.toJSON v)).toList
    ++ (p.disableBreaks.map fun v => ("disableBreaks", ToJSON.toJSON v)).toList
    ++ (p.replMode.map fun v => ("replMode", ToJSON.toJSON v)).toList
    ++ (p.allowUnsafeEvalBlockedByCSP.map fun v => ("allowUnsafeEvalBlockedByCSP", ToJSON.toJSON v)).toList
    ++ (p.uniqueContextId.map fun v => ("uniqueContextId", ToJSON.toJSON v)).toList
    ++ (p.generateWebDriverValue.map fun v => ("generateWebDriverValue", ToJSON.toJSON v)).toList

/-- Response of the `Runtime.evaluate` command. -/
structure Evaluate where
  result : RemoteObject
  exceptionDetails : Option ExceptionDetails := none
  deriving Repr, BEq

instance : FromJSON Evaluate where
  parseJSON v := do
    .ok
      { result := ← Value.getField v "result" >>= FromJSON.parseJSON
        exceptionDetails := ← (← Value.getFieldOpt v "exceptionDetails").mapM FromJSON.parseJSON }

instance : Command PEvaluate where
  Response := Evaluate
  commandName _ := "Runtime.evaluate"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Runtime.getIsolateId` command: returns the isolate id. -/
structure PGetIsolateId where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetIsolateId where toJSON _ := .null

/-- Response of the `Runtime.getIsolateId` command. -/
structure GetIsolateId where
  id : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON GetIsolateId where
  parseJSON v := do .ok { id := ← Value.getField v "id" >>= FromJSON.parseJSON }

instance : Command PGetIsolateId where
  Response := GetIsolateId
  commandName _ := "Runtime.getIsolateId"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Runtime.getHeapUsage` command: returns the JavaScript
    heap usage — the total usage of the corresponding isolate, not scoped to a
    particular `Runtime`. -/
structure PGetHeapUsage where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetHeapUsage where toJSON _ := .null

/-- Response of the `Runtime.getHeapUsage` command. -/
structure GetHeapUsage where
  usedSize : Float
  totalSize : Float
  deriving Repr, BEq, DecidableEq

instance : FromJSON GetHeapUsage where
  parseJSON v := do
    .ok
      { usedSize := ← Value.getField v "usedSize" >>= FromJSON.parseJSON
        totalSize := ← Value.getField v "totalSize" >>= FromJSON.parseJSON }

instance : Command PGetHeapUsage where
  Response := GetHeapUsage
  commandName _ := "Runtime.getHeapUsage"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Runtime.getProperties` command: returns properties of a
    given object. The result's object group is inherited from the target
    object. -/
structure PGetProperties where
  /-- Identifier of the object to return properties for. -/
  objectId : RemoteObjectId
  /-- If `true`, return only properties belonging to the element itself, not
      its prototype chain. -/
  ownProperties : Option Bool := none
  /-- If `true`, return only accessor properties (with getter/setter);
      internal properties are not returned either. -/
  accessorPropertiesOnly : Option Bool := none
  generatePreview : Option Bool := none
  /-- If `true`, return only non-indexed properties. -/
  nonIndexedPropertiesOnly : Option Bool := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetProperties where
  toJSON p := Data.Json.object <|
    [("objectId", ToJSON.toJSON p.objectId)]
    ++ (p.ownProperties.map fun v => ("ownProperties", ToJSON.toJSON v)).toList
    ++ (p.accessorPropertiesOnly.map fun v => ("accessorPropertiesOnly", ToJSON.toJSON v)).toList
    ++ (p.generatePreview.map fun v => ("generatePreview", ToJSON.toJSON v)).toList
    ++ (p.nonIndexedPropertiesOnly.map fun v => ("nonIndexedPropertiesOnly", ToJSON.toJSON v)).toList

/-- Response of the `Runtime.getProperties` command. -/
structure GetProperties where
  result : List PropertyDescriptor
  /-- Only of the element itself. -/
  internalProperties : Option (List InternalPropertyDescriptor) := none
  privateProperties : Option (List PrivatePropertyDescriptor) := none
  exceptionDetails : Option ExceptionDetails := none
  deriving Repr, BEq

instance : FromJSON GetProperties where
  parseJSON v := do
    .ok
      { result := ← Value.getField v "result" >>= FromJSON.parseJSON
        internalProperties := ← (← Value.getFieldOpt v "internalProperties").mapM FromJSON.parseJSON
        privateProperties := ← (← Value.getFieldOpt v "privateProperties").mapM FromJSON.parseJSON
        exceptionDetails := ← (← Value.getFieldOpt v "exceptionDetails").mapM FromJSON.parseJSON }

instance : Command PGetProperties where
  Response := GetProperties
  commandName _ := "Runtime.getProperties"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Runtime.globalLexicalScopeNames` command: returns all
    `let`, `const`, and `class` variables from global scope. -/
structure PGlobalLexicalScopeNames where
  /-- Where to look up global scope variables. -/
  executionContextId : Option ExecutionContextId := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGlobalLexicalScopeNames where
  toJSON p := Data.Json.object
    ((p.executionContextId.map fun v => ("executionContextId", ToJSON.toJSON v)).toList)

/-- Response of the `Runtime.globalLexicalScopeNames` command. -/
structure GlobalLexicalScopeNames where
  names : List String
  deriving Repr, BEq, DecidableEq

instance : FromJSON GlobalLexicalScopeNames where
  parseJSON v := do .ok { names := ← Value.getField v "names" >>= FromJSON.parseJSON }

instance : Command PGlobalLexicalScopeNames where
  Response := GlobalLexicalScopeNames
  commandName _ := "Runtime.globalLexicalScopeNames"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Runtime.queryObjects` command. -/
structure PQueryObjects where
  /-- Identifier of the prototype to return objects for. -/
  prototypeObjectId : RemoteObjectId
  /-- Symbolic group name that can be used to release the results. -/
  objectGroup : Option String := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PQueryObjects where
  toJSON p := Data.Json.object <|
    [("prototypeObjectId", ToJSON.toJSON p.prototypeObjectId)]
    ++ (p.objectGroup.map fun v => ("objectGroup", ToJSON.toJSON v)).toList

/-- Response of the `Runtime.queryObjects` command. -/
structure QueryObjects where
  /-- Array with objects. -/
  objects : RemoteObject
  deriving Repr, BEq

instance : FromJSON QueryObjects where
  parseJSON v := do .ok { objects := ← Value.getField v "objects" >>= FromJSON.parseJSON }

instance : Command PQueryObjects where
  Response := QueryObjects
  commandName _ := "Runtime.queryObjects"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Runtime.releaseObject` command: releases the remote
    object with the given id. -/
structure PReleaseObject where
  /-- Identifier of the object to release. -/
  objectId : RemoteObjectId
  deriving Repr, BEq, DecidableEq

instance : ToJSON PReleaseObject where
  toJSON p := Data.Json.object [("objectId", ToJSON.toJSON p.objectId)]

instance : Command PReleaseObject where
  Response := Unit
  commandName _ := "Runtime.releaseObject"
  decodeResponse _ := .ok ()

/-- Parameters of the `Runtime.releaseObjectGroup` command: releases all
    remote objects belonging to a given group. -/
structure PReleaseObjectGroup where
  /-- Symbolic object group name. -/
  objectGroup : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PReleaseObjectGroup where
  toJSON p := Data.Json.object [("objectGroup", ToJSON.toJSON p.objectGroup)]

instance : Command PReleaseObjectGroup where
  Response := Unit
  commandName _ := "Runtime.releaseObjectGroup"
  decodeResponse _ := .ok ()

/-- Parameters of the `Runtime.runIfWaitingForDebugger` command: tells the
    inspected instance to run if it was waiting for the debugger to attach. -/
structure PRunIfWaitingForDebugger where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PRunIfWaitingForDebugger where toJSON _ := .null

instance : Command PRunIfWaitingForDebugger where
  Response := Unit
  commandName _ := "Runtime.runIfWaitingForDebugger"
  decodeResponse _ := .ok ()

/-- Parameters of the `Runtime.runScript` command: runs a script with the
    given id in the given context. -/
structure PRunScript where
  /-- Id of the script to run. -/
  scriptId : ScriptId
  /-- If omitted, evaluation is performed in the context of the inspected
      page. -/
  executionContextId : Option ExecutionContextId := none
  objectGroup : Option String := none
  /-- Overrides `setPauseOnException` state. -/
  silent : Option Bool := none
  includeCommandLineAPI : Option Bool := none
  returnByValue : Option Bool := none
  generatePreview : Option Bool := none
  awaitPromise : Option Bool := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PRunScript where
  toJSON p := Data.Json.object <|
    [("scriptId", ToJSON.toJSON p.scriptId)]
    ++ (p.executionContextId.map fun v => ("executionContextId", ToJSON.toJSON v)).toList
    ++ (p.objectGroup.map fun v => ("objectGroup", ToJSON.toJSON v)).toList
    ++ (p.silent.map fun v => ("silent", ToJSON.toJSON v)).toList
    ++ (p.includeCommandLineAPI.map fun v => ("includeCommandLineAPI", ToJSON.toJSON v)).toList
    ++ (p.returnByValue.map fun v => ("returnByValue", ToJSON.toJSON v)).toList
    ++ (p.generatePreview.map fun v => ("generatePreview", ToJSON.toJSON v)).toList
    ++ (p.awaitPromise.map fun v => ("awaitPromise", ToJSON.toJSON v)).toList

/-- Response of the `Runtime.runScript` command. -/
structure RunScript where
  result : RemoteObject
  exceptionDetails : Option ExceptionDetails := none
  deriving Repr, BEq

instance : FromJSON RunScript where
  parseJSON v := do
    .ok
      { result := ← Value.getField v "result" >>= FromJSON.parseJSON
        exceptionDetails := ← (← Value.getFieldOpt v "exceptionDetails").mapM FromJSON.parseJSON }

instance : Command PRunScript where
  Response := RunScript
  commandName _ := "Runtime.runScript"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Runtime.setAsyncCallStackDepth` command: enables or
    disables async call stack tracking. -/
structure PSetAsyncCallStackDepth where
  /-- Maximum depth of async call stacks. `0` effectively disables collecting
      them (the default). -/
  maxDepth : Int
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetAsyncCallStackDepth where
  toJSON p := Data.Json.object [("maxDepth", ToJSON.toJSON p.maxDepth)]

instance : Command PSetAsyncCallStackDepth where
  Response := Unit
  commandName _ := "Runtime.setAsyncCallStackDepth"
  decodeResponse _ := .ok ()

/-- Parameters of the `Runtime.setCustomObjectFormatterEnabled` command. -/
structure PSetCustomObjectFormatterEnabled where
  enabled : Bool
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetCustomObjectFormatterEnabled where
  toJSON p := Data.Json.object [("enabled", ToJSON.toJSON p.enabled)]

instance : Command PSetCustomObjectFormatterEnabled where
  Response := Unit
  commandName _ := "Runtime.setCustomObjectFormatterEnabled"
  decodeResponse _ := .ok ()

/-- Parameters of the `Runtime.setMaxCallStackSizeToCapture` command. -/
structure PSetMaxCallStackSizeToCapture where
  size : Int
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetMaxCallStackSizeToCapture where
  toJSON p := Data.Json.object [("size", ToJSON.toJSON p.size)]

instance : Command PSetMaxCallStackSizeToCapture where
  Response := Unit
  commandName _ := "Runtime.setMaxCallStackSizeToCapture"
  decodeResponse _ := .ok ()

/-- Parameters of the `Runtime.terminateExecution` command: terminates the
    current or next JavaScript execution. The termination is cancelled once
    the outer-most script execution ends. -/
structure PTerminateExecution where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PTerminateExecution where toJSON _ := .null

instance : Command PTerminateExecution where
  Response := Unit
  commandName _ := "Runtime.terminateExecution"
  decodeResponse _ := .ok ()

/-- Parameters of the `Runtime.addBinding` command: if `executionContextId` is
    empty, adds a binding with the given name on the global objects of all
    inspected contexts (including those created later; bindings survive
    reloads). The binding function takes exactly one (string) argument — any
    other input throws. Each call produces a `Runtime.bindingCalled`
    notification. -/
structure PAddBinding where
  name : String
  /-- If specified, only exposed to the execution context with a matching
      name — even for contexts created after the binding is added. Mutually
      exclusive with `executionContextId`. -/
  executionContextName : Option String := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PAddBinding where
  toJSON p := Data.Json.object <|
    [("name", ToJSON.toJSON p.name)]
    ++ (p.executionContextName.map fun v => ("executionContextName", ToJSON.toJSON v)).toList

instance : Command PAddBinding where
  Response := Unit
  commandName _ := "Runtime.addBinding"
  decodeResponse _ := .ok ()

/-- Parameters of the `Runtime.removeBinding` command: doesn't remove the
    binding function from the global object, but unsubscribes the current
    runtime agent from `Runtime.bindingCalled` notifications. -/
structure PRemoveBinding where
  name : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PRemoveBinding where
  toJSON p := Data.Json.object [("name", ToJSON.toJSON p.name)]

instance : Command PRemoveBinding where
  Response := Unit
  commandName _ := "Runtime.removeBinding"
  decodeResponse _ := .ok ()

/-- Parameters of the `Runtime.getExceptionDetails` command: looks up and
    populates exception details for a JavaScript `Error` object. The
    `stackTrace` portion of the result is only populated if the `Runtime`
    domain was enabled at the time the error was thrown. -/
structure PGetExceptionDetails where
  /-- The error object to resolve exception details for. -/
  errorObjectId : RemoteObjectId
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetExceptionDetails where
  toJSON p := Data.Json.object [("errorObjectId", ToJSON.toJSON p.errorObjectId)]

/-- Response of the `Runtime.getExceptionDetails` command. -/
structure GetExceptionDetails where
  exceptionDetails : Option ExceptionDetails := none
  deriving Repr, BEq

instance : FromJSON GetExceptionDetails where
  parseJSON v := do
    .ok { exceptionDetails := ← (← Value.getFieldOpt v "exceptionDetails").mapM FromJSON.parseJSON }

instance : Command PGetExceptionDetails where
  Response := GetExceptionDetails
  commandName _ := "Runtime.getExceptionDetails"
  decodeResponse := FromJSON.parseJSON

end CDP.Domains.Runtime
