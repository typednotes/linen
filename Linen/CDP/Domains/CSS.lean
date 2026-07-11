/-
  Linen.CDP.Domains.CSS — the `CSS` CDP domain

  This domain exposes CSS read/write operations. All CSS objects (stylesheets,
  rules, and styles) have an associated id used in subsequent operations on
  the related object. Each object type has a specific id structure, and those
  are not interchangeable between objects of different kinds. CSS objects can
  be loaded using the `get*ForNode` calls (which accept a DOM node id). A
  client can also keep track of stylesheets via the `styleSheetAdded`/
  `styleSheetRemoved` events and subsequently load the required stylesheet
  contents using the `getStyleSheet[Text]` methods.

  Ports `CDP.Domains.CSS` (see `docs/imports/cdp/dependencies.md`); naming
  conventions as in `CDP.Domains.CacheStorage`'s docstring, and as in
  `CDP.Domains.Debugger`'s docstring for referencing another already-ported
  domain module. Upstream flattens every type with a `CSS`-domain prefix
  (`cSSCSSRule`, `cSSCSSStyle`, …); here that outer domain prefix is dropped as
  usual, but the *type's own* name (itself literally `CSS…`, since the CDP
  type is e.g. `CSS.CSSRule`) is kept, giving `CSSRule`, `CSSStyle`, etc. The
  one exception is the CDP type `CSS.Value` (upstream's `CSSValue`): stripping
  the outer prefix would give a bare `Value`, clashing with this module's
  opened `Data.Json.Value`, so it keeps the upstream name `CSSValue` instead.

  Two of this module's own types are self-referential: `CSSProperty` (via
  `longhandProperties`, a shorthand's own longhand components) and
  `CSSLayerData` (via `subLayers`, a cascade layer's nested sub-layers). Both
  fields are `Option (List Self)`, exactly the shape of
  `CDP.Domains.IndexedDB.Key.array`, so their `FromJSON`/`ToJSON` instances
  follow that module's `parseKey`/`encodeKey` technique verbatim (a
  `finish…`-style helper plus a `Value.lookup_sizeOf_lt`/`array_sizeOf_lt`
  termination proof), rather than `CDP.Domains.HeapProfiler`'s variant for a
  required (non-`Option`) recursive field. Every type transitively containing
  one of these two therefore derives only `Repr, BEq` (no `DecidableEq`),
  matching that module's convention.
-/
import Linen.CDP.Internal.Utils
import Linen.CDP.Domains.DOMPageNetworkEmulationSecurity

namespace CDP.Domains.CSS

open Data.Json (Value ToJSON FromJSON)
open CDP.Internal.Utils (Command Event)

-- ── Identifiers ──

/-- `CSS.StyleSheetId`. -/
abbrev StyleSheetId := String

-- ── Simple leaf types ──

/-- Stylesheet type: `injected` for stylesheets injected via extension,
    `userAgent` for user-agent stylesheets, `inspector` for stylesheets
    created by the inspector (i.e. those holding the "via inspector" rules),
    `regular` for regular stylesheets. -/
inductive StyleSheetOrigin where
  | injected | userAgent | inspector | regular
  deriving Repr, BEq, DecidableEq

instance : FromJSON StyleSheetOrigin where
  parseJSON
    | .string "injected" => .ok .injected
    | .string "user-agent" => .ok .userAgent
    | .string "inspector" => .ok .inspector
    | .string "regular" => .ok .regular
    | v => .error s!"failed to parse StyleSheetOrigin: {repr v}"

instance : ToJSON StyleSheetOrigin where
  toJSON
    | .injected => .string "injected" | .userAgent => .string "user-agent"
    | .inspector => .string "inspector" | .regular => .string "regular"

/-- Text range within a resource. All numbers are zero-based. -/
structure SourceRange where
  /-- Start line of range. -/
  startLine : Int
  /-- Start column of range (inclusive). -/
  startColumn : Int
  /-- End line of range. -/
  endLine : Int
  /-- End column of range (exclusive). -/
  endColumn : Int
  deriving Repr, BEq, DecidableEq

instance : FromJSON SourceRange where
  parseJSON v := do
    .ok
      { startLine := ← Value.getField v "startLine" >>= FromJSON.parseJSON
        startColumn := ← Value.getField v "startColumn" >>= FromJSON.parseJSON
        endLine := ← Value.getField v "endLine" >>= FromJSON.parseJSON
        endColumn := ← Value.getField v "endColumn" >>= FromJSON.parseJSON }

instance : ToJSON SourceRange where
  toJSON p := Data.Json.object
    [ ("startLine", ToJSON.toJSON p.startLine), ("startColumn", ToJSON.toJSON p.startColumn)
    , ("endLine", ToJSON.toJSON p.endLine), ("endColumn", ToJSON.toJSON p.endColumn) ]

/-- A shorthand property's computed longhand. -/
structure ShorthandEntry where
  /-- Shorthand name. -/
  name : String
  /-- Shorthand value. -/
  value : String
  /-- Whether the property has `!important` annotation (implies `false` if
      absent). -/
  important : Option Bool := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON ShorthandEntry where
  parseJSON v := do
    .ok
      { name := ← Value.getField v "name" >>= FromJSON.parseJSON
        value := ← Value.getField v "value" >>= FromJSON.parseJSON
        important := ← (← Value.getFieldOpt v "important").mapM FromJSON.parseJSON }

instance : ToJSON ShorthandEntry where
  toJSON p := Data.Json.object <|
    [("name", ToJSON.toJSON p.name), ("value", ToJSON.toJSON p.value)]
    ++ (p.important.map fun v => ("important", ToJSON.toJSON v)).toList

/-- `CSS.CSSComputedStyleProperty`. -/
structure CSSComputedStyleProperty where
  /-- Computed style property name. -/
  name : String
  /-- Computed style property value. -/
  value : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON CSSComputedStyleProperty where
  parseJSON v := do
    .ok
      { name := ← Value.getField v "name" >>= FromJSON.parseJSON
        value := ← Value.getField v "value" >>= FromJSON.parseJSON }

instance : ToJSON CSSComputedStyleProperty where
  toJSON p := Data.Json.object [("name", ToJSON.toJSON p.name), ("value", ToJSON.toJSON p.value)]

/-- Information about the amount of glyphs that were rendered with a given
    font. -/
structure PlatformFontUsage where
  /-- Font's family name reported by platform. -/
  familyName : String
  /-- Indicates if the font was downloaded or resolved locally. -/
  isCustomFont : Bool
  /-- Amount of glyphs that were rendered with this font. -/
  glyphCount : Float
  deriving Repr, BEq, DecidableEq

instance : FromJSON PlatformFontUsage where
  parseJSON v := do
    .ok
      { familyName := ← Value.getField v "familyName" >>= FromJSON.parseJSON
        isCustomFont := ← Value.getField v "isCustomFont" >>= FromJSON.parseJSON
        glyphCount := ← Value.getField v "glyphCount" >>= FromJSON.parseJSON }

instance : ToJSON PlatformFontUsage where
  toJSON p := Data.Json.object
    [ ("familyName", ToJSON.toJSON p.familyName), ("isCustomFont", ToJSON.toJSON p.isCustomFont)
    , ("glyphCount", ToJSON.toJSON p.glyphCount) ]

/-- Information about font variation axes for variable fonts. -/
structure FontVariationAxis where
  /-- The font-variation-setting tag (a.k.a. "axis tag"). -/
  tag : String
  /-- Human-readable variation name in the default language (normally
      "en"). -/
  name : String
  /-- The minimum value (inclusive) the font supports for this tag. -/
  minValue : Float
  /-- The maximum value (inclusive) the font supports for this tag. -/
  maxValue : Float
  /-- The default value. -/
  defaultValue : Float
  deriving Repr, BEq, DecidableEq

instance : FromJSON FontVariationAxis where
  parseJSON v := do
    .ok
      { tag := ← Value.getField v "tag" >>= FromJSON.parseJSON
        name := ← Value.getField v "name" >>= FromJSON.parseJSON
        minValue := ← Value.getField v "minValue" >>= FromJSON.parseJSON
        maxValue := ← Value.getField v "maxValue" >>= FromJSON.parseJSON
        defaultValue := ← Value.getField v "defaultValue" >>= FromJSON.parseJSON }

instance : ToJSON FontVariationAxis where
  toJSON p := Data.Json.object
    [ ("tag", ToJSON.toJSON p.tag), ("name", ToJSON.toJSON p.name)
    , ("minValue", ToJSON.toJSON p.minValue), ("maxValue", ToJSON.toJSON p.maxValue)
    , ("defaultValue", ToJSON.toJSON p.defaultValue) ]

/-- Properties of a web font (see the CSS2 font descriptions spec) and
    additional information such as `platformFontFamily` and
    `fontVariationAxes`. -/
structure FontFace where
  /-- The font-family. -/
  fontFamily : String
  /-- The font-style. -/
  fontStyle : String
  /-- The font-variant. -/
  fontVariant : String
  /-- The font-weight. -/
  fontWeight : String
  /-- The font-stretch. -/
  fontStretch : String
  /-- The font-display. -/
  fontDisplay : String
  /-- The unicode-range. -/
  unicodeRange : String
  /-- The src. -/
  src : String
  /-- The resolved platform font family. -/
  platformFontFamily : String
  /-- Available variation settings (a.k.a. "axes"). -/
  fontVariationAxes : Option (List FontVariationAxis) := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON FontFace where
  parseJSON v := do
    .ok
      { fontFamily := ← Value.getField v "fontFamily" >>= FromJSON.parseJSON
        fontStyle := ← Value.getField v "fontStyle" >>= FromJSON.parseJSON
        fontVariant := ← Value.getField v "fontVariant" >>= FromJSON.parseJSON
        fontWeight := ← Value.getField v "fontWeight" >>= FromJSON.parseJSON
        fontStretch := ← Value.getField v "fontStretch" >>= FromJSON.parseJSON
        fontDisplay := ← Value.getField v "fontDisplay" >>= FromJSON.parseJSON
        unicodeRange := ← Value.getField v "unicodeRange" >>= FromJSON.parseJSON
        src := ← Value.getField v "src" >>= FromJSON.parseJSON
        platformFontFamily := ← Value.getField v "platformFontFamily" >>= FromJSON.parseJSON
        fontVariationAxes := ← (← Value.getFieldOpt v "fontVariationAxes").mapM FromJSON.parseJSON }

instance : ToJSON FontFace where
  toJSON p := Data.Json.object <|
    [ ("fontFamily", ToJSON.toJSON p.fontFamily), ("fontStyle", ToJSON.toJSON p.fontStyle)
    , ("fontVariant", ToJSON.toJSON p.fontVariant), ("fontWeight", ToJSON.toJSON p.fontWeight)
    , ("fontStretch", ToJSON.toJSON p.fontStretch), ("fontDisplay", ToJSON.toJSON p.fontDisplay)
    , ("unicodeRange", ToJSON.toJSON p.unicodeRange), ("src", ToJSON.toJSON p.src)
    , ("platformFontFamily", ToJSON.toJSON p.platformFontFamily) ]
    ++ (p.fontVariationAxes.map fun v => ("fontVariationAxes", ToJSON.toJSON v)).toList

/-- Source of a `CSSMedia`'s media query: `mediaRule` if specified by an
    `@media` rule, `importRule` if specified by an `@import` rule,
    `linkedSheet` if specified by a `media` attribute in a linked
    stylesheet's `LINK` tag, `inlineSheet` if specified by a `media`
    attribute in an inline stylesheet's `STYLE` tag. -/
inductive CSSMediaSource where
  | mediaRule | importRule | linkedSheet | inlineSheet
  deriving Repr, BEq, DecidableEq

instance : FromJSON CSSMediaSource where
  parseJSON
    | .string "mediaRule" => .ok .mediaRule
    | .string "importRule" => .ok .importRule
    | .string "linkedSheet" => .ok .linkedSheet
    | .string "inlineSheet" => .ok .inlineSheet
    | v => .error s!"failed to parse CSSMediaSource: {repr v}"

instance : ToJSON CSSMediaSource where
  toJSON
    | .mediaRule => .string "mediaRule" | .importRule => .string "importRule"
    | .linkedSheet => .string "linkedSheet" | .inlineSheet => .string "inlineSheet"

/-- CSS coverage information. -/
structure RuleUsage where
  /-- The css style sheet identifier (absent for user agent stylesheet and
      user-specified stylesheet rules) this rule came from. -/
  styleSheetId : StyleSheetId
  /-- Offset of the start of the rule (including selector) from the beginning
      of the stylesheet. -/
  startOffset : Float
  /-- Offset of the end of the rule body from the beginning of the
      stylesheet. -/
  endOffset : Float
  /-- Indicates whether the rule was actually used by some element in the
      page. -/
  used : Bool
  deriving Repr, BEq, DecidableEq

instance : FromJSON RuleUsage where
  parseJSON v := do
    .ok
      { styleSheetId := ← Value.getField v "styleSheetId" >>= FromJSON.parseJSON
        startOffset := ← Value.getField v "startOffset" >>= FromJSON.parseJSON
        endOffset := ← Value.getField v "endOffset" >>= FromJSON.parseJSON
        used := ← Value.getField v "used" >>= FromJSON.parseJSON }

instance : ToJSON RuleUsage where
  toJSON p := Data.Json.object
    [ ("styleSheetId", ToJSON.toJSON p.styleSheetId), ("startOffset", ToJSON.toJSON p.startOffset)
    , ("endOffset", ToJSON.toJSON p.endOffset), ("used", ToJSON.toJSON p.used) ]

/-- A descriptor of an operation to mutate a style declaration's text. -/
structure StyleDeclarationEdit where
  /-- The css style sheet identifier. -/
  styleSheetId : StyleSheetId
  /-- The range of the style text in the enclosing stylesheet. -/
  range : SourceRange
  /-- New style text. -/
  text : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON StyleDeclarationEdit where
  parseJSON v := do
    .ok
      { styleSheetId := ← Value.getField v "styleSheetId" >>= FromJSON.parseJSON
        range := ← Value.getField v "range" >>= FromJSON.parseJSON
        text := ← Value.getField v "text" >>= FromJSON.parseJSON }

instance : ToJSON StyleDeclarationEdit where
  toJSON p := Data.Json.object
    [ ("styleSheetId", ToJSON.toJSON p.styleSheetId), ("range", ToJSON.toJSON p.range)
    , ("text", ToJSON.toJSON p.text) ]

/-- `CSS.Value`: data for a simple selector (these are delimited by commas in
    a selector list). Named `CSSValue` rather than the bare `Value` its
    upstream domain-stripped name would otherwise be, to avoid clashing with
    this module's opened `Data.Json.Value` — see the module docstring. -/
structure CSSValue where
  /-- Value text. -/
  text : String
  /-- Value range in the underlying resource (if available). -/
  range : Option SourceRange := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON CSSValue where
  parseJSON v := do
    .ok
      { text := ← Value.getField v "text" >>= FromJSON.parseJSON
        range := ← (← Value.getFieldOpt v "range").mapM FromJSON.parseJSON }

instance : ToJSON CSSValue where
  toJSON p := Data.Json.object <|
    [("text", ToJSON.toJSON p.text)] ++ (p.range.map fun v => ("range", ToJSON.toJSON v)).toList

/-- Selector list data. -/
structure SelectorList where
  /-- Selectors in the list. -/
  selectors : List CSSValue
  /-- Rule selector text. -/
  text : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON SelectorList where
  parseJSON v := do
    .ok
      { selectors := ← Value.getField v "selectors" >>= FromJSON.parseJSON
        text := ← Value.getField v "text" >>= FromJSON.parseJSON }

instance : ToJSON SelectorList where
  toJSON p := Data.Json.object
    [("selectors", ToJSON.toJSON p.selectors), ("text", ToJSON.toJSON p.text)]

/-- Media query expression descriptor. -/
structure MediaQueryExpression where
  /-- Media query expression value. -/
  value : Float
  /-- Media query expression units. -/
  unit : String
  /-- Media query expression feature. -/
  feature : String
  /-- The associated range of the value text in the enclosing stylesheet (if
      available). -/
  valueRange : Option SourceRange := none
  /-- Computed length of media query expression (if applicable). -/
  computedLength : Option Float := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON MediaQueryExpression where
  parseJSON v := do
    .ok
      { value := ← Value.getField v "value" >>= FromJSON.parseJSON
        unit := ← Value.getField v "unit" >>= FromJSON.parseJSON
        feature := ← Value.getField v "feature" >>= FromJSON.parseJSON
        valueRange := ← (← Value.getFieldOpt v "valueRange").mapM FromJSON.parseJSON
        computedLength := ← (← Value.getFieldOpt v "computedLength").mapM FromJSON.parseJSON }

instance : ToJSON MediaQueryExpression where
  toJSON p := Data.Json.object <|
    [ ("value", ToJSON.toJSON p.value), ("unit", ToJSON.toJSON p.unit)
    , ("feature", ToJSON.toJSON p.feature) ]
    ++ (p.valueRange.map fun v => ("valueRange", ToJSON.toJSON v)).toList
    ++ (p.computedLength.map fun v => ("computedLength", ToJSON.toJSON v)).toList

/-- Media query descriptor. -/
structure MediaQuery where
  /-- Array of media query expressions. -/
  expressions : List MediaQueryExpression
  /-- Whether the media query condition is satisfied. -/
  active : Bool
  deriving Repr, BEq, DecidableEq

instance : FromJSON MediaQuery where
  parseJSON v := do
    .ok
      { expressions := ← Value.getField v "expressions" >>= FromJSON.parseJSON
        active := ← Value.getField v "active" >>= FromJSON.parseJSON }

instance : ToJSON MediaQuery where
  toJSON p := Data.Json.object
    [("expressions", ToJSON.toJSON p.expressions), ("active", ToJSON.toJSON p.active)]

/-- `CSS.CSSMedia`: CSS media rule descriptor. -/
structure CSSMedia where
  /-- Media query text. -/
  text : String
  /-- Source of the media query. -/
  source : CSSMediaSource
  /-- URL of the document containing the media query description. -/
  sourceURL : Option String := none
  /-- The associated rule (`@media` or `@import`) header range in the
      enclosing stylesheet (if available). -/
  range : Option SourceRange := none
  /-- Identifier of the stylesheet containing this object (if exists). -/
  styleSheetId : Option StyleSheetId := none
  /-- Array of media queries. -/
  mediaList : Option (List MediaQuery) := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON CSSMedia where
  parseJSON v := do
    .ok
      { text := ← Value.getField v "text" >>= FromJSON.parseJSON
        source := ← Value.getField v "source" >>= FromJSON.parseJSON
        sourceURL := ← (← Value.getFieldOpt v "sourceURL").mapM FromJSON.parseJSON
        range := ← (← Value.getFieldOpt v "range").mapM FromJSON.parseJSON
        styleSheetId := ← (← Value.getFieldOpt v "styleSheetId").mapM FromJSON.parseJSON
        mediaList := ← (← Value.getFieldOpt v "mediaList").mapM FromJSON.parseJSON }

instance : ToJSON CSSMedia where
  toJSON p := Data.Json.object <|
    [("text", ToJSON.toJSON p.text), ("source", ToJSON.toJSON p.source)]
    ++ (p.sourceURL.map fun v => ("sourceURL", ToJSON.toJSON v)).toList
    ++ (p.range.map fun v => ("range", ToJSON.toJSON v)).toList
    ++ (p.styleSheetId.map fun v => ("styleSheetId", ToJSON.toJSON v)).toList
    ++ (p.mediaList.map fun v => ("mediaList", ToJSON.toJSON v)).toList

/-- `CSS.CSSContainerQuery`: CSS container query rule descriptor. -/
structure CSSContainerQuery where
  /-- Container query text. -/
  text : String
  /-- The associated rule header range in the enclosing stylesheet (if
      available). -/
  range : Option SourceRange := none
  /-- Identifier of the stylesheet containing this object (if exists). -/
  styleSheetId : Option StyleSheetId := none
  /-- Optional name for the container. -/
  name : Option String := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON CSSContainerQuery where
  parseJSON v := do
    .ok
      { text := ← Value.getField v "text" >>= FromJSON.parseJSON
        range := ← (← Value.getFieldOpt v "range").mapM FromJSON.parseJSON
        styleSheetId := ← (← Value.getFieldOpt v "styleSheetId").mapM FromJSON.parseJSON
        name := ← (← Value.getFieldOpt v "name").mapM FromJSON.parseJSON }

instance : ToJSON CSSContainerQuery where
  toJSON p := Data.Json.object <|
    [("text", ToJSON.toJSON p.text)]
    ++ (p.range.map fun v => ("range", ToJSON.toJSON v)).toList
    ++ (p.styleSheetId.map fun v => ("styleSheetId", ToJSON.toJSON v)).toList
    ++ (p.name.map fun v => ("name", ToJSON.toJSON v)).toList

/-- `CSS.CSSSupports`: CSS `@supports` at-rule descriptor. -/
structure CSSSupports where
  /-- Supports rule text. -/
  text : String
  /-- Whether the supports condition is satisfied. -/
  active : Bool
  /-- The associated rule header range in the enclosing stylesheet (if
      available). -/
  range : Option SourceRange := none
  /-- Identifier of the stylesheet containing this object (if exists). -/
  styleSheetId : Option StyleSheetId := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON CSSSupports where
  parseJSON v := do
    .ok
      { text := ← Value.getField v "text" >>= FromJSON.parseJSON
        active := ← Value.getField v "active" >>= FromJSON.parseJSON
        range := ← (← Value.getFieldOpt v "range").mapM FromJSON.parseJSON
        styleSheetId := ← (← Value.getFieldOpt v "styleSheetId").mapM FromJSON.parseJSON }

instance : ToJSON CSSSupports where
  toJSON p := Data.Json.object <|
    [("text", ToJSON.toJSON p.text), ("active", ToJSON.toJSON p.active)]
    ++ (p.range.map fun v => ("range", ToJSON.toJSON v)).toList
    ++ (p.styleSheetId.map fun v => ("styleSheetId", ToJSON.toJSON v)).toList

/-- `CSS.CSSScope`: CSS `@scope` at-rule descriptor. -/
structure CSSScope where
  /-- Scope rule text. -/
  text : String
  /-- The associated rule header range in the enclosing stylesheet (if
      available). -/
  range : Option SourceRange := none
  /-- Identifier of the stylesheet containing this object (if exists). -/
  styleSheetId : Option StyleSheetId := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON CSSScope where
  parseJSON v := do
    .ok
      { text := ← Value.getField v "text" >>= FromJSON.parseJSON
        range := ← (← Value.getFieldOpt v "range").mapM FromJSON.parseJSON
        styleSheetId := ← (← Value.getFieldOpt v "styleSheetId").mapM FromJSON.parseJSON }

instance : ToJSON CSSScope where
  toJSON p := Data.Json.object <|
    [("text", ToJSON.toJSON p.text)]
    ++ (p.range.map fun v => ("range", ToJSON.toJSON v)).toList
    ++ (p.styleSheetId.map fun v => ("styleSheetId", ToJSON.toJSON v)).toList

/-- `CSS.CSSLayer`: CSS `@layer` at-rule descriptor. -/
structure CSSLayer where
  /-- Layer name. -/
  text : String
  /-- The associated rule header range in the enclosing stylesheet (if
      available). -/
  range : Option SourceRange := none
  /-- Identifier of the stylesheet containing this object (if exists). -/
  styleSheetId : Option StyleSheetId := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON CSSLayer where
  parseJSON v := do
    .ok
      { text := ← Value.getField v "text" >>= FromJSON.parseJSON
        range := ← (← Value.getFieldOpt v "range").mapM FromJSON.parseJSON
        styleSheetId := ← (← Value.getFieldOpt v "styleSheetId").mapM FromJSON.parseJSON }

instance : ToJSON CSSLayer where
  toJSON p := Data.Json.object <|
    [("text", ToJSON.toJSON p.text)]
    ++ (p.range.map fun v => ("range", ToJSON.toJSON v)).toList
    ++ (p.styleSheetId.map fun v => ("styleSheetId", ToJSON.toJSON v)).toList

-- ── Self-referential types ──

/-- `CSS.CSSLayerData`: CSS layer data. Self-referential via `subLayers` — see
    the module docstring. -/
structure CSSLayerData where
  /-- Layer name. -/
  name : String
  /-- Direct sub-layers. -/
  subLayers : Option (List CSSLayerData) := none
  /-- Layer order. The order determines the order of the layer in the cascade
      order. A higher number has higher priority in the cascade order. -/
  order : Float
  deriving Repr, BEq

/-- Finish decoding a `CSSLayerData` given its already-decoded recursive
    `subLayers` field. Factored out so both branches of `parseCSSLayerData`
    share it, mirroring `CDP.Domains.IndexedDB.finishKey`. -/
private def finishCSSLayerData (v : Value) (subLayers : Option (List CSSLayerData)) :
    Except String CSSLayerData := do
  .ok
    { name := ← Value.getField v "name" >>= FromJSON.parseJSON
      subLayers
      order := ← Value.getField v "order" >>= FromJSON.parseJSON }

set_option linter.unusedVariables false in
mutual

/-- Decode a `CSSLayerData`. A plain recursive `def` — rather than
    `subLayers` going through the generic `FromJSON (List α)` instance — to
    sidestep the circular instance dependency a self-referential `instance :
    FromJSON CSSLayerData` would otherwise have on itself. Terminates on
    `sizeOf`, via `Value.lookup_sizeOf_lt`. -/
def parseCSSLayerData (v : Value) : Except String CSSLayerData :=
  match h : v.lookup "subLayers" with
  | none => finishCSSLayerData v none
  | some .null => finishCSSLayerData v none
  | some subV =>
    match parseCSSLayerDataList subV with
    | .error e => .error e
    | .ok subLayers => finishCSSLayerData v (some subLayers)
termination_by sizeOf v
decreasing_by exact Value.lookup_sizeOf_lt h

private def parseCSSLayerDataList (v : Value) : Except String (List CSSLayerData) :=
  match v with
  | .array arr => arr.attach.toList.mapM fun p => parseCSSLayerData p.1
  | v => .error s!"expected array, got {repr v}"
termination_by sizeOf v
decreasing_by
  simp_wf
  have := Array.sizeOf_lt_of_mem p.2
  omega

end

instance : FromJSON CSSLayerData where parseJSON := parseCSSLayerData

private theorem CSSLayerData.subLayers_sizeOf_lt {p : CSSLayerData} {subs : List CSSLayerData}
    (h : p.subLayers = some subs) : sizeOf subs < sizeOf p := by
  cases p with
  | mk name subLayers order =>
    have h' : subLayers = some subs := h
    subst h'
    simp only [CSSLayerData.mk.sizeOf_spec, Option.some.sizeOf_spec]
    omega

set_option linter.unusedVariables false in
mutual

/-- Encode a `CSSLayerData`. A plain recursive `def`, for the same reason
    `parseCSSLayerData` is: sidesteps the circular instance dependency a
    self-referential `instance : ToJSON CSSLayerData` would have on itself
    through the generic `ToJSON (List α)` instance. Terminates structurally
    on `CSSLayerData.subLayers_sizeOf_lt`. -/
def encodeCSSLayerData (p : CSSLayerData) : Value :=
  match h : p.subLayers with
  | none => Data.Json.object [("name", ToJSON.toJSON p.name), ("order", ToJSON.toJSON p.order)]
  | some subs =>
    Data.Json.object
      [ ("name", ToJSON.toJSON p.name), ("subLayers", encodeCSSLayerDataList subs)
      , ("order", ToJSON.toJSON p.order) ]
termination_by sizeOf p
decreasing_by exact CSSLayerData.subLayers_sizeOf_lt h

private def encodeCSSLayerDataList (l : List CSSLayerData) : Value :=
  Value.array (l.map encodeCSSLayerData).toArray
termination_by sizeOf l
decreasing_by
  rename_i hmem
  have := List.sizeOf_lt_of_mem hmem
  omega

end

instance : ToJSON CSSLayerData where toJSON := encodeCSSLayerData

/-- `CSS.CSSProperty`: CSS property declaration data. Self-referential via
    `longhandProperties` — see the module docstring. -/
structure CSSProperty where
  /-- The property name. -/
  name : String
  /-- The property value. -/
  value : String
  /-- Whether the property has `!important` annotation (implies `false` if
      absent). -/
  important : Option Bool := none
  /-- Whether the property is implicit (implies `false` if absent). -/
  implicit : Option Bool := none
  /-- The full property text as specified in the style. -/
  text : Option String := none
  /-- Whether the property is understood by the browser (implies `true` if
      absent). -/
  parsedOk : Option Bool := none
  /-- Whether the property is disabled by the user (present for source-based
      properties only). -/
  disabled : Option Bool := none
  /-- The entire property range in the enclosing style declaration (if
      available). -/
  range : Option SourceRange := none
  /-- Parsed longhand components of this property if it is a shorthand. Empty
      if the property is not a shorthand. -/
  longhandProperties : Option (List CSSProperty) := none
  deriving Repr, BEq

/-- Finish decoding a `CSSProperty` given its already-decoded recursive
    `longhandProperties` field. Factored out so both branches of
    `parseCSSProperty` share it, mirroring `CDP.Domains.IndexedDB.finishKey`. -/
private def finishCSSProperty (v : Value) (longhandProperties : Option (List CSSProperty)) :
    Except String CSSProperty := do
  .ok
    { name := ← Value.getField v "name" >>= FromJSON.parseJSON
      value := ← Value.getField v "value" >>= FromJSON.parseJSON
      important := ← (← Value.getFieldOpt v "important").mapM FromJSON.parseJSON
      implicit := ← (← Value.getFieldOpt v "implicit").mapM FromJSON.parseJSON
      text := ← (← Value.getFieldOpt v "text").mapM FromJSON.parseJSON
      parsedOk := ← (← Value.getFieldOpt v "parsedOk").mapM FromJSON.parseJSON
      disabled := ← (← Value.getFieldOpt v "disabled").mapM FromJSON.parseJSON
      range := ← (← Value.getFieldOpt v "range").mapM FromJSON.parseJSON
      longhandProperties }

set_option linter.unusedVariables false in
mutual

/-- Decode a `CSSProperty`. A plain recursive `def` — rather than
    `longhandProperties` going through the generic `FromJSON (List α)`
    instance — to sidestep the circular instance dependency a
    self-referential `instance : FromJSON CSSProperty` would otherwise have
    on itself. Terminates on `sizeOf`, via `Value.lookup_sizeOf_lt`. -/
def parseCSSProperty (v : Value) : Except String CSSProperty :=
  match h : v.lookup "longhandProperties" with
  | none => finishCSSProperty v none
  | some .null => finishCSSProperty v none
  | some subV =>
    match parseCSSPropertyList subV with
    | .error e => .error e
    | .ok longhandProperties => finishCSSProperty v (some longhandProperties)
termination_by sizeOf v
decreasing_by exact Value.lookup_sizeOf_lt h

private def parseCSSPropertyList (v : Value) : Except String (List CSSProperty) :=
  match v with
  | .array arr => arr.attach.toList.mapM fun p => parseCSSProperty p.1
  | v => .error s!"expected array, got {repr v}"
termination_by sizeOf v
decreasing_by
  simp_wf
  have := Array.sizeOf_lt_of_mem p.2
  omega

end

instance : FromJSON CSSProperty where parseJSON := parseCSSProperty

private theorem CSSProperty.longhandProperties_sizeOf_lt {p : CSSProperty}
    {lst : List CSSProperty} (h : p.longhandProperties = some lst) : sizeOf lst < sizeOf p := by
  cases p with
  | mk name value important implicit text parsedOk disabled range longhandProperties =>
    have h' : longhandProperties = some lst := h
    subst h'
    simp only [CSSProperty.mk.sizeOf_spec, Option.some.sizeOf_spec]
    omega

set_option linter.unusedVariables false in
mutual

/-- Encode a `CSSProperty`. A plain recursive `def`, for the same reason
    `parseCSSProperty` is: sidesteps the circular instance dependency a
    self-referential `instance : ToJSON CSSProperty` would have on itself
    through the generic `ToJSON (List α)` instance. Terminates structurally
    on `CSSProperty.longhandProperties_sizeOf_lt`. -/
def encodeCSSProperty (p : CSSProperty) : Value :=
  match h : p.longhandProperties with
  | none =>
    Data.Json.object <|
      [("name", ToJSON.toJSON p.name), ("value", ToJSON.toJSON p.value)]
      ++ (p.important.map fun v => ("important", ToJSON.toJSON v)).toList
      ++ (p.implicit.map fun v => ("implicit", ToJSON.toJSON v)).toList
      ++ (p.text.map fun v => ("text", ToJSON.toJSON v)).toList
      ++ (p.parsedOk.map fun v => ("parsedOk", ToJSON.toJSON v)).toList
      ++ (p.disabled.map fun v => ("disabled", ToJSON.toJSON v)).toList
      ++ (p.range.map fun v => ("range", ToJSON.toJSON v)).toList
  | some lst =>
    Data.Json.object <|
      [("name", ToJSON.toJSON p.name), ("value", ToJSON.toJSON p.value)]
      ++ (p.important.map fun v => ("important", ToJSON.toJSON v)).toList
      ++ (p.implicit.map fun v => ("implicit", ToJSON.toJSON v)).toList
      ++ (p.text.map fun v => ("text", ToJSON.toJSON v)).toList
      ++ (p.parsedOk.map fun v => ("parsedOk", ToJSON.toJSON v)).toList
      ++ (p.disabled.map fun v => ("disabled", ToJSON.toJSON v)).toList
      ++ (p.range.map fun v => ("range", ToJSON.toJSON v)).toList
      ++ [("longhandProperties", encodeCSSPropertyList lst)]
termination_by sizeOf p
decreasing_by exact CSSProperty.longhandProperties_sizeOf_lt h

private def encodeCSSPropertyList (l : List CSSProperty) : Value :=
  Value.array (l.map encodeCSSProperty).toArray
termination_by sizeOf l
decreasing_by
  rename_i hmem
  have := List.sizeOf_lt_of_mem hmem
  omega

end

instance : ToJSON CSSProperty where toJSON := encodeCSSProperty

-- ── Types built on the above ──

/-- `CSS.CSSStyle`: CSS style representation. Transitively contains
    `CSSProperty`, so derives only `Repr, BEq` — see the module docstring. -/
structure CSSStyle where
  /-- The css style sheet identifier (absent for user agent stylesheet and
      user-specified stylesheet rules) this rule came from. -/
  styleSheetId : Option StyleSheetId := none
  /-- CSS properties in the style. -/
  cssProperties : List CSSProperty
  /-- Computed values for all shorthands found in the style. -/
  shorthandEntries : List ShorthandEntry
  /-- Style declaration text (if available). -/
  cssText : Option String := none
  /-- Style declaration range in the enclosing stylesheet (if available). -/
  range : Option SourceRange := none
  deriving Repr, BEq

instance : FromJSON CSSStyle where
  parseJSON v := do
    .ok
      { styleSheetId := ← (← Value.getFieldOpt v "styleSheetId").mapM FromJSON.parseJSON
        cssProperties := ← Value.getField v "cssProperties" >>= FromJSON.parseJSON
        shorthandEntries := ← Value.getField v "shorthandEntries" >>= FromJSON.parseJSON
        cssText := ← (← Value.getFieldOpt v "cssText").mapM FromJSON.parseJSON
        range := ← (← Value.getFieldOpt v "range").mapM FromJSON.parseJSON }

instance : ToJSON CSSStyle where
  toJSON p := Data.Json.object <|
    (p.styleSheetId.map fun v => ("styleSheetId", ToJSON.toJSON v)).toList
    ++ [("cssProperties", ToJSON.toJSON p.cssProperties), ("shorthandEntries", ToJSON.toJSON p.shorthandEntries)]
    ++ (p.cssText.map fun v => ("cssText", ToJSON.toJSON v)).toList
    ++ (p.range.map fun v => ("range", ToJSON.toJSON v)).toList

/-- `CSS.CSSRule`: CSS rule representation. Transitively contains
    `CSSProperty` (via `style`), so derives only `Repr, BEq`. -/
structure CSSRule where
  /-- The css style sheet identifier (absent for user agent stylesheet and
      user-specified stylesheet rules) this rule came from. -/
  styleSheetId : Option StyleSheetId := none
  /-- Rule selector data. -/
  selectorList : SelectorList
  /-- Parent stylesheet's origin. -/
  origin : StyleSheetOrigin
  /-- Associated style declaration. -/
  style : CSSStyle
  /-- Media list array (for rules involving media queries). The array
      enumerates media queries starting with the innermost one, going
      outwards. -/
  media : Option (List CSSMedia) := none
  /-- Container query list array (for rules involving container queries).
      The array enumerates container queries starting with the innermost
      one, going outwards. -/
  containerQueries : Option (List CSSContainerQuery) := none
  /-- `@supports` CSS at-rule array. The array enumerates `@supports`
      at-rules starting with the innermost one, going outwards. -/
  supports : Option (List CSSSupports) := none
  /-- Cascade layer array. Contains the layer hierarchy that this rule
      belongs to, starting with the innermost layer and going outwards. -/
  layers : Option (List CSSLayer) := none
  /-- `@scope` CSS at-rule array. The array enumerates `@scope` at-rules
      starting with the innermost one, going outwards. -/
  scopes : Option (List CSSScope) := none
  deriving Repr, BEq

instance : FromJSON CSSRule where
  parseJSON v := do
    .ok
      { styleSheetId := ← (← Value.getFieldOpt v "styleSheetId").mapM FromJSON.parseJSON
        selectorList := ← Value.getField v "selectorList" >>= FromJSON.parseJSON
        origin := ← Value.getField v "origin" >>= FromJSON.parseJSON
        style := ← Value.getField v "style" >>= FromJSON.parseJSON
        media := ← (← Value.getFieldOpt v "media").mapM FromJSON.parseJSON
        containerQueries := ← (← Value.getFieldOpt v "containerQueries").mapM FromJSON.parseJSON
        supports := ← (← Value.getFieldOpt v "supports").mapM FromJSON.parseJSON
        layers := ← (← Value.getFieldOpt v "layers").mapM FromJSON.parseJSON
        scopes := ← (← Value.getFieldOpt v "scopes").mapM FromJSON.parseJSON }

instance : ToJSON CSSRule where
  toJSON p := Data.Json.object <|
    (p.styleSheetId.map fun v => ("styleSheetId", ToJSON.toJSON v)).toList
    ++ [("selectorList", ToJSON.toJSON p.selectorList), ("origin", ToJSON.toJSON p.origin)
       , ("style", ToJSON.toJSON p.style)]
    ++ (p.media.map fun v => ("media", ToJSON.toJSON v)).toList
    ++ (p.containerQueries.map fun v => ("containerQueries", ToJSON.toJSON v)).toList
    ++ (p.supports.map fun v => ("supports", ToJSON.toJSON v)).toList
    ++ (p.layers.map fun v => ("layers", ToJSON.toJSON v)).toList
    ++ (p.scopes.map fun v => ("scopes", ToJSON.toJSON v)).toList

/-- Match data for a CSS rule. Transitively contains `CSSProperty` (via
    `rule`), so derives only `Repr, BEq`. -/
structure RuleMatch where
  /-- CSS rule in the match. -/
  rule : CSSRule
  /-- Matching selector indices in the rule's `selectorList` selectors
      (0-based). -/
  matchingSelectors : List Int
  deriving Repr, BEq

instance : FromJSON RuleMatch where
  parseJSON v := do
    .ok
      { rule := ← Value.getField v "rule" >>= FromJSON.parseJSON
        matchingSelectors := ← Value.getField v "matchingSelectors" >>= FromJSON.parseJSON }

instance : ToJSON RuleMatch where
  toJSON p := Data.Json.object
    [("rule", ToJSON.toJSON p.rule), ("matchingSelectors", ToJSON.toJSON p.matchingSelectors)]

/-- CSS rule collection for a single pseudo style. Transitively contains
    `CSSProperty` (via `matches`), so derives only `Repr, BEq`. -/
structure PseudoElementMatches where
  /-- Pseudo element type. -/
  pseudoType : DOMPageNetworkEmulationSecurity.DOM.PseudoType
  /-- Pseudo element custom ident. -/
  pseudoIdentifier : Option String := none
  /-- Matches of CSS rules applicable to the pseudo style. -/
  «matches» : List RuleMatch
  deriving Repr, BEq

instance : FromJSON PseudoElementMatches where
  parseJSON v := do
    .ok
      { pseudoType := ← Value.getField v "pseudoType" >>= FromJSON.parseJSON
        pseudoIdentifier := ← (← Value.getFieldOpt v "pseudoIdentifier").mapM FromJSON.parseJSON
        «matches» := ← Value.getField v "matches" >>= FromJSON.parseJSON }

instance : ToJSON PseudoElementMatches where
  toJSON p := Data.Json.object <|
    [("pseudoType", ToJSON.toJSON p.pseudoType)]
    ++ (p.pseudoIdentifier.map fun v => ("pseudoIdentifier", ToJSON.toJSON v)).toList
    ++ [("matches", ToJSON.toJSON p.«matches»)]

/-- Inherited CSS rule collection from an ancestor node. Transitively
    contains `CSSProperty`, so derives only `Repr, BEq`. -/
structure InheritedStyleEntry where
  /-- The ancestor node's inline style, if any, in the style inheritance
      chain. -/
  inlineStyle : Option CSSStyle := none
  /-- Matches of CSS rules matching the ancestor node in the style
      inheritance chain. -/
  matchedCSSRules : List RuleMatch
  deriving Repr, BEq

instance : FromJSON InheritedStyleEntry where
  parseJSON v := do
    .ok
      { inlineStyle := ← (← Value.getFieldOpt v "inlineStyle").mapM FromJSON.parseJSON
        matchedCSSRules := ← Value.getField v "matchedCSSRules" >>= FromJSON.parseJSON }

instance : ToJSON InheritedStyleEntry where
  toJSON p := Data.Json.object <|
    (p.inlineStyle.map fun v => ("inlineStyle", ToJSON.toJSON v)).toList
    ++ [("matchedCSSRules", ToJSON.toJSON p.matchedCSSRules)]

/-- Inherited pseudo element matches from the pseudos of an ancestor node.
    Transitively contains `CSSProperty`, so derives only `Repr, BEq`. -/
structure InheritedPseudoElementMatches where
  /-- Matches of pseudo styles from the pseudos of an ancestor node. -/
  pseudoElements : List PseudoElementMatches
  deriving Repr, BEq

instance : FromJSON InheritedPseudoElementMatches where
  parseJSON v := do
    .ok { pseudoElements := ← Value.getField v "pseudoElements" >>= FromJSON.parseJSON }

instance : ToJSON InheritedPseudoElementMatches where
  toJSON p := Data.Json.object [("pseudoElements", ToJSON.toJSON p.pseudoElements)]

/-- CSS stylesheet metainformation. -/
structure CSSStyleSheetHeader where
  /-- The stylesheet identifier. -/
  styleSheetId : StyleSheetId
  /-- Owner frame identifier. -/
  frameId : DOMPageNetworkEmulationSecurity.Page.FrameId
  /-- Stylesheet resource URL. Empty if this is a constructed stylesheet
      created using `new CSSStyleSheet()` (but non-empty if this is a
      constructed stylesheet imported as a CSS module script). -/
  sourceURL : String
  /-- URL of source map associated with the stylesheet (if any). -/
  sourceMapURL : Option String := none
  /-- Stylesheet origin. -/
  origin : StyleSheetOrigin
  /-- Stylesheet title. -/
  title : String
  /-- The backend id for the owner node of the stylesheet. -/
  ownerNode : Option DOMPageNetworkEmulationSecurity.DOM.BackendNodeId := none
  /-- Denotes whether the stylesheet is disabled. -/
  disabled : Bool
  /-- Whether the `sourceURL` field value comes from the `sourceURL`
      comment. -/
  hasSourceURL : Option Bool := none
  /-- Whether this stylesheet is created for a `STYLE` tag by the parser.
      This flag is not set for `document.write`-created `STYLE` tags. -/
  isInline : Bool
  /-- Whether this stylesheet is mutable. Inline stylesheets become mutable
      after they have been modified via the CSSOM API; a `<link>` element's
      stylesheets become mutable only if DevTools modifies them. Constructed
      stylesheets (`new CSSStyleSheet()`) are mutable immediately after
      creation. -/
  isMutable : Bool
  /-- `true` if this stylesheet is created through `new CSSStyleSheet()` or
      imported as a CSS module script. -/
  isConstructed : Bool
  /-- Line offset of the stylesheet within the resource (zero based). -/
  startLine : Float
  /-- Column offset of the stylesheet within the resource (zero based). -/
  startColumn : Float
  /-- Size of the content (in characters). -/
  length : Float
  /-- Line offset of the end of the stylesheet within the resource (zero
      based). -/
  endLine : Float
  /-- Column offset of the end of the stylesheet within the resource (zero
      based). -/
  endColumn : Float
  deriving Repr, BEq, DecidableEq

instance : FromJSON CSSStyleSheetHeader where
  parseJSON v := do
    .ok
      { styleSheetId := ← Value.getField v "styleSheetId" >>= FromJSON.parseJSON
        frameId := ← Value.getField v "frameId" >>= FromJSON.parseJSON
        sourceURL := ← Value.getField v "sourceURL" >>= FromJSON.parseJSON
        sourceMapURL := ← (← Value.getFieldOpt v "sourceMapURL").mapM FromJSON.parseJSON
        origin := ← Value.getField v "origin" >>= FromJSON.parseJSON
        title := ← Value.getField v "title" >>= FromJSON.parseJSON
        ownerNode := ← (← Value.getFieldOpt v "ownerNode").mapM FromJSON.parseJSON
        disabled := ← Value.getField v "disabled" >>= FromJSON.parseJSON
        hasSourceURL := ← (← Value.getFieldOpt v "hasSourceURL").mapM FromJSON.parseJSON
        isInline := ← Value.getField v "isInline" >>= FromJSON.parseJSON
        isMutable := ← Value.getField v "isMutable" >>= FromJSON.parseJSON
        isConstructed := ← Value.getField v "isConstructed" >>= FromJSON.parseJSON
        startLine := ← Value.getField v "startLine" >>= FromJSON.parseJSON
        startColumn := ← Value.getField v "startColumn" >>= FromJSON.parseJSON
        length := ← Value.getField v "length" >>= FromJSON.parseJSON
        endLine := ← Value.getField v "endLine" >>= FromJSON.parseJSON
        endColumn := ← Value.getField v "endColumn" >>= FromJSON.parseJSON }

instance : ToJSON CSSStyleSheetHeader where
  toJSON p := Data.Json.object <|
    [("styleSheetId", ToJSON.toJSON p.styleSheetId), ("frameId", ToJSON.toJSON p.frameId)
    , ("sourceURL", ToJSON.toJSON p.sourceURL)]
    ++ (p.sourceMapURL.map fun v => ("sourceMapURL", ToJSON.toJSON v)).toList
    ++ [("origin", ToJSON.toJSON p.origin), ("title", ToJSON.toJSON p.title)]
    ++ (p.ownerNode.map fun v => ("ownerNode", ToJSON.toJSON v)).toList
    ++ [("disabled", ToJSON.toJSON p.disabled)]
    ++ (p.hasSourceURL.map fun v => ("hasSourceURL", ToJSON.toJSON v)).toList
    ++ [ ("isInline", ToJSON.toJSON p.isInline), ("isMutable", ToJSON.toJSON p.isMutable)
       , ("isConstructed", ToJSON.toJSON p.isConstructed), ("startLine", ToJSON.toJSON p.startLine)
       , ("startColumn", ToJSON.toJSON p.startColumn), ("length", ToJSON.toJSON p.length)
       , ("endLine", ToJSON.toJSON p.endLine), ("endColumn", ToJSON.toJSON p.endColumn) ]

/-- `CSS.CSSKeyframeRule`: CSS keyframe rule representation. Transitively
    contains `CSSProperty` (via `style`), so derives only `Repr, BEq`. -/
structure CSSKeyframeRule where
  /-- The css style sheet identifier (absent for user agent stylesheet and
      user-specified stylesheet rules) this rule came from. -/
  styleSheetId : Option StyleSheetId := none
  /-- Parent stylesheet's origin. -/
  origin : StyleSheetOrigin
  /-- Associated key text. -/
  keyText : CSSValue
  /-- Associated style declaration. -/
  style : CSSStyle
  deriving Repr, BEq

instance : FromJSON CSSKeyframeRule where
  parseJSON v := do
    .ok
      { styleSheetId := ← (← Value.getFieldOpt v "styleSheetId").mapM FromJSON.parseJSON
        origin := ← Value.getField v "origin" >>= FromJSON.parseJSON
        keyText := ← Value.getField v "keyText" >>= FromJSON.parseJSON
        style := ← Value.getField v "style" >>= FromJSON.parseJSON }

instance : ToJSON CSSKeyframeRule where
  toJSON p := Data.Json.object <|
    (p.styleSheetId.map fun v => ("styleSheetId", ToJSON.toJSON v)).toList
    ++ [("origin", ToJSON.toJSON p.origin), ("keyText", ToJSON.toJSON p.keyText)
       , ("style", ToJSON.toJSON p.style)]

/-- `CSS.CSSKeyframesRule`: CSS keyframes rule representation. Transitively
    contains `CSSProperty` (via `keyframes`), so derives only `Repr, BEq`. -/
structure CSSKeyframesRule where
  /-- Animation name. -/
  animationName : CSSValue
  /-- List of keyframes. -/
  keyframes : List CSSKeyframeRule
  deriving Repr, BEq

instance : FromJSON CSSKeyframesRule where
  parseJSON v := do
    .ok
      { animationName := ← Value.getField v "animationName" >>= FromJSON.parseJSON
        keyframes := ← Value.getField v "keyframes" >>= FromJSON.parseJSON }

instance : ToJSON CSSKeyframesRule where
  toJSON p := Data.Json.object
    [("animationName", ToJSON.toJSON p.animationName), ("keyframes", ToJSON.toJSON p.keyframes)]

-- ── Events ──

/-- The `CSS.fontsUpdated` event: fired whenever a web font is updated
    (added, removed, or loading/loaded). -/
structure FontsUpdated where
  /-- The web font that has loaded. -/
  font : Option FontFace := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON FontsUpdated where
  parseJSON v := do .ok { font := ← (← Value.getFieldOpt v "font").mapM FromJSON.parseJSON }

instance : Event FontsUpdated where
  eventName := "CSS.fontsUpdated"

/-- The `CSS.mediaQueryResultChanged` event: fires whenever a MediaQuery
    result changes (for example, after a browser window has been
    resized). The current implementation considers only viewport-dependent
    media features. -/
structure MediaQueryResultChanged where
  deriving Repr, BEq, DecidableEq

instance : FromJSON MediaQueryResultChanged where parseJSON _ := .ok {}

instance : Event MediaQueryResultChanged where
  eventName := "CSS.mediaQueryResultChanged"

/-- The `CSS.styleSheetAdded` event: fired whenever an active document
    stylesheet is added. -/
structure StyleSheetAdded where
  /-- Added stylesheet metainfo. -/
  header : CSSStyleSheetHeader
  deriving Repr, BEq, DecidableEq

instance : FromJSON StyleSheetAdded where
  parseJSON v := do .ok { header := ← Value.getField v "header" >>= FromJSON.parseJSON }

instance : Event StyleSheetAdded where
  eventName := "CSS.styleSheetAdded"

/-- The `CSS.styleSheetChanged` event: fired whenever a stylesheet is
    changed as a result of the client operation. -/
structure StyleSheetChanged where
  styleSheetId : StyleSheetId
  deriving Repr, BEq, DecidableEq

instance : FromJSON StyleSheetChanged where
  parseJSON v := do .ok { styleSheetId := ← Value.getField v "styleSheetId" >>= FromJSON.parseJSON }

instance : Event StyleSheetChanged where
  eventName := "CSS.styleSheetChanged"

/-- The `CSS.styleSheetRemoved` event: fired whenever an active document
    stylesheet is removed. -/
structure StyleSheetRemoved where
  /-- Identifier of the removed stylesheet. -/
  styleSheetId : StyleSheetId
  deriving Repr, BEq, DecidableEq

instance : FromJSON StyleSheetRemoved where
  parseJSON v := do .ok { styleSheetId := ← Value.getField v "styleSheetId" >>= FromJSON.parseJSON }

instance : Event StyleSheetRemoved where
  eventName := "CSS.styleSheetRemoved"

-- ── Commands ──

/-- Parameters of the `CSS.addRule` command: inserts a new rule with the
    given `ruleText` in a stylesheet with given `styleSheetId`, at the
    position specified by `location`. -/
structure PAddRule where
  /-- The css style sheet identifier where a new rule should be inserted. -/
  styleSheetId : StyleSheetId
  /-- The text of a new rule. -/
  ruleText : String
  /-- Text position of a new rule in the target style sheet. -/
  location : SourceRange
  deriving Repr, BEq, DecidableEq

instance : ToJSON PAddRule where
  toJSON p := Data.Json.object
    [ ("styleSheetId", ToJSON.toJSON p.styleSheetId), ("ruleText", ToJSON.toJSON p.ruleText)
    , ("location", ToJSON.toJSON p.location) ]

/-- Response of the `CSS.addRule` command. -/
structure AddRule where
  /-- The newly created rule. -/
  rule : CSSRule
  deriving Repr, BEq

instance : FromJSON AddRule where
  parseJSON v := do .ok { rule := ← Value.getField v "rule" >>= FromJSON.parseJSON }

instance : Command PAddRule where
  Response := AddRule
  commandName _ := "CSS.addRule"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `CSS.collectClassNames` command: returns all class
    names from the specified stylesheet. -/
structure PCollectClassNames where
  styleSheetId : StyleSheetId
  deriving Repr, BEq, DecidableEq

instance : ToJSON PCollectClassNames where
  toJSON p := Data.Json.object [("styleSheetId", ToJSON.toJSON p.styleSheetId)]

/-- Response of the `CSS.collectClassNames` command. -/
structure CollectClassNames where
  /-- Class name list. -/
  classNames : List String
  deriving Repr, BEq, DecidableEq

instance : FromJSON CollectClassNames where
  parseJSON v := do .ok { classNames := ← Value.getField v "classNames" >>= FromJSON.parseJSON }

instance : Command PCollectClassNames where
  Response := CollectClassNames
  commandName _ := "CSS.collectClassNames"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `CSS.createStyleSheet` command: creates a new special
    "via-inspector" stylesheet in the frame with given `frameId`. -/
structure PCreateStyleSheet where
  /-- Identifier of the frame where the "via-inspector" stylesheet should be
      created. -/
  frameId : DOMPageNetworkEmulationSecurity.Page.FrameId
  deriving Repr, BEq, DecidableEq

instance : ToJSON PCreateStyleSheet where
  toJSON p := Data.Json.object [("frameId", ToJSON.toJSON p.frameId)]

/-- Response of the `CSS.createStyleSheet` command. -/
structure CreateStyleSheet where
  /-- Identifier of the created "via-inspector" stylesheet. -/
  styleSheetId : StyleSheetId
  deriving Repr, BEq, DecidableEq

instance : FromJSON CreateStyleSheet where
  parseJSON v := do .ok { styleSheetId := ← Value.getField v "styleSheetId" >>= FromJSON.parseJSON }

instance : Command PCreateStyleSheet where
  Response := CreateStyleSheet
  commandName _ := "CSS.createStyleSheet"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `CSS.disable` command: disables the CSS agent for the
    given page. -/
structure PDisable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PDisable where toJSON _ := .null

instance : Command PDisable where
  Response := Unit
  commandName _ := "CSS.disable"
  decodeResponse _ := .ok ()

/-- Parameters of the `CSS.enable` command: enables the CSS agent for the
    given page. Clients should not assume that the CSS agent has been
    enabled until the result of this command is received. -/
structure PEnable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PEnable where toJSON _ := .null

instance : Command PEnable where
  Response := Unit
  commandName _ := "CSS.enable"
  decodeResponse _ := .ok ()

/-- Parameters of the `CSS.forcePseudoState` command: ensures that the given
    node will have specified pseudo-classes whenever its style is computed
    by the browser. -/
structure PForcePseudoState where
  /-- The element id for which to force the pseudo state. -/
  nodeId : DOMPageNetworkEmulationSecurity.DOM.NodeId
  /-- Element pseudo classes to force when computing the element's style. -/
  forcedPseudoClasses : List String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PForcePseudoState where
  toJSON p := Data.Json.object
    [("nodeId", ToJSON.toJSON p.nodeId), ("forcedPseudoClasses", ToJSON.toJSON p.forcedPseudoClasses)]

instance : Command PForcePseudoState where
  Response := Unit
  commandName _ := "CSS.forcePseudoState"
  decodeResponse _ := .ok ()

/-- Parameters of the `CSS.getBackgroundColors` command. -/
structure PGetBackgroundColors where
  /-- Id of the node to get background colors for. -/
  nodeId : DOMPageNetworkEmulationSecurity.DOM.NodeId
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetBackgroundColors where
  toJSON p := Data.Json.object [("nodeId", ToJSON.toJSON p.nodeId)]

/-- Response of the `CSS.getBackgroundColors` command. -/
structure GetBackgroundColors where
  /-- The range of background colors behind this element, if it contains any
      visible text. If no visible text is present, this will be `none`. In
      the case of a flat background color, this will consist of simply that
      color. In the case of a gradient, this will consist of each of the
      color stops. For anything more complicated, this will be an empty
      array. Images are ignored (as if the image had failed to load). -/
  backgroundColors : Option (List String) := none
  /-- The computed font size for this node, as a CSS computed value string
      (e.g. `'12px'`). -/
  computedFontSize : Option String := none
  /-- The computed font weight for this node, as a CSS computed value
      string (e.g. `'normal'` or `'100'`). -/
  computedFontWeight : Option String := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON GetBackgroundColors where
  parseJSON v := do
    .ok
      { backgroundColors := ← (← Value.getFieldOpt v "backgroundColors").mapM FromJSON.parseJSON
        computedFontSize := ← (← Value.getFieldOpt v "computedFontSize").mapM FromJSON.parseJSON
        computedFontWeight :=
          ← (← Value.getFieldOpt v "computedFontWeight").mapM FromJSON.parseJSON }

instance : Command PGetBackgroundColors where
  Response := GetBackgroundColors
  commandName _ := "CSS.getBackgroundColors"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `CSS.getComputedStyleForNode` command: returns the
    computed style for a DOM node identified by `nodeId`. -/
structure PGetComputedStyleForNode where
  nodeId : DOMPageNetworkEmulationSecurity.DOM.NodeId
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetComputedStyleForNode where
  toJSON p := Data.Json.object [("nodeId", ToJSON.toJSON p.nodeId)]

/-- Response of the `CSS.getComputedStyleForNode` command. -/
structure GetComputedStyleForNode where
  /-- Computed style for the specified DOM node. -/
  computedStyle : List CSSComputedStyleProperty
  deriving Repr, BEq, DecidableEq

instance : FromJSON GetComputedStyleForNode where
  parseJSON v := do .ok { computedStyle := ← Value.getField v "computedStyle" >>= FromJSON.parseJSON }

instance : Command PGetComputedStyleForNode where
  Response := GetComputedStyleForNode
  commandName _ := "CSS.getComputedStyleForNode"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `CSS.getInlineStylesForNode` command: returns the
    styles defined inline (explicitly in the "style" attribute and
    implicitly, using DOM attributes) for a DOM node identified by
    `nodeId`. -/
structure PGetInlineStylesForNode where
  nodeId : DOMPageNetworkEmulationSecurity.DOM.NodeId
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetInlineStylesForNode where
  toJSON p := Data.Json.object [("nodeId", ToJSON.toJSON p.nodeId)]

/-- Response of the `CSS.getInlineStylesForNode` command. Transitively
    contains `CSSProperty`, so derives only `Repr, BEq`. -/
structure GetInlineStylesForNode where
  /-- Inline style for the specified DOM node. -/
  inlineStyle : Option CSSStyle := none
  /-- Attribute-defined element style (e.g. resulting from
      `width=20 height=100%`). -/
  attributesStyle : Option CSSStyle := none
  deriving Repr, BEq

instance : FromJSON GetInlineStylesForNode where
  parseJSON v := do
    .ok
      { inlineStyle := ← (← Value.getFieldOpt v "inlineStyle").mapM FromJSON.parseJSON
        attributesStyle := ← (← Value.getFieldOpt v "attributesStyle").mapM FromJSON.parseJSON }

instance : Command PGetInlineStylesForNode where
  Response := GetInlineStylesForNode
  commandName _ := "CSS.getInlineStylesForNode"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `CSS.getMatchedStylesForNode` command: returns
    requested styles for a DOM node identified by `nodeId`. -/
structure PGetMatchedStylesForNode where
  nodeId : DOMPageNetworkEmulationSecurity.DOM.NodeId
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetMatchedStylesForNode where
  toJSON p := Data.Json.object [("nodeId", ToJSON.toJSON p.nodeId)]

/-- Response of the `CSS.getMatchedStylesForNode` command. Transitively
    contains `CSSProperty`, so derives only `Repr, BEq`. -/
structure GetMatchedStylesForNode where
  /-- Inline style for the specified DOM node. -/
  inlineStyle : Option CSSStyle := none
  /-- Attribute-defined element style (e.g. resulting from
      `width=20 height=100%`). -/
  attributesStyle : Option CSSStyle := none
  /-- CSS rules matching this node, from all applicable stylesheets. -/
  matchedCSSRules : Option (List RuleMatch) := none
  /-- Pseudo style matches for this node. -/
  pseudoElements : Option (List PseudoElementMatches) := none
  /-- A chain of inherited styles (from the immediate node parent up to the
      DOM tree root). -/
  inherited : Option (List InheritedStyleEntry) := none
  /-- A chain of inherited pseudo element styles (from the immediate node
      parent up to the DOM tree root). -/
  inheritedPseudoElements : Option (List InheritedPseudoElementMatches) := none
  /-- A list of CSS keyframed animations matching this node. -/
  cssKeyframesRules : Option (List CSSKeyframesRule) := none
  /-- Id of the first parent element that does not have `display: contents`. -/
  parentLayoutNodeId : Option DOMPageNetworkEmulationSecurity.DOM.NodeId := none
  deriving Repr, BEq

instance : FromJSON GetMatchedStylesForNode where
  parseJSON v := do
    .ok
      { inlineStyle := ← (← Value.getFieldOpt v "inlineStyle").mapM FromJSON.parseJSON
        attributesStyle := ← (← Value.getFieldOpt v "attributesStyle").mapM FromJSON.parseJSON
        matchedCSSRules := ← (← Value.getFieldOpt v "matchedCSSRules").mapM FromJSON.parseJSON
        pseudoElements := ← (← Value.getFieldOpt v "pseudoElements").mapM FromJSON.parseJSON
        inherited := ← (← Value.getFieldOpt v "inherited").mapM FromJSON.parseJSON
        inheritedPseudoElements :=
          ← (← Value.getFieldOpt v "inheritedPseudoElements").mapM FromJSON.parseJSON
        cssKeyframesRules := ← (← Value.getFieldOpt v "cssKeyframesRules").mapM FromJSON.parseJSON
        parentLayoutNodeId := ← (← Value.getFieldOpt v "parentLayoutNodeId").mapM FromJSON.parseJSON }

instance : Command PGetMatchedStylesForNode where
  Response := GetMatchedStylesForNode
  commandName _ := "CSS.getMatchedStylesForNode"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `CSS.getMediaQueries` command: returns all media
    queries parsed by the rendering engine. -/
structure PGetMediaQueries where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetMediaQueries where toJSON _ := .null

/-- Response of the `CSS.getMediaQueries` command. -/
structure GetMediaQueries where
  medias : List CSSMedia
  deriving Repr, BEq, DecidableEq

instance : FromJSON GetMediaQueries where
  parseJSON v := do .ok { medias := ← Value.getField v "medias" >>= FromJSON.parseJSON }

instance : Command PGetMediaQueries where
  Response := GetMediaQueries
  commandName _ := "CSS.getMediaQueries"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `CSS.getPlatformFontsForNode` command: requests
    information about platform fonts which were used to render child
    TextNodes in the given node. -/
structure PGetPlatformFontsForNode where
  nodeId : DOMPageNetworkEmulationSecurity.DOM.NodeId
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetPlatformFontsForNode where
  toJSON p := Data.Json.object [("nodeId", ToJSON.toJSON p.nodeId)]

/-- Response of the `CSS.getPlatformFontsForNode` command. -/
structure GetPlatformFontsForNode where
  /-- Usage statistics for every employed platform font. -/
  fonts : List PlatformFontUsage
  deriving Repr, BEq, DecidableEq

instance : FromJSON GetPlatformFontsForNode where
  parseJSON v := do .ok { fonts := ← Value.getField v "fonts" >>= FromJSON.parseJSON }

instance : Command PGetPlatformFontsForNode where
  Response := GetPlatformFontsForNode
  commandName _ := "CSS.getPlatformFontsForNode"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `CSS.getStyleSheetText` command: returns the current
    textual content for a stylesheet. -/
structure PGetStyleSheetText where
  styleSheetId : StyleSheetId
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetStyleSheetText where
  toJSON p := Data.Json.object [("styleSheetId", ToJSON.toJSON p.styleSheetId)]

/-- Response of the `CSS.getStyleSheetText` command. -/
structure GetStyleSheetText where
  /-- The stylesheet text. -/
  text : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON GetStyleSheetText where
  parseJSON v := do .ok { text := ← Value.getField v "text" >>= FromJSON.parseJSON }

instance : Command PGetStyleSheetText where
  Response := GetStyleSheetText
  commandName _ := "CSS.getStyleSheetText"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `CSS.getLayersForNode` command: returns all layers
    parsed by the rendering engine for the tree scope of a node. Given a DOM
    element identified by `nodeId`, `getLayersForNode` returns the root
    layer for the nearest ancestor document or shadow root. The layer root
    contains the full layer tree for the tree scope and their ordering. -/
structure PGetLayersForNode where
  nodeId : DOMPageNetworkEmulationSecurity.DOM.NodeId
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetLayersForNode where
  toJSON p := Data.Json.object [("nodeId", ToJSON.toJSON p.nodeId)]

/-- Response of the `CSS.getLayersForNode` command. Transitively contains
    `CSSLayerData`, so derives only `Repr, BEq`. -/
structure GetLayersForNode where
  rootLayer : CSSLayerData
  deriving Repr, BEq

instance : FromJSON GetLayersForNode where
  parseJSON v := do .ok { rootLayer := ← Value.getField v "rootLayer" >>= FromJSON.parseJSON }

instance : Command PGetLayersForNode where
  Response := GetLayersForNode
  commandName _ := "CSS.getLayersForNode"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `CSS.trackComputedStyleUpdates` command: starts
    tracking the given computed styles for updates. The specified array of
    properties replaces the one previously specified. Pass an empty array to
    disable tracking. Use `takeComputedStyleUpdates` to retrieve the list of
    nodes that had properties modified. The changes to computed style
    properties are only tracked for nodes pushed to the front-end by the DOM
    agent. If no changes to the tracked properties occur after the node has
    been pushed to the front-end, no updates will be issued for the node. -/
structure PTrackComputedStyleUpdates where
  propertiesToTrack : List CSSComputedStyleProperty
  deriving Repr, BEq, DecidableEq

instance : ToJSON PTrackComputedStyleUpdates where
  toJSON p := Data.Json.object [("propertiesToTrack", ToJSON.toJSON p.propertiesToTrack)]

instance : Command PTrackComputedStyleUpdates where
  Response := Unit
  commandName _ := "CSS.trackComputedStyleUpdates"
  decodeResponse _ := .ok ()

/-- Parameters of the `CSS.takeComputedStyleUpdates` command: polls the next
    batch of computed style updates. -/
structure PTakeComputedStyleUpdates where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PTakeComputedStyleUpdates where toJSON _ := .null

/-- Response of the `CSS.takeComputedStyleUpdates` command. -/
structure TakeComputedStyleUpdates where
  /-- The list of node ids that have their tracked computed styles
      updated. -/
  nodeIds : List DOMPageNetworkEmulationSecurity.DOM.NodeId
  deriving Repr, BEq, DecidableEq

instance : FromJSON TakeComputedStyleUpdates where
  parseJSON v := do .ok { nodeIds := ← Value.getField v "nodeIds" >>= FromJSON.parseJSON }

instance : Command PTakeComputedStyleUpdates where
  Response := TakeComputedStyleUpdates
  commandName _ := "CSS.takeComputedStyleUpdates"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `CSS.setEffectivePropertyValueForNode` command: finds a
    rule with the given active property for the given node and sets the new
    value for this property. -/
structure PSetEffectivePropertyValueForNode where
  /-- The element id for which to set property. -/
  nodeId : DOMPageNetworkEmulationSecurity.DOM.NodeId
  propertyName : String
  value : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetEffectivePropertyValueForNode where
  toJSON p := Data.Json.object
    [ ("nodeId", ToJSON.toJSON p.nodeId), ("propertyName", ToJSON.toJSON p.propertyName)
    , ("value", ToJSON.toJSON p.value) ]

instance : Command PSetEffectivePropertyValueForNode where
  Response := Unit
  commandName _ := "CSS.setEffectivePropertyValueForNode"
  decodeResponse _ := .ok ()

/-- Parameters of the `CSS.setKeyframeKey` command: modifies the keyframe
    rule key text. -/
structure PSetKeyframeKey where
  styleSheetId : StyleSheetId
  range : SourceRange
  keyText : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetKeyframeKey where
  toJSON p := Data.Json.object
    [ ("styleSheetId", ToJSON.toJSON p.styleSheetId), ("range", ToJSON.toJSON p.range)
    , ("keyText", ToJSON.toJSON p.keyText) ]

/-- Response of the `CSS.setKeyframeKey` command. -/
structure SetKeyframeKey where
  /-- The resulting key text after modification. -/
  keyText : CSSValue
  deriving Repr, BEq, DecidableEq

instance : FromJSON SetKeyframeKey where
  parseJSON v := do .ok { keyText := ← Value.getField v "keyText" >>= FromJSON.parseJSON }

instance : Command PSetKeyframeKey where
  Response := SetKeyframeKey
  commandName _ := "CSS.setKeyframeKey"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `CSS.setMediaText` command: modifies the media rule's
    text. -/
structure PSetMediaText where
  styleSheetId : StyleSheetId
  range : SourceRange
  text : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetMediaText where
  toJSON p := Data.Json.object
    [ ("styleSheetId", ToJSON.toJSON p.styleSheetId), ("range", ToJSON.toJSON p.range)
    , ("text", ToJSON.toJSON p.text) ]

/-- Response of the `CSS.setMediaText` command. -/
structure SetMediaText where
  /-- The resulting CSS media rule after modification. -/
  media : CSSMedia
  deriving Repr, BEq, DecidableEq

instance : FromJSON SetMediaText where
  parseJSON v := do .ok { media := ← Value.getField v "media" >>= FromJSON.parseJSON }

instance : Command PSetMediaText where
  Response := SetMediaText
  commandName _ := "CSS.setMediaText"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `CSS.setContainerQueryText` command: modifies the
    expression of a container query. -/
structure PSetContainerQueryText where
  styleSheetId : StyleSheetId
  range : SourceRange
  text : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetContainerQueryText where
  toJSON p := Data.Json.object
    [ ("styleSheetId", ToJSON.toJSON p.styleSheetId), ("range", ToJSON.toJSON p.range)
    , ("text", ToJSON.toJSON p.text) ]

/-- Response of the `CSS.setContainerQueryText` command. -/
structure SetContainerQueryText where
  /-- The resulting CSS container query rule after modification. -/
  containerQuery : CSSContainerQuery
  deriving Repr, BEq, DecidableEq

instance : FromJSON SetContainerQueryText where
  parseJSON v := do .ok { containerQuery := ← Value.getField v "containerQuery" >>= FromJSON.parseJSON }

instance : Command PSetContainerQueryText where
  Response := SetContainerQueryText
  commandName _ := "CSS.setContainerQueryText"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `CSS.setSupportsText` command: modifies the expression
    of a supports at-rule. -/
structure PSetSupportsText where
  styleSheetId : StyleSheetId
  range : SourceRange
  text : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetSupportsText where
  toJSON p := Data.Json.object
    [ ("styleSheetId", ToJSON.toJSON p.styleSheetId), ("range", ToJSON.toJSON p.range)
    , ("text", ToJSON.toJSON p.text) ]

/-- Response of the `CSS.setSupportsText` command. -/
structure SetSupportsText where
  /-- The resulting CSS Supports rule after modification. -/
  supports : CSSSupports
  deriving Repr, BEq, DecidableEq

instance : FromJSON SetSupportsText where
  parseJSON v := do .ok { supports := ← Value.getField v "supports" >>= FromJSON.parseJSON }

instance : Command PSetSupportsText where
  Response := SetSupportsText
  commandName _ := "CSS.setSupportsText"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `CSS.setScopeText` command: modifies the expression of
    a scope at-rule. -/
structure PSetScopeText where
  styleSheetId : StyleSheetId
  range : SourceRange
  text : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetScopeText where
  toJSON p := Data.Json.object
    [ ("styleSheetId", ToJSON.toJSON p.styleSheetId), ("range", ToJSON.toJSON p.range)
    , ("text", ToJSON.toJSON p.text) ]

/-- Response of the `CSS.setScopeText` command. -/
structure SetScopeText where
  /-- The resulting CSS Scope rule after modification. -/
  scope : CSSScope
  deriving Repr, BEq, DecidableEq

instance : FromJSON SetScopeText where
  parseJSON v := do .ok { scope := ← Value.getField v "scope" >>= FromJSON.parseJSON }

instance : Command PSetScopeText where
  Response := SetScopeText
  commandName _ := "CSS.setScopeText"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `CSS.setRuleSelector` command: modifies the rule
    selector. -/
structure PSetRuleSelector where
  styleSheetId : StyleSheetId
  range : SourceRange
  selector : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetRuleSelector where
  toJSON p := Data.Json.object
    [ ("styleSheetId", ToJSON.toJSON p.styleSheetId), ("range", ToJSON.toJSON p.range)
    , ("selector", ToJSON.toJSON p.selector) ]

/-- Response of the `CSS.setRuleSelector` command. -/
structure SetRuleSelector where
  /-- The resulting selector list after modification. -/
  selectorList : SelectorList
  deriving Repr, BEq, DecidableEq

instance : FromJSON SetRuleSelector where
  parseJSON v := do .ok { selectorList := ← Value.getField v "selectorList" >>= FromJSON.parseJSON }

instance : Command PSetRuleSelector where
  Response := SetRuleSelector
  commandName _ := "CSS.setRuleSelector"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `CSS.setStyleSheetText` command: sets the new
    stylesheet text. -/
structure PSetStyleSheetText where
  styleSheetId : StyleSheetId
  text : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetStyleSheetText where
  toJSON p := Data.Json.object [("styleSheetId", ToJSON.toJSON p.styleSheetId), ("text", ToJSON.toJSON p.text)]

/-- Response of the `CSS.setStyleSheetText` command. -/
structure SetStyleSheetText where
  /-- URL of source map associated with the script (if any). -/
  sourceMapURL : Option String := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON SetStyleSheetText where
  parseJSON v := do
    .ok { sourceMapURL := ← (← Value.getFieldOpt v "sourceMapURL").mapM FromJSON.parseJSON }

instance : Command PSetStyleSheetText where
  Response := SetStyleSheetText
  commandName _ := "CSS.setStyleSheetText"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `CSS.setStyleTexts` command: applies the specified
    style edits one after another in the given order. -/
structure PSetStyleTexts where
  edits : List StyleDeclarationEdit
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetStyleTexts where
  toJSON p := Data.Json.object [("edits", ToJSON.toJSON p.edits)]

/-- Response of the `CSS.setStyleTexts` command. Transitively contains
    `CSSProperty`, so derives only `Repr, BEq`. -/
structure SetStyleTexts where
  /-- The resulting styles after modification. -/
  styles : List CSSStyle
  deriving Repr, BEq

instance : FromJSON SetStyleTexts where
  parseJSON v := do .ok { styles := ← Value.getField v "styles" >>= FromJSON.parseJSON }

instance : Command PSetStyleTexts where
  Response := SetStyleTexts
  commandName _ := "CSS.setStyleTexts"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `CSS.startRuleUsageTracking` command: enables the
    selector recording. -/
structure PStartRuleUsageTracking where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PStartRuleUsageTracking where toJSON _ := .null

instance : Command PStartRuleUsageTracking where
  Response := Unit
  commandName _ := "CSS.startRuleUsageTracking"
  decodeResponse _ := .ok ()

/-- Parameters of the `CSS.stopRuleUsageTracking` command: stops tracking
    rule usage and returns the list of rules that were used since the last
    call to `takeCoverageDelta` (or since the start of coverage
    instrumentation). -/
structure PStopRuleUsageTracking where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PStopRuleUsageTracking where toJSON _ := .null

/-- Response of the `CSS.stopRuleUsageTracking` command. -/
structure StopRuleUsageTracking where
  ruleUsage : List RuleUsage
  deriving Repr, BEq, DecidableEq

instance : FromJSON StopRuleUsageTracking where
  parseJSON v := do .ok { ruleUsage := ← Value.getField v "ruleUsage" >>= FromJSON.parseJSON }

instance : Command PStopRuleUsageTracking where
  Response := StopRuleUsageTracking
  commandName _ := "CSS.stopRuleUsageTracking"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `CSS.takeCoverageDelta` command: obtains the list of
    rules that became used since the last call to this method (or since the
    start of coverage instrumentation). -/
structure PTakeCoverageDelta where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PTakeCoverageDelta where toJSON _ := .null

/-- Response of the `CSS.takeCoverageDelta` command. -/
structure TakeCoverageDelta where
  coverage : List RuleUsage
  /-- Monotonically increasing time, in seconds. -/
  timestamp : Float
  deriving Repr, BEq, DecidableEq

instance : FromJSON TakeCoverageDelta where
  parseJSON v := do
    .ok
      { coverage := ← Value.getField v "coverage" >>= FromJSON.parseJSON
        timestamp := ← Value.getField v "timestamp" >>= FromJSON.parseJSON }

instance : Command PTakeCoverageDelta where
  Response := TakeCoverageDelta
  commandName _ := "CSS.takeCoverageDelta"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `CSS.setLocalFontsEnabled` command: enables/disables
    rendering of local CSS fonts (enabled by default). -/
structure PSetLocalFontsEnabled where
  /-- Whether rendering of local fonts is enabled. -/
  enabled : Bool
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetLocalFontsEnabled where
  toJSON p := Data.Json.object [("enabled", ToJSON.toJSON p.enabled)]

instance : Command PSetLocalFontsEnabled where
  Response := Unit
  commandName _ := "CSS.setLocalFontsEnabled"
  decodeResponse _ := .ok ()

end CDP.Domains.CSS
