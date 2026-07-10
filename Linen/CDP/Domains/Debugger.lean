/-
  Linen.CDP.Domains.Debugger — the `Debugger` CDP domain

  Exposes JavaScript debugging capabilities: setting and removing
  breakpoints, stepping through execution, exploring stack traces, etc. Ports
  `CDP.Domains.Debugger` (see `docs/imports/cdp/dependencies.md`); naming
  conventions as in `CDP.Domains.CacheStorage`'s docstring.

  None of this module's own types are self- or mutually-recursive (unlike
  `CDP.Domains.Runtime`'s `StackTrace`/`ObjectPreview`, which it merely
  references), so no termination proofs are needed here.

  `end` is a Lean keyword; the field upstream calls `end` (on
  `LocationRange`) is written `«end»` here.
-/
import Linen.CDP.Internal.Utils
import Linen.CDP.Domains.Runtime

namespace CDP.Domains.Debugger

open Data.Json (Value ToJSON FromJSON)
open CDP.Internal.Utils (Command Event)

-- ── Identifiers ──

/-- Breakpoint identifier. -/
abbrev BreakpointId := String

/-- Call frame identifier. -/
abbrev CallFrameId := String

-- ── Locations ──

/-- Location in the source code. -/
structure Location where
  /-- Script identifier as reported in `Debugger.scriptParsed`. -/
  scriptId : Runtime.ScriptId
  /-- Line number in the script (0-based). -/
  lineNumber : Int
  /-- Column number in the script (0-based). -/
  columnNumber : Option Int := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON Location where
  parseJSON v := do
    .ok
      { scriptId := ← Value.getField v "scriptId" >>= FromJSON.parseJSON
        lineNumber := ← Value.getField v "lineNumber" >>= FromJSON.parseJSON
        columnNumber := ← (← Value.getFieldOpt v "columnNumber").mapM FromJSON.parseJSON }

instance : ToJSON Location where
  toJSON p := Data.Json.object <|
    [("scriptId", ToJSON.toJSON p.scriptId), ("lineNumber", ToJSON.toJSON p.lineNumber)]
    ++ (p.columnNumber.map fun v => ("columnNumber", ToJSON.toJSON v)).toList

/-- Location in the source code, as a line/column pair. -/
structure ScriptPosition where
  lineNumber : Int
  columnNumber : Int
  deriving Repr, BEq, DecidableEq

instance : FromJSON ScriptPosition where
  parseJSON v := do
    .ok
      { lineNumber := ← Value.getField v "lineNumber" >>= FromJSON.parseJSON
        columnNumber := ← Value.getField v "columnNumber" >>= FromJSON.parseJSON }

instance : ToJSON ScriptPosition where
  toJSON p := Data.Json.object
    [("lineNumber", ToJSON.toJSON p.lineNumber), ("columnNumber", ToJSON.toJSON p.columnNumber)]

/-- Location range within one script. -/
structure LocationRange where
  scriptId : Runtime.ScriptId
  start : ScriptPosition
  «end» : ScriptPosition
  deriving Repr, BEq, DecidableEq

instance : FromJSON LocationRange where
  parseJSON v := do
    .ok
      { scriptId := ← Value.getField v "scriptId" >>= FromJSON.parseJSON
        start := ← Value.getField v "start" >>= FromJSON.parseJSON
        «end» := ← Value.getField v "end" >>= FromJSON.parseJSON }

instance : ToJSON LocationRange where
  toJSON p := Data.Json.object
    [ ("scriptId", ToJSON.toJSON p.scriptId), ("start", ToJSON.toJSON p.start)
    , ("end", ToJSON.toJSON p.«end») ]

-- ── Scopes and call frames ──

/-- Scope type of a `Scope`. -/
inductive ScopeType where
  | global | local | with | closure | catch | block | script | eval | module
  | wasmExpressionStack
  deriving Repr, BEq, DecidableEq

instance : FromJSON ScopeType where
  parseJSON
    | .string "global" => .ok .global
    | .string "local" => .ok .local
    | .string "with" => .ok .with
    | .string "closure" => .ok .closure
    | .string "catch" => .ok .catch
    | .string "block" => .ok .block
    | .string "script" => .ok .script
    | .string "eval" => .ok .eval
    | .string "module" => .ok .module
    | .string "wasm-expression-stack" => .ok .wasmExpressionStack
    | v => .error s!"failed to parse ScopeType: {repr v}"

instance : ToJSON ScopeType where
  toJSON
    | .global => .string "global" | .local => .string "local" | .with => .string "with"
    | .closure => .string "closure" | .catch => .string "catch" | .block => .string "block"
    | .script => .string "script" | .eval => .string "eval" | .module => .string "module"
    | .wasmExpressionStack => .string "wasm-expression-stack"

/-- Scope description. -/
structure Scope where
  /-- Scope type. -/
  type : ScopeType
  /-- Object representing the scope. For `global` and `with` scopes it
      represents the actual object; for the rest of the scopes, it is an
      artificial transient object enumerating scope variables as its
      properties. -/
  object : Runtime.RemoteObject
  name : Option String := none
  /-- Location in the source code where the scope starts. -/
  startLocation : Option Location := none
  /-- Location in the source code where the scope ends. -/
  endLocation : Option Location := none
  deriving Repr, BEq

instance : FromJSON Scope where
  parseJSON v := do
    .ok
      { type := ← Value.getField v "type" >>= FromJSON.parseJSON
        object := ← Value.getField v "object" >>= FromJSON.parseJSON
        name := ← (← Value.getFieldOpt v "name").mapM FromJSON.parseJSON
        startLocation := ← (← Value.getFieldOpt v "startLocation").mapM FromJSON.parseJSON
        endLocation := ← (← Value.getFieldOpt v "endLocation").mapM FromJSON.parseJSON }

instance : ToJSON Scope where
  toJSON p := Data.Json.object <|
    [("type", ToJSON.toJSON p.type), ("object", ToJSON.toJSON p.object)]
    ++ (p.name.map fun v => ("name", ToJSON.toJSON v)).toList
    ++ (p.startLocation.map fun v => ("startLocation", ToJSON.toJSON v)).toList
    ++ (p.endLocation.map fun v => ("endLocation", ToJSON.toJSON v)).toList

/-- JavaScript call frame. An array of call frames form the call stack. -/
structure CallFrame where
  /-- Call frame identifier. Only valid while the virtual machine is
      paused. -/
  callFrameId : CallFrameId
  /-- Name of the JavaScript function called on this call frame. -/
  functionName : String
  /-- Location in the source code. -/
  functionLocation : Option Location := none
  /-- Location in the source code. -/
  location : Location
  /-- Scope chain for this call frame. -/
  scopeChain : List Scope
  /-- `this` object for this call frame. -/
  «this» : Runtime.RemoteObject
  /-- The value being returned, if the function is at a return point. -/
  returnValue : Option Runtime.RemoteObject := none
  /-- Valid only while the VM is paused; indicates whether this frame can be
      restarted or not. A `true` value here does not guarantee that
      `Debugger.restartFrame` with this `CallFrameId` will succeed, but it is
      very likely. -/
  canBeRestarted : Option Bool := none
  deriving Repr, BEq

instance : FromJSON CallFrame where
  parseJSON v := do
    .ok
      { callFrameId := ← Value.getField v "callFrameId" >>= FromJSON.parseJSON
        functionName := ← Value.getField v "functionName" >>= FromJSON.parseJSON
        functionLocation := ← (← Value.getFieldOpt v "functionLocation").mapM FromJSON.parseJSON
        location := ← Value.getField v "location" >>= FromJSON.parseJSON
        scopeChain := ← Value.getField v "scopeChain" >>= FromJSON.parseJSON
        «this» := ← Value.getField v "this" >>= FromJSON.parseJSON
        returnValue := ← (← Value.getFieldOpt v "returnValue").mapM FromJSON.parseJSON
        canBeRestarted := ← (← Value.getFieldOpt v "canBeRestarted").mapM FromJSON.parseJSON }

instance : ToJSON CallFrame where
  toJSON p := Data.Json.object <|
    [ ("callFrameId", ToJSON.toJSON p.callFrameId), ("functionName", ToJSON.toJSON p.functionName) ]
    ++ (p.functionLocation.map fun v => ("functionLocation", ToJSON.toJSON v)).toList
    ++ [("location", ToJSON.toJSON p.location), ("scopeChain", ToJSON.toJSON p.scopeChain)
       , ("this", ToJSON.toJSON p.«this»)]
    ++ (p.returnValue.map fun v => ("returnValue", ToJSON.toJSON v)).toList
    ++ (p.canBeRestarted.map fun v => ("canBeRestarted", ToJSON.toJSON v)).toList

-- ── Search and breakpoints ──

/-- Search match for resource content. -/
structure SearchMatch where
  /-- Line number in resource content. -/
  lineNumber : Float
  /-- Line with match content. -/
  lineContent : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON SearchMatch where
  parseJSON v := do
    .ok
      { lineNumber := ← Value.getField v "lineNumber" >>= FromJSON.parseJSON
        lineContent := ← Value.getField v "lineContent" >>= FromJSON.parseJSON }

instance : ToJSON SearchMatch where
  toJSON p := Data.Json.object
    [("lineNumber", ToJSON.toJSON p.lineNumber), ("lineContent", ToJSON.toJSON p.lineContent)]

/-- Kind of a `BreakLocation`. -/
inductive BreakLocationType where
  | debuggerStatement | call | «return»
  deriving Repr, BEq, DecidableEq

instance : FromJSON BreakLocationType where
  parseJSON
    | .string "debuggerStatement" => .ok .debuggerStatement
    | .string "call" => .ok .call
    | .string "return" => .ok .«return»
    | v => .error s!"failed to parse BreakLocationType: {repr v}"

instance : ToJSON BreakLocationType where
  toJSON
    | .debuggerStatement => .string "debuggerStatement" | .call => .string "call"
    | .«return» => .string "return"

/-- A possible breakpoint location. -/
structure BreakLocation where
  /-- Script identifier as reported in `Debugger.scriptParsed`. -/
  scriptId : Runtime.ScriptId
  /-- Line number in the script (0-based). -/
  lineNumber : Int
  /-- Column number in the script (0-based). -/
  columnNumber : Option Int := none
  type : Option BreakLocationType := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON BreakLocation where
  parseJSON v := do
    .ok
      { scriptId := ← Value.getField v "scriptId" >>= FromJSON.parseJSON
        lineNumber := ← Value.getField v "lineNumber" >>= FromJSON.parseJSON
        columnNumber := ← (← Value.getFieldOpt v "columnNumber").mapM FromJSON.parseJSON
        type := ← (← Value.getFieldOpt v "type").mapM FromJSON.parseJSON }

instance : ToJSON BreakLocation where
  toJSON p := Data.Json.object <|
    [("scriptId", ToJSON.toJSON p.scriptId), ("lineNumber", ToJSON.toJSON p.lineNumber)]
    ++ (p.columnNumber.map fun v => ("columnNumber", ToJSON.toJSON v)).toList
    ++ (p.type.map fun v => ("type", ToJSON.toJSON v)).toList

-- ── WebAssembly ──

/-- A chunk of disassembled WebAssembly module lines. -/
structure WasmDisassemblyChunk where
  /-- The next chunk of disassembled lines. -/
  lines : List String
  /-- The bytecode offsets describing the start of each line. -/
  bytecodeOffsets : List Int
  deriving Repr, BEq, DecidableEq

instance : FromJSON WasmDisassemblyChunk where
  parseJSON v := do
    .ok
      { lines := ← Value.getField v "lines" >>= FromJSON.parseJSON
        bytecodeOffsets := ← Value.getField v "bytecodeOffsets" >>= FromJSON.parseJSON }

instance : ToJSON WasmDisassemblyChunk where
  toJSON p := Data.Json.object
    [("lines", ToJSON.toJSON p.lines), ("bytecodeOffsets", ToJSON.toJSON p.bytecodeOffsets)]

/-- Enum of possible script languages. -/
inductive ScriptLanguage where
  | javaScript | webAssembly
  deriving Repr, BEq, DecidableEq

instance : FromJSON ScriptLanguage where
  parseJSON
    | .string "JavaScript" => .ok .javaScript
    | .string "WebAssembly" => .ok .webAssembly
    | v => .error s!"failed to parse ScriptLanguage: {repr v}"

instance : ToJSON ScriptLanguage where
  toJSON | .javaScript => .string "JavaScript" | .webAssembly => .string "WebAssembly"

/-- Kind of debug symbols available for a wasm script. -/
inductive DebugSymbolsType where
  | none | sourceMap | embeddedDWARF | externalDWARF
  deriving Repr, BEq, DecidableEq

instance : FromJSON DebugSymbolsType where
  parseJSON
    | .string "None" => .ok .none
    | .string "SourceMap" => .ok .sourceMap
    | .string "EmbeddedDWARF" => .ok .embeddedDWARF
    | .string "ExternalDWARF" => .ok .externalDWARF
    | v => .error s!"failed to parse DebugSymbolsType: {repr v}"

instance : ToJSON DebugSymbolsType where
  toJSON
    | .none => .string "None" | .sourceMap => .string "SourceMap"
    | .embeddedDWARF => .string "EmbeddedDWARF" | .externalDWARF => .string "ExternalDWARF"

/-- Debug symbols available for a wasm script. -/
structure DebugSymbols where
  /-- Type of the debug symbols. -/
  type : DebugSymbolsType
  /-- URL of the external symbol source. -/
  externalURL : Option String := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON DebugSymbols where
  parseJSON v := do
    .ok
      { type := ← Value.getField v "type" >>= FromJSON.parseJSON
        externalURL := ← (← Value.getFieldOpt v "externalURL").mapM FromJSON.parseJSON }

instance : ToJSON DebugSymbols where
  toJSON p := Data.Json.object <|
    [("type", ToJSON.toJSON p.type)]
    ++ (p.externalURL.map fun v => ("externalURL", ToJSON.toJSON v)).toList

-- ── Events ──

/-- The `Debugger.breakpointResolved` event: fired when a breakpoint is
    resolved to an actual script and location. -/
structure BreakpointResolved where
  /-- Breakpoint unique identifier. -/
  breakpointId : BreakpointId
  /-- Actual breakpoint location. -/
  location : Location
  deriving Repr, BEq, DecidableEq

instance : FromJSON BreakpointResolved where
  parseJSON v := do
    .ok
      { breakpointId := ← Value.getField v "breakpointId" >>= FromJSON.parseJSON
        location := ← Value.getField v "location" >>= FromJSON.parseJSON }

instance : Event BreakpointResolved where
  eventName := "Debugger.breakpointResolved"

/-- Why the virtual machine paused, as reported by `Debugger.paused`. -/
inductive PausedReason where
  | ambiguous | assert | cspViolation | debugCommand | dom | eventListener | exception
  | instrumentation | oom | other | promiseRejection | xhr
  deriving Repr, BEq, DecidableEq

instance : FromJSON PausedReason where
  parseJSON
    | .string "ambiguous" => .ok .ambiguous
    | .string "assert" => .ok .assert
    | .string "CSPViolation" => .ok .cspViolation
    | .string "debugCommand" => .ok .debugCommand
    | .string "DOM" => .ok .dom
    | .string "EventListener" => .ok .eventListener
    | .string "exception" => .ok .exception
    | .string "instrumentation" => .ok .instrumentation
    | .string "OOM" => .ok .oom
    | .string "other" => .ok .other
    | .string "promiseRejection" => .ok .promiseRejection
    | .string "XHR" => .ok .xhr
    | v => .error s!"failed to parse PausedReason: {repr v}"

instance : ToJSON PausedReason where
  toJSON
    | .ambiguous => .string "ambiguous" | .assert => .string "assert"
    | .cspViolation => .string "CSPViolation" | .debugCommand => .string "debugCommand"
    | .dom => .string "DOM" | .eventListener => .string "EventListener"
    | .exception => .string "exception" | .instrumentation => .string "instrumentation"
    | .oom => .string "OOM" | .other => .string "other"
    | .promiseRejection => .string "promiseRejection" | .xhr => .string "XHR"

/-- The `Debugger.paused` event: fired when the virtual machine stopped on
    breakpoint or exception or any other stop criteria. -/
structure Paused where
  /-- Call stack the virtual machine stopped on. -/
  callFrames : List CallFrame
  /-- Pause reason. -/
  reason : PausedReason
  /-- Object containing break-specific auxiliary properties. -/
  data : Option (List (String × String)) := none
  /-- Hit breakpoint ids. -/
  hitBreakpoints : Option (List String) := none
  /-- Async stack trace, if any. -/
  asyncStackTrace : Option Runtime.StackTrace := none
  /-- Async stack trace, if any. -/
  asyncStackTraceId : Option Runtime.StackTraceId := none
  deriving Repr, BEq

instance : FromJSON Paused where
  parseJSON v := do
    .ok
      { callFrames := ← Value.getField v "callFrames" >>= FromJSON.parseJSON
        reason := ← Value.getField v "reason" >>= FromJSON.parseJSON
        data := ← (← Value.getFieldOpt v "data").mapM FromJSON.parseJSON
        hitBreakpoints := ← (← Value.getFieldOpt v "hitBreakpoints").mapM FromJSON.parseJSON
        asyncStackTrace := ← (← Value.getFieldOpt v "asyncStackTrace").mapM FromJSON.parseJSON
        asyncStackTraceId := ← (← Value.getFieldOpt v "asyncStackTraceId").mapM FromJSON.parseJSON }

instance : Event Paused where
  eventName := "Debugger.paused"

/-- The `Debugger.resumed` event: fired when the virtual machine resumed
    execution. -/
structure Resumed where
  deriving Repr, BEq, DecidableEq

instance : FromJSON Resumed where parseJSON _ := .ok {}

instance : Event Resumed where
  eventName := "Debugger.resumed"

/-- The `Debugger.scriptFailedToParse` event: fired when a virtual machine
    fails to parse a script. -/
structure ScriptFailedToParse where
  /-- Identifier of the script parsed. -/
  scriptId : Runtime.ScriptId
  /-- URL or name of the script parsed (if any). -/
  url : String
  /-- Line offset of the script within the resource with given URL (for
      script tags). -/
  startLine : Int
  /-- Column offset of the script within the resource with given URL. -/
  startColumn : Int
  /-- Last line of the script. -/
  endLine : Int
  /-- Length of the last line of the script. -/
  endColumn : Int
  /-- Specifies script creation context. -/
  executionContextId : Runtime.ExecutionContextId
  /-- Content hash of the script, SHA-256. -/
  hash : String
  /-- Embedder-specific auxiliary data. -/
  executionContextAuxData : Option (List (String × String)) := none
  /-- URL of source map associated with script (if any). -/
  sourceMapURL : Option String := none
  /-- `true` if this script has a `sourceURL`. -/
  hasSourceURL : Option Bool := none
  /-- `true` if this script is an ES6 module. -/
  isModule : Option Bool := none
  /-- This script's length. -/
  length : Option Int := none
  /-- JavaScript top stack frame of where the script parsed event was
      triggered, if available. -/
  stackTrace : Option Runtime.StackTrace := none
  /-- If the scriptLanguage is WebAssembly, the code section offset in the
      module. -/
  codeOffset : Option Int := none
  /-- The language of the script. -/
  scriptLanguage : Option ScriptLanguage := none
  /-- The name the embedder supplied for this script. -/
  embedderName : Option String := none
  deriving Repr, BEq

instance : FromJSON ScriptFailedToParse where
  parseJSON v := do
    .ok
      { scriptId := ← Value.getField v "scriptId" >>= FromJSON.parseJSON
        url := ← Value.getField v "url" >>= FromJSON.parseJSON
        startLine := ← Value.getField v "startLine" >>= FromJSON.parseJSON
        startColumn := ← Value.getField v "startColumn" >>= FromJSON.parseJSON
        endLine := ← Value.getField v "endLine" >>= FromJSON.parseJSON
        endColumn := ← Value.getField v "endColumn" >>= FromJSON.parseJSON
        executionContextId := ← Value.getField v "executionContextId" >>= FromJSON.parseJSON
        hash := ← Value.getField v "hash" >>= FromJSON.parseJSON
        executionContextAuxData :=
          ← (← Value.getFieldOpt v "executionContextAuxData").mapM FromJSON.parseJSON
        sourceMapURL := ← (← Value.getFieldOpt v "sourceMapURL").mapM FromJSON.parseJSON
        hasSourceURL := ← (← Value.getFieldOpt v "hasSourceURL").mapM FromJSON.parseJSON
        isModule := ← (← Value.getFieldOpt v "isModule").mapM FromJSON.parseJSON
        length := ← (← Value.getFieldOpt v "length").mapM FromJSON.parseJSON
        stackTrace := ← (← Value.getFieldOpt v "stackTrace").mapM FromJSON.parseJSON
        codeOffset := ← (← Value.getFieldOpt v "codeOffset").mapM FromJSON.parseJSON
        scriptLanguage := ← (← Value.getFieldOpt v "scriptLanguage").mapM FromJSON.parseJSON
        embedderName := ← (← Value.getFieldOpt v "embedderName").mapM FromJSON.parseJSON }

instance : Event ScriptFailedToParse where
  eventName := "Debugger.scriptFailedToParse"

/-- The `Debugger.scriptParsed` event: fired when a virtual machine parses
    script. This event is also fired for all known and uncollected scripts
    upon enabling debugger. -/
structure ScriptParsed where
  /-- Identifier of the script parsed. -/
  scriptId : Runtime.ScriptId
  /-- URL or name of the script parsed (if any). -/
  url : String
  /-- Line offset of the script within the resource with given URL (for
      script tags). -/
  startLine : Int
  /-- Column offset of the script within the resource with given URL. -/
  startColumn : Int
  /-- Last line of the script. -/
  endLine : Int
  /-- Length of the last line of the script. -/
  endColumn : Int
  /-- Specifies script creation context. -/
  executionContextId : Runtime.ExecutionContextId
  /-- Content hash of the script, SHA-256. -/
  hash : String
  /-- Embedder-specific auxiliary data. -/
  executionContextAuxData : Option (List (String × String)) := none
  /-- `true` if this script is generated as a result of the live edit
      operation. -/
  isLiveEdit : Option Bool := none
  /-- URL of source map associated with script (if any). -/
  sourceMapURL : Option String := none
  /-- `true` if this script has a `sourceURL`. -/
  hasSourceURL : Option Bool := none
  /-- `true` if this script is an ES6 module. -/
  isModule : Option Bool := none
  /-- This script's length. -/
  length : Option Int := none
  /-- JavaScript top stack frame of where the script parsed event was
      triggered, if available. -/
  stackTrace : Option Runtime.StackTrace := none
  /-- If the scriptLanguage is WebAssembly, the code section offset in the
      module. -/
  codeOffset : Option Int := none
  /-- The language of the script. -/
  scriptLanguage : Option ScriptLanguage := none
  /-- If the scriptLanguage is WebAssembly, the source of debug symbols for
      the module. -/
  debugSymbols : Option DebugSymbols := none
  /-- The name the embedder supplied for this script. -/
  embedderName : Option String := none
  deriving Repr, BEq

instance : FromJSON ScriptParsed where
  parseJSON v := do
    .ok
      { scriptId := ← Value.getField v "scriptId" >>= FromJSON.parseJSON
        url := ← Value.getField v "url" >>= FromJSON.parseJSON
        startLine := ← Value.getField v "startLine" >>= FromJSON.parseJSON
        startColumn := ← Value.getField v "startColumn" >>= FromJSON.parseJSON
        endLine := ← Value.getField v "endLine" >>= FromJSON.parseJSON
        endColumn := ← Value.getField v "endColumn" >>= FromJSON.parseJSON
        executionContextId := ← Value.getField v "executionContextId" >>= FromJSON.parseJSON
        hash := ← Value.getField v "hash" >>= FromJSON.parseJSON
        executionContextAuxData :=
          ← (← Value.getFieldOpt v "executionContextAuxData").mapM FromJSON.parseJSON
        isLiveEdit := ← (← Value.getFieldOpt v "isLiveEdit").mapM FromJSON.parseJSON
        sourceMapURL := ← (← Value.getFieldOpt v "sourceMapURL").mapM FromJSON.parseJSON
        hasSourceURL := ← (← Value.getFieldOpt v "hasSourceURL").mapM FromJSON.parseJSON
        isModule := ← (← Value.getFieldOpt v "isModule").mapM FromJSON.parseJSON
        length := ← (← Value.getFieldOpt v "length").mapM FromJSON.parseJSON
        stackTrace := ← (← Value.getFieldOpt v "stackTrace").mapM FromJSON.parseJSON
        codeOffset := ← (← Value.getFieldOpt v "codeOffset").mapM FromJSON.parseJSON
        scriptLanguage := ← (← Value.getFieldOpt v "scriptLanguage").mapM FromJSON.parseJSON
        debugSymbols := ← (← Value.getFieldOpt v "debugSymbols").mapM FromJSON.parseJSON
        embedderName := ← (← Value.getFieldOpt v "embedderName").mapM FromJSON.parseJSON }

instance : Event ScriptParsed where
  eventName := "Debugger.scriptParsed"

-- ── Commands ──

/-- Which call frames `Debugger.continueToLocation` should apply to. -/
inductive TargetCallFrames where
  | any | current
  deriving Repr, BEq, DecidableEq

instance : FromJSON TargetCallFrames where
  parseJSON
    | .string "any" => .ok .any
    | .string "current" => .ok .current
    | v => .error s!"failed to parse TargetCallFrames: {repr v}"

instance : ToJSON TargetCallFrames where
  toJSON | .any => .string "any" | .current => .string "current"

/-- Parameters of the `Debugger.continueToLocation` command: continues
    execution until a specific location is reached. -/
structure PContinueToLocation where
  /-- Location to continue to. -/
  location : Location
  targetCallFrames : Option TargetCallFrames := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PContinueToLocation where
  toJSON p := Data.Json.object <|
    [("location", ToJSON.toJSON p.location)]
    ++ (p.targetCallFrames.map fun v => ("targetCallFrames", ToJSON.toJSON v)).toList

instance : Command PContinueToLocation where
  Response := Unit
  commandName _ := "Debugger.continueToLocation"
  decodeResponse _ := .ok ()

/-- Parameters of the `Debugger.disable` command: disables debugger for the
    given page. -/
structure PDisable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PDisable where toJSON _ := .null

instance : Command PDisable where
  Response := Unit
  commandName _ := "Debugger.disable"
  decodeResponse _ := .ok ()

/-- Parameters of the `Debugger.enable` command: enables debugger for the
    given page. Clients should not assume that debugging has been enabled
    until the result for this command is received. -/
structure PEnable where
  /-- The maximum size in bytes of collected scripts (not referenced by
      other heap objects) the debugger can hold. Puts no limit if the
      parameter is omitted. -/
  maxScriptsCacheSize : Option Float := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PEnable where
  toJSON p := Data.Json.object <|
    (p.maxScriptsCacheSize.map fun v => ("maxScriptsCacheSize", ToJSON.toJSON v)).toList

/-- Response of the `Debugger.enable` command. -/
structure Enable where
  /-- Unique identifier of the debugger. -/
  debuggerId : Runtime.UniqueDebuggerId
  deriving Repr, BEq, DecidableEq

instance : FromJSON Enable where
  parseJSON v := do .ok { debuggerId := ← Value.getField v "debuggerId" >>= FromJSON.parseJSON }

instance : Command PEnable where
  Response := Enable
  commandName _ := "Debugger.enable"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Debugger.evaluateOnCallFrame` command: evaluates an
    expression on a given call frame. -/
structure PEvaluateOnCallFrame where
  /-- Call frame identifier to evaluate on. -/
  callFrameId : CallFrameId
  /-- Expression to evaluate. -/
  expression : String
  /-- String object group name to put the result into (allows rapid
      releasing resulting object handles using `releaseObjectGroup`). -/
  objectGroup : Option String := none
  /-- Specifies whether command line API should be available to the
      evaluated expression, defaults to `false`. -/
  includeCommandLineAPI : Option Bool := none
  /-- In silent mode exceptions thrown during evaluation are not reported
      and do not pause execution. Overrides `setPauseOnException` state. -/
  silent : Option Bool := none
  /-- Whether the result is expected to be a JSON object that should be sent
      by value. -/
  returnByValue : Option Bool := none
  /-- Whether preview should be generated for the result. -/
  generatePreview : Option Bool := none
  /-- Whether to throw an exception if a side effect cannot be ruled out
      during evaluation. -/
  throwOnSideEffect : Option Bool := none
  /-- Terminate execution after timing out (number of milliseconds). -/
  timeout : Option Runtime.TimeDelta := none
  deriving Repr, BEq

instance : ToJSON PEvaluateOnCallFrame where
  toJSON p := Data.Json.object <|
    [("callFrameId", ToJSON.toJSON p.callFrameId), ("expression", ToJSON.toJSON p.expression)]
    ++ (p.objectGroup.map fun v => ("objectGroup", ToJSON.toJSON v)).toList
    ++ (p.includeCommandLineAPI.map fun v => ("includeCommandLineAPI", ToJSON.toJSON v)).toList
    ++ (p.silent.map fun v => ("silent", ToJSON.toJSON v)).toList
    ++ (p.returnByValue.map fun v => ("returnByValue", ToJSON.toJSON v)).toList
    ++ (p.generatePreview.map fun v => ("generatePreview", ToJSON.toJSON v)).toList
    ++ (p.throwOnSideEffect.map fun v => ("throwOnSideEffect", ToJSON.toJSON v)).toList
    ++ (p.timeout.map fun v => ("timeout", ToJSON.toJSON v)).toList

/-- Response of the `Debugger.evaluateOnCallFrame` command. -/
structure EvaluateOnCallFrame where
  /-- Object wrapper for the evaluation result. -/
  result : Runtime.RemoteObject
  /-- Exception details. -/
  exceptionDetails : Option Runtime.ExceptionDetails := none
  deriving Repr, BEq

instance : FromJSON EvaluateOnCallFrame where
  parseJSON v := do
    .ok
      { result := ← Value.getField v "result" >>= FromJSON.parseJSON
        exceptionDetails := ← (← Value.getFieldOpt v "exceptionDetails").mapM FromJSON.parseJSON }

instance : Command PEvaluateOnCallFrame where
  Response := EvaluateOnCallFrame
  commandName _ := "Debugger.evaluateOnCallFrame"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Debugger.getPossibleBreakpoints` command: returns
    possible locations for a breakpoint. `scriptId` in the start and end
    range locations should be the same. -/
structure PGetPossibleBreakpoints where
  /-- Start of range to search possible breakpoint locations in. -/
  start : Location
  /-- End of range to search possible breakpoint locations in (excluding).
      When not specified, the end of the script is used as the end of the
      range. -/
  «end» : Option Location := none
  /-- Only consider locations which are in the same (non-nested) function as
      `start`. -/
  restrictToFunction : Option Bool := none
  deriving Repr, BEq

instance : ToJSON PGetPossibleBreakpoints where
  toJSON p := Data.Json.object <|
    [("start", ToJSON.toJSON p.start)]
    ++ (p.«end».map fun v => ("end", ToJSON.toJSON v)).toList
    ++ (p.restrictToFunction.map fun v => ("restrictToFunction", ToJSON.toJSON v)).toList

/-- Response of the `Debugger.getPossibleBreakpoints` command. -/
structure GetPossibleBreakpoints where
  /-- List of the possible breakpoint locations. -/
  locations : List BreakLocation
  deriving Repr, BEq, DecidableEq

instance : FromJSON GetPossibleBreakpoints where
  parseJSON v := do .ok { locations := ← Value.getField v "locations" >>= FromJSON.parseJSON }

instance : Command PGetPossibleBreakpoints where
  Response := GetPossibleBreakpoints
  commandName _ := "Debugger.getPossibleBreakpoints"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Debugger.getScriptSource` command: returns the source
    for the script with the given id. -/
structure PGetScriptSource where
  /-- Id of the script to get the source for. -/
  scriptId : Runtime.ScriptId
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetScriptSource where
  toJSON p := Data.Json.object [("scriptId", ToJSON.toJSON p.scriptId)]

/-- Response of the `Debugger.getScriptSource` command. -/
structure GetScriptSource where
  /-- Script source (empty in case of Wasm bytecode). -/
  scriptSource : String
  /-- Wasm bytecode. (Encoded as a base64 string when passed over JSON.) -/
  bytecode : Option String := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON GetScriptSource where
  parseJSON v := do
    .ok
      { scriptSource := ← Value.getField v "scriptSource" >>= FromJSON.parseJSON
        bytecode := ← (← Value.getFieldOpt v "bytecode").mapM FromJSON.parseJSON }

instance : Command PGetScriptSource where
  Response := GetScriptSource
  commandName _ := "Debugger.getScriptSource"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Debugger.disassembleWasmModule` command. -/
structure PDisassembleWasmModule where
  /-- Id of the script to disassemble. -/
  scriptId : Runtime.ScriptId
  deriving Repr, BEq, DecidableEq

instance : ToJSON PDisassembleWasmModule where
  toJSON p := Data.Json.object [("scriptId", ToJSON.toJSON p.scriptId)]

/-- Response of the `Debugger.disassembleWasmModule` command. -/
structure DisassembleWasmModule where
  /-- For large modules, a stream from which additional chunks of
      disassembly can be read successively. -/
  streamId : Option String := none
  /-- The total number of lines in the disassembly text. -/
  totalNumberOfLines : Int
  /-- The offsets of all function bodies, in the format `[start1, end1,
      start2, end2, ...]` where all ends are exclusive. -/
  functionBodyOffsets : List Int
  /-- The first chunk of disassembly. -/
  chunk : WasmDisassemblyChunk
  deriving Repr, BEq, DecidableEq

instance : FromJSON DisassembleWasmModule where
  parseJSON v := do
    .ok
      { streamId := ← (← Value.getFieldOpt v "streamId").mapM FromJSON.parseJSON
        totalNumberOfLines := ← Value.getField v "totalNumberOfLines" >>= FromJSON.parseJSON
        functionBodyOffsets := ← Value.getField v "functionBodyOffsets" >>= FromJSON.parseJSON
        chunk := ← Value.getField v "chunk" >>= FromJSON.parseJSON }

instance : Command PDisassembleWasmModule where
  Response := DisassembleWasmModule
  commandName _ := "Debugger.disassembleWasmModule"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Debugger.nextWasmDisassemblyChunk` command:
    disassemble the next chunk of lines for the module corresponding to the
    stream. If disassembly is complete, this invalidates the `streamId` and
    returns an empty chunk. Any subsequent calls for the now-invalid stream
    return errors. -/
structure PNextWasmDisassemblyChunk where
  streamId : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PNextWasmDisassemblyChunk where
  toJSON p := Data.Json.object [("streamId", ToJSON.toJSON p.streamId)]

/-- Response of the `Debugger.nextWasmDisassemblyChunk` command. -/
structure NextWasmDisassemblyChunk where
  /-- The next chunk of disassembly. -/
  chunk : WasmDisassemblyChunk
  deriving Repr, BEq, DecidableEq

instance : FromJSON NextWasmDisassemblyChunk where
  parseJSON v := do .ok { chunk := ← Value.getField v "chunk" >>= FromJSON.parseJSON }

instance : Command PNextWasmDisassemblyChunk where
  Response := NextWasmDisassemblyChunk
  commandName _ := "Debugger.nextWasmDisassemblyChunk"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Debugger.getStackTrace` command: returns the stack
    trace with the given `stackTraceId`. -/
structure PGetStackTrace where
  stackTraceId : Runtime.StackTraceId
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetStackTrace where
  toJSON p := Data.Json.object [("stackTraceId", ToJSON.toJSON p.stackTraceId)]

/-- Response of the `Debugger.getStackTrace` command. -/
structure GetStackTrace where
  stackTrace : Runtime.StackTrace
  deriving Repr, BEq

instance : FromJSON GetStackTrace where
  parseJSON v := do .ok { stackTrace := ← Value.getField v "stackTrace" >>= FromJSON.parseJSON }

instance : Command PGetStackTrace where
  Response := GetStackTrace
  commandName _ := "Debugger.getStackTrace"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Debugger.pause` command: stops on the next JavaScript
    statement. -/
structure PPause where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PPause where toJSON _ := .null

instance : Command PPause where
  Response := Unit
  commandName _ := "Debugger.pause"
  decodeResponse _ := .ok ()

/-- Parameters of the `Debugger.removeBreakpoint` command: removes a
    JavaScript breakpoint. -/
structure PRemoveBreakpoint where
  breakpointId : BreakpointId
  deriving Repr, BEq, DecidableEq

instance : ToJSON PRemoveBreakpoint where
  toJSON p := Data.Json.object [("breakpointId", ToJSON.toJSON p.breakpointId)]

instance : Command PRemoveBreakpoint where
  Response := Unit
  commandName _ := "Debugger.removeBreakpoint"
  decodeResponse _ := .ok ()

/-- The (only) supported mode for `Debugger.restartFrame`. -/
inductive RestartFrameMode where
  | stepInto
  deriving Repr, BEq, DecidableEq

instance : FromJSON RestartFrameMode where
  parseJSON
    | .string "StepInto" => .ok .stepInto
    | v => .error s!"failed to parse RestartFrameMode: {repr v}"

instance : ToJSON RestartFrameMode where
  toJSON | .stepInto => .string "StepInto"

/-- Parameters of the `Debugger.restartFrame` command: restarts a particular
    call frame from the beginning.

    The old, deprecated behavior of `restartFrame` was to stay paused and
    allow further CDP commands after a restart was scheduled. This could
    cause problems with restarting, so it now continues execution
    immediately after the restart is scheduled, until the beginning of the
    restarted frame is reached.

    To stay backwards compatible, `restartFrame` now expects a `mode`
    parameter to be present; if `mode` is missing, `restartFrame` errors
    out.

    The various return values are deprecated and `callFrames` is always
    empty. Use the call frames from the `Debugger.paused` event instead,
    which fires once V8 pauses at the beginning of the restarted
    function. -/
structure PRestartFrame where
  /-- Call frame identifier to evaluate on. -/
  callFrameId : CallFrameId
  /-- The `mode` parameter must be present and set to `stepInto`, otherwise
      `restartFrame` will error out. -/
  mode : Option RestartFrameMode := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PRestartFrame where
  toJSON p := Data.Json.object <|
    [("callFrameId", ToJSON.toJSON p.callFrameId)]
    ++ (p.mode.map fun v => ("mode", ToJSON.toJSON v)).toList

instance : Command PRestartFrame where
  Response := Unit
  commandName _ := "Debugger.restartFrame"
  decodeResponse _ := .ok ()

/-- Parameters of the `Debugger.resume` command: resumes JavaScript
    execution. -/
structure PResume where
  /-- Set to `true` to terminate execution upon resuming execution. In
      contrast to `Runtime.terminateExecution`, this allows further
      JavaScript to execute (e.g. via evaluation) until execution of the
      paused code is actually resumed, at which point termination is
      triggered. If execution is currently not paused, this parameter has no
      effect. -/
  terminateOnResume : Option Bool := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PResume where
  toJSON p := Data.Json.object <|
    (p.terminateOnResume.map fun v => ("terminateOnResume", ToJSON.toJSON v)).toList

instance : Command PResume where
  Response := Unit
  commandName _ := "Debugger.resume"
  decodeResponse _ := .ok ()

/-- Parameters of the `Debugger.searchInContent` command: searches for a
    given string in script content. -/
structure PSearchInContent where
  /-- Id of the script to search in. -/
  scriptId : Runtime.ScriptId
  /-- String to search for. -/
  query : String
  /-- If `true`, search is case sensitive. -/
  caseSensitive : Option Bool := none
  /-- If `true`, treats the string parameter as a regex. -/
  isRegex : Option Bool := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSearchInContent where
  toJSON p := Data.Json.object <|
    [("scriptId", ToJSON.toJSON p.scriptId), ("query", ToJSON.toJSON p.query)]
    ++ (p.caseSensitive.map fun v => ("caseSensitive", ToJSON.toJSON v)).toList
    ++ (p.isRegex.map fun v => ("isRegex", ToJSON.toJSON v)).toList

/-- Response of the `Debugger.searchInContent` command. -/
structure SearchInContent where
  /-- List of search matches. -/
  result : List SearchMatch
  deriving Repr, BEq, DecidableEq

instance : FromJSON SearchInContent where
  parseJSON v := do .ok { result := ← Value.getField v "result" >>= FromJSON.parseJSON }

instance : Command PSearchInContent where
  Response := SearchInContent
  commandName _ := "Debugger.searchInContent"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Debugger.setAsyncCallStackDepth` command: enables or
    disables async call stacks tracking. -/
structure PSetAsyncCallStackDepth where
  /-- Maximum depth of async call stacks. Setting to `0` effectively
      disables collecting async call stacks (the default). -/
  maxDepth : Int
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetAsyncCallStackDepth where
  toJSON p := Data.Json.object [("maxDepth", ToJSON.toJSON p.maxDepth)]

instance : Command PSetAsyncCallStackDepth where
  Response := Unit
  commandName _ := "Debugger.setAsyncCallStackDepth"
  decodeResponse _ := .ok ()

/-- Parameters of the `Debugger.setBlackboxPatterns` command: replaces
    previous blackbox patterns with the passed ones. Forces the backend to
    skip stepping/pausing in scripts with a url matching one of the
    patterns. The VM will try to leave a blackboxed script by performing
    "step in" several times, finally resorting to "step out" if
    unsuccessful. -/
structure PSetBlackboxPatterns where
  /-- Array of regexps used to check a script url for blackbox state. -/
  patterns : List String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetBlackboxPatterns where
  toJSON p := Data.Json.object [("patterns", ToJSON.toJSON p.patterns)]

instance : Command PSetBlackboxPatterns where
  Response := Unit
  commandName _ := "Debugger.setBlackboxPatterns"
  decodeResponse _ := .ok ()

/-- Parameters of the `Debugger.setBlackboxedRanges` command: makes the
    backend skip steps in the script in blackboxed ranges. The VM will try
    to leave blacklisted scripts by performing "step in" several times,
    finally resorting to "step out" if unsuccessful. `positions` contains
    positions where the blackbox state changes; the first interval isn't
    blackboxed and the array should be sorted. -/
structure PSetBlackboxedRanges where
  /-- Id of the script. -/
  scriptId : Runtime.ScriptId
  positions : List ScriptPosition
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetBlackboxedRanges where
  toJSON p := Data.Json.object
    [("scriptId", ToJSON.toJSON p.scriptId), ("positions", ToJSON.toJSON p.positions)]

instance : Command PSetBlackboxedRanges where
  Response := Unit
  commandName _ := "Debugger.setBlackboxedRanges"
  decodeResponse _ := .ok ()

/-- Parameters of the `Debugger.setBreakpoint` command: sets a JavaScript
    breakpoint at a given location. -/
structure PSetBreakpoint where
  /-- Location to set the breakpoint at. -/
  location : Location
  /-- Expression to use as a breakpoint condition. When specified, the
      debugger only stops on the breakpoint if this expression evaluates to
      true. -/
  condition : Option String := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetBreakpoint where
  toJSON p := Data.Json.object <|
    [("location", ToJSON.toJSON p.location)]
    ++ (p.condition.map fun v => ("condition", ToJSON.toJSON v)).toList

/-- Response of the `Debugger.setBreakpoint` command. -/
structure SetBreakpoint where
  /-- Id of the created breakpoint for further reference. -/
  breakpointId : BreakpointId
  /-- Location this breakpoint resolved into. -/
  actualLocation : Location
  deriving Repr, BEq, DecidableEq

instance : FromJSON SetBreakpoint where
  parseJSON v := do
    .ok
      { breakpointId := ← Value.getField v "breakpointId" >>= FromJSON.parseJSON
        actualLocation := ← Value.getField v "actualLocation" >>= FromJSON.parseJSON }

instance : Command PSetBreakpoint where
  Response := SetBreakpoint
  commandName _ := "Debugger.setBreakpoint"
  decodeResponse := FromJSON.parseJSON

/-- Which instrumentation to break on, for `Debugger.setInstrumentationBreakpoint`. -/
inductive Instrumentation where
  | beforeScriptExecution | beforeScriptWithSourceMapExecution
  deriving Repr, BEq, DecidableEq

instance : FromJSON Instrumentation where
  parseJSON
    | .string "beforeScriptExecution" => .ok .beforeScriptExecution
    | .string "beforeScriptWithSourceMapExecution" => .ok .beforeScriptWithSourceMapExecution
    | v => .error s!"failed to parse Instrumentation: {repr v}"

instance : ToJSON Instrumentation where
  toJSON
    | .beforeScriptExecution => .string "beforeScriptExecution"
    | .beforeScriptWithSourceMapExecution => .string "beforeScriptWithSourceMapExecution"

/-- Parameters of the `Debugger.setInstrumentationBreakpoint` command: sets
    an instrumentation breakpoint. -/
structure PSetInstrumentationBreakpoint where
  /-- Instrumentation name. -/
  instrumentation : Instrumentation
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetInstrumentationBreakpoint where
  toJSON p := Data.Json.object [("instrumentation", ToJSON.toJSON p.instrumentation)]

/-- Response of the `Debugger.setInstrumentationBreakpoint` command. -/
structure SetInstrumentationBreakpoint where
  /-- Id of the created breakpoint for further reference. -/
  breakpointId : BreakpointId
  deriving Repr, BEq, DecidableEq

instance : FromJSON SetInstrumentationBreakpoint where
  parseJSON v := do .ok { breakpointId := ← Value.getField v "breakpointId" >>= FromJSON.parseJSON }

instance : Command PSetInstrumentationBreakpoint where
  Response := SetInstrumentationBreakpoint
  commandName _ := "Debugger.setInstrumentationBreakpoint"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Debugger.setBreakpointByUrl` command: sets a
    JavaScript breakpoint at a given location specified either by URL or URL
    regex. Once this command is issued, all existing parsed scripts will
    have breakpoints resolved and returned in the `locations` property.
    Further matching script parsing will result in subsequent
    `breakpointResolved` events being issued. This logical breakpoint
    survives page reloads. -/
structure PSetBreakpointByUrl where
  /-- Line number to set the breakpoint at. -/
  lineNumber : Int
  /-- URL of the resources to set the breakpoint on. -/
  url : Option String := none
  /-- Regex pattern for the URLs of the resources to set breakpoints on.
      Either `url` or `urlRegex` must be specified. -/
  urlRegex : Option String := none
  /-- Script hash of the resources to set the breakpoint on. -/
  scriptHash : Option String := none
  /-- Offset in the line to set the breakpoint at. -/
  columnNumber : Option Int := none
  /-- Expression to use as a breakpoint condition. When specified, the
      debugger only stops on the breakpoint if this expression evaluates to
      true. -/
  condition : Option String := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetBreakpointByUrl where
  toJSON p := Data.Json.object <|
    [("lineNumber", ToJSON.toJSON p.lineNumber)]
    ++ (p.url.map fun v => ("url", ToJSON.toJSON v)).toList
    ++ (p.urlRegex.map fun v => ("urlRegex", ToJSON.toJSON v)).toList
    ++ (p.scriptHash.map fun v => ("scriptHash", ToJSON.toJSON v)).toList
    ++ (p.columnNumber.map fun v => ("columnNumber", ToJSON.toJSON v)).toList
    ++ (p.condition.map fun v => ("condition", ToJSON.toJSON v)).toList

/-- Response of the `Debugger.setBreakpointByUrl` command. -/
structure SetBreakpointByUrl where
  /-- Id of the created breakpoint for further reference. -/
  breakpointId : BreakpointId
  /-- List of the locations this breakpoint resolved into upon addition. -/
  locations : List Location
  deriving Repr, BEq, DecidableEq

instance : FromJSON SetBreakpointByUrl where
  parseJSON v := do
    .ok
      { breakpointId := ← Value.getField v "breakpointId" >>= FromJSON.parseJSON
        locations := ← Value.getField v "locations" >>= FromJSON.parseJSON }

instance : Command PSetBreakpointByUrl where
  Response := SetBreakpointByUrl
  commandName _ := "Debugger.setBreakpointByUrl"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Debugger.setBreakpointOnFunctionCall` command: sets a
    JavaScript breakpoint before each call to the given function. If another
    function was created from the same source as a given one, calling it
    will also trigger the breakpoint. -/
structure PSetBreakpointOnFunctionCall where
  /-- Function object id. -/
  objectId : Runtime.RemoteObjectId
  /-- Expression to use as a breakpoint condition. When specified, the
      debugger stops on the breakpoint if this expression evaluates to
      true. -/
  condition : Option String := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetBreakpointOnFunctionCall where
  toJSON p := Data.Json.object <|
    [("objectId", ToJSON.toJSON p.objectId)]
    ++ (p.condition.map fun v => ("condition", ToJSON.toJSON v)).toList

/-- Response of the `Debugger.setBreakpointOnFunctionCall` command. -/
structure SetBreakpointOnFunctionCall where
  /-- Id of the created breakpoint for further reference. -/
  breakpointId : BreakpointId
  deriving Repr, BEq, DecidableEq

instance : FromJSON SetBreakpointOnFunctionCall where
  parseJSON v := do .ok { breakpointId := ← Value.getField v "breakpointId" >>= FromJSON.parseJSON }

instance : Command PSetBreakpointOnFunctionCall where
  Response := SetBreakpointOnFunctionCall
  commandName _ := "Debugger.setBreakpointOnFunctionCall"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Debugger.setBreakpointsActive` command: activates /
    deactivates all breakpoints on the page. -/
structure PSetBreakpointsActive where
  /-- New value for the breakpoints active state. -/
  active : Bool
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetBreakpointsActive where
  toJSON p := Data.Json.object [("active", ToJSON.toJSON p.active)]

instance : Command PSetBreakpointsActive where
  Response := Unit
  commandName _ := "Debugger.setBreakpointsActive"
  decodeResponse _ := .ok ()

/-- Pause-on-exceptions mode for `Debugger.setPauseOnExceptions`. -/
inductive PauseOnExceptionsState where
  | none | uncaught | all
  deriving Repr, BEq, DecidableEq

instance : FromJSON PauseOnExceptionsState where
  parseJSON
    | .string "none" => .ok .none
    | .string "uncaught" => .ok .uncaught
    | .string "all" => .ok .all
    | v => .error s!"failed to parse PauseOnExceptionsState: {repr v}"

instance : ToJSON PauseOnExceptionsState where
  toJSON | .none => .string "none" | .uncaught => .string "uncaught" | .all => .string "all"

/-- Parameters of the `Debugger.setPauseOnExceptions` command: defines the
    pause-on-exceptions state. Can be set to stop on all exceptions,
    uncaught exceptions, or no exceptions. The initial pause-on-exceptions
    state is `none`. -/
structure PSetPauseOnExceptions where
  /-- Pause on exceptions mode. -/
  state : PauseOnExceptionsState
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetPauseOnExceptions where
  toJSON p := Data.Json.object [("state", ToJSON.toJSON p.state)]

instance : Command PSetPauseOnExceptions where
  Response := Unit
  commandName _ := "Debugger.setPauseOnExceptions"
  decodeResponse _ := .ok ()

/-- Parameters of the `Debugger.setReturnValue` command: changes the return
    value in the top frame. Available only at a return break position. -/
structure PSetReturnValue where
  /-- New return value. -/
  newValue : Runtime.CallArgument
  deriving Repr, BEq

instance : ToJSON PSetReturnValue where
  toJSON p := Data.Json.object [("newValue", ToJSON.toJSON p.newValue)]

instance : Command PSetReturnValue where
  Response := Unit
  commandName _ := "Debugger.setReturnValue"
  decodeResponse _ := .ok ()

/-- Status of a `Debugger.setScriptSource` live-edit attempt. -/
inductive SetScriptSourceStatus where
  | ok | compileError | blockedByActiveGenerator | blockedByActiveFunction
  deriving Repr, BEq, DecidableEq

instance : FromJSON SetScriptSourceStatus where
  parseJSON
    | .string "Ok" => .ok .ok
    | .string "CompileError" => .ok .compileError
    | .string "BlockedByActiveGenerator" => .ok .blockedByActiveGenerator
    | .string "BlockedByActiveFunction" => .ok .blockedByActiveFunction
    | v => .error s!"failed to parse SetScriptSourceStatus: {repr v}"

instance : ToJSON SetScriptSourceStatus where
  toJSON
    | .ok => .string "Ok" | .compileError => .string "CompileError"
    | .blockedByActiveGenerator => .string "BlockedByActiveGenerator"
    | .blockedByActiveFunction => .string "BlockedByActiveFunction"

/-- Parameters of the `Debugger.setScriptSource` command: edits JavaScript
    source live.

    In general, functions that are currently on the stack can not be edited,
    with a single exception: if the edited function is the top-most stack
    frame and that is the only activation of that function on the stack, the
    live edit will be successful and a `Debugger.restartFrame` for the
    top-most function is automatically triggered. -/
structure PSetScriptSource where
  /-- Id of the script to edit. -/
  scriptId : Runtime.ScriptId
  /-- New content of the script. -/
  scriptSource : String
  /-- If `true` the change will not actually be applied. A dry run may be
      used to get the result description without actually modifying the
      code. -/
  dryRun : Option Bool := none
  /-- If `true`, then `scriptSource` is allowed to change the function on
      top of the stack as long as the top-most stack frame is the only
      activation of that function. -/
  allowTopFrameEditing : Option Bool := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetScriptSource where
  toJSON p := Data.Json.object <|
    [("scriptId", ToJSON.toJSON p.scriptId), ("scriptSource", ToJSON.toJSON p.scriptSource)]
    ++ (p.dryRun.map fun v => ("dryRun", ToJSON.toJSON v)).toList
    ++ (p.allowTopFrameEditing.map fun v => ("allowTopFrameEditing", ToJSON.toJSON v)).toList

/-- Response of the `Debugger.setScriptSource` command. -/
structure SetScriptSource where
  /-- Whether the operation was successful or not. Only `ok` denotes a
      successful live edit while the other cases denote why the live edit
      failed. -/
  status : SetScriptSourceStatus
  /-- Exception details, if any. Only present when `status` is
      `compileError`. -/
  exceptionDetails : Option Runtime.ExceptionDetails := none
  deriving Repr, BEq

instance : FromJSON SetScriptSource where
  parseJSON v := do
    .ok
      { status := ← Value.getField v "status" >>= FromJSON.parseJSON
        exceptionDetails := ← (← Value.getFieldOpt v "exceptionDetails").mapM FromJSON.parseJSON }

instance : Command PSetScriptSource where
  Response := SetScriptSource
  commandName _ := "Debugger.setScriptSource"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Debugger.setSkipAllPauses` command: makes the page
    not interrupt on any pauses (breakpoint, exception, DOM exception,
    etc). -/
structure PSetSkipAllPauses where
  /-- New value for the skip-pauses state. -/
  skip : Bool
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetSkipAllPauses where
  toJSON p := Data.Json.object [("skip", ToJSON.toJSON p.skip)]

instance : Command PSetSkipAllPauses where
  Response := Unit
  commandName _ := "Debugger.setSkipAllPauses"
  decodeResponse _ := .ok ()

/-- Parameters of the `Debugger.setVariableValue` command: changes the value
    of a variable in a call frame. Object-based scopes are not supported and
    must be mutated manually. -/
structure PSetVariableValue where
  /-- 0-based number of the scope as listed in the scope chain. Only
      `local`, `closure`, and `catch` scope types are allowed; other scopes
      could be manipulated manually. -/
  scopeNumber : Int
  /-- Variable name. -/
  variableName : String
  /-- New variable value. -/
  newValue : Runtime.CallArgument
  /-- Id of the call frame that holds the variable. -/
  callFrameId : CallFrameId
  deriving Repr, BEq

instance : ToJSON PSetVariableValue where
  toJSON p := Data.Json.object
    [ ("scopeNumber", ToJSON.toJSON p.scopeNumber), ("variableName", ToJSON.toJSON p.variableName)
    , ("newValue", ToJSON.toJSON p.newValue), ("callFrameId", ToJSON.toJSON p.callFrameId) ]

instance : Command PSetVariableValue where
  Response := Unit
  commandName _ := "Debugger.setVariableValue"
  decodeResponse _ := .ok ()

/-- Parameters of the `Debugger.stepInto` command: steps into the function
    call. -/
structure PStepInto where
  /-- The debugger will pause on the execution of the first async task which
      was scheduled before the next pause. -/
  breakOnAsyncCall : Option Bool := none
  /-- Location ranges that should be skipped on step into. -/
  skipList : Option (List LocationRange) := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PStepInto where
  toJSON p := Data.Json.object <|
    (p.breakOnAsyncCall.map fun v => ("breakOnAsyncCall", ToJSON.toJSON v)).toList
    ++ (p.skipList.map fun v => ("skipList", ToJSON.toJSON v)).toList

instance : Command PStepInto where
  Response := Unit
  commandName _ := "Debugger.stepInto"
  decodeResponse _ := .ok ()

/-- Parameters of the `Debugger.stepOut` command: steps out of the function
    call. -/
structure PStepOut where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PStepOut where toJSON _ := .null

instance : Command PStepOut where
  Response := Unit
  commandName _ := "Debugger.stepOut"
  decodeResponse _ := .ok ()

/-- Parameters of the `Debugger.stepOver` command: steps over the
    statement. -/
structure PStepOver where
  /-- Location ranges that should be skipped on step over. -/
  skipList : Option (List LocationRange) := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PStepOver where
  toJSON p := Data.Json.object <|
    (p.skipList.map fun v => ("skipList", ToJSON.toJSON v)).toList

instance : Command PStepOver where
  Response := Unit
  commandName _ := "Debugger.stepOver"
  decodeResponse _ := .ok ()

end CDP.Domains.Debugger
