/-
  Tests for `Linen.CDP.Domains.Debugger`.
-/
import Linen.CDP.Domains.Debugger

open CDP.Domains.Debugger
open CDP.Internal.Utils (Command Event)
open Data.Json (ToJSON FromJSON)
open Data.Json.Decode (decodeAs)
open Data.Json.Encode (encode)

namespace Tests.CDP.Domains.Debugger

-- ── Locations ──

#guard decodeAs "{\"scriptId\": \"1\", \"lineNumber\": 2}" (α := Location)
  = .ok { scriptId := "1", lineNumber := 2 }
#guard encode (ToJSON.toJSON ({ scriptId := "1", lineNumber := 2, columnNumber := some 3 } : Location))
  = "{\"scriptId\":\"1\",\"lineNumber\":2,\"columnNumber\":3}"

#guard decodeAs "{\"lineNumber\": 1, \"columnNumber\": 2}" (α := ScriptPosition)
  = .ok { lineNumber := 1, columnNumber := 2 }
#guard encode (ToJSON.toJSON ({ lineNumber := 1, columnNumber := 2 } : ScriptPosition))
  = "{\"lineNumber\":1,\"columnNumber\":2}"

#guard decodeAs
    "{\"scriptId\": \"1\", \"start\": {\"lineNumber\": 0, \"columnNumber\": 0}, \"end\": {\"lineNumber\": 1, \"columnNumber\": 0}}"
    (α := LocationRange)
  = .ok { scriptId := "1", start := { lineNumber := 0, columnNumber := 0 }
        , «end» := { lineNumber := 1, columnNumber := 0 } }
#guard encode (ToJSON.toJSON
    ({ scriptId := "1", start := { lineNumber := 0, columnNumber := 0 }
     , «end» := { lineNumber := 1, columnNumber := 0 } } : LocationRange))
  = "{\"scriptId\":\"1\",\"start\":{\"lineNumber\":0,\"columnNumber\":0},\"end\":{\"lineNumber\":1,\"columnNumber\":0}}"

-- ── Scopes and call frames ──

#guard decodeAs "\"local\"" (α := ScopeType) = .ok .local
#guard encode (ToJSON.toJSON ScopeType.wasmExpressionStack) = "\"wasm-expression-stack\""

#guard decodeAs
    "{\"lineNumber\": 1, \"lineContent\": \"foo\"}" (α := SearchMatch)
  = .ok { lineNumber := 1, lineContent := "foo" }
#guard encode (ToJSON.toJSON ({ lineNumber := 1, lineContent := "foo" } : SearchMatch))
  = "{\"lineNumber\":1,\"lineContent\":\"foo\"}"

#guard decodeAs "\"return\"" (α := BreakLocationType) = .ok .«return»
#guard encode (ToJSON.toJSON BreakLocationType.debuggerStatement) = "\"debuggerStatement\""

#guard decodeAs "{\"scriptId\": \"1\", \"lineNumber\": 2}" (α := BreakLocation)
  = .ok { scriptId := "1", lineNumber := 2 }
#guard encode (ToJSON.toJSON ({ scriptId := "1", lineNumber := 2 } : BreakLocation))
  = "{\"scriptId\":\"1\",\"lineNumber\":2}"

-- ── WebAssembly ──

#guard decodeAs "{\"lines\": [\"a\"], \"bytecodeOffsets\": [0]}" (α := WasmDisassemblyChunk)
  = .ok { lines := ["a"], bytecodeOffsets := [0] }
#guard encode (ToJSON.toJSON ({ lines := ["a"], bytecodeOffsets := [0] } : WasmDisassemblyChunk))
  = "{\"lines\":[\"a\"],\"bytecodeOffsets\":[0]}"

#guard decodeAs "\"JavaScript\"" (α := ScriptLanguage) = .ok .javaScript
#guard encode (ToJSON.toJSON ScriptLanguage.webAssembly) = "\"WebAssembly\""

#guard decodeAs "\"EmbeddedDWARF\"" (α := DebugSymbolsType) = .ok .embeddedDWARF
#guard encode (ToJSON.toJSON DebugSymbolsType.none) = "\"None\""

#guard decodeAs "{\"type\": \"None\"}" (α := DebugSymbols) = .ok { type := .none }
#guard encode (ToJSON.toJSON ({ type := .sourceMap, externalURL := some "u" } : DebugSymbols))
  = "{\"type\":\"SourceMap\",\"externalURL\":\"u\"}"

-- ── Events ──

#guard decodeAs "{\"breakpointId\": \"b\", \"location\": {\"scriptId\": \"1\", \"lineNumber\": 0}}"
    (α := BreakpointResolved)
  = .ok { breakpointId := "b", location := { scriptId := "1", lineNumber := 0 } }
#guard Event.eventName (α := BreakpointResolved) = "Debugger.breakpointResolved"

#guard decodeAs "\"exception\"" (α := PausedReason) = .ok .exception

#guard (decodeAs "{\"callFrames\": [], \"reason\": \"other\"}" (α := Paused)
    |>.map (fun p => p.callFrames == [] && p.reason == .other)) = .ok true
#guard Event.eventName (α := Paused) = "Debugger.paused"

#guard decodeAs "{}" (α := Resumed) = .ok {}
#guard Event.eventName (α := Resumed) = "Debugger.resumed"

#guard (decodeAs
    ("{\"scriptId\": \"1\", \"url\": \"u\", \"startLine\": 0, \"startColumn\": 0, " ++
     "\"endLine\": 1, \"endColumn\": 0, \"executionContextId\": 1, \"hash\": \"h\"}")
    (α := ScriptFailedToParse)
    |>.map (fun p => p.scriptId == "1" && p.url == "u" && p.hash == "h")) = .ok true
#guard Event.eventName (α := ScriptFailedToParse) = "Debugger.scriptFailedToParse"

#guard (decodeAs
    ("{\"scriptId\": \"1\", \"url\": \"u\", \"startLine\": 0, \"startColumn\": 0, " ++
     "\"endLine\": 1, \"endColumn\": 0, \"executionContextId\": 1, \"hash\": \"h\"}")
    (α := ScriptParsed)
    |>.map (fun p => p.scriptId == "1" && p.url == "u" && p.hash == "h")) = .ok true
#guard Event.eventName (α := ScriptParsed) = "Debugger.scriptParsed"

-- ── Commands ──

#guard decodeAs "\"current\"" (α := TargetCallFrames) = .ok .current

#guard encode (ToJSON.toJSON
    ({ location := { scriptId := "1", lineNumber := 0 } } : PContinueToLocation))
  = "{\"location\":{\"scriptId\":\"1\",\"lineNumber\":0}}"
#guard Command.commandName ({ location := { scriptId := "1", lineNumber := 0 } } : PContinueToLocation)
  = "Debugger.continueToLocation"

#guard encode (ToJSON.toJSON ({} : PDisable)) = "null"
#guard Command.commandName ({} : PDisable) = "Debugger.disable"

#guard encode (ToJSON.toJSON ({} : PEnable)) = "{}"
#guard decodeAs "{\"debuggerId\": \"d\"}" (α := Enable) = .ok { debuggerId := "d" }
#guard Command.commandName ({} : PEnable) = "Debugger.enable"

#guard encode (ToJSON.toJSON
    ({ callFrameId := "c", expression := "1+1" } : PEvaluateOnCallFrame))
  = "{\"callFrameId\":\"c\",\"expression\":\"1+1\"}"
#guard Command.commandName ({ callFrameId := "c", expression := "1+1" } : PEvaluateOnCallFrame)
  = "Debugger.evaluateOnCallFrame"

#guard encode (ToJSON.toJSON ({ start := { scriptId := "1", lineNumber := 0 } } : PGetPossibleBreakpoints))
  = "{\"start\":{\"scriptId\":\"1\",\"lineNumber\":0}}"
#guard decodeAs "{\"locations\": []}" (α := GetPossibleBreakpoints) = .ok { locations := [] }
#guard Command.commandName ({ start := { scriptId := "1", lineNumber := 0 } } : PGetPossibleBreakpoints)
  = "Debugger.getPossibleBreakpoints"

#guard encode (ToJSON.toJSON ({ scriptId := "1" } : PGetScriptSource)) = "{\"scriptId\":\"1\"}"
#guard decodeAs "{\"scriptSource\": \"s\"}" (α := GetScriptSource) = .ok { scriptSource := "s" }
#guard Command.commandName ({ scriptId := "1" } : PGetScriptSource) = "Debugger.getScriptSource"

#guard encode (ToJSON.toJSON ({ scriptId := "1" } : PDisassembleWasmModule)) = "{\"scriptId\":\"1\"}"
#guard decodeAs
    "{\"totalNumberOfLines\": 1, \"functionBodyOffsets\": [], \"chunk\": {\"lines\": [], \"bytecodeOffsets\": []}}"
    (α := DisassembleWasmModule)
  = .ok { totalNumberOfLines := 1, functionBodyOffsets := []
        , chunk := { lines := [], bytecodeOffsets := [] } }
#guard Command.commandName ({ scriptId := "1" } : PDisassembleWasmModule)
  = "Debugger.disassembleWasmModule"

#guard encode (ToJSON.toJSON ({ streamId := "s" } : PNextWasmDisassemblyChunk))
  = "{\"streamId\":\"s\"}"
#guard decodeAs "{\"chunk\": {\"lines\": [], \"bytecodeOffsets\": []}}" (α := NextWasmDisassemblyChunk)
  = .ok { chunk := { lines := [], bytecodeOffsets := [] } }
#guard Command.commandName ({ streamId := "s" } : PNextWasmDisassemblyChunk)
  = "Debugger.nextWasmDisassemblyChunk"

#guard encode (ToJSON.toJSON ({ stackTraceId := { id := "s" } } : PGetStackTrace))
  = "{\"stackTraceId\":{\"id\":\"s\"}}"
#guard Command.commandName ({ stackTraceId := { id := "s" } } : PGetStackTrace) = "Debugger.getStackTrace"

#guard encode (ToJSON.toJSON ({} : PPause)) = "null"
#guard Command.commandName ({} : PPause) = "Debugger.pause"

#guard encode (ToJSON.toJSON ({ breakpointId := "b" } : PRemoveBreakpoint)) = "{\"breakpointId\":\"b\"}"
#guard Command.commandName ({ breakpointId := "b" } : PRemoveBreakpoint) = "Debugger.removeBreakpoint"

#guard decodeAs "\"StepInto\"" (α := RestartFrameMode) = .ok .stepInto
#guard encode (ToJSON.toJSON ({ callFrameId := "c", mode := some .stepInto } : PRestartFrame))
  = "{\"callFrameId\":\"c\",\"mode\":\"StepInto\"}"
#guard Command.commandName ({ callFrameId := "c" } : PRestartFrame) = "Debugger.restartFrame"

#guard encode (ToJSON.toJSON ({} : PResume)) = "{}"
#guard Command.commandName ({} : PResume) = "Debugger.resume"

#guard encode (ToJSON.toJSON ({ scriptId := "1", query := "q" } : PSearchInContent))
  = "{\"scriptId\":\"1\",\"query\":\"q\"}"
#guard decodeAs "{\"result\": []}" (α := SearchInContent) = .ok { result := [] }
#guard Command.commandName ({ scriptId := "1", query := "q" } : PSearchInContent)
  = "Debugger.searchInContent"

#guard encode (ToJSON.toJSON ({ maxDepth := 0 } : PSetAsyncCallStackDepth)) = "{\"maxDepth\":0}"
#guard Command.commandName ({ maxDepth := 0 } : PSetAsyncCallStackDepth)
  = "Debugger.setAsyncCallStackDepth"

#guard encode (ToJSON.toJSON ({ patterns := ["p"] } : PSetBlackboxPatterns)) = "{\"patterns\":[\"p\"]}"
#guard Command.commandName ({ patterns := ["p"] } : PSetBlackboxPatterns) = "Debugger.setBlackboxPatterns"

#guard encode (ToJSON.toJSON ({ scriptId := "1", positions := [] } : PSetBlackboxedRanges))
  = "{\"scriptId\":\"1\",\"positions\":[]}"
#guard Command.commandName ({ scriptId := "1", positions := [] } : PSetBlackboxedRanges)
  = "Debugger.setBlackboxedRanges"

#guard encode (ToJSON.toJSON ({ location := { scriptId := "1", lineNumber := 0 } } : PSetBreakpoint))
  = "{\"location\":{\"scriptId\":\"1\",\"lineNumber\":0}}"
#guard decodeAs
    "{\"breakpointId\": \"b\", \"actualLocation\": {\"scriptId\": \"1\", \"lineNumber\": 0}}"
    (α := SetBreakpoint)
  = .ok { breakpointId := "b", actualLocation := { scriptId := "1", lineNumber := 0 } }
#guard Command.commandName ({ location := { scriptId := "1", lineNumber := 0 } } : PSetBreakpoint)
  = "Debugger.setBreakpoint"

#guard decodeAs "\"beforeScriptExecution\"" (α := Instrumentation) = .ok .beforeScriptExecution
#guard encode (ToJSON.toJSON ({ instrumentation := .beforeScriptExecution } : PSetInstrumentationBreakpoint))
  = "{\"instrumentation\":\"beforeScriptExecution\"}"
#guard decodeAs "{\"breakpointId\": \"b\"}" (α := SetInstrumentationBreakpoint)
  = .ok { breakpointId := "b" }
#guard Command.commandName ({ instrumentation := .beforeScriptExecution } : PSetInstrumentationBreakpoint)
  = "Debugger.setInstrumentationBreakpoint"

#guard encode (ToJSON.toJSON ({ lineNumber := 1 } : PSetBreakpointByUrl)) = "{\"lineNumber\":1}"
#guard decodeAs "{\"breakpointId\": \"b\", \"locations\": []}" (α := SetBreakpointByUrl)
  = .ok { breakpointId := "b", locations := [] }
#guard Command.commandName ({ lineNumber := 1 } : PSetBreakpointByUrl) = "Debugger.setBreakpointByUrl"

#guard encode (ToJSON.toJSON ({ objectId := "o" } : PSetBreakpointOnFunctionCall))
  = "{\"objectId\":\"o\"}"
#guard decodeAs "{\"breakpointId\": \"b\"}" (α := SetBreakpointOnFunctionCall)
  = .ok { breakpointId := "b" }
#guard Command.commandName ({ objectId := "o" } : PSetBreakpointOnFunctionCall)
  = "Debugger.setBreakpointOnFunctionCall"

#guard encode (ToJSON.toJSON ({ active := true } : PSetBreakpointsActive)) = "{\"active\":true}"
#guard Command.commandName ({ active := true } : PSetBreakpointsActive)
  = "Debugger.setBreakpointsActive"

#guard decodeAs "\"uncaught\"" (α := PauseOnExceptionsState) = .ok .uncaught
#guard encode (ToJSON.toJSON ({ state := .all } : PSetPauseOnExceptions)) = "{\"state\":\"all\"}"
#guard Command.commandName ({ state := .all } : PSetPauseOnExceptions) = "Debugger.setPauseOnExceptions"

#guard Command.commandName
    ({ newValue := { value := some (Data.Json.Value.string "v") } } : PSetReturnValue)
  = "Debugger.setReturnValue"

#guard decodeAs "\"CompileError\"" (α := SetScriptSourceStatus) = .ok .compileError
#guard encode (ToJSON.toJSON ({ scriptId := "1", scriptSource := "s" } : PSetScriptSource))
  = "{\"scriptId\":\"1\",\"scriptSource\":\"s\"}"
#guard (decodeAs "{\"status\": \"Ok\"}" (α := SetScriptSource)
    |>.map (fun r => r.status == .ok)) = .ok true
#guard Command.commandName ({ scriptId := "1", scriptSource := "s" } : PSetScriptSource)
  = "Debugger.setScriptSource"

#guard encode (ToJSON.toJSON ({ skip := true } : PSetSkipAllPauses)) = "{\"skip\":true}"
#guard Command.commandName ({ skip := true } : PSetSkipAllPauses) = "Debugger.setSkipAllPauses"

#guard Command.commandName
    ({ scopeNumber := 0, variableName := "v"
     , newValue := { value := some (Data.Json.Value.string "v") }, callFrameId := "c" }
     : PSetVariableValue)
  = "Debugger.setVariableValue"

#guard encode (ToJSON.toJSON ({} : PStepInto)) = "{}"
#guard Command.commandName ({} : PStepInto) = "Debugger.stepInto"

#guard encode (ToJSON.toJSON ({} : PStepOut)) = "null"
#guard Command.commandName ({} : PStepOut) = "Debugger.stepOut"

#guard encode (ToJSON.toJSON ({} : PStepOver)) = "{}"
#guard Command.commandName ({} : PStepOver) = "Debugger.stepOver"

end Tests.CDP.Domains.Debugger
