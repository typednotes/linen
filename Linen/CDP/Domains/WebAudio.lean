/-
  Linen.CDP.Domains.WebAudio — the `WebAudio` CDP domain

  Inspection of the Web Audio API (<https://webaudio.github.io/web-audio-api/>).
  Ports `CDP.Domains.WebAudio` (see `docs/imports/cdp/dependencies.md`); naming
  conventions as in `CDP.Domains.CacheStorage`'s docstring.
-/
import Linen.CDP.Internal.Utils

namespace CDP.Domains.WebAudio

open Data.Json (Value ToJSON FromJSON)
open CDP.Internal.Utils (Command Event)

-- ── Core types ──

/-- An unique ID for a graph object (`AudioContext`, `AudioNode`,
    `AudioParam`) in the Web Audio API. -/
abbrev GraphObjectId := String

/-- Enum of `BaseAudioContext` types. -/
inductive ContextType where
  | realtime | offline
  deriving Repr, BEq, DecidableEq

instance : FromJSON ContextType where
  parseJSON
    | .string "realtime" => .ok .realtime
    | .string "offline" => .ok .offline
    | v => .error s!"failed to parse ContextType: {repr v}"

instance : ToJSON ContextType where
  toJSON | .realtime => .string "realtime" | .offline => .string "offline"

/-- Enum of `AudioContextState` from the spec. -/
inductive ContextState where
  | suspended | running | closed
  deriving Repr, BEq, DecidableEq

instance : FromJSON ContextState where
  parseJSON
    | .string "suspended" => .ok .suspended
    | .string "running" => .ok .running
    | .string "closed" => .ok .closed
    | v => .error s!"failed to parse ContextState: {repr v}"

instance : ToJSON ContextState where
  toJSON
    | .suspended => .string "suspended" | .running => .string "running" | .closed => .string "closed"

/-- Enum of `AudioNode` types. -/
abbrev NodeType := String

/-- Enum of `AudioNode::ChannelCountMode` from the spec. -/
inductive ChannelCountMode where
  | clampedMax | explicit | max
  deriving Repr, BEq, DecidableEq

instance : FromJSON ChannelCountMode where
  parseJSON
    | .string "clamped-max" => .ok .clampedMax
    | .string "explicit" => .ok .explicit
    | .string "max" => .ok .max
    | v => .error s!"failed to parse ChannelCountMode: {repr v}"

instance : ToJSON ChannelCountMode where
  toJSON
    | .clampedMax => .string "clamped-max" | .explicit => .string "explicit" | .max => .string "max"

/-- Enum of `AudioNode::ChannelInterpretation` from the spec. -/
inductive ChannelInterpretation where
  | discrete | speakers
  deriving Repr, BEq, DecidableEq

instance : FromJSON ChannelInterpretation where
  parseJSON
    | .string "discrete" => .ok .discrete
    | .string "speakers" => .ok .speakers
    | v => .error s!"failed to parse ChannelInterpretation: {repr v}"

instance : ToJSON ChannelInterpretation where
  toJSON | .discrete => .string "discrete" | .speakers => .string "speakers"

/-- Enum of `AudioParam` types. -/
abbrev ParamType := String

/-- Enum of `AudioParam::AutomationRate` from the spec. -/
inductive AutomationRate where
  | aRate | kRate
  deriving Repr, BEq, DecidableEq

instance : FromJSON AutomationRate where
  parseJSON
    | .string "a-rate" => .ok .aRate
    | .string "k-rate" => .ok .kRate
    | v => .error s!"failed to parse AutomationRate: {repr v}"

instance : ToJSON AutomationRate where
  toJSON | .aRate => .string "a-rate" | .kRate => .string "k-rate"

/-- Fields in `AudioContext` that change in real-time. -/
structure ContextRealtimeData where
  /-- The current context time in seconds in `BaseAudioContext`. -/
  currentTime : Float
  /-- The time spent on rendering graph divided by render quantum duration,
      multiplied by 100. 100 means the audio renderer reached the full
      capacity and glitch may occur. -/
  renderCapacity : Float
  /-- A running mean of callback interval. -/
  callbackIntervalMean : Float
  /-- A running variance of callback interval. -/
  callbackIntervalVariance : Float
  deriving Repr, BEq, DecidableEq

instance : FromJSON ContextRealtimeData where
  parseJSON v := do
    .ok
      { currentTime := ← Value.getField v "currentTime" >>= FromJSON.parseJSON
        renderCapacity := ← Value.getField v "renderCapacity" >>= FromJSON.parseJSON
        callbackIntervalMean := ← Value.getField v "callbackIntervalMean" >>= FromJSON.parseJSON
        callbackIntervalVariance := ← Value.getField v "callbackIntervalVariance" >>= FromJSON.parseJSON }

instance : ToJSON ContextRealtimeData where
  toJSON p := Data.Json.object
    [ ("currentTime", ToJSON.toJSON p.currentTime)
    , ("renderCapacity", ToJSON.toJSON p.renderCapacity)
    , ("callbackIntervalMean", ToJSON.toJSON p.callbackIntervalMean)
    , ("callbackIntervalVariance", ToJSON.toJSON p.callbackIntervalVariance) ]

/-- Protocol object for `BaseAudioContext`. -/
structure BaseAudioContext where
  contextId : GraphObjectId
  contextType : ContextType
  contextState : ContextState
  realtimeData : Option ContextRealtimeData := none
  /-- Platform-dependent callback buffer size. -/
  callbackBufferSize : Float
  /-- Number of output channels supported by audio hardware in use. -/
  maxOutputChannelCount : Float
  /-- Context sample rate. -/
  sampleRate : Float
  deriving Repr, BEq, DecidableEq

instance : FromJSON BaseAudioContext where
  parseJSON v := do
    .ok
      { contextId := ← Value.getField v "contextId" >>= FromJSON.parseJSON
        contextType := ← Value.getField v "contextType" >>= FromJSON.parseJSON
        contextState := ← Value.getField v "contextState" >>= FromJSON.parseJSON
        realtimeData := ← (← Value.getFieldOpt v "realtimeData").mapM FromJSON.parseJSON
        callbackBufferSize := ← Value.getField v "callbackBufferSize" >>= FromJSON.parseJSON
        maxOutputChannelCount := ← Value.getField v "maxOutputChannelCount" >>= FromJSON.parseJSON
        sampleRate := ← Value.getField v "sampleRate" >>= FromJSON.parseJSON }

instance : ToJSON BaseAudioContext where
  toJSON p := Data.Json.object <|
    [ ("contextId", ToJSON.toJSON p.contextId)
    , ("contextType", ToJSON.toJSON p.contextType)
    , ("contextState", ToJSON.toJSON p.contextState) ]
    ++ (p.realtimeData.map fun v => ("realtimeData", ToJSON.toJSON v)).toList
    ++ [ ("callbackBufferSize", ToJSON.toJSON p.callbackBufferSize)
       , ("maxOutputChannelCount", ToJSON.toJSON p.maxOutputChannelCount)
       , ("sampleRate", ToJSON.toJSON p.sampleRate) ]

/-- Protocol object for `AudioListener`. -/
structure AudioListener where
  listenerId : GraphObjectId
  contextId : GraphObjectId
  deriving Repr, BEq, DecidableEq

instance : FromJSON AudioListener where
  parseJSON v := do
    .ok
      { listenerId := ← Value.getField v "listenerId" >>= FromJSON.parseJSON
        contextId := ← Value.getField v "contextId" >>= FromJSON.parseJSON }

instance : ToJSON AudioListener where
  toJSON p := Data.Json.object [("listenerId", ToJSON.toJSON p.listenerId), ("contextId", ToJSON.toJSON p.contextId)]

/-- Protocol object for `AudioNode`. -/
structure AudioNode where
  nodeId : GraphObjectId
  contextId : GraphObjectId
  nodeType : NodeType
  numberOfInputs : Float
  numberOfOutputs : Float
  channelCount : Float
  channelCountMode : ChannelCountMode
  channelInterpretation : ChannelInterpretation
  deriving Repr, BEq, DecidableEq

instance : FromJSON AudioNode where
  parseJSON v := do
    .ok
      { nodeId := ← Value.getField v "nodeId" >>= FromJSON.parseJSON
        contextId := ← Value.getField v "contextId" >>= FromJSON.parseJSON
        nodeType := ← Value.getField v "nodeType" >>= FromJSON.parseJSON
        numberOfInputs := ← Value.getField v "numberOfInputs" >>= FromJSON.parseJSON
        numberOfOutputs := ← Value.getField v "numberOfOutputs" >>= FromJSON.parseJSON
        channelCount := ← Value.getField v "channelCount" >>= FromJSON.parseJSON
        channelCountMode := ← Value.getField v "channelCountMode" >>= FromJSON.parseJSON
        channelInterpretation := ← Value.getField v "channelInterpretation" >>= FromJSON.parseJSON }

instance : ToJSON AudioNode where
  toJSON p := Data.Json.object
    [ ("nodeId", ToJSON.toJSON p.nodeId), ("contextId", ToJSON.toJSON p.contextId)
    , ("nodeType", ToJSON.toJSON p.nodeType), ("numberOfInputs", ToJSON.toJSON p.numberOfInputs)
    , ("numberOfOutputs", ToJSON.toJSON p.numberOfOutputs), ("channelCount", ToJSON.toJSON p.channelCount)
    , ("channelCountMode", ToJSON.toJSON p.channelCountMode)
    , ("channelInterpretation", ToJSON.toJSON p.channelInterpretation) ]

/-- Protocol object for `AudioParam`. -/
structure AudioParam where
  paramId : GraphObjectId
  nodeId : GraphObjectId
  contextId : GraphObjectId
  paramType : ParamType
  rate : AutomationRate
  defaultValue : Float
  minValue : Float
  maxValue : Float
  deriving Repr, BEq, DecidableEq

instance : FromJSON AudioParam where
  parseJSON v := do
    .ok
      { paramId := ← Value.getField v "paramId" >>= FromJSON.parseJSON
        nodeId := ← Value.getField v "nodeId" >>= FromJSON.parseJSON
        contextId := ← Value.getField v "contextId" >>= FromJSON.parseJSON
        paramType := ← Value.getField v "paramType" >>= FromJSON.parseJSON
        rate := ← Value.getField v "rate" >>= FromJSON.parseJSON
        defaultValue := ← Value.getField v "defaultValue" >>= FromJSON.parseJSON
        minValue := ← Value.getField v "minValue" >>= FromJSON.parseJSON
        maxValue := ← Value.getField v "maxValue" >>= FromJSON.parseJSON }

instance : ToJSON AudioParam where
  toJSON p := Data.Json.object
    [ ("paramId", ToJSON.toJSON p.paramId), ("nodeId", ToJSON.toJSON p.nodeId)
    , ("contextId", ToJSON.toJSON p.contextId), ("paramType", ToJSON.toJSON p.paramType)
    , ("rate", ToJSON.toJSON p.rate), ("defaultValue", ToJSON.toJSON p.defaultValue)
    , ("minValue", ToJSON.toJSON p.minValue), ("maxValue", ToJSON.toJSON p.maxValue) ]

-- ── Events ──

/-- The `WebAudio.contextCreated` event. -/
structure ContextCreated where
  context : BaseAudioContext
  deriving Repr, BEq, DecidableEq

instance : FromJSON ContextCreated where
  parseJSON v := do .ok { context := ← Value.getField v "context" >>= FromJSON.parseJSON }

instance : Event ContextCreated where
  eventName := "WebAudio.contextCreated"

/-- The `WebAudio.contextWillBeDestroyed` event. -/
structure ContextWillBeDestroyed where
  contextId : GraphObjectId
  deriving Repr, BEq, DecidableEq

instance : FromJSON ContextWillBeDestroyed where
  parseJSON v := do .ok { contextId := ← Value.getField v "contextId" >>= FromJSON.parseJSON }

instance : Event ContextWillBeDestroyed where
  eventName := "WebAudio.contextWillBeDestroyed"

/-- The `WebAudio.contextChanged` event. -/
structure ContextChanged where
  context : BaseAudioContext
  deriving Repr, BEq, DecidableEq

instance : FromJSON ContextChanged where
  parseJSON v := do .ok { context := ← Value.getField v "context" >>= FromJSON.parseJSON }

instance : Event ContextChanged where
  eventName := "WebAudio.contextChanged"

/-- The `WebAudio.audioListenerCreated` event. -/
structure AudioListenerCreated where
  listener : AudioListener
  deriving Repr, BEq, DecidableEq

instance : FromJSON AudioListenerCreated where
  parseJSON v := do .ok { listener := ← Value.getField v "listener" >>= FromJSON.parseJSON }

instance : Event AudioListenerCreated where
  eventName := "WebAudio.audioListenerCreated"

/-- The `WebAudio.audioListenerWillBeDestroyed` event. -/
structure AudioListenerWillBeDestroyed where
  contextId : GraphObjectId
  listenerId : GraphObjectId
  deriving Repr, BEq, DecidableEq

instance : FromJSON AudioListenerWillBeDestroyed where
  parseJSON v := do
    .ok
      { contextId := ← Value.getField v "contextId" >>= FromJSON.parseJSON
        listenerId := ← Value.getField v "listenerId" >>= FromJSON.parseJSON }

instance : Event AudioListenerWillBeDestroyed where
  eventName := "WebAudio.audioListenerWillBeDestroyed"

/-- The `WebAudio.audioNodeCreated` event. -/
structure AudioNodeCreated where
  node : AudioNode
  deriving Repr, BEq, DecidableEq

instance : FromJSON AudioNodeCreated where
  parseJSON v := do .ok { node := ← Value.getField v "node" >>= FromJSON.parseJSON }

instance : Event AudioNodeCreated where
  eventName := "WebAudio.audioNodeCreated"

/-- The `WebAudio.audioNodeWillBeDestroyed` event. -/
structure AudioNodeWillBeDestroyed where
  contextId : GraphObjectId
  nodeId : GraphObjectId
  deriving Repr, BEq, DecidableEq

instance : FromJSON AudioNodeWillBeDestroyed where
  parseJSON v := do
    .ok
      { contextId := ← Value.getField v "contextId" >>= FromJSON.parseJSON
        nodeId := ← Value.getField v "nodeId" >>= FromJSON.parseJSON }

instance : Event AudioNodeWillBeDestroyed where
  eventName := "WebAudio.audioNodeWillBeDestroyed"

/-- The `WebAudio.audioParamCreated` event. -/
structure AudioParamCreated where
  param : AudioParam
  deriving Repr, BEq, DecidableEq

instance : FromJSON AudioParamCreated where
  parseJSON v := do .ok { param := ← Value.getField v "param" >>= FromJSON.parseJSON }

instance : Event AudioParamCreated where
  eventName := "WebAudio.audioParamCreated"

/-- The `WebAudio.audioParamWillBeDestroyed` event. -/
structure AudioParamWillBeDestroyed where
  contextId : GraphObjectId
  nodeId : GraphObjectId
  paramId : GraphObjectId
  deriving Repr, BEq, DecidableEq

instance : FromJSON AudioParamWillBeDestroyed where
  parseJSON v := do
    .ok
      { contextId := ← Value.getField v "contextId" >>= FromJSON.parseJSON
        nodeId := ← Value.getField v "nodeId" >>= FromJSON.parseJSON
        paramId := ← Value.getField v "paramId" >>= FromJSON.parseJSON }

instance : Event AudioParamWillBeDestroyed where
  eventName := "WebAudio.audioParamWillBeDestroyed"

/-- The `WebAudio.nodesConnected` event. -/
structure NodesConnected where
  contextId : GraphObjectId
  sourceId : GraphObjectId
  destinationId : GraphObjectId
  sourceOutputIndex : Option Float := none
  destinationInputIndex : Option Float := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON NodesConnected where
  parseJSON v := do
    .ok
      { contextId := ← Value.getField v "contextId" >>= FromJSON.parseJSON
        sourceId := ← Value.getField v "sourceId" >>= FromJSON.parseJSON
        destinationId := ← Value.getField v "destinationId" >>= FromJSON.parseJSON
        sourceOutputIndex := ← (← Value.getFieldOpt v "sourceOutputIndex").mapM FromJSON.parseJSON
        destinationInputIndex := ← (← Value.getFieldOpt v "destinationInputIndex").mapM FromJSON.parseJSON }

instance : Event NodesConnected where
  eventName := "WebAudio.nodesConnected"

/-- The `WebAudio.nodesDisconnected` event. -/
structure NodesDisconnected where
  contextId : GraphObjectId
  sourceId : GraphObjectId
  destinationId : GraphObjectId
  sourceOutputIndex : Option Float := none
  destinationInputIndex : Option Float := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON NodesDisconnected where
  parseJSON v := do
    .ok
      { contextId := ← Value.getField v "contextId" >>= FromJSON.parseJSON
        sourceId := ← Value.getField v "sourceId" >>= FromJSON.parseJSON
        destinationId := ← Value.getField v "destinationId" >>= FromJSON.parseJSON
        sourceOutputIndex := ← (← Value.getFieldOpt v "sourceOutputIndex").mapM FromJSON.parseJSON
        destinationInputIndex := ← (← Value.getFieldOpt v "destinationInputIndex").mapM FromJSON.parseJSON }

instance : Event NodesDisconnected where
  eventName := "WebAudio.nodesDisconnected"

/-- The `WebAudio.nodeParamConnected` event. -/
structure NodeParamConnected where
  contextId : GraphObjectId
  sourceId : GraphObjectId
  destinationId : GraphObjectId
  sourceOutputIndex : Option Float := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON NodeParamConnected where
  parseJSON v := do
    .ok
      { contextId := ← Value.getField v "contextId" >>= FromJSON.parseJSON
        sourceId := ← Value.getField v "sourceId" >>= FromJSON.parseJSON
        destinationId := ← Value.getField v "destinationId" >>= FromJSON.parseJSON
        sourceOutputIndex := ← (← Value.getFieldOpt v "sourceOutputIndex").mapM FromJSON.parseJSON }

instance : Event NodeParamConnected where
  eventName := "WebAudio.nodeParamConnected"

/-- The `WebAudio.nodeParamDisconnected` event. -/
structure NodeParamDisconnected where
  contextId : GraphObjectId
  sourceId : GraphObjectId
  destinationId : GraphObjectId
  sourceOutputIndex : Option Float := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON NodeParamDisconnected where
  parseJSON v := do
    .ok
      { contextId := ← Value.getField v "contextId" >>= FromJSON.parseJSON
        sourceId := ← Value.getField v "sourceId" >>= FromJSON.parseJSON
        destinationId := ← Value.getField v "destinationId" >>= FromJSON.parseJSON
        sourceOutputIndex := ← (← Value.getFieldOpt v "sourceOutputIndex").mapM FromJSON.parseJSON }

instance : Event NodeParamDisconnected where
  eventName := "WebAudio.nodeParamDisconnected"

-- ── Commands ──

/-- Parameters of the `WebAudio.enable` command: enables the WebAudio domain
    and starts sending context lifetime events. -/
structure PEnable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PEnable where toJSON _ := .null

instance : Command PEnable where
  Response := Unit
  commandName _ := "WebAudio.enable"
  decodeResponse _ := .ok ()

/-- Parameters of the `WebAudio.disable` command: disables the WebAudio
    domain. -/
structure PDisable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PDisable where toJSON _ := .null

instance : Command PDisable where
  Response := Unit
  commandName _ := "WebAudio.disable"
  decodeResponse _ := .ok ()

/-- Parameters of the `WebAudio.getRealtimeData` command: fetch the realtime
    data from the registered contexts. -/
structure PGetRealtimeData where
  contextId : GraphObjectId
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetRealtimeData where
  toJSON p := Data.Json.object [("contextId", ToJSON.toJSON p.contextId)]

/-- Response of the `WebAudio.getRealtimeData` command. -/
structure GetRealtimeData where
  realtimeData : ContextRealtimeData
  deriving Repr, BEq, DecidableEq

instance : FromJSON GetRealtimeData where
  parseJSON v := do .ok { realtimeData := ← Value.getField v "realtimeData" >>= FromJSON.parseJSON }

instance : Command PGetRealtimeData where
  Response := GetRealtimeData
  commandName _ := "WebAudio.getRealtimeData"
  decodeResponse := FromJSON.parseJSON

end CDP.Domains.WebAudio
