/-
  Tests for `Linen.CDP.Domains.WebAudio`.
-/
import Linen.CDP.Domains.WebAudio

open CDP.Domains.WebAudio
open CDP.Internal.Utils (Command Event)
open Data.Json (ToJSON FromJSON)
open Data.Json.Decode (decodeAs)
open Data.Json.Encode (encode)

namespace Tests.CDP.Domains.WebAudio

-- ── Core types ──

#guard decodeAs "\"realtime\"" (α := ContextType) = .ok .realtime
#guard encode (ToJSON.toJSON ContextType.offline) = "\"offline\""

#guard decodeAs "\"suspended\"" (α := ContextState) = .ok .suspended
#guard encode (ToJSON.toJSON ContextState.closed) = "\"closed\""

#guard decodeAs "\"clamped-max\"" (α := ChannelCountMode) = .ok .clampedMax
#guard encode (ToJSON.toJSON ChannelCountMode.max) = "\"max\""

#guard decodeAs "\"discrete\"" (α := ChannelInterpretation) = .ok .discrete
#guard encode (ToJSON.toJSON ChannelInterpretation.speakers) = "\"speakers\""

#guard decodeAs "\"a-rate\"" (α := AutomationRate) = .ok .aRate
#guard encode (ToJSON.toJSON AutomationRate.kRate) = "\"k-rate\""

#guard decodeAs
  "{\"currentTime\": 1, \"renderCapacity\": 2, \"callbackIntervalMean\": 3, \"callbackIntervalVariance\": 4}"
  (α := ContextRealtimeData)
  = .ok { currentTime := 1, renderCapacity := 2, callbackIntervalMean := 3, callbackIntervalVariance := 4 }
#guard encode
  (ToJSON.toJSON
    ({ currentTime := 1, renderCapacity := 2, callbackIntervalMean := 3, callbackIntervalVariance := 4 }
      : ContextRealtimeData))
  = "{\"currentTime\":1,\"renderCapacity\":2,\"callbackIntervalMean\":3,\"callbackIntervalVariance\":4}"

#guard decodeAs
  "{\"contextId\": \"c1\", \"contextType\": \"realtime\", \"contextState\": \"running\", \
    \"callbackBufferSize\": 128, \"maxOutputChannelCount\": 2, \"sampleRate\": 44100}"
  (α := BaseAudioContext)
  = .ok
    { contextId := "c1", contextType := .realtime, contextState := .running, realtimeData := none,
      callbackBufferSize := 128, maxOutputChannelCount := 2, sampleRate := 44100 }
#guard encode
  (ToJSON.toJSON
    ({ contextId := "c1", contextType := .realtime, contextState := .running, realtimeData := none,
       callbackBufferSize := 128, maxOutputChannelCount := 2, sampleRate := 44100 } : BaseAudioContext))
  = "{\"contextId\":\"c1\",\"contextType\":\"realtime\",\"contextState\":\"running\",\
     \"callbackBufferSize\":128,\"maxOutputChannelCount\":2,\"sampleRate\":44100}"

#guard decodeAs "{\"listenerId\": \"l1\", \"contextId\": \"c1\"}" (α := AudioListener)
  = .ok { listenerId := "l1", contextId := "c1" }
#guard encode (ToJSON.toJSON ({ listenerId := "l1", contextId := "c1" } : AudioListener))
  = "{\"listenerId\":\"l1\",\"contextId\":\"c1\"}"

#guard decodeAs
  "{\"nodeId\": \"n1\", \"contextId\": \"c1\", \"nodeType\": \"Gain\", \"numberOfInputs\": 1, \
    \"numberOfOutputs\": 1, \"channelCount\": 2, \"channelCountMode\": \"max\", \
    \"channelInterpretation\": \"speakers\"}"
  (α := AudioNode)
  = .ok
    { nodeId := "n1", contextId := "c1", nodeType := "Gain", numberOfInputs := 1, numberOfOutputs := 1,
      channelCount := 2, channelCountMode := .max, channelInterpretation := .speakers }

#guard decodeAs
  "{\"paramId\": \"p1\", \"nodeId\": \"n1\", \"contextId\": \"c1\", \"paramType\": \"gain\", \
    \"rate\": \"a-rate\", \"defaultValue\": 1, \"minValue\": 0, \"maxValue\": 1}"
  (α := AudioParam)
  = .ok
    { paramId := "p1", nodeId := "n1", contextId := "c1", paramType := "gain", rate := .aRate,
      defaultValue := 1, minValue := 0, maxValue := 1 }

-- ── Events ──

#guard decodeAs
  "{\"context\": {\"contextId\": \"c1\", \"contextType\": \"realtime\", \"contextState\": \"running\", \
    \"callbackBufferSize\": 128, \"maxOutputChannelCount\": 2, \"sampleRate\": 44100}}"
  (α := ContextCreated)
  |>.isOk
#guard Event.eventName (α := ContextCreated) = "WebAudio.contextCreated"

#guard decodeAs "{\"contextId\": \"c1\"}" (α := ContextWillBeDestroyed)
  = .ok { contextId := "c1" }
#guard Event.eventName (α := ContextWillBeDestroyed) = "WebAudio.contextWillBeDestroyed"

#guard decodeAs
  "{\"context\": {\"contextId\": \"c1\", \"contextType\": \"offline\", \"contextState\": \"closed\", \
    \"callbackBufferSize\": 128, \"maxOutputChannelCount\": 2, \"sampleRate\": 44100}}"
  (α := ContextChanged)
  |>.isOk
#guard Event.eventName (α := ContextChanged) = "WebAudio.contextChanged"

#guard decodeAs "{\"listener\": {\"listenerId\": \"l1\", \"contextId\": \"c1\"}}" (α := AudioListenerCreated)
  = .ok { listener := { listenerId := "l1", contextId := "c1" } }
#guard Event.eventName (α := AudioListenerCreated) = "WebAudio.audioListenerCreated"

#guard decodeAs "{\"contextId\": \"c1\", \"listenerId\": \"l1\"}" (α := AudioListenerWillBeDestroyed)
  = .ok { contextId := "c1", listenerId := "l1" }
#guard Event.eventName (α := AudioListenerWillBeDestroyed) = "WebAudio.audioListenerWillBeDestroyed"

#guard decodeAs
  "{\"node\": {\"nodeId\": \"n1\", \"contextId\": \"c1\", \"nodeType\": \"Gain\", \"numberOfInputs\": 1, \
    \"numberOfOutputs\": 1, \"channelCount\": 2, \"channelCountMode\": \"max\", \
    \"channelInterpretation\": \"speakers\"}}"
  (α := AudioNodeCreated)
  |>.isOk
#guard Event.eventName (α := AudioNodeCreated) = "WebAudio.audioNodeCreated"

#guard decodeAs "{\"contextId\": \"c1\", \"nodeId\": \"n1\"}" (α := AudioNodeWillBeDestroyed)
  = .ok { contextId := "c1", nodeId := "n1" }
#guard Event.eventName (α := AudioNodeWillBeDestroyed) = "WebAudio.audioNodeWillBeDestroyed"

#guard decodeAs
  "{\"param\": {\"paramId\": \"p1\", \"nodeId\": \"n1\", \"contextId\": \"c1\", \"paramType\": \"gain\", \
    \"rate\": \"a-rate\", \"defaultValue\": 1, \"minValue\": 0, \"maxValue\": 1}}"
  (α := AudioParamCreated)
  |>.isOk
#guard Event.eventName (α := AudioParamCreated) = "WebAudio.audioParamCreated"

#guard decodeAs "{\"contextId\": \"c1\", \"nodeId\": \"n1\", \"paramId\": \"p1\"}"
  (α := AudioParamWillBeDestroyed)
  = .ok { contextId := "c1", nodeId := "n1", paramId := "p1" }
#guard Event.eventName (α := AudioParamWillBeDestroyed) = "WebAudio.audioParamWillBeDestroyed"

#guard decodeAs "{\"contextId\": \"c1\", \"sourceId\": \"s1\", \"destinationId\": \"d1\"}"
  (α := NodesConnected)
  = .ok { contextId := "c1", sourceId := "s1", destinationId := "d1" }
#guard decodeAs
  "{\"contextId\": \"c1\", \"sourceId\": \"s1\", \"destinationId\": \"d1\", \"sourceOutputIndex\": 0, \
    \"destinationInputIndex\": 1}"
  (α := NodesConnected)
  = .ok
    { contextId := "c1", sourceId := "s1", destinationId := "d1", sourceOutputIndex := some 0,
      destinationInputIndex := some 1 }
#guard Event.eventName (α := NodesConnected) = "WebAudio.nodesConnected"

#guard decodeAs "{\"contextId\": \"c1\", \"sourceId\": \"s1\", \"destinationId\": \"d1\"}"
  (α := NodesDisconnected)
  = .ok { contextId := "c1", sourceId := "s1", destinationId := "d1" }
#guard Event.eventName (α := NodesDisconnected) = "WebAudio.nodesDisconnected"

#guard decodeAs "{\"contextId\": \"c1\", \"sourceId\": \"s1\", \"destinationId\": \"d1\"}"
  (α := NodeParamConnected)
  = .ok { contextId := "c1", sourceId := "s1", destinationId := "d1" }
#guard Event.eventName (α := NodeParamConnected) = "WebAudio.nodeParamConnected"

#guard decodeAs "{\"contextId\": \"c1\", \"sourceId\": \"s1\", \"destinationId\": \"d1\"}"
  (α := NodeParamDisconnected)
  = .ok { contextId := "c1", sourceId := "s1", destinationId := "d1" }
#guard Event.eventName (α := NodeParamDisconnected) = "WebAudio.nodeParamDisconnected"

-- ── Commands ──

#guard encode (ToJSON.toJSON ({} : PEnable)) = "null"
#guard Command.commandName ({} : PEnable) = "WebAudio.enable"

#guard encode (ToJSON.toJSON ({} : PDisable)) = "null"
#guard Command.commandName ({} : PDisable) = "WebAudio.disable"

#guard encode (ToJSON.toJSON ({ contextId := "c1" } : PGetRealtimeData)) = "{\"contextId\":\"c1\"}"
#guard Command.commandName ({ contextId := "c1" } : PGetRealtimeData) = "WebAudio.getRealtimeData"
#guard decodeAs
  "{\"realtimeData\": {\"currentTime\": 1, \"renderCapacity\": 2, \"callbackIntervalMean\": 3, \
    \"callbackIntervalVariance\": 4}}"
  (α := GetRealtimeData)
  = .ok { realtimeData := { currentTime := 1, renderCapacity := 2, callbackIntervalMean := 3,
                             callbackIntervalVariance := 4 } }

end Tests.CDP.Domains.WebAudio
