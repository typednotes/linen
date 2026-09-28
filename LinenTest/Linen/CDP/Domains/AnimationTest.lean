/-
  Tests for `Linen.CDP.Domains.Animation`.
-/
import Linen.CDP.Domains.Animation

open CDP.Domains.Animation
open CDP.Internal.Utils (Command Event)
open Data.Json (ToJSON FromJSON)
open Data.Json.Decode (decodeAs)
open Data.Json.Encode (encode)

namespace Tests.CDP.Domains.Animation

#guard decodeAs "\"CSSTransition\"" (α := AnimationType) = .ok .cssTransition
#guard decodeAs "\"CSSAnimation\"" (α := AnimationType) = .ok .cssAnimation
#guard decodeAs "\"WebAnimation\"" (α := AnimationType) = .ok .webAnimation
#guard encode (ToJSON.toJSON AnimationType.cssTransition) = "\"CSSTransition\""

#guard decodeAs "{\"offset\": \"0%\", \"easing\": \"linear\"}" (α := KeyframeStyle)
  = .ok { offset := "0%", easing := "linear" }
#guard encode (ToJSON.toJSON ({ offset := "0%", easing := "linear" } : KeyframeStyle))
  = "{\"offset\":\"0%\",\"easing\":\"linear\"}"

#guard decodeAs "{\"keyframes\": []}" (α := KeyframesRule) = .ok { name := none, keyframes := [] }
#guard decodeAs "{\"name\": \"spin\", \"keyframes\": [{\"offset\": \"0%\", \"easing\": \"linear\"}]}"
    (α := KeyframesRule)
  = .ok { name := some "spin", keyframes := [{ offset := "0%", easing := "linear" }] }
#guard encode (ToJSON.toJSON ({ name := none, keyframes := [] } : KeyframesRule)) = "{\"keyframes\":[]}"

#guard decodeAs
    "{\"delay\": 0, \"endDelay\": 0, \"iterationStart\": 0, \"iterations\": 1, \"duration\": 100, \"direction\": \"normal\", \"fill\": \"none\", \"easing\": \"linear\"}"
    (α := AnimationEffect)
  = .ok
    { delay := 0, endDelay := 0, iterationStart := 0, iterations := 1, duration := 100
      direction := "normal", fill := "none", backendNodeId := none, keyframesRule := none
      easing := "linear" }
#guard encode (ToJSON.toJSON
    ({ delay := 0, endDelay := 0, iterationStart := 0, iterations := 1, duration := 100
       direction := "normal", fill := "none", backendNodeId := some 3, keyframesRule := none
       easing := "linear" } : AnimationEffect))
  = "{\"delay\":0,\"endDelay\":0,\"iterationStart\":0,\"iterations\":1,\"duration\":100,\"direction\":\"normal\",\"fill\":\"none\",\"backendNodeId\":3,\"easing\":\"linear\"}"

#guard decodeAs
    "{\"id\": \"a1\", \"name\": \"n\", \"pausedState\": false, \"playState\": \"running\", \"playbackRate\": 1, \"startTime\": 0, \"currentTime\": 10, \"type\": \"WebAnimation\"}"
    (α := Animation)
  = .ok
    { id := "a1", name := "n", pausedState := false, playState := "running", playbackRate := 1
      startTime := 0, currentTime := 10, type := .webAnimation, source := none, cssId := none }
#guard encode (ToJSON.toJSON
    ({ id := "a1", name := "n", pausedState := false, playState := "running", playbackRate := 1
       startTime := 0, currentTime := 10, type := .webAnimation, source := none
       cssId := none } : Animation))
  = "{\"id\":\"a1\",\"name\":\"n\",\"pausedState\":false,\"playState\":\"running\",\"playbackRate\":1,\"startTime\":0,\"currentTime\":10,\"type\":\"WebAnimation\"}"

#guard decodeAs "{\"id\": \"a1\"}" (α := AnimationCanceled) = .ok { id := "a1" }
#guard Event.eventName (α := AnimationCanceled) = "Animation.animationCanceled"

#guard decodeAs "{\"id\": \"a1\"}" (α := AnimationCreated) = .ok { id := "a1" }
#guard Event.eventName (α := AnimationCreated) = "Animation.animationCreated"

#guard decodeAs
    "{\"animation\": {\"id\": \"a1\", \"name\": \"n\", \"pausedState\": false, \"playState\": \"running\", \"playbackRate\": 1, \"startTime\": 0, \"currentTime\": 10, \"type\": \"WebAnimation\"}}"
    (α := AnimationStarted)
  = .ok
    { animation :=
      { id := "a1", name := "n", pausedState := false, playState := "running", playbackRate := 1
        startTime := 0, currentTime := 10, type := .webAnimation, source := none, cssId := none } }
#guard Event.eventName (α := AnimationStarted) = "Animation.animationStarted"

#guard encode (ToJSON.toJSON ({} : PDisable)) = "null"
#guard Command.commandName ({} : PDisable) = "Animation.disable"
#guard encode (ToJSON.toJSON ({} : PEnable)) = "null"
#guard Command.commandName ({} : PEnable) = "Animation.enable"

#guard encode (ToJSON.toJSON ({ id := "a1" } : PGetCurrentTime)) = "{\"id\":\"a1\"}"
#guard Command.commandName ({ id := "a1" } : PGetCurrentTime) = "Animation.getCurrentTime"
#guard decodeAs "{\"currentTime\": 42}" (α := GetCurrentTime) = .ok { currentTime := 42 }

#guard encode (ToJSON.toJSON ({} : PGetPlaybackRate)) = "null"
#guard Command.commandName ({} : PGetPlaybackRate) = "Animation.getPlaybackRate"
#guard decodeAs "{\"playbackRate\": 1.5}" (α := GetPlaybackRate) = .ok { playbackRate := 1.5 }

#guard encode (ToJSON.toJSON ({ animations := ["a1", "a2"] } : PReleaseAnimations))
  = "{\"animations\":[\"a1\",\"a2\"]}"
#guard Command.commandName ({ animations := ["a1"] } : PReleaseAnimations) = "Animation.releaseAnimations"

#guard encode (ToJSON.toJSON ({ animationId := "a1" } : PResolveAnimation)) = "{\"animationId\":\"a1\"}"
#guard Command.commandName ({ animationId := "a1" } : PResolveAnimation) = "Animation.resolveAnimation"
#guard match decodeAs "{\"remoteObject\": {\"type\": \"object\"}}" (α := ResolveAnimation) with
  | .ok v => v.remoteObject.type == .object
  | .error _ => false

#guard encode (ToJSON.toJSON ({ animations := ["a1"], currentTime := 5 } : PSeekAnimations))
  = "{\"animations\":[\"a1\"],\"currentTime\":5}"
#guard Command.commandName ({ animations := ["a1"], currentTime := 5 } : PSeekAnimations)
  = "Animation.seekAnimations"

#guard encode (ToJSON.toJSON ({ animations := ["a1"], paused := true } : PSetPaused))
  = "{\"animations\":[\"a1\"],\"paused\":true}"
#guard Command.commandName ({ animations := ["a1"], paused := true } : PSetPaused) = "Animation.setPaused"

#guard encode (ToJSON.toJSON ({ playbackRate := 2 } : PSetPlaybackRate)) = "{\"playbackRate\":2}"
#guard Command.commandName ({ playbackRate := 2 } : PSetPlaybackRate) = "Animation.setPlaybackRate"

#guard encode (ToJSON.toJSON ({ animationId := "a1", duration := 100, delay := 0 } : PSetTiming))
  = "{\"animationId\":\"a1\",\"duration\":100,\"delay\":0}"
#guard Command.commandName ({ animationId := "a1", duration := 100, delay := 0 } : PSetTiming)
  = "Animation.setTiming"

end Tests.CDP.Domains.Animation
