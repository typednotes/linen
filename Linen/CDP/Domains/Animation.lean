/-
  Linen.CDP.Domains.Animation — the `Animation` CDP domain

  Ports `CDP.Domains.Animation` (see `docs/imports/cdp/dependencies.md`);
  naming conventions as in `CDP.Domains.Memory`'s docstring. References
  `DOM.BackendNodeId` from `CDP.Domains.DOMPageNetworkEmulationSecurity` and
  `RemoteObject` from `CDP.Domains.Runtime`.

  None of this module's own types are self- or mutually-recursive, so no
  termination proofs are needed here. `Runtime.RemoteObject` lacks
  `DecidableEq` (it embeds `Data.Json.Value`), so `ResolveAnimation` — which
  carries one — derives only `Repr, BEq`.
-/
import Linen.CDP.Internal.Utils
import Linen.CDP.Domains.DOMPageNetworkEmulationSecurity
import Linen.CDP.Domains.Runtime

namespace CDP.Domains.Animation

open Data.Json (Value ToJSON FromJSON)
open CDP.Internal.Utils (Command Event)

-- ── Types ──

/-- Animation type: how the animation was created. -/
inductive AnimationType where
  | cssTransition | cssAnimation | webAnimation
  deriving Repr, BEq, DecidableEq

instance : FromJSON AnimationType where
  parseJSON
    | .string "CSSTransition" => .ok .cssTransition
    | .string "CSSAnimation" => .ok .cssAnimation
    | .string "WebAnimation" => .ok .webAnimation
    | v => .error s!"failed to parse AnimationType: {repr v}"

instance : ToJSON AnimationType where
  toJSON
    | .cssTransition => .string "CSSTransition"
    | .cssAnimation => .string "CSSAnimation"
    | .webAnimation => .string "WebAnimation"

/-- Keyframe Style. -/
structure KeyframeStyle where
  /-- Keyframe's time offset. -/
  offset : String
  /-- `AnimationEffect`'s timing function. -/
  easing : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON KeyframeStyle where
  parseJSON v := do
    .ok
      { offset := ← Value.getField v "offset" >>= FromJSON.parseJSON
        easing := ← Value.getField v "easing" >>= FromJSON.parseJSON }

instance : ToJSON KeyframeStyle where
  toJSON p := Data.Json.object [("offset", ToJSON.toJSON p.offset), ("easing", ToJSON.toJSON p.easing)]

/-- Keyframes Rule. -/
structure KeyframesRule where
  /-- CSS keyframed animation's name. -/
  name : Option String := none
  /-- List of animation keyframes. -/
  keyframes : List KeyframeStyle
  deriving Repr, BEq, DecidableEq

instance : FromJSON KeyframesRule where
  parseJSON v := do
    .ok
      { name := ← (← Value.getFieldOpt v "name").mapM FromJSON.parseJSON
        keyframes := ← Value.getField v "keyframes" >>= FromJSON.parseJSON }

instance : ToJSON KeyframesRule where
  toJSON p := Data.Json.object <|
    (p.name.map fun v => ("name", ToJSON.toJSON v)).toList
    ++ [("keyframes", ToJSON.toJSON p.keyframes)]

/-- `AnimationEffect` instance. -/
structure AnimationEffect where
  /-- `AnimationEffect`'s delay. -/
  delay : Float
  /-- `AnimationEffect`'s end delay. -/
  endDelay : Float
  /-- `AnimationEffect`'s iteration start. -/
  iterationStart : Float
  /-- `AnimationEffect`'s iterations. -/
  iterations : Float
  /-- `AnimationEffect`'s iteration duration. -/
  duration : Float
  /-- `AnimationEffect`'s playback direction. -/
  direction : String
  /-- `AnimationEffect`'s fill mode. -/
  fill : String
  /-- `AnimationEffect`'s target node. -/
  backendNodeId : Option CDP.Domains.DOMPageNetworkEmulationSecurity.DOM.BackendNodeId := none
  /-- `AnimationEffect`'s keyframes. -/
  keyframesRule : Option KeyframesRule := none
  /-- `AnimationEffect`'s timing function. -/
  easing : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON AnimationEffect where
  parseJSON v := do
    .ok
      { delay := ← Value.getField v "delay" >>= FromJSON.parseJSON
        endDelay := ← Value.getField v "endDelay" >>= FromJSON.parseJSON
        iterationStart := ← Value.getField v "iterationStart" >>= FromJSON.parseJSON
        iterations := ← Value.getField v "iterations" >>= FromJSON.parseJSON
        duration := ← Value.getField v "duration" >>= FromJSON.parseJSON
        direction := ← Value.getField v "direction" >>= FromJSON.parseJSON
        fill := ← Value.getField v "fill" >>= FromJSON.parseJSON
        backendNodeId := ← (← Value.getFieldOpt v "backendNodeId").mapM FromJSON.parseJSON
        keyframesRule := ← (← Value.getFieldOpt v "keyframesRule").mapM FromJSON.parseJSON
        easing := ← Value.getField v "easing" >>= FromJSON.parseJSON }

instance : ToJSON AnimationEffect where
  toJSON p := Data.Json.object <|
    [ ("delay", ToJSON.toJSON p.delay), ("endDelay", ToJSON.toJSON p.endDelay)
    , ("iterationStart", ToJSON.toJSON p.iterationStart), ("iterations", ToJSON.toJSON p.iterations)
    , ("duration", ToJSON.toJSON p.duration), ("direction", ToJSON.toJSON p.direction)
    , ("fill", ToJSON.toJSON p.fill) ]
    ++ (p.backendNodeId.map fun v => ("backendNodeId", ToJSON.toJSON v)).toList
    ++ (p.keyframesRule.map fun v => ("keyframesRule", ToJSON.toJSON v)).toList
    ++ [("easing", ToJSON.toJSON p.easing)]

/-- Animation instance. -/
structure Animation where
  /-- `Animation`'s id. -/
  id : String
  /-- `Animation`'s name. -/
  name : String
  /-- `Animation`'s internal paused state. -/
  pausedState : Bool
  /-- `Animation`'s play state. -/
  playState : String
  /-- `Animation`'s playback rate. -/
  playbackRate : Float
  /-- `Animation`'s start time. -/
  startTime : Float
  /-- `Animation`'s current time. -/
  currentTime : Float
  /-- Animation type of `Animation`. -/
  type : AnimationType
  /-- `Animation`'s source animation node. -/
  source : Option AnimationEffect := none
  /-- A unique ID for `Animation` representing the sources that triggered this
      CSS animation/transition. -/
  cssId : Option String := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON Animation where
  parseJSON v := do
    .ok
      { id := ← Value.getField v "id" >>= FromJSON.parseJSON
        name := ← Value.getField v "name" >>= FromJSON.parseJSON
        pausedState := ← Value.getField v "pausedState" >>= FromJSON.parseJSON
        playState := ← Value.getField v "playState" >>= FromJSON.parseJSON
        playbackRate := ← Value.getField v "playbackRate" >>= FromJSON.parseJSON
        startTime := ← Value.getField v "startTime" >>= FromJSON.parseJSON
        currentTime := ← Value.getField v "currentTime" >>= FromJSON.parseJSON
        type := ← Value.getField v "type" >>= FromJSON.parseJSON
        source := ← (← Value.getFieldOpt v "source").mapM FromJSON.parseJSON
        cssId := ← (← Value.getFieldOpt v "cssId").mapM FromJSON.parseJSON }

instance : ToJSON Animation where
  toJSON p := Data.Json.object <|
    [ ("id", ToJSON.toJSON p.id), ("name", ToJSON.toJSON p.name)
    , ("pausedState", ToJSON.toJSON p.pausedState), ("playState", ToJSON.toJSON p.playState)
    , ("playbackRate", ToJSON.toJSON p.playbackRate), ("startTime", ToJSON.toJSON p.startTime)
    , ("currentTime", ToJSON.toJSON p.currentTime), ("type", ToJSON.toJSON p.type) ]
    ++ (p.source.map fun v => ("source", ToJSON.toJSON v)).toList
    ++ (p.cssId.map fun v => ("cssId", ToJSON.toJSON v)).toList

-- ── Events ──

/-- Type of the `Animation.animationCanceled` event. -/
structure AnimationCanceled where
  /-- Id of the animation that was cancelled. -/
  id : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON AnimationCanceled where
  parseJSON v := do .ok { id := ← Value.getField v "id" >>= FromJSON.parseJSON }

instance : Event AnimationCanceled where
  eventName := "Animation.animationCanceled"

/-- Type of the `Animation.animationCreated` event. -/
structure AnimationCreated where
  /-- Id of the animation that was created. -/
  id : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON AnimationCreated where
  parseJSON v := do .ok { id := ← Value.getField v "id" >>= FromJSON.parseJSON }

instance : Event AnimationCreated where
  eventName := "Animation.animationCreated"

/-- Type of the `Animation.animationStarted` event. -/
structure AnimationStarted where
  /-- Animation that was started. -/
  animation : Animation
  deriving Repr, BEq, DecidableEq

instance : FromJSON AnimationStarted where
  parseJSON v := do .ok { animation := ← Value.getField v "animation" >>= FromJSON.parseJSON }

instance : Event AnimationStarted where
  eventName := "Animation.animationStarted"

-- ── Commands ──

/-- Parameters of the `Animation.disable` command: disables animation domain
    notifications. -/
structure PDisable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PDisable where toJSON _ := .null

instance : Command PDisable where
  Response := Unit
  commandName _ := "Animation.disable"
  decodeResponse _ := .ok ()

/-- Parameters of the `Animation.enable` command: enables animation domain
    notifications. -/
structure PEnable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PEnable where toJSON _ := .null

instance : Command PEnable where
  Response := Unit
  commandName _ := "Animation.enable"
  decodeResponse _ := .ok ()

/-- Parameters of the `Animation.getCurrentTime` command: returns the current
    time of an animation. -/
structure PGetCurrentTime where
  /-- Id of animation. -/
  id : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetCurrentTime where
  toJSON p := Data.Json.object [("id", ToJSON.toJSON p.id)]

/-- Response of the `Animation.getCurrentTime` command. -/
structure GetCurrentTime where
  /-- Current time of the page. -/
  currentTime : Float
  deriving Repr, BEq, DecidableEq

instance : FromJSON GetCurrentTime where
  parseJSON v := do .ok { currentTime := ← Value.getField v "currentTime" >>= FromJSON.parseJSON }

instance : Command PGetCurrentTime where
  Response := GetCurrentTime
  commandName _ := "Animation.getCurrentTime"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Animation.getPlaybackRate` command: gets the playback
    rate of the document timeline. -/
structure PGetPlaybackRate where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetPlaybackRate where toJSON _ := .null

/-- Response of the `Animation.getPlaybackRate` command. -/
structure GetPlaybackRate where
  /-- Playback rate for animations on page. -/
  playbackRate : Float
  deriving Repr, BEq, DecidableEq

instance : FromJSON GetPlaybackRate where
  parseJSON v := do .ok { playbackRate := ← Value.getField v "playbackRate" >>= FromJSON.parseJSON }

instance : Command PGetPlaybackRate where
  Response := GetPlaybackRate
  commandName _ := "Animation.getPlaybackRate"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Animation.releaseAnimations` command: releases a set of
    animations to no longer be manipulated. -/
structure PReleaseAnimations where
  /-- List of animation ids to seek. -/
  animations : List String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PReleaseAnimations where
  toJSON p := Data.Json.object [("animations", ToJSON.toJSON p.animations)]

instance : Command PReleaseAnimations where
  Response := Unit
  commandName _ := "Animation.releaseAnimations"
  decodeResponse _ := .ok ()

/-- Parameters of the `Animation.resolveAnimation` command: gets the remote
    object of the Animation. -/
structure PResolveAnimation where
  /-- Animation id. -/
  animationId : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PResolveAnimation where
  toJSON p := Data.Json.object [("animationId", ToJSON.toJSON p.animationId)]

/-- Response of the `Animation.resolveAnimation` command. -/
structure ResolveAnimation where
  /-- Corresponding remote object. -/
  remoteObject : CDP.Domains.Runtime.RemoteObject
  deriving Repr, BEq

instance : FromJSON ResolveAnimation where
  parseJSON v := do .ok { remoteObject := ← Value.getField v "remoteObject" >>= FromJSON.parseJSON }

instance : Command PResolveAnimation where
  Response := ResolveAnimation
  commandName _ := "Animation.resolveAnimation"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `Animation.seekAnimations` command: seeks a set of
    animations to a particular time within each animation. -/
structure PSeekAnimations where
  /-- List of animation ids to seek. -/
  animations : List String
  /-- Set the current time of each animation. -/
  currentTime : Float
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSeekAnimations where
  toJSON p := Data.Json.object
    [("animations", ToJSON.toJSON p.animations), ("currentTime", ToJSON.toJSON p.currentTime)]

instance : Command PSeekAnimations where
  Response := Unit
  commandName _ := "Animation.seekAnimations"
  decodeResponse _ := .ok ()

/-- Parameters of the `Animation.setPaused` command: sets the paused state of
    a set of animations. -/
structure PSetPaused where
  /-- Animations to set the pause state of. -/
  animations : List String
  /-- Paused state to set to. -/
  paused : Bool
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetPaused where
  toJSON p := Data.Json.object [("animations", ToJSON.toJSON p.animations), ("paused", ToJSON.toJSON p.paused)]

instance : Command PSetPaused where
  Response := Unit
  commandName _ := "Animation.setPaused"
  decodeResponse _ := .ok ()

/-- Parameters of the `Animation.setPlaybackRate` command: sets the playback
    rate of the document timeline. -/
structure PSetPlaybackRate where
  /-- Playback rate for animations on page. -/
  playbackRate : Float
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetPlaybackRate where
  toJSON p := Data.Json.object [("playbackRate", ToJSON.toJSON p.playbackRate)]

instance : Command PSetPlaybackRate where
  Response := Unit
  commandName _ := "Animation.setPlaybackRate"
  decodeResponse _ := .ok ()

/-- Parameters of the `Animation.setTiming` command: sets the timing of an
    animation node. -/
structure PSetTiming where
  /-- Animation id. -/
  animationId : String
  /-- Duration of the animation. -/
  duration : Float
  /-- Delay of the animation. -/
  delay : Float
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetTiming where
  toJSON p := Data.Json.object
    [ ("animationId", ToJSON.toJSON p.animationId), ("duration", ToJSON.toJSON p.duration)
    , ("delay", ToJSON.toJSON p.delay) ]

instance : Command PSetTiming where
  Response := Unit
  commandName _ := "Animation.setTiming"
  decodeResponse _ := .ok ()

end CDP.Domains.Animation
