/-
  Linen.CDP.Domains.HeadlessExperimental — the `HeadlessExperimental` CDP domain

  Ports `CDP.Domains.HeadlessExperimental` (see
  `docs/imports/cdp/dependencies.md`); naming conventions as in
  `CDP.Domains.CacheStorage`'s docstring.
-/
import Linen.CDP.Internal.Utils

namespace CDP.Domains.HeadlessExperimental

open Data.Json (Value ToJSON FromJSON)
open CDP.Internal.Utils (Command)

/-- `HeadlessExperimental.ScreenshotParams`'s image compression format. -/
inductive ScreenshotParams.Format where
  | jpeg | png
  deriving Repr, BEq, DecidableEq

instance : FromJSON ScreenshotParams.Format where
  parseJSON
    | .string "jpeg" => .ok .jpeg
    | .string "png" => .ok .png
    | v => .error s!"failed to parse ScreenshotParams.Format: {repr v}"

instance : ToJSON ScreenshotParams.Format where
  toJSON | .jpeg => .string "jpeg" | .png => .string "png"

/-- Encoding options for a screenshot. -/
structure ScreenshotParams where
  /-- Image compression format (defaults to png). -/
  format : Option ScreenshotParams.Format := none
  /-- Compression quality from range `[0..100]` (jpeg only). -/
  quality : Option Int := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON ScreenshotParams where
  parseJSON v := do
    .ok
      { format := ← (← Value.getFieldOpt v "format").mapM FromJSON.parseJSON
        quality := ← (← Value.getFieldOpt v "quality").mapM FromJSON.parseJSON }

instance : ToJSON ScreenshotParams where
  toJSON p := Data.Json.object <|
    (p.format.map fun v => ("format", ToJSON.toJSON v)).toList
    ++ (p.quality.map fun v => ("quality", ToJSON.toJSON v)).toList

/-- Parameters of the `HeadlessExperimental.beginFrame` command: sends a
    `BeginFrame` to the target and returns when the frame was completed.
    Optionally captures a screenshot from the resulting frame. Requires that
    the target was created with `BeginFrameControl` enabled. -/
structure PBeginFrame where
  /-- Timestamp of this `BeginFrame` in Renderer TimeTicks (milliseconds of
      uptime). If not set, the current time will be used. -/
  frameTimeTicks : Option Float := none
  /-- The interval between `BeginFrame`s that is reported to the compositor, in
      milliseconds. Defaults to a 60 frames/second interval, i.e. about 16.666
      milliseconds. -/
  interval : Option Float := none
  /-- Whether updates should not be committed and drawn onto the display. `false`
      by default. -/
  noDisplayUpdates : Option Bool := none
  /-- If set, a screenshot of the frame will be captured and returned in the
      response. -/
  screenshot : Option ScreenshotParams := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PBeginFrame where
  toJSON p := Data.Json.object <|
    (p.frameTimeTicks.map fun v => ("frameTimeTicks", ToJSON.toJSON v)).toList
    ++ (p.interval.map fun v => ("interval", ToJSON.toJSON v)).toList
    ++ (p.noDisplayUpdates.map fun v => ("noDisplayUpdates", ToJSON.toJSON v)).toList
    ++ (p.screenshot.map fun v => ("screenshot", ToJSON.toJSON v)).toList

/-- Response of the `HeadlessExperimental.beginFrame` command. -/
structure BeginFrame where
  /-- Whether the `BeginFrame` resulted in damage and, thus, a new frame was
      committed to the display. -/
  hasDamage : Bool
  /-- Base64-encoded image data of the screenshot, if one was requested and
      successfully taken. -/
  screenshotData : Option String := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON BeginFrame where
  parseJSON v := do
    .ok
      { hasDamage := ← Value.getField v "hasDamage" >>= FromJSON.parseJSON
        screenshotData := ← (← Value.getFieldOpt v "screenshotData").mapM FromJSON.parseJSON }

instance : Command PBeginFrame where
  Response := BeginFrame
  commandName _ := "HeadlessExperimental.beginFrame"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `HeadlessExperimental.disable` command: disables headless
    events for the target. -/
structure PDisable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PDisable where toJSON _ := .null

instance : Command PDisable where
  Response := Unit
  commandName _ := "HeadlessExperimental.disable"
  decodeResponse _ := .ok ()

/-- Parameters of the `HeadlessExperimental.enable` command: enables headless
    events for the target. -/
structure PEnable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PEnable where toJSON _ := .null

instance : Command PEnable where
  Response := Unit
  commandName _ := "HeadlessExperimental.enable"
  decodeResponse _ := .ok ()

end CDP.Domains.HeadlessExperimental
