/-
  Linen.CDP.Domains.Input — the `Input` CDP domain

  Ports `CDP.Domains.Input` (see `docs/imports/cdp/dependencies.md`); naming
  conventions as in `CDP.Domains.CacheStorage`'s docstring. Each command's
  own `Type`/`PointerType` enum is nested under that command's params
  structure (e.g. `PDispatchMouseEvent.Type`), matching how
  `HeadlessExperimental.ScreenshotParams.Format` is nested.
-/
import Linen.CDP.Internal.Utils

namespace CDP.Domains.Input

open Data.Json (Value ToJSON FromJSON)
open CDP.Internal.Utils (Command Event)

/-- A touch point. -/
structure TouchPoint where
  /-- X coordinate of the event relative to the main frame's viewport in CSS
      pixels. -/
  x : Float
  /-- Y coordinate of the event relative to the main frame's viewport in CSS
      pixels. 0 refers to the top of the viewport and Y increases as it
      proceeds towards the bottom of the viewport. -/
  y : Float
  /-- X radius of the touch area (default: 1.0). -/
  radiusX : Option Float := none
  /-- Y radius of the touch area (default: 1.0). -/
  radiusY : Option Float := none
  /-- Rotation angle (default: 0.0). -/
  rotationAngle : Option Float := none
  /-- Force (default: 1.0). -/
  force : Option Float := none
  /-- The normalized tangential pressure, which has a range of `[-1,1]`
      (default: 0). -/
  tangentialPressure : Option Float := none
  /-- The plane angle between the Y-Z plane and the plane containing both the
      stylus axis and the Y axis, in degrees of the range `[-90,90]`, a
      positive `tiltX` is to the right (default: 0). -/
  tiltX : Option Int := none
  /-- The plane angle between the X-Z plane and the plane containing both the
      stylus axis and the X axis, in degrees of the range `[-90,90]`, a
      positive `tiltY` is towards the user (default: 0). -/
  tiltY : Option Int := none
  /-- The clockwise rotation of a pen stylus around its own major axis, in
      degrees in the range `[0,359]` (default: 0). -/
  twist : Option Int := none
  /-- Identifier used to track touch sources between events, must be unique
      within an event. -/
  id : Option Float := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON TouchPoint where
  parseJSON v := do
    .ok
      { x := ← Value.getField v "x" >>= FromJSON.parseJSON
        y := ← Value.getField v "y" >>= FromJSON.parseJSON
        radiusX := ← (← Value.getFieldOpt v "radiusX").mapM FromJSON.parseJSON
        radiusY := ← (← Value.getFieldOpt v "radiusY").mapM FromJSON.parseJSON
        rotationAngle := ← (← Value.getFieldOpt v "rotationAngle").mapM FromJSON.parseJSON
        force := ← (← Value.getFieldOpt v "force").mapM FromJSON.parseJSON
        tangentialPressure := ← (← Value.getFieldOpt v "tangentialPressure").mapM FromJSON.parseJSON
        tiltX := ← (← Value.getFieldOpt v "tiltX").mapM FromJSON.parseJSON
        tiltY := ← (← Value.getFieldOpt v "tiltY").mapM FromJSON.parseJSON
        twist := ← (← Value.getFieldOpt v "twist").mapM FromJSON.parseJSON
        id := ← (← Value.getFieldOpt v "id").mapM FromJSON.parseJSON }

instance : ToJSON TouchPoint where
  toJSON p := Data.Json.object <|
    [("x", ToJSON.toJSON p.x), ("y", ToJSON.toJSON p.y)]
    ++ (p.radiusX.map fun v => ("radiusX", ToJSON.toJSON v)).toList
    ++ (p.radiusY.map fun v => ("radiusY", ToJSON.toJSON v)).toList
    ++ (p.rotationAngle.map fun v => ("rotationAngle", ToJSON.toJSON v)).toList
    ++ (p.force.map fun v => ("force", ToJSON.toJSON v)).toList
    ++ (p.tangentialPressure.map fun v => ("tangentialPressure", ToJSON.toJSON v)).toList
    ++ (p.tiltX.map fun v => ("tiltX", ToJSON.toJSON v)).toList
    ++ (p.tiltY.map fun v => ("tiltY", ToJSON.toJSON v)).toList
    ++ (p.twist.map fun v => ("twist", ToJSON.toJSON v)).toList
    ++ (p.id.map fun v => ("id", ToJSON.toJSON v)).toList

/-- The source of a synthesized gesture. -/
inductive GestureSourceType where
  | default | touch | mouse
  deriving Repr, BEq, DecidableEq

instance : FromJSON GestureSourceType where
  parseJSON
    | .string "default" => .ok .default
    | .string "touch" => .ok .touch
    | .string "mouse" => .ok .mouse
    | v => .error s!"failed to parse GestureSourceType: {repr v}"

instance : ToJSON GestureSourceType where
  toJSON | .default => .string "default" | .touch => .string "touch" | .mouse => .string "mouse"

/-- A mouse button. -/
inductive MouseButton where
  | none | left | middle | right | back | forward
  deriving Repr, BEq, DecidableEq

instance : FromJSON MouseButton where
  parseJSON
    | .string "none" => .ok .none
    | .string "left" => .ok .left
    | .string "middle" => .ok .middle
    | .string "right" => .ok .right
    | .string "back" => .ok .back
    | .string "forward" => .ok .forward
    | v => .error s!"failed to parse MouseButton: {repr v}"

instance : ToJSON MouseButton where
  toJSON
    | .none => .string "none" | .left => .string "left" | .middle => .string "middle"
    | .right => .string "right" | .back => .string "back" | .forward => .string "forward"

/-- UTC time in seconds, counted from January 1, 1970. -/
abbrev TimeSinceEpoch := Float

/-- One item of dragged data. -/
structure DragDataItem where
  /-- Mime type of the dragged data. -/
  mimeType : String
  /-- Depending on the value of `mimeType`, contains the dragged link, text,
      HTML markup, or any other data. -/
  data : String
  /-- Title associated with a link. Only valid when `mimeType` == `"text/uri-list"`. -/
  title : Option String := none
  /-- Stores the base URL for the contained markup. Only valid when `mimeType`
      == `"text/html"`. -/
  baseURL : Option String := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON DragDataItem where
  parseJSON v := do
    .ok
      { mimeType := ← Value.getField v "mimeType" >>= FromJSON.parseJSON
        data := ← Value.getField v "data" >>= FromJSON.parseJSON
        title := ← (← Value.getFieldOpt v "title").mapM FromJSON.parseJSON
        baseURL := ← (← Value.getFieldOpt v "baseURL").mapM FromJSON.parseJSON }

instance : ToJSON DragDataItem where
  toJSON p := Data.Json.object <|
    [("mimeType", ToJSON.toJSON p.mimeType), ("data", ToJSON.toJSON p.data)]
    ++ (p.title.map fun v => ("title", ToJSON.toJSON v)).toList
    ++ (p.baseURL.map fun v => ("baseURL", ToJSON.toJSON v)).toList

/-- Dragged data. -/
structure DragData where
  items : List DragDataItem
  /-- List of filenames that should be included when dropping. -/
  files : Option (List String) := none
  /-- Bit field representing allowed drag operations. Copy = 1, Link = 2, Move
      = 16. -/
  dragOperationsMask : Int
  deriving Repr, BEq, DecidableEq

instance : FromJSON DragData where
  parseJSON v := do
    .ok
      { items := ← Value.getField v "items" >>= FromJSON.parseJSON
        files := ← (← Value.getFieldOpt v "files").mapM FromJSON.parseJSON
        dragOperationsMask := ← Value.getField v "dragOperationsMask" >>= FromJSON.parseJSON }

instance : ToJSON DragData where
  toJSON p := Data.Json.object <|
    [("items", ToJSON.toJSON p.items)]
    ++ (p.files.map fun v => ("files", ToJSON.toJSON v)).toList
    ++ [("dragOperationsMask", ToJSON.toJSON p.dragOperationsMask)]

/-- The `Input.dragIntercepted` event. -/
structure DragIntercepted where
  data : DragData
  deriving Repr, BEq, DecidableEq

instance : FromJSON DragIntercepted where
  parseJSON v := do .ok { data := ← Value.getField v "data" >>= FromJSON.parseJSON }

instance : Event DragIntercepted where
  eventName := "Input.dragIntercepted"

-- ── dispatchDragEvent ──

/-- The type of a drag event. -/
inductive PDispatchDragEvent.Type where
  | dragEnter | dragOver | drop | dragCancel
  deriving Repr, BEq, DecidableEq

instance : FromJSON PDispatchDragEvent.Type where
  parseJSON
    | .string "dragEnter" => .ok .dragEnter
    | .string "dragOver" => .ok .dragOver
    | .string "drop" => .ok .drop
    | .string "dragCancel" => .ok .dragCancel
    | v => .error s!"failed to parse PDispatchDragEvent.Type: {repr v}"

instance : ToJSON PDispatchDragEvent.Type where
  toJSON
    | .dragEnter => .string "dragEnter" | .dragOver => .string "dragOver"
    | .drop => .string "drop" | .dragCancel => .string "dragCancel"

/-- Parameters of the `Input.dispatchDragEvent` command: dispatches a drag
    event into the page. -/
structure PDispatchDragEvent where
  /-- Type of the drag event. -/
  type : PDispatchDragEvent.Type
  /-- X coordinate of the event relative to the main frame's viewport in CSS
      pixels. -/
  x : Float
  /-- Y coordinate of the event relative to the main frame's viewport in CSS
      pixels. 0 refers to the top of the viewport and Y increases as it
      proceeds towards the bottom of the viewport. -/
  y : Float
  data : DragData
  /-- Bit field representing pressed modifier keys. Alt=1, Ctrl=2,
      Meta/Command=4, Shift=8 (default: 0). -/
  modifiers : Option Int := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PDispatchDragEvent where
  toJSON p := Data.Json.object <|
    [("type", ToJSON.toJSON p.type), ("x", ToJSON.toJSON p.x), ("y", ToJSON.toJSON p.y)
    , ("data", ToJSON.toJSON p.data)]
    ++ (p.modifiers.map fun v => ("modifiers", ToJSON.toJSON v)).toList

instance : Command PDispatchDragEvent where
  Response := Unit
  commandName _ := "Input.dispatchDragEvent"
  decodeResponse _ := .ok ()

-- ── dispatchKeyEvent ──

/-- The type of a key event. -/
inductive PDispatchKeyEvent.Type where
  | keyDown | keyUp | rawKeyDown | char
  deriving Repr, BEq, DecidableEq

instance : FromJSON PDispatchKeyEvent.Type where
  parseJSON
    | .string "keyDown" => .ok .keyDown
    | .string "keyUp" => .ok .keyUp
    | .string "rawKeyDown" => .ok .rawKeyDown
    | .string "char" => .ok .char
    | v => .error s!"failed to parse PDispatchKeyEvent.Type: {repr v}"

instance : ToJSON PDispatchKeyEvent.Type where
  toJSON
    | .keyDown => .string "keyDown" | .keyUp => .string "keyUp"
    | .rawKeyDown => .string "rawKeyDown" | .char => .string "char"

/-- Parameters of the `Input.dispatchKeyEvent` command: dispatches a key event
    to the page. -/
structure PDispatchKeyEvent where
  /-- Type of the key event. -/
  type : PDispatchKeyEvent.Type
  /-- Bit field representing pressed modifier keys. Alt=1, Ctrl=2,
      Meta/Command=4, Shift=8 (default: 0). -/
  modifiers : Option Int := none
  /-- Time at which the event occurred. -/
  timestamp : Option TimeSinceEpoch := none
  /-- Text as generated by processing a virtual key code with a keyboard
      layout. Not needed for `keyUp` and `rawKeyDown` events (default: ""). -/
  text : Option String := none
  /-- Text that would have been generated by the keyboard if no modifiers were
      pressed (except for shift). Useful for shortcut (accelerator) key
      handling (default: ""). -/
  unmodifiedText : Option String := none
  /-- Unique key identifier (e.g. `"U+0041"`) (default: ""). -/
  keyIdentifier : Option String := none
  /-- Unique DOM defined string value for each physical key (e.g. `"KeyA"`)
      (default: ""). -/
  code : Option String := none
  /-- Unique DOM defined string value describing the meaning of the key in the
      context of active modifiers, keyboard layout, etc (e.g. `"AltGr"`)
      (default: ""). -/
  key : Option String := none
  /-- Windows virtual key code (default: 0). -/
  windowsVirtualKeyCode : Option Int := none
  /-- Native virtual key code (default: 0). -/
  nativeVirtualKeyCode : Option Int := none
  /-- Whether the event was generated from auto repeat (default: false). -/
  autoRepeat : Option Bool := none
  /-- Whether the event was generated from the keypad (default: false). -/
  isKeypad : Option Bool := none
  /-- Whether the event was a system key event (default: false). -/
  isSystemKey : Option Bool := none
  /-- Whether the event was from the left or right side of the keyboard.
      1=Left, 2=Right (default: 0). -/
  location : Option Int := none
  /-- Editing commands to send with the key event (e.g. `"selectAll"`)
      (default: `[]`). -/
  commands : Option (List String) := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PDispatchKeyEvent where
  toJSON p := Data.Json.object <|
    [("type", ToJSON.toJSON p.type)]
    ++ (p.modifiers.map fun v => ("modifiers", ToJSON.toJSON v)).toList
    ++ (p.timestamp.map fun v => ("timestamp", ToJSON.toJSON v)).toList
    ++ (p.text.map fun v => ("text", ToJSON.toJSON v)).toList
    ++ (p.unmodifiedText.map fun v => ("unmodifiedText", ToJSON.toJSON v)).toList
    ++ (p.keyIdentifier.map fun v => ("keyIdentifier", ToJSON.toJSON v)).toList
    ++ (p.code.map fun v => ("code", ToJSON.toJSON v)).toList
    ++ (p.key.map fun v => ("key", ToJSON.toJSON v)).toList
    ++ (p.windowsVirtualKeyCode.map fun v => ("windowsVirtualKeyCode", ToJSON.toJSON v)).toList
    ++ (p.nativeVirtualKeyCode.map fun v => ("nativeVirtualKeyCode", ToJSON.toJSON v)).toList
    ++ (p.autoRepeat.map fun v => ("autoRepeat", ToJSON.toJSON v)).toList
    ++ (p.isKeypad.map fun v => ("isKeypad", ToJSON.toJSON v)).toList
    ++ (p.isSystemKey.map fun v => ("isSystemKey", ToJSON.toJSON v)).toList
    ++ (p.location.map fun v => ("location", ToJSON.toJSON v)).toList
    ++ (p.commands.map fun v => ("commands", ToJSON.toJSON v)).toList

instance : Command PDispatchKeyEvent where
  Response := Unit
  commandName _ := "Input.dispatchKeyEvent"
  decodeResponse _ := .ok ()

-- ── insertText ──

/-- Parameters of the `Input.insertText` command: emulates inserting text that
    doesn't come from a key press, e.g. an emoji keyboard or an IME. -/
structure PInsertText where
  /-- The text to insert. -/
  text : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PInsertText where
  toJSON p := Data.Json.object [("text", ToJSON.toJSON p.text)]

instance : Command PInsertText where
  Response := Unit
  commandName _ := "Input.insertText"
  decodeResponse _ := .ok ()

-- ── imeSetComposition ──

/-- Parameters of the `Input.imeSetComposition` command: sets the current
    candidate text for IME. Use `imeCommitComposition` (not modeled; upstream
    doesn't expose it either) to commit the final text; use `imeSetComposition`
    with an empty string as `text` to cancel composition. -/
structure PImeSetComposition where
  /-- The text to insert. -/
  text : String
  /-- Selection start. -/
  selectionStart : Int
  /-- Selection end. -/
  selectionEnd : Int
  /-- Replacement start. -/
  replacementStart : Option Int := none
  /-- Replacement end. -/
  replacementEnd : Option Int := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PImeSetComposition where
  toJSON p := Data.Json.object <|
    [ ("text", ToJSON.toJSON p.text), ("selectionStart", ToJSON.toJSON p.selectionStart)
    , ("selectionEnd", ToJSON.toJSON p.selectionEnd) ]
    ++ (p.replacementStart.map fun v => ("replacementStart", ToJSON.toJSON v)).toList
    ++ (p.replacementEnd.map fun v => ("replacementEnd", ToJSON.toJSON v)).toList

instance : Command PImeSetComposition where
  Response := Unit
  commandName _ := "Input.imeSetComposition"
  decodeResponse _ := .ok ()

-- ── dispatchMouseEvent ──

/-- The type of a mouse event. -/
inductive PDispatchMouseEvent.Type where
  | mousePressed | mouseReleased | mouseMoved | mouseWheel
  deriving Repr, BEq, DecidableEq

instance : FromJSON PDispatchMouseEvent.Type where
  parseJSON
    | .string "mousePressed" => .ok .mousePressed
    | .string "mouseReleased" => .ok .mouseReleased
    | .string "mouseMoved" => .ok .mouseMoved
    | .string "mouseWheel" => .ok .mouseWheel
    | v => .error s!"failed to parse PDispatchMouseEvent.Type: {repr v}"

instance : ToJSON PDispatchMouseEvent.Type where
  toJSON
    | .mousePressed => .string "mousePressed" | .mouseReleased => .string "mouseReleased"
    | .mouseMoved => .string "mouseMoved" | .mouseWheel => .string "mouseWheel"

/-- The type of pointer for a mouse event. -/
inductive PDispatchMouseEvent.PointerType where
  | mouse | pen
  deriving Repr, BEq, DecidableEq

instance : FromJSON PDispatchMouseEvent.PointerType where
  parseJSON
    | .string "mouse" => .ok .mouse
    | .string "pen" => .ok .pen
    | v => .error s!"failed to parse PDispatchMouseEvent.PointerType: {repr v}"

instance : ToJSON PDispatchMouseEvent.PointerType where
  toJSON | .mouse => .string "mouse" | .pen => .string "pen"

/-- Parameters of the `Input.dispatchMouseEvent` command: dispatches a mouse
    event to the page. -/
structure PDispatchMouseEvent where
  /-- Type of the mouse event. -/
  type : PDispatchMouseEvent.Type
  /-- X coordinate of the event relative to the main frame's viewport in CSS
      pixels. -/
  x : Float
  /-- Y coordinate of the event relative to the main frame's viewport in CSS
      pixels. 0 refers to the top of the viewport and Y increases as it
      proceeds towards the bottom of the viewport. -/
  y : Float
  /-- Bit field representing pressed modifier keys. Alt=1, Ctrl=2,
      Meta/Command=4, Shift=8 (default: 0). -/
  modifiers : Option Int := none
  /-- Time at which the event occurred. -/
  timestamp : Option TimeSinceEpoch := none
  /-- Mouse button (default: "none"). -/
  button : Option MouseButton := none
  /-- A number indicating which buttons are pressed on the mouse when a mouse
      event is triggered. Left=1, Right=2, Middle=4, Back=8, Forward=16,
      None=0. -/
  buttons : Option Int := none
  /-- Number of times the mouse button was clicked (default: 0). -/
  clickCount : Option Int := none
  /-- The normalized pressure, which has a range of `[0,1]` (default: 0). -/
  force : Option Float := none
  /-- The normalized tangential pressure, which has a range of `[-1,1]`
      (default: 0). -/
  tangentialPressure : Option Float := none
  /-- (default: 0). -/
  tiltX : Option Int := none
  /-- (default: 0). -/
  tiltY : Option Int := none
  /-- The clockwise rotation of a pen stylus around its own major axis, in
      degrees in the range `[0,359]` (default: 0). -/
  twist : Option Int := none
  /-- X delta in CSS pixels for mouse wheel event (default: 0). -/
  deltaX : Option Float := none
  /-- Y delta in CSS pixels for mouse wheel event (default: 0). -/
  deltaY : Option Float := none
  /-- Pointer type (default: "mouse"). -/
  pointerType : Option PDispatchMouseEvent.PointerType := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PDispatchMouseEvent where
  toJSON p := Data.Json.object <|
    [("type", ToJSON.toJSON p.type), ("x", ToJSON.toJSON p.x), ("y", ToJSON.toJSON p.y)]
    ++ (p.modifiers.map fun v => ("modifiers", ToJSON.toJSON v)).toList
    ++ (p.timestamp.map fun v => ("timestamp", ToJSON.toJSON v)).toList
    ++ (p.button.map fun v => ("button", ToJSON.toJSON v)).toList
    ++ (p.buttons.map fun v => ("buttons", ToJSON.toJSON v)).toList
    ++ (p.clickCount.map fun v => ("clickCount", ToJSON.toJSON v)).toList
    ++ (p.force.map fun v => ("force", ToJSON.toJSON v)).toList
    ++ (p.tangentialPressure.map fun v => ("tangentialPressure", ToJSON.toJSON v)).toList
    ++ (p.tiltX.map fun v => ("tiltX", ToJSON.toJSON v)).toList
    ++ (p.tiltY.map fun v => ("tiltY", ToJSON.toJSON v)).toList
    ++ (p.twist.map fun v => ("twist", ToJSON.toJSON v)).toList
    ++ (p.deltaX.map fun v => ("deltaX", ToJSON.toJSON v)).toList
    ++ (p.deltaY.map fun v => ("deltaY", ToJSON.toJSON v)).toList
    ++ (p.pointerType.map fun v => ("pointerType", ToJSON.toJSON v)).toList

instance : Command PDispatchMouseEvent where
  Response := Unit
  commandName _ := "Input.dispatchMouseEvent"
  decodeResponse _ := .ok ()

-- ── dispatchTouchEvent ──

/-- The type of a touch event. -/
inductive PDispatchTouchEvent.Type where
  | touchStart | touchEnd | touchMove | touchCancel
  deriving Repr, BEq, DecidableEq

instance : FromJSON PDispatchTouchEvent.Type where
  parseJSON
    | .string "touchStart" => .ok .touchStart
    | .string "touchEnd" => .ok .touchEnd
    | .string "touchMove" => .ok .touchMove
    | .string "touchCancel" => .ok .touchCancel
    | v => .error s!"failed to parse PDispatchTouchEvent.Type: {repr v}"

instance : ToJSON PDispatchTouchEvent.Type where
  toJSON
    | .touchStart => .string "touchStart" | .touchEnd => .string "touchEnd"
    | .touchMove => .string "touchMove" | .touchCancel => .string "touchCancel"

/-- Parameters of the `Input.dispatchTouchEvent` command: dispatches a touch
    event to the page. -/
structure PDispatchTouchEvent where
  /-- Type of the touch event. `touchEnd` and `touchCancel` must not contain
      any touch points, while `touchStart` and `touchMove` must contain at
      least one. -/
  type : PDispatchTouchEvent.Type
  /-- Active touch points on the touch device. One event per any changed point
      (compared to the previous touch event in a sequence) is generated,
      emulating pressing/moving/releasing points one by one. -/
  touchPoints : List TouchPoint
  /-- Bit field representing pressed modifier keys. Alt=1, Ctrl=2,
      Meta/Command=4, Shift=8 (default: 0). -/
  modifiers : Option Int := none
  /-- Time at which the event occurred. -/
  timestamp : Option TimeSinceEpoch := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PDispatchTouchEvent where
  toJSON p := Data.Json.object <|
    [("type", ToJSON.toJSON p.type), ("touchPoints", ToJSON.toJSON p.touchPoints)]
    ++ (p.modifiers.map fun v => ("modifiers", ToJSON.toJSON v)).toList
    ++ (p.timestamp.map fun v => ("timestamp", ToJSON.toJSON v)).toList

instance : Command PDispatchTouchEvent where
  Response := Unit
  commandName _ := "Input.dispatchTouchEvent"
  decodeResponse _ := .ok ()

-- ── emulateTouchFromMouseEvent ──

/-- The type of a mouse event being emulated as touch. -/
inductive PEmulateTouchFromMouseEvent.Type where
  | mousePressed | mouseReleased | mouseMoved | mouseWheel
  deriving Repr, BEq, DecidableEq

instance : FromJSON PEmulateTouchFromMouseEvent.Type where
  parseJSON
    | .string "mousePressed" => .ok .mousePressed
    | .string "mouseReleased" => .ok .mouseReleased
    | .string "mouseMoved" => .ok .mouseMoved
    | .string "mouseWheel" => .ok .mouseWheel
    | v => .error s!"failed to parse PEmulateTouchFromMouseEvent.Type: {repr v}"

instance : ToJSON PEmulateTouchFromMouseEvent.Type where
  toJSON
    | .mousePressed => .string "mousePressed" | .mouseReleased => .string "mouseReleased"
    | .mouseMoved => .string "mouseMoved" | .mouseWheel => .string "mouseWheel"

/-- Parameters of the `Input.emulateTouchFromMouseEvent` command: emulates a
    touch event from mouse event parameters. -/
structure PEmulateTouchFromMouseEvent where
  /-- Type of the mouse event. -/
  type : PEmulateTouchFromMouseEvent.Type
  /-- X coordinate of the mouse pointer in DIP. -/
  x : Int
  /-- Y coordinate of the mouse pointer in DIP. -/
  y : Int
  /-- Mouse button. Only "none", "left", "right" are supported. -/
  button : MouseButton
  /-- Time at which the event occurred (default: current time). -/
  timestamp : Option TimeSinceEpoch := none
  /-- X delta in DIP for mouse wheel event (default: 0). -/
  deltaX : Option Float := none
  /-- Y delta in DIP for mouse wheel event (default: 0). -/
  deltaY : Option Float := none
  /-- Bit field representing pressed modifier keys. Alt=1, Ctrl=2,
      Meta/Command=4, Shift=8 (default: 0). -/
  modifiers : Option Int := none
  /-- Number of times the mouse button was clicked (default: 0). -/
  clickCount : Option Int := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PEmulateTouchFromMouseEvent where
  toJSON p := Data.Json.object <|
    [ ("type", ToJSON.toJSON p.type), ("x", ToJSON.toJSON p.x), ("y", ToJSON.toJSON p.y)
    , ("button", ToJSON.toJSON p.button) ]
    ++ (p.timestamp.map fun v => ("timestamp", ToJSON.toJSON v)).toList
    ++ (p.deltaX.map fun v => ("deltaX", ToJSON.toJSON v)).toList
    ++ (p.deltaY.map fun v => ("deltaY", ToJSON.toJSON v)).toList
    ++ (p.modifiers.map fun v => ("modifiers", ToJSON.toJSON v)).toList
    ++ (p.clickCount.map fun v => ("clickCount", ToJSON.toJSON v)).toList

instance : Command PEmulateTouchFromMouseEvent where
  Response := Unit
  commandName _ := "Input.emulateTouchFromMouseEvent"
  decodeResponse _ := .ok ()

-- ── setIgnoreInputEvents ──

/-- Parameters of the `Input.setIgnoreInputEvents` command: ignores input
    events (useful while auditing a page). -/
structure PSetIgnoreInputEvents where
  /-- Ignores input events processing when set to `true`. -/
  ignore : Bool
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetIgnoreInputEvents where
  toJSON p := Data.Json.object [("ignore", ToJSON.toJSON p.ignore)]

instance : Command PSetIgnoreInputEvents where
  Response := Unit
  commandName _ := "Input.setIgnoreInputEvents"
  decodeResponse _ := .ok ()

-- ── setInterceptDrags ──

/-- Parameters of the `Input.setInterceptDrags` command: prevents default drag
    and drop behavior and instead emits `Input.dragIntercepted` events. Drag
    and drop behavior can be directly controlled via `Input.dispatchDragEvent`. -/
structure PSetInterceptDrags where
  enabled : Bool
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetInterceptDrags where
  toJSON p := Data.Json.object [("enabled", ToJSON.toJSON p.enabled)]

instance : Command PSetInterceptDrags where
  Response := Unit
  commandName _ := "Input.setInterceptDrags"
  decodeResponse _ := .ok ()

-- ── synthesizePinchGesture ──

/-- Parameters of the `Input.synthesizePinchGesture` command: synthesizes a
    pinch gesture over a time period by issuing appropriate touch events. -/
structure PSynthesizePinchGesture where
  /-- X coordinate of the start of the gesture in CSS pixels. -/
  x : Float
  /-- Y coordinate of the start of the gesture in CSS pixels. -/
  y : Float
  /-- Relative scale factor after zooming (`>1.0` zooms in, `<1.0` zooms out). -/
  scaleFactor : Float
  /-- Relative pointer speed in pixels per second (default: 800). -/
  relativeSpeed : Option Int := none
  /-- Which type of input events to generate (default: `default`, which queries
      the platform for the preferred input type). -/
  gestureSourceType : Option GestureSourceType := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSynthesizePinchGesture where
  toJSON p := Data.Json.object <|
    [("x", ToJSON.toJSON p.x), ("y", ToJSON.toJSON p.y), ("scaleFactor", ToJSON.toJSON p.scaleFactor)]
    ++ (p.relativeSpeed.map fun v => ("relativeSpeed", ToJSON.toJSON v)).toList
    ++ (p.gestureSourceType.map fun v => ("gestureSourceType", ToJSON.toJSON v)).toList

instance : Command PSynthesizePinchGesture where
  Response := Unit
  commandName _ := "Input.synthesizePinchGesture"
  decodeResponse _ := .ok ()

-- ── synthesizeScrollGesture ──

/-- Parameters of the `Input.synthesizeScrollGesture` command: synthesizes a
    scroll gesture over a time period by issuing appropriate touch events. -/
structure PSynthesizeScrollGesture where
  /-- X coordinate of the start of the gesture in CSS pixels. -/
  x : Float
  /-- Y coordinate of the start of the gesture in CSS pixels. -/
  y : Float
  /-- The distance to scroll along the X axis (positive to scroll left). -/
  xDistance : Option Float := none
  /-- The distance to scroll along the Y axis (positive to scroll up). -/
  yDistance : Option Float := none
  /-- The number of additional pixels to scroll back along the X axis, in
      addition to the given distance. -/
  xOverscroll : Option Float := none
  /-- The number of additional pixels to scroll back along the Y axis, in
      addition to the given distance. -/
  yOverscroll : Option Float := none
  /-- Prevent fling (default: true). -/
  preventFling : Option Bool := none
  /-- Swipe speed in pixels per second (default: 800). -/
  speed : Option Int := none
  /-- Which type of input events to generate (default: `default`, which queries
      the platform for the preferred input type). -/
  gestureSourceType : Option GestureSourceType := none
  /-- The number of times to repeat the gesture (default: 0). -/
  repeatCount : Option Int := none
  /-- The number of milliseconds delay between each repeat (default: 250). -/
  repeatDelayMs : Option Int := none
  /-- The name of the interaction markers to generate, if not empty
      (default: ""). -/
  interactionMarkerName : Option String := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSynthesizeScrollGesture where
  toJSON p := Data.Json.object <|
    [("x", ToJSON.toJSON p.x), ("y", ToJSON.toJSON p.y)]
    ++ (p.xDistance.map fun v => ("xDistance", ToJSON.toJSON v)).toList
    ++ (p.yDistance.map fun v => ("yDistance", ToJSON.toJSON v)).toList
    ++ (p.xOverscroll.map fun v => ("xOverscroll", ToJSON.toJSON v)).toList
    ++ (p.yOverscroll.map fun v => ("yOverscroll", ToJSON.toJSON v)).toList
    ++ (p.preventFling.map fun v => ("preventFling", ToJSON.toJSON v)).toList
    ++ (p.speed.map fun v => ("speed", ToJSON.toJSON v)).toList
    ++ (p.gestureSourceType.map fun v => ("gestureSourceType", ToJSON.toJSON v)).toList
    ++ (p.repeatCount.map fun v => ("repeatCount", ToJSON.toJSON v)).toList
    ++ (p.repeatDelayMs.map fun v => ("repeatDelayMs", ToJSON.toJSON v)).toList
    ++ (p.interactionMarkerName.map fun v => ("interactionMarkerName", ToJSON.toJSON v)).toList

instance : Command PSynthesizeScrollGesture where
  Response := Unit
  commandName _ := "Input.synthesizeScrollGesture"
  decodeResponse _ := .ok ()

-- ── synthesizeTapGesture ──

/-- Parameters of the `Input.synthesizeTapGesture` command: synthesizes a tap
    gesture over a time period by issuing appropriate touch events. -/
structure PSynthesizeTapGesture where
  /-- X coordinate of the start of the gesture in CSS pixels. -/
  x : Float
  /-- Y coordinate of the start of the gesture in CSS pixels. -/
  y : Float
  /-- Duration between touchdown and touchup events in ms (default: 50). -/
  duration : Option Int := none
  /-- Number of times to perform the tap (e.g. 2 for double tap, default: 1). -/
  tapCount : Option Int := none
  /-- Which type of input events to generate (default: `default`, which queries
      the platform for the preferred input type). -/
  gestureSourceType : Option GestureSourceType := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSynthesizeTapGesture where
  toJSON p := Data.Json.object <|
    [("x", ToJSON.toJSON p.x), ("y", ToJSON.toJSON p.y)]
    ++ (p.duration.map fun v => ("duration", ToJSON.toJSON v)).toList
    ++ (p.tapCount.map fun v => ("tapCount", ToJSON.toJSON v)).toList
    ++ (p.gestureSourceType.map fun v => ("gestureSourceType", ToJSON.toJSON v)).toList

instance : Command PSynthesizeTapGesture where
  Response := Unit
  commandName _ := "Input.synthesizeTapGesture"
  decodeResponse _ := .ok ()

end CDP.Domains.Input
