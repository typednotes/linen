/-
  Tests for `Linen.CDP.Domains.Input`.
-/
import Linen.CDP.Domains.Input

open CDP.Domains.Input
open CDP.Internal.Utils (Command Event)
open Data.Json (ToJSON FromJSON)
open Data.Json.Decode (decodeAs)
open Data.Json.Encode (encode)

namespace Tests.CDP.Domains.Input

/-! ### Enums, including `MouseButton.none` (a constructor named `none`,
    distinct from `Option.none`) -/

#guard decodeAs "\"none\"" (α := MouseButton) = .ok .none
#guard encode (ToJSON.toJSON MouseButton.none) = "\"none\""
#guard decodeAs "\"left\"" (α := MouseButton) = .ok .left
#guard decodeAs "\"touch\"" (α := GestureSourceType) = .ok .touch

/-! ### TouchPoint -/

#guard decodeAs "{\"x\": 1, \"y\": 2}" (α := TouchPoint)
  = .ok { x := 1, y := 2, radiusX := none, radiusY := none, rotationAngle := none, force := none
        , tangentialPressure := none, tiltX := none, tiltY := none, twist := none, id := none }
#guard encode (ToJSON.toJSON ({ x := 1, y := 2 } : TouchPoint)) = "{\"x\":1,\"y\":2}"

/-! ### DragData / DragIntercepted event -/

#guard decodeAs "{\"items\": [], \"dragOperationsMask\": 1}" (α := DragData)
  = .ok { items := [], files := none, dragOperationsMask := 1 }
#guard Event.eventName (α := DragIntercepted) = "Input.dragIntercepted"

/-! ### Nested per-command enums -/

#guard encode (ToJSON.toJSON PDispatchDragEvent.Type.dragEnter) = "\"dragEnter\""
#guard encode (ToJSON.toJSON PDispatchKeyEvent.Type.rawKeyDown) = "\"rawKeyDown\""
#guard encode (ToJSON.toJSON PDispatchMouseEvent.Type.mouseWheel) = "\"mouseWheel\""
#guard encode (ToJSON.toJSON PDispatchMouseEvent.PointerType.pen) = "\"pen\""
#guard encode (ToJSON.toJSON PDispatchTouchEvent.Type.touchCancel) = "\"touchCancel\""
#guard encode (ToJSON.toJSON PEmulateTouchFromMouseEvent.Type.mouseMoved) = "\"mouseMoved\""

/-! ### Commands: required fields only, and command names -/

#guard encode (ToJSON.toJSON
    ({ type := .dragEnter, x := 1, y := 2, data := { items := [], dragOperationsMask := 0 } } :
      PDispatchDragEvent))
  = "{\"type\":\"dragEnter\",\"x\":1,\"y\":2,\"data\":{\"items\":[],\"dragOperationsMask\":0}}"
#guard Command.commandName
    ({ type := .dragEnter, x := 1, y := 2, data := { items := [], dragOperationsMask := 0 } } :
      PDispatchDragEvent)
  = "Input.dispatchDragEvent"

#guard Command.commandName ({ type := .keyDown } : PDispatchKeyEvent) = "Input.dispatchKeyEvent"
#guard encode (ToJSON.toJSON ({ type := .keyDown } : PDispatchKeyEvent)) = "{\"type\":\"keyDown\"}"

#guard Command.commandName ({ text := "hi" } : PInsertText) = "Input.insertText"
#guard Command.commandName
    ({ text := "t", selectionStart := 0, selectionEnd := 1 } : PImeSetComposition)
  = "Input.imeSetComposition"

#guard Command.commandName ({ type := .mousePressed, x := 1, y := 2 } : PDispatchMouseEvent)
  = "Input.dispatchMouseEvent"

#guard Command.commandName ({ type := .touchStart, touchPoints := [] } : PDispatchTouchEvent)
  = "Input.dispatchTouchEvent"

#guard Command.commandName
    ({ type := .mousePressed, x := 1, y := 2, button := .left } : PEmulateTouchFromMouseEvent)
  = "Input.emulateTouchFromMouseEvent"

#guard encode (ToJSON.toJSON ({ ignore := true } : PSetIgnoreInputEvents)) = "{\"ignore\":true}"
#guard Command.commandName ({ ignore := true } : PSetIgnoreInputEvents) = "Input.setIgnoreInputEvents"
#guard Command.commandName ({ enabled := true } : PSetInterceptDrags) = "Input.setInterceptDrags"

#guard Command.commandName ({ x := 1, y := 2, scaleFactor := 1.5 } : PSynthesizePinchGesture)
  = "Input.synthesizePinchGesture"
#guard Command.commandName ({ x := 1, y := 2 } : PSynthesizeScrollGesture)
  = "Input.synthesizeScrollGesture"
#guard Command.commandName ({ x := 1, y := 2 } : PSynthesizeTapGesture) = "Input.synthesizeTapGesture"

end Tests.CDP.Domains.Input
