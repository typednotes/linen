/-
  Examples.Todo — a small in-memory TODO-list web app, showing
  `Web.Html`/`Web.Css`'s compile-time content-model guarantees wired onto
  `Network.WebApp`'s `Application`/`AppM` machinery.

  Every page is assembled from typed `Web.Html`/`Web.Css` constructors —
  the list `<ul>`/`<li>` nesting, the `<form>`s, and each item's inline
  `style` all go through the same illegal-construct-is-a-compile-error
  discipline demonstrated in `Tests.Linen.Web.HtmlTest`/`CssTest`, there is
  no hand-written HTML string anywhere in this file. Routing and the
  in-memory `IO.mkRef` state reuse `Network.WebApp`'s `Application`, driven
  by the real `Network.WebApp.Server` engine via `withApplication`, exactly
  as `Examples.Server` drives `Examples.WebApp`'s `demoApplication`.

  Args: (none) -- starts the server, runs a few self-check round trips
  (add/toggle/delete) against it, then keeps the same server running on the
  printed port for manual `curl` testing (`Ctrl-C` to stop); `check` runs
  the same round trips and exits instead of staying up (non-zero on any
  mismatch).
-/
import Linen.Network.WebApp.Extra.Parse
import Linen.Network.WebApp.Server.WithApplication
import Linen.Web.Html
import Linen.Web.Css
import Examples.WebApp

open Network.WebApp
open Network.WebApp.Extra.Parse
open Network.WebApp.Server
open Network.HTTP.Types
open Web.Html Web.Html.Html
open Web.Css (Length Declaration)

namespace Examples.Todo

-- ── State ──

/-- One TODO item: a unique id, its text, and whether it is done. -/
structure Item where
  id : Nat
  text : String
  done : Bool
deriving Repr, BEq

/-- In-memory state: the item list plus a counter for fresh ids. -/
structure State where
  items : List Item
  nextId : Nat
deriving Repr, BEq

def State.initial : State := { items := [], nextId := 0 }

-- ── Rendering (Web.Html / Web.Css, illegal nesting is a compile error) ──

/-- Declarations shared by every item's inline style, plus a strike-through
    when the item is done — a `Web.Css.Declaration` list, not a string. -/
def itemDeclarations (item : Item) : List Declaration :=
  [Web.Css.display .inline] ++
    (if item.done then [Web.Css.textDecoration .lineThrough] else [])

/-- One `<li>`: a toggle button, the item text, and a delete button, each a
    tiny inline `<form>` since void `<button>` clicks need a POST target.
    `li`'s children are `Html .flow`, so `form`/`span` mix freely — `span`
    (phrasing) coerces to flow automatically via the `Coe` instance. -/
def renderItem (item : Item) : Html .listItem :=
  li []
    [ form [action s!"/toggle/{item.id}", method_ "post", style [Web.Css.display .inline]]
        [button [type_ "submit"] [text (if item.done then "[x]" else "[ ]")]]
    , span [style (itemDeclarations item)] [text item.text]
    , form [action s!"/delete/{item.id}", method_ "post", style [Web.Css.display .inline]]
        [button [type_ "submit"] [text "delete"]]
    ]

/-- The whole page: an add-item form and the `<ul>` of items. -/
def renderPage (state : State) : String :=
  Html.renderDocument "TODO"
    [styleSheet (Web.Css.Stylesheet.render
      [rule! (.tag "body") { Web.Css.fontFamily ["system-ui", "sans-serif"], Web.Css.margin (.px 24) }])]
    [ h1 [] [text "TODO"]
    , form [action "/add", method_ "post"]
        [ input [name_ "text", placeholder "What needs doing?"]
        , button [type_ "submit"] [text "Add"]
        ]
    , ul [] (state.items.map renderItem)
    ]

-- ── Application ──

/-- Build a fresh `Application` closing over its own `IO.mkRef State`, so
    each call to `mkApp` gets independent, isolated state. -/
def mkApp : IO Application := do
  let state ← IO.mkRef State.initial
  let renderResponse : IO Response := do
    pure (responseLBS status200 [(hContentType, "text/html; charset=utf-8")] (renderPage (← state.get)))
  pure fun req respond =>
    AppM.respondIO respond do
      match req.requestMethod, req.pathInfo with
      | .standard .GET, [] => renderResponse
      | .standard .POST, ["add"] =>
        let (params, _) ← parseRequestBody req
        match params.find? (·.1 == "text") with
        | some pair =>
          let text := pair.2
          if text.isEmpty then
            renderResponse
          else
            state.modify fun s =>
              { items := s.items ++ [{ id := s.nextId, text := text, done := false }]
                nextId := s.nextId + 1 }
            renderResponse
        | none => renderResponse
      | .standard .POST, ["toggle", idStr] =>
        match idStr.toNat? with
        | some id =>
          state.modify fun s =>
            { s with items := s.items.map fun item =>
                if item.id == id then { item with done := !item.done } else item }
          renderResponse
        | none => pure (responseLBS status404 [] "not found")
      | .standard .POST, ["delete", idStr] =>
        match idStr.toNat? with
        | some id =>
          state.modify fun s => { s with items := s.items.filter (·.id != id) }
          renderResponse
        | none => pure (responseLBS status404 [] "not found")
      | _, _ => pure (responseLBS status404 [] "not found")

-- ── Self-checking demo, driven over the real `Network.WebApp.Server` ──

/-- Fetch just the rendered body for a request against `port`. -/
def get (port : UInt16) (path : String) : IO String :=
  Prod.snd <$> Examples.WebApp.sendRequest port "GET" path "Connection: close\r\n" ""

def post (port : UInt16) (path : String) (body : String) : IO (Nat × String) :=
  Examples.WebApp.sendRequest port "POST" path
    "Content-Type: application/x-www-form-urlencoded\r\nConnection: close\r\n" body

/-- Run the add/toggle/delete round trip against the already-listening
    server at `port`, printing progress. -/
def runChecks (port : UInt16) : IO Bool := do
  let empty ← get port "/"
  IO.println "  GET / -> empty list"
  let ok1 := (empty.splitOn "<ul></ul>").length > 1

  let (addStatus, afterAdd) ← post port "/add" "text=buy+milk"
  IO.println s!"  POST /add text=buy milk -> {addStatus}"
  let ok2 := addStatus == 200 && (afterAdd.splitOn "buy milk").length > 1 &&
    (afterAdd.splitOn "[ ]").length > 1

  let (toggleStatus, afterToggle) ← post port "/toggle/0" ""
  IO.println s!"  POST /toggle/0 -> {toggleStatus}"
  let ok3 := toggleStatus == 200 && (afterToggle.splitOn "[x]").length > 1 &&
    (afterToggle.splitOn "line-through").length > 1

  let (deleteStatus, afterDelete) ← post port "/delete/0" ""
  IO.println s!"  POST /delete/0 -> {deleteStatus}"
  let ok4 := deleteStatus == 200 && (afterDelete.splitOn "buy milk").length == 1

  pure (ok1 && ok2 && ok3 && ok4)

/-- Run the self-check round trip against a fresh server, then exit
    non-zero on any mismatch instead of staying up. -/
def demoRoundTrip : IO Bool := do
  IO.println "── Examples.Todo: typed Web.Html/Web.Css over Network.WebApp ──"
  withApplication mkApp fun port => do
    IO.println s!"  server listening on 127.0.0.1:{port}"
    runChecks port

/-- Start the server, self-check it, then keep the very same `Application`
    (and its accumulated state) running on the printed port forever, for
    manual `curl` testing, e.g. `curl localhost:<port>` /
    `curl -X POST localhost:<port>/add -d text=milk`. (`withApplication`
    always lets the OS pick the port, reporting the real bound port back —
    see its docstring.) -/
def runServer : IO Unit :=
  withApplication mkApp fun port => do
    IO.println "── Examples.Todo: typed Web.Html/Web.Css over Network.WebApp ──"
    IO.println s!"  server listening on 127.0.0.1:{port}"
    if ← runChecks port then
      IO.println "  self-check passed"
    else
      IO.println "  self-check FAILED"
    IO.println s!"\ntodo server running on 127.0.0.1:{port}  ·  Ctrl-C to stop"
    (← IO.getStdout).flush
    IO.sleep (24 * 60 * 60 * 1000)

def run (args : List String) : IO Unit := do
  match args with
  | "check" :: _ =>
    if ← demoRoundTrip then
      IO.println "\ntodo demo done · all checks passed"
    else
      throw (IO.userError "todo demo done · some checks failed")
  | _ => runServer

end Examples.Todo
