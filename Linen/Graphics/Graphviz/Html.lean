/-
  `Graphics.Graphviz.Html` — self-contained HTML pages rendering DOT in the browser

  `page g` is a complete HTML document that draws the typed DOT graph `g` as
  SVG, using Graphviz compiled to WebAssembly (`@hpcc-js/wasm-graphviz`,
  vendored under `vendor/hpcc-js-wasm-graphviz/` and embedded when this module
  is compiled). The page needs no network and no server: open the file.

  ## How the page is put together

  Three `<script>` elements, each with one job, so that each body is a single
  `Web.Html.RawText` whose safety is established on its own:

  1. `<script type="text/plain" id="graphviz-wasm">` — the Graphviz bundle, as
     inert text. `bundle_safe` proves, when this module is compiled, that it
     contains neither `</script` nor `<!--`.
  2. `<script type="application/json" id="graphviz-dot">` — the DOT, as a JSON
     string (`RawText.jsonString`), which is proven safe for *any* text.
  3. `<script type="module">` — a fixed loader (a `raw!` literal, checked at
     compile time) that imports the bundle from a `blob:` URL, parses the
     DOT, and puts the SVG in `#graphviz-graph` — or the error in
     `#graphviz-status`.

  ## Lean 4 dependent-type guarantees

  - **The DOT is well formed**: `page` takes a typed
    `Graphics.Graphviz.Graph`, never a string (see that module).
  - **The HTML is well formed**: the document is a `Web.Html` tree, whose
    content model is enforced by its types, and whose raw-text bodies carry
    proofs that they cannot close their element.
  - `bundle_safe` uses `native_decide`: the bundle is 819 KB, far beyond what
    the kernel can decide by evaluation. It is the only such proof here.
-/
import Linen.Graphics.Graphviz
import Linen.Web.Html

namespace Graphics.Graphviz.Html

open Web.Html Web.Html.Html

-- ── The vendored bundle ─────────────────────────────────────────────────────

/-- The vendored `@hpcc-js/wasm-graphviz` version. -/
def bundleVersion : String := "1.29.1"

/-- The vendored bundle, embedded when this module is compiled. -/
def bundleSource : String := include_str "../../../vendor/hpcc-js-wasm-graphviz/index.js"

/-- The bundle cannot close the `<script>` element it is embedded in. -/
theorem bundle_safe : RawText.Safe .script bundleSource := by native_decide

/-- The bundle, as the body of a `<script>` element. -/
def bundle : RawText .script := ⟨bundleSource, bundle_safe⟩

-- ── The page ────────────────────────────────────────────────────────────────

/-- Loads the bundle from the inert `#graphviz-wasm` script, renders the DOT
    from `#graphviz-dot`, and shows the SVG — or the error. -/
def loader : RawText .script := raw! "
const status = document.getElementById('graphviz-status');
try {
  const source = document.getElementById('graphviz-wasm').textContent;
  const url = URL.createObjectURL(new Blob([source], { type: 'text/javascript' }));
  const { Graphviz } = await import(url);
  URL.revokeObjectURL(url);
  const graphviz = await Graphviz.load();
  const dot = JSON.parse(document.getElementById('graphviz-dot').textContent);
  document.getElementById('graphviz-graph').innerHTML = graphviz.dot(dot);
  status.remove();
} catch (e) {
  status.textContent = 'Could not render the graph: ' + e;
}
"

/-- The page's stylesheet. -/
def css : RawText .style := raw! "
body { margin: 0; font-family: system-ui, sans-serif; }
#graphviz-graph { padding: 1rem; overflow: auto; }
#graphviz-graph svg { max-width: 100%; height: auto; }
#graphviz-status { padding: 1rem; color: #555; }
"

/-- A complete, offline HTML document drawing `g`. -/
def page {k : Kind} (g : Graph k) (title : String := g.name) : String :=
  Html.renderDocument title
    [ meta_ [charset "utf-8"],
      styleSheet css,
      script [type_ "text/plain", id_ "graphviz-wasm"] bundle,
      script [type_ "application/json", id_ "graphviz-dot"] (RawText.jsonString g.render),
      script [type_ "module"] loader ]
    [ div [id_ "graphviz-graph"] [],
      p [id_ "graphviz-status"] [text "Rendering…"] ]

/-- Write `page g` to a file. -/
def writePage {k : Kind} (path : System.FilePath) (g : Graph k) (title : String := g.name) :
    IO Unit :=
  IO.FS.writeFile path (page g title)

end Graphics.Graphviz.Html
