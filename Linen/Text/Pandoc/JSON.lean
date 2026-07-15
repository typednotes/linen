/-
  `Linen.Text.Pandoc.JSON` — the AST ↔ JSON bridge and `toJSONFilter` helper.

  ## Haskell source

  Ported from `Text.Pandoc.JSON` in the `pandoc-types` package
  (v1.23.1, `src/Text/Pandoc/JSON.hs`).

  Re-exports the `ToJSON`/`FromJSON` instances from `Text.Pandoc.Definition`
  (via the import) and provides `toJSONFilter`, which turns an AST
  transformation into a stdin→stdout JSON filter (as used by `pandoc
  --filter`).

  ### Deviations from upstream

  * `toJSONFilter` is specialised to `IO` (upstream is `MonadIO m`).  The
    supported transformation shapes are `a → a` and `a → IO a` for
    `a ∈ {Inline, Block, Pandoc, …}` with `Walkable a Pandoc`.  The
    list-splicing shapes (`a → [a]`) and the argument-aware wrappers
    (`[String] → a`, `Maybe Format → a`) are omitted: the former needs a
    splice-walk not ported here, and Lean has no portable `getArgs` outside
    `main`.
  * JSON (de)serialisation goes through `Linen.Data.Json` (`decode`/`encode`)
    rather than aeson's `ByteString` lazy IO.
-/

import Linen.Text.Pandoc.Definition
import Linen.Text.Pandoc.Walk

namespace Linen.Text.Pandoc

open Data.Json

-- ── String ↔ Pandoc ───────────────────────────────────────────────────

/-- Parse a Pandoc document from a JSON string. -/
def readPandoc (s : String) : Except String Pandoc := do
  FromJSON.parseJSON (← Data.Json.Decode.decode s)

/-- Render a Pandoc document to a JSON string. -/
def writePandoc (d : Pandoc) : String := Data.Json.Encode.encode (ToJSON.toJSON d)

-- ── The `toJSONFilter` driver ─────────────────────────────────────────

/-- Read a Pandoc document from stdin as JSON, apply the monadic
    transformation, and write the result to stdout as JSON. -/
def runFilterM (g : Pandoc → IO Pandoc) : IO Unit := do
  let input ← (← IO.getStdin).readToEnd
  match readPandoc input with
  | .error e => throw (IO.userError s!"pandoc JSON decode failed: {e}")
  | .ok d => IO.print (writePandoc (← g d))

/-- Turn a transformation into a stdin→stdout JSON filter. -/
class ToJSONFilter (a : Type) where
  toJSONFilter : a → IO Unit

instance [Walkable α Pandoc] : ToJSONFilter (α → α) where
  toJSONFilter f := runFilterM (fun d => pure (walk f d))

instance [Walkable α Pandoc] : ToJSONFilter (α → IO α) where
  toJSONFilter f := runFilterM (fun d => walkM f d)

/-- Apply a filter transformation (top-level convenience). -/
def toJSONFilter [ToJSONFilter a] (f : a) : IO Unit := ToJSONFilter.toJSONFilter f

end Linen.Text.Pandoc
