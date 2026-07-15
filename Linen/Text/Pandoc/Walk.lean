/-
  `Linen.Text.Pandoc.Walk` — typed generic traversal of the Pandoc AST.

  ## Haskell source

  Ported from `Text.Pandoc.Walk` in the `pandoc-types` package
  (v1.23.1, `src/Text/Pandoc/Walk.hs`).

  Provides bottom-up traversal (`walk`/`walkM`) that replaces every occurrence
  of an element type inside a larger structure, and `query`, which folds a
  contribution out of every such occurrence.  The element type `a` is `Inline`
  or `Block`; the container `b` ranges over the AST types.

  ### Deviations from upstream

  * Upstream's `Walkable a b` uses `MultiParamTypeClasses` with overlapping
    instances (a fully generic `Foldable`/`Traversable` instance plus many
    hand-written specialisations).  Lean has no overlapping instances, so the
    recursion is realised as two explicit mutually-recursive families over the
    AST (one monadic transformer, one monoidal collector), each driven by a
    small `Actions`/`QActions` record of per-node callbacks, and `Walkable`
    exposes exactly the concrete `(a, b)` instances that are used.  This gives
    the same observable `walk`/`walkM`/`query` behaviour.
  * `query`'s result monoid is any `[Append c] [Inhabited c]` (upstream's
    `Monoid c`), with `default` playing the role of `mempty`.
-/

import Linen.Text.Pandoc.Definition

namespace Linen.Text.Pandoc

-- ── Monadic transformer core ──────────────────────────────────────────

/-- Per-node callbacks for a monadic bottom-up traversal: `onInline` is
    applied to every `Inline` (after its children), `onBlock` to every
    `Block`. -/
structure Actions (m : Type → Type) where
  onInline : Inline → m Inline
  onBlock : Block → m Block

variable {m : Type → Type} [Monad m]

mutual

private def goInline (t : Actions m) : Inline → m Inline
  | .Str s => t.onInline (.Str s)
  | .Emph xs => do t.onInline (.Emph (← goInlineList t xs))
  | .Underline xs => do t.onInline (.Underline (← goInlineList t xs))
  | .Strong xs => do t.onInline (.Strong (← goInlineList t xs))
  | .Strikeout xs => do t.onInline (.Strikeout (← goInlineList t xs))
  | .Superscript xs => do t.onInline (.Superscript (← goInlineList t xs))
  | .Subscript xs => do t.onInline (.Subscript (← goInlineList t xs))
  | .SmallCaps xs => do t.onInline (.SmallCaps (← goInlineList t xs))
  | .Quoted qt xs => do t.onInline (.Quoted qt (← goInlineList t xs))
  | .Cite cs xs => do t.onInline (.Cite (← goCitationList t cs) (← goInlineList t xs))
  | .Code a s => t.onInline (.Code a s)
  | .Space => t.onInline .Space
  | .SoftBreak => t.onInline .SoftBreak
  | .LineBreak => t.onInline .LineBreak
  | .Math mt s => t.onInline (.Math mt s)
  | .RawInline f s => t.onInline (.RawInline f s)
  | .Link a xs tg => do t.onInline (.Link a (← goInlineList t xs) tg)
  | .Image a xs tg => do t.onInline (.Image a (← goInlineList t xs) tg)
  | .Note bs => do t.onInline (.Note (← goBlockList t bs))
  | .Span a xs => do t.onInline (.Span a (← goInlineList t xs))

private def goInlineList (t : Actions m) : List Inline → m (List Inline)
  | [] => pure []
  | x :: xs => do return (← goInline t x) :: (← goInlineList t xs)

private def goInlineListList (t : Actions m) : List (List Inline) → m (List (List Inline))
  | [] => pure []
  | x :: xs => do return (← goInlineList t x) :: (← goInlineListList t xs)

private def goBlock (t : Actions m) : Block → m Block
  | .Plain xs => do t.onBlock (.Plain (← goInlineList t xs))
  | .Para xs => do t.onBlock (.Para (← goInlineList t xs))
  | .LineBlock xss => do t.onBlock (.LineBlock (← goInlineListList t xss))
  | .CodeBlock a s => t.onBlock (.CodeBlock a s)
  | .RawBlock f s => t.onBlock (.RawBlock f s)
  | .BlockQuote bs => do t.onBlock (.BlockQuote (← goBlockList t bs))
  | .OrderedList la items => do t.onBlock (.OrderedList la (← goBlockListList t items))
  | .BulletList items => do t.onBlock (.BulletList (← goBlockListList t items))
  | .DefinitionList items => do t.onBlock (.DefinitionList (← goDefList t items))
  | .Header lvl a xs => do t.onBlock (.Header lvl a (← goInlineList t xs))
  | .HorizontalRule => t.onBlock .HorizontalRule
  | .Table a capt specs hd bs ft => do
    t.onBlock (.Table a (← goCaption t capt) specs (← goTableHead t hd)
                        (← goTableBodyList t bs) (← goTableFoot t ft))
  | .Figure a capt bs => do t.onBlock (.Figure a (← goCaption t capt) (← goBlockList t bs))
  | .Div a bs => do t.onBlock (.Div a (← goBlockList t bs))

private def goBlockList (t : Actions m) : List Block → m (List Block)
  | [] => pure []
  | x :: xs => do return (← goBlock t x) :: (← goBlockList t xs)

private def goBlockListList (t : Actions m) : List (List Block) → m (List (List Block))
  | [] => pure []
  | x :: xs => do return (← goBlockList t x) :: (← goBlockListList t xs)

private def goDefList (t : Actions m) :
    List (List Inline × List (List Block)) → m (List (List Inline × List (List Block)))
  | [] => pure []
  | (term, defs) :: rest => do
    return (← goInlineList t term, ← goBlockListList t defs) :: (← goDefList t rest)

private def goCitation (t : Actions m) : Citation → m Citation
  | .mk cid pref suff mode nn hash => do
    return .mk cid (← goInlineList t pref) (← goInlineList t suff) mode nn hash

private def goCitationList (t : Actions m) : List Citation → m (List Citation)
  | [] => pure []
  | x :: xs => do return (← goCitation t x) :: (← goCitationList t xs)

private def goCell (t : Actions m) : Cell → m Cell
  | .Cell a al rs cs bs => do return .Cell a al rs cs (← goBlockList t bs)

private def goCellList (t : Actions m) : List Cell → m (List Cell)
  | [] => pure []
  | x :: xs => do return (← goCell t x) :: (← goCellList t xs)

private def goRow (t : Actions m) : Row → m Row
  | .Row a cells => do return .Row a (← goCellList t cells)

private def goRowList (t : Actions m) : List Row → m (List Row)
  | [] => pure []
  | x :: xs => do return (← goRow t x) :: (← goRowList t xs)

private def goTableHead (t : Actions m) : TableHead → m TableHead
  | .TableHead a rows => do return .TableHead a (← goRowList t rows)

private def goTableBody (t : Actions m) : TableBody → m TableBody
  | .TableBody a rhc hd bd => do return .TableBody a rhc (← goRowList t hd) (← goRowList t bd)

private def goTableBodyList (t : Actions m) : List TableBody → m (List TableBody)
  | [] => pure []
  | x :: xs => do return (← goTableBody t x) :: (← goTableBodyList t xs)

private def goTableFoot (t : Actions m) : TableFoot → m TableFoot
  | .TableFoot a rows => do return .TableFoot a (← goRowList t rows)

private def goCaption (t : Actions m) : Caption → m Caption
  | .Caption none bs => do return .Caption none (← goBlockList t bs)
  | .Caption (some short) bs => do return .Caption (some (← goInlineList t short)) (← goBlockList t bs)

private def goMetaValue (t : Actions m) : MetaValue → m MetaValue
  | .MetaMap kvs => do return .MetaMap (← goMetaFields t kvs)
  | .MetaList xs => do return .MetaList (← goMetaValueList t xs)
  | .MetaBool b => pure (.MetaBool b)
  | .MetaString s => pure (.MetaString s)
  | .MetaInlines xs => do return .MetaInlines (← goInlineList t xs)
  | .MetaBlocks bs => do return .MetaBlocks (← goBlockList t bs)

private def goMetaValueList (t : Actions m) : List MetaValue → m (List MetaValue)
  | [] => pure []
  | x :: xs => do return (← goMetaValue t x) :: (← goMetaValueList t xs)

private def goMetaFields (t : Actions m) :
    List (String × MetaValue) → m (List (String × MetaValue))
  | [] => pure []
  | (k, v) :: rest => do return (k, ← goMetaValue t v) :: (← goMetaFields t rest)

end

private def goMeta (t : Actions m) (mv : Meta) : m Meta := do
  return ⟨Data.Map.fromList (← goMetaFields t mv.unMeta.toList')⟩

private def goPandoc (t : Actions m) (d : Pandoc) : m Pandoc := do
  return ⟨← goMeta t d.docMeta, ← goBlockList t d.blocks⟩

/-- Actions that transform inlines with `f` and leave blocks untouched. -/
private def inlineActions (f : Inline → m Inline) : Actions m := ⟨f, pure⟩

/-- Actions that transform blocks with `f` and leave inlines untouched. -/
private def blockActions (f : Block → m Block) : Actions m := ⟨pure, f⟩

-- ── Monoidal collector core ───────────────────────────────────────────

/-- Per-node contributions for a query: `onInline`/`onBlock` map each node to
    a monoid value, combined pre-order (node before its children). -/
structure QActions (c : Type) where
  onInline : Inline → c
  onBlock : Block → c

variable {c : Type} [Append c] [Inhabited c]

/-- The monoid identity used by `query` (upstream `mempty`). -/
private def qempty : c := default

mutual

private def qInline (q : QActions c) : Inline → c
  | i@(.Emph xs) => q.onInline i ++ qInlineList q xs
  | i@(.Underline xs) => q.onInline i ++ qInlineList q xs
  | i@(.Strong xs) => q.onInline i ++ qInlineList q xs
  | i@(.Strikeout xs) => q.onInline i ++ qInlineList q xs
  | i@(.Superscript xs) => q.onInline i ++ qInlineList q xs
  | i@(.Subscript xs) => q.onInline i ++ qInlineList q xs
  | i@(.SmallCaps xs) => q.onInline i ++ qInlineList q xs
  | i@(.Quoted _ xs) => q.onInline i ++ qInlineList q xs
  | i@(.Link _ xs _) => q.onInline i ++ qInlineList q xs
  | i@(.Image _ xs _) => q.onInline i ++ qInlineList q xs
  | i@(.Span _ xs) => q.onInline i ++ qInlineList q xs
  | i@(.Cite cs xs) => q.onInline i ++ (qCitationList q cs ++ qInlineList q xs)
  | i@(.Note bs) => q.onInline i ++ qBlockList q bs
  | i => q.onInline i ++ qempty

private def qInlineList (q : QActions c) : List Inline → c
  | [] => qempty
  | x :: xs => qInline q x ++ qInlineList q xs

private def qInlineListList (q : QActions c) : List (List Inline) → c
  | [] => qempty
  | x :: xs => qInlineList q x ++ qInlineListList q xs

private def qBlock (q : QActions c) : Block → c
  | b@(.Plain xs) => q.onBlock b ++ qInlineList q xs
  | b@(.Para xs) => q.onBlock b ++ qInlineList q xs
  | b@(.Header _ _ xs) => q.onBlock b ++ qInlineList q xs
  | b@(.LineBlock xss) => q.onBlock b ++ qInlineListList q xss
  | b@(.BlockQuote bs) => q.onBlock b ++ qBlockList q bs
  | b@(.Div _ bs) => q.onBlock b ++ qBlockList q bs
  | b@(.OrderedList _ items) => q.onBlock b ++ qBlockListList q items
  | b@(.BulletList items) => q.onBlock b ++ qBlockListList q items
  | b@(.DefinitionList items) => q.onBlock b ++ qDefList q items
  | b@(.Table _ capt _ hd bs ft) =>
    q.onBlock b ++ (qCaption q capt ++ qTableHead q hd ++ qTableBodyList q bs ++ qTableFoot q ft)
  | b@(.Figure _ capt bs) => q.onBlock b ++ (qCaption q capt ++ qBlockList q bs)
  | b => q.onBlock b ++ qempty

private def qBlockList (q : QActions c) : List Block → c
  | [] => qempty
  | x :: xs => qBlock q x ++ qBlockList q xs

private def qBlockListList (q : QActions c) : List (List Block) → c
  | [] => qempty
  | x :: xs => qBlockList q x ++ qBlockListList q xs

private def qDefList (q : QActions c) : List (List Inline × List (List Block)) → c
  | [] => qempty
  | (term, defs) :: rest => qInlineList q term ++ qBlockListList q defs ++ qDefList q rest

private def qCitationList (q : QActions c) : List Citation → c
  | [] => qempty
  | .mk _ pref suff _ _ _ :: xs => qInlineList q pref ++ qInlineList q suff ++ qCitationList q xs

private def qCell (q : QActions c) : Cell → c
  | .Cell _ _ _ _ bs => qBlockList q bs

private def qCellList (q : QActions c) : List Cell → c
  | [] => qempty
  | x :: xs => qCell q x ++ qCellList q xs

private def qRow (q : QActions c) : Row → c
  | .Row _ cells => qCellList q cells

private def qRowList (q : QActions c) : List Row → c
  | [] => qempty
  | x :: xs => qRow q x ++ qRowList q xs

private def qTableHead (q : QActions c) : TableHead → c
  | .TableHead _ rows => qRowList q rows

private def qTableBody (q : QActions c) : TableBody → c
  | .TableBody _ _ hd bd => qRowList q hd ++ qRowList q bd

private def qTableBodyList (q : QActions c) : List TableBody → c
  | [] => qempty
  | x :: xs => qTableBody q x ++ qTableBodyList q xs

private def qTableFoot (q : QActions c) : TableFoot → c
  | .TableFoot _ rows => qRowList q rows

private def qCaption (q : QActions c) : Caption → c
  | .Caption none bs => qBlockList q bs
  | .Caption (some short) bs => qInlineList q short ++ qBlockList q bs

private def qMetaValue (q : QActions c) : MetaValue → c
  | .MetaMap kvs => qMetaFields q kvs
  | .MetaList xs => qMetaValueList q xs
  | .MetaBool _ => qempty
  | .MetaString _ => qempty
  | .MetaInlines xs => qInlineList q xs
  | .MetaBlocks bs => qBlockList q bs

private def qMetaValueList (q : QActions c) : List MetaValue → c
  | [] => qempty
  | x :: xs => qMetaValue q x ++ qMetaValueList q xs

private def qMetaFields (q : QActions c) : List (String × MetaValue) → c
  | [] => qempty
  | (_, v) :: rest => qMetaValue q v ++ qMetaFields q rest

end

private def qMeta (q : QActions c) (mv : Meta) : c := qMetaFields q mv.unMeta.toList'

private def qPandoc (q : QActions c) (d : Pandoc) : c := qMeta q d.docMeta ++ qBlockList q d.blocks

/-- Query contributions coming from inlines only. -/
private def inlineQ (f : Inline → c) : QActions c := ⟨f, fun _ => qempty⟩

/-- Query contributions coming from blocks only. -/
private def blockQ (f : Block → c) : QActions c := ⟨fun _ => qempty, f⟩

-- ── The `Walkable` class ──────────────────────────────────────────────

/-- `Walkable a b`: replace/query occurrences of `a` inside `b`.  Mirrors
    upstream's `Walkable`, specialised to the concrete `(a, b)` pairs that
    the pandoc AST uses (`a` is `Inline` or `Block`). -/
class Walkable (a b : Type) where
  /-- Monadic bottom-up transformation. -/
  walkM {m : Type → Type} [Monad m] : (a → m a) → b → m b
  /-- Monoidal query. -/
  query {c : Type} [Append c] [Inhabited c] : (a → c) → b → c

/-- Pure bottom-up transformation. -/
def walk [Walkable a b] (f : a → a) (x : b) : b :=
  Id.run (Walkable.walkM (m := Id) (fun z => pure (f z)) x)

/-- Monadic bottom-up transformation (re-exported class method). -/
abbrev walkM [Walkable a b] {m : Type → Type} [Monad m] (f : a → m a) (x : b) : m b :=
  Walkable.walkM f x

/-- Monoidal query (re-exported class method). -/
abbrev query [Walkable a b] {c : Type} [Append c] [Inhabited c] (f : a → c) (x : b) : c :=
  Walkable.query f x

-- Inline over the AST ----------------------------------------------------

instance : Walkable Inline Inline where
  walkM f x := goInline (inlineActions f) x
  query f x := qInline (inlineQ f) x

instance : Walkable Inline Block where
  walkM f x := goBlock (inlineActions f) x
  query f x := qBlock (inlineQ f) x

instance : Walkable Inline (List Inline) where
  walkM f x := goInlineList (inlineActions f) x
  query f x := qInlineList (inlineQ f) x

instance : Walkable Inline (List Block) where
  walkM f x := goBlockList (inlineActions f) x
  query f x := qBlockList (inlineQ f) x

instance : Walkable Inline MetaValue where
  walkM f x := goMetaValue (inlineActions f) x
  query f x := qMetaValue (inlineQ f) x

instance : Walkable Inline Meta where
  walkM f x := goMeta (inlineActions f) x
  query f x := qMeta (inlineQ f) x

instance : Walkable Inline Citation where
  walkM f x := goCitation (inlineActions f) x
  query f x := qCitationList (inlineQ f) [x]

instance : Walkable Inline Pandoc where
  walkM f x := goPandoc (inlineActions f) x
  query f x := qPandoc (inlineQ f) x

-- Block over the AST -----------------------------------------------------

instance : Walkable Block Inline where
  walkM f x := goInline (blockActions f) x
  query f x := qInline (blockQ f) x

instance : Walkable Block Block where
  walkM f x := goBlock (blockActions f) x
  query f x := qBlock (blockQ f) x

instance : Walkable Block (List Inline) where
  walkM f x := goInlineList (blockActions f) x
  query f x := qInlineList (blockQ f) x

instance : Walkable Block (List Block) where
  walkM f x := goBlockList (blockActions f) x
  query f x := qBlockList (blockQ f) x

instance : Walkable Block MetaValue where
  walkM f x := goMetaValue (blockActions f) x
  query f x := qMetaValue (blockQ f) x

instance : Walkable Block Meta where
  walkM f x := goMeta (blockActions f) x
  query f x := qMeta (blockQ f) x

instance : Walkable Block Pandoc where
  walkM f x := goPandoc (blockActions f) x
  query f x := qPandoc (blockQ f) x

-- ── Top-down transformation (unsafe) ──────────────────────────────────

/-
  Top-down traversal (`everywhere'` in `syb`) applies the transformation to
  each node *before* descending into the children of the *result*.  Because
  the recursion continues into the transformed node — whose children may be
  larger than the original's — this is not structurally (or well-founded)
  terminating in general, exactly like upstream's `Data`-generic version.  It
  is therefore an `unsafe def`, following the documented-escape precedent used
  elsewhere in `linen` (e.g. `Linen/Data/Stream/Type.lean`) for functions with
  no Lean termination argument.  Only the top-down variant is affected; the
  bottom-up `walk`/`walkM`/`query` above are total.
-/

mutual

private unsafe def tdInline (t : Actions m) (i : Inline) : m Inline := do
  match (← t.onInline i) with
  | .Emph xs => return .Emph (← tdInlineList t xs)
  | .Underline xs => return .Underline (← tdInlineList t xs)
  | .Strong xs => return .Strong (← tdInlineList t xs)
  | .Strikeout xs => return .Strikeout (← tdInlineList t xs)
  | .Superscript xs => return .Superscript (← tdInlineList t xs)
  | .Subscript xs => return .Subscript (← tdInlineList t xs)
  | .SmallCaps xs => return .SmallCaps (← tdInlineList t xs)
  | .Quoted qt xs => return .Quoted qt (← tdInlineList t xs)
  | .Cite cs xs => return .Cite (← tdCitationList t cs) (← tdInlineList t xs)
  | .Link a xs tg => return .Link a (← tdInlineList t xs) tg
  | .Image a xs tg => return .Image a (← tdInlineList t xs) tg
  | .Span a xs => return .Span a (← tdInlineList t xs)
  | .Note bs => return .Note (← tdBlockList t bs)
  | leaf => pure leaf

private unsafe def tdInlineList (t : Actions m) : List Inline → m (List Inline)
  | [] => pure []
  | x :: xs => do return (← tdInline t x) :: (← tdInlineList t xs)

private unsafe def tdInlineListList (t : Actions m) : List (List Inline) → m (List (List Inline))
  | [] => pure []
  | x :: xs => do return (← tdInlineList t x) :: (← tdInlineListList t xs)

private unsafe def tdBlock (t : Actions m) (b : Block) : m Block := do
  match (← t.onBlock b) with
  | .Plain xs => return .Plain (← tdInlineList t xs)
  | .Para xs => return .Para (← tdInlineList t xs)
  | .LineBlock xss => return .LineBlock (← tdInlineListList t xss)
  | .BlockQuote bs => return .BlockQuote (← tdBlockList t bs)
  | .OrderedList la items => return .OrderedList la (← tdBlockListList t items)
  | .BulletList items => return .BulletList (← tdBlockListList t items)
  | .DefinitionList items => return .DefinitionList (← tdDefList t items)
  | .Header lvl a xs => return .Header lvl a (← tdInlineList t xs)
  | .Table a capt specs hd bs ft =>
    return .Table a (← tdCaption t capt) specs (← tdTableHead t hd)
                    (← tdTableBodyList t bs) (← tdTableFoot t ft)
  | .Figure a capt bs => return .Figure a (← tdCaption t capt) (← tdBlockList t bs)
  | .Div a bs => return .Div a (← tdBlockList t bs)
  | leaf => pure leaf

private unsafe def tdBlockList (t : Actions m) : List Block → m (List Block)
  | [] => pure []
  | x :: xs => do return (← tdBlock t x) :: (← tdBlockList t xs)

private unsafe def tdBlockListList (t : Actions m) : List (List Block) → m (List (List Block))
  | [] => pure []
  | x :: xs => do return (← tdBlockList t x) :: (← tdBlockListList t xs)

private unsafe def tdDefList (t : Actions m) :
    List (List Inline × List (List Block)) → m (List (List Inline × List (List Block)))
  | [] => pure []
  | (term, defs) :: rest => do
    return (← tdInlineList t term, ← tdBlockListList t defs) :: (← tdDefList t rest)

private unsafe def tdCitation (t : Actions m) : Citation → m Citation
  | .mk cid pref suff mode nn hash => do
    return .mk cid (← tdInlineList t pref) (← tdInlineList t suff) mode nn hash

private unsafe def tdCitationList (t : Actions m) : List Citation → m (List Citation)
  | [] => pure []
  | x :: xs => do return (← tdCitation t x) :: (← tdCitationList t xs)

private unsafe def tdCell (t : Actions m) : Cell → m Cell
  | .Cell a al rs cs bs => do return .Cell a al rs cs (← tdBlockList t bs)

private unsafe def tdCellList (t : Actions m) : List Cell → m (List Cell)
  | [] => pure []
  | x :: xs => do return (← tdCell t x) :: (← tdCellList t xs)

private unsafe def tdRow (t : Actions m) : Row → m Row
  | .Row a cells => do return .Row a (← tdCellList t cells)

private unsafe def tdRowList (t : Actions m) : List Row → m (List Row)
  | [] => pure []
  | x :: xs => do return (← tdRow t x) :: (← tdRowList t xs)

private unsafe def tdTableHead (t : Actions m) : TableHead → m TableHead
  | .TableHead a rows => do return .TableHead a (← tdRowList t rows)

private unsafe def tdTableBody (t : Actions m) : TableBody → m TableBody
  | .TableBody a rhc hd bd => do return .TableBody a rhc (← tdRowList t hd) (← tdRowList t bd)

private unsafe def tdTableBodyList (t : Actions m) : List TableBody → m (List TableBody)
  | [] => pure []
  | x :: xs => do return (← tdTableBody t x) :: (← tdTableBodyList t xs)

private unsafe def tdTableFoot (t : Actions m) : TableFoot → m TableFoot
  | .TableFoot a rows => do return .TableFoot a (← tdRowList t rows)

private unsafe def tdCaption (t : Actions m) : Caption → m Caption
  | .Caption none bs => do return .Caption none (← tdBlockList t bs)
  | .Caption (some short) bs => do return .Caption (some (← tdInlineList t short)) (← tdBlockList t bs)

private unsafe def tdMetaValue (t : Actions m) : MetaValue → m MetaValue
  | .MetaMap kvs => do return .MetaMap (← tdMetaFields t kvs)
  | .MetaList xs => do return .MetaList (← tdMetaValueList t xs)
  | .MetaBool b => pure (.MetaBool b)
  | .MetaString s => pure (.MetaString s)
  | .MetaInlines xs => do return .MetaInlines (← tdInlineList t xs)
  | .MetaBlocks bs => do return .MetaBlocks (← tdBlockList t bs)

private unsafe def tdMetaValueList (t : Actions m) : List MetaValue → m (List MetaValue)
  | [] => pure []
  | x :: xs => do return (← tdMetaValue t x) :: (← tdMetaValueList t xs)

private unsafe def tdMetaFields (t : Actions m) :
    List (String × MetaValue) → m (List (String × MetaValue))
  | [] => pure []
  | (k, v) :: rest => do return (k, ← tdMetaValue t v) :: (← tdMetaFields t rest)

end

private unsafe def tdMeta (t : Actions m) (mv : Meta) : m Meta := do
  return ⟨Data.Map.fromList (← tdMetaFields t mv.unMeta.toList')⟩

private unsafe def tdPandoc (t : Actions m) (d : Pandoc) : m Pandoc := do
  return ⟨← tdMeta t d.docMeta, ← tdBlockList t d.blocks⟩

/-- Top-down analogue of `Walkable`, for the `topDown`/`topDownM` transforms in
    `Text.Pandoc.Generic`.  `unsafe` because top-down traversal has no Lean
    termination argument (see the note above). -/
unsafe class WalkableTD (a b : Type) where
  walkTopDownM {m : Type → Type} [Monad m] : (a → m a) → b → m b

unsafe instance : WalkableTD Inline Inline := ⟨fun f x => tdInline (inlineActions f) x⟩
unsafe instance : WalkableTD Inline Block := ⟨fun f x => tdBlock (inlineActions f) x⟩
unsafe instance : WalkableTD Inline (List Inline) := ⟨fun f x => tdInlineList (inlineActions f) x⟩
unsafe instance : WalkableTD Inline (List Block) := ⟨fun f x => tdBlockList (inlineActions f) x⟩
unsafe instance : WalkableTD Inline MetaValue := ⟨fun f x => tdMetaValue (inlineActions f) x⟩
unsafe instance : WalkableTD Inline Pandoc := ⟨fun f x => tdPandoc (inlineActions f) x⟩
unsafe instance : WalkableTD Block Inline := ⟨fun f x => tdInline (blockActions f) x⟩
unsafe instance : WalkableTD Block Block := ⟨fun f x => tdBlock (blockActions f) x⟩
unsafe instance : WalkableTD Block (List Inline) := ⟨fun f x => tdInlineList (blockActions f) x⟩
unsafe instance : WalkableTD Block (List Block) := ⟨fun f x => tdBlockList (blockActions f) x⟩
unsafe instance : WalkableTD Block MetaValue := ⟨fun f x => tdMetaValue (blockActions f) x⟩
unsafe instance : WalkableTD Block Pandoc := ⟨fun f x => tdPandoc (blockActions f) x⟩

/-- Monadic top-down transformation. -/
unsafe def walkTopDownM [WalkableTD a b] {m : Type → Type} [Monad m] (f : a → m a) (x : b) : m b :=
  WalkableTD.walkTopDownM f x

/-- Pure top-down transformation. -/
unsafe def walkTopDown [WalkableTD a b] (f : a → a) (x : b) : b :=
  Id.run (WalkableTD.walkTopDownM (m := Id) (fun z => pure (f z)) x)

end Linen.Text.Pandoc
