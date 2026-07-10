/-
  Data.PDF.Content.Transform — 2D affine transforms

  Ports `Pdf.Content.Transform` from Hackage's `pdf-toolbox-content`
  (https://github.com/Yuras/pdf-toolbox, `content/lib/Pdf/Content/Transform.hs`,
  fetched from
  `https://raw.githubusercontent.com/Yuras/pdf-toolbox/master/content/lib/Pdf/Content/Transform.hs`),
  module 1 of the `pdf-toolbox-content` import documented in
  `docs/imports/PdfToolboxContent/dependencies.md`.

  A PDF content stream's `cm` operator (PDF32000-1:2008 §8.3.4) supplies a
  $2 \times 3$ affine transform matrix
  $$\begin{pmatrix} a & b & 0 \\ c & d & 0 \\ e & f & 1 \end{pmatrix}$$
  acting on row vectors $(x\;y\;1)$ by right-multiplication. This module
  ports upstream's small algebra of such matrices verbatim: no recursion, no
  partiality, just arithmetic — so there is nothing to prove terminating.

  ## Design

  Both `Transform` and `Vector` stay generic in their element type (`α`),
  matching upstream's own `Transform a`/`Vector a` polymorphism (always
  instantiated at `Double`, ported here as `Float`, by the rest of the
  package). `Num a` constraints become `[Add α] [Mul α] [OfNat α 1]
  [OfNat α 0]` as needed per definition, rather than a blanket ring-like
  typeclass `linen` doesn't otherwise use for this kind of code.
-/

namespace Data.PDF.Content.Transform

/-! ── The `Transform` type ── -/

/-- A 2D affine transform matrix
    $$\begin{pmatrix} a & b & 0 \\ c & d & 0 \\ e & f & 1 \end{pmatrix},$$
    stored as its six free entries `a b c d e f`. Mirrors upstream's
    `Transform a = Transform a a a a a a`. -/
structure Transform (α : Type u) where
  /-- Row 1, column 1. -/
  a : α
  /-- Row 1, column 2. -/
  b : α
  /-- Row 2, column 1. -/
  c : α
  /-- Row 2, column 2. -/
  d : α
  /-- Row 3, column 1 (the x-translation). -/
  e : α
  /-- Row 3, column 2 (the y-translation). -/
  f : α
deriving BEq, Repr

/-! ── The `Vector` type ── -/

/-- A 2D vector or point $(x, y)$. Mirrors upstream's `Vector a = Vector a a`. -/
structure Vector (α : Type u) where
  /-- The x coordinate. -/
  x : α
  /-- The y coordinate. -/
  y : α
deriving BEq, Repr

/-! ── Constructors ── -/

/-- The identity transform
    $$\begin{pmatrix} 1 & 0 & 0 \\ 0 & 1 & 0 \\ 0 & 0 & 1 \end{pmatrix}.$$ -/
def identity [OfNat α 1] [OfNat α 0] : Transform α :=
  ⟨1, 0, 0, 1, 0, 0⟩

/-- A pure translation by $(t_x, t_y)$. -/
def translation [OfNat α 1] [OfNat α 0] (tx ty : α) : Transform α :=
  ⟨1, 0, 0, 1, tx, ty⟩

/-- A pure (axis-aligned) scale by $(s_x, s_y)$. -/
def scale [OfNat α 0] (sx sy : α) : Transform α :=
  ⟨sx, 0, 0, sy, 0, 0⟩

/-! ── Operations ── -/

/-- Apply a transform to a vector/point:
    $$T(v) = (a v_x + c v_y + e,\; b v_x + d v_y + f).$$ -/
def transform [Add α] [Mul α] (t : Transform α) (v : Vector α) : Vector α :=
  ⟨t.a * v.x + t.c * v.y + t.e, t.b * v.x + t.d * v.y + t.f⟩

/-- Combine two transforms, applying `s` first and then `t`:
    matrix-multiply `s`'s matrix by `t`'s matrix. Mirrors upstream's
    `multiply`. -/
def multiply [Add α] [Mul α] (s t : Transform α) : Transform α :=
  ⟨s.a * t.a + s.b * t.c,
   s.a * t.b + s.b * t.d,
   s.c * t.a + s.d * t.c,
   s.c * t.b + s.d * t.d,
   s.e * t.a + s.f * t.c + t.e,
   s.e * t.b + s.f * t.d + t.f⟩

/-- Prepend a translation by $(t_x, t_y)$ to a transform `t`: mirrors
    upstream's `translate tx ty t = translation tx ty \`multiply\` t`. -/
def translate [Add α] [Mul α] [OfNat α 1] [OfNat α 0] (tx ty : α) (t : Transform α) :
    Transform α :=
  multiply (translation tx ty) t

end Data.PDF.Content.Transform
