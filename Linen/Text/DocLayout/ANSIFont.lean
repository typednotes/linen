/-
  `Linen.Text.DocLayout.ANSIFont` — ANSI/terminal font styling.

  ## Haskell source

  Ported from `Text.DocLayout.ANSIFont` in the `doclayout` package
  (v0.5.0.3, `src/Text/DocLayout/ANSIFont.hs`).

  Provides the `Font` record describing terminal text attributes (weight,
  shape, underline, strikeout, foreground/background colour, and an optional
  hyperlink target), the `StyleReq` single-attribute change requests applied
  with the `(~>)` operator, and the SGR / OSC-8 escape-code renderers
  `renderFont`/`renderOSC8`.

  Upstream renders escapes polymorphically over `(Semigroup a, IsString a)`;
  here we specialise to `String`, the single string type in Lean. The derived
  `Data`/`Typeable` instances are dropped (no Lean analogue); we derive
  `Repr`/`DecidableEq`/`Inhabited`/`Ord` instead.
-/

namespace Text.DocLayout

/- ── Style enumerations ────────────────────────────────────────────── -/

/-- Font weight. -/
inductive Weight where
  | Normal | Bold
deriving Repr, DecidableEq, Ord, Inhabited

/-- Font shape. -/
inductive Shape where
  | Roman | Italic
deriving Repr, DecidableEq, Ord, Inhabited

/-- The eight basic ANSI colours.  Their `toNat` is the SGR colour index. -/
inductive Color8 where
  | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
deriving Repr, DecidableEq, Ord, Inhabited

/-- SGR colour index (`fromEnum` upstream). -/
def Color8.toNat : Color8 → Nat
  | .Black => 0 | .Red => 1 | .Green => 2 | .Yellow => 3
  | .Blue => 4 | .Magenta => 5 | .Cyan => 6 | .White => 7

/-- Underline style. -/
inductive Underline where
  | ULNone | ULSingle | ULDouble | ULCurly
deriving Repr, DecidableEq, Ord, Inhabited

/-- Strikeout state. -/
inductive Strikeout where
  | Unstruck | Struck
deriving Repr, DecidableEq, Ord, Inhabited

/-- Foreground colour: the terminal default or one of the eight colours. -/
inductive Foreground where
  | FGDefault | FG (c : Color8)
deriving Repr, DecidableEq, Ord, Inhabited

/-- Background colour: the terminal default or one of the eight colours. -/
inductive Background where
  | BGDefault | BG (c : Color8)
deriving Repr, DecidableEq, Ord, Inhabited

/- ── The `Font` record ─────────────────────────────────────────────── -/

/-- A terminal font: weight, shape, underline, strikeout, colours, and an
optional hyperlink target. -/
structure Font where
  ftWeight : Weight
  ftShape : Shape
  ftUnderline : Underline
  ftStrikeout : Strikeout
  ftForeground : Foreground
  ftBackground : Background
  ftLink : Option String
deriving Repr, DecidableEq, Ord, Inhabited

/-- The base (unstyled) font, rendered as the SGR reset. -/
def baseFont : Font :=
  { ftWeight := .Normal, ftShape := .Roman, ftUnderline := .ULNone
  , ftStrikeout := .Unstruck, ftForeground := .FGDefault
  , ftBackground := .BGDefault, ftLink := none }

/- ── Style-application requests ────────────────────────────────────── -/

/-- A request to change a single font attribute. -/
inductive StyleReq where
  | RWeight (w : Weight)
  | RShape (s : Shape)
  | RForeground (c : Foreground)
  | RBackground (c : Background)
  | RUnderline (u : Underline)
  | RStrikeout (s : Strikeout)
deriving Repr, DecidableEq, Ord, Inhabited

/-- Apply a single-attribute style request to a font. -/
def applyStyle (f : Font) : StyleReq → Font
  | .RWeight w => { f with ftWeight := w }
  | .RShape s => { f with ftShape := s }
  | .RForeground c => { f with ftForeground := c }
  | .RBackground c => { f with ftBackground := c }
  | .RUnderline u => { f with ftUnderline := u }
  | .RStrikeout s => { f with ftStrikeout := s }

@[inherit_doc applyStyle]
infixl:65 " ~> " => applyStyle

/- ── SGR / OSC-8 escape rendering ──────────────────────────────────── -/

/-- Wrap an SGR parameter string in the CSI … m escape sequence. -/
def rawSGR (n : String) : String := "\x1b[" ++ n ++ "m"

def Weight.renderSGR : Weight → String
  | .Normal => rawSGR "22"
  | .Bold => rawSGR "1"

def Shape.renderSGR : Shape → String
  | .Roman => rawSGR "23"
  | .Italic => rawSGR "3"

def Foreground.renderSGR : Foreground → String
  | .FGDefault => rawSGR "39"
  | .FG a => rawSGR (toString (30 + a.toNat))

def Background.renderSGR : Background → String
  | .BGDefault => rawSGR "49"
  | .BG a => rawSGR (toString (40 + a.toNat))

def Underline.renderSGR : Underline → String
  | .ULNone => rawSGR "24"
  | .ULSingle => rawSGR "4"
  | .ULDouble => rawSGR "21"
  | .ULCurly => rawSGR "4:3"

def Strikeout.renderSGR : Strikeout → String
  | .Unstruck => rawSGR "29"
  | .Struck => rawSGR "9"

/-- Render a font as SGR escape codes.  The base font renders as the reset
sequence `ESC[0m`; otherwise each attribute is rendered in turn. -/
def renderFont (f : Font) : String :=
  if f == baseFont then rawSGR "0"
  else
    f.ftWeight.renderSGR
      ++ f.ftShape.renderSGR
      ++ f.ftForeground.renderSGR
      ++ f.ftBackground.renderSGR
      ++ f.ftUnderline.renderSGR
      ++ f.ftStrikeout.renderSGR

/-- Render an OSC-8 hyperlink escape.  `none` closes the current link. -/
def renderOSC8 : Option String → String
  | none => "\x1b]8;;\x1b\\"
  | some t => "\x1b]8;;" ++ t ++ "\x1b\\"

end Text.DocLayout
