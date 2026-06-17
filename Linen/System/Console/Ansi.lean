/-
  Linen.System.Console.Ansi — ANSI terminal escape codes

  Provides constants and functions for colored terminal output.
-/
namespace System.Console.Ansi

/-- ANSI color codes. -/
inductive Color where
  | black | red | green | yellow | blue | magenta | cyan | white
deriving BEq, Repr

/-- ANSI text intensity. -/
inductive Intensity where
  | bold | normal
deriving BEq, Repr

/-- Reset all attributes. -/
def reset : String := "\x1b[0m"

/-- Set foreground color. -/
def setFg (c : Color) : String :=
  let code := match c with
    | .black => 30 | .red => 31 | .green => 32 | .yellow => 33
    | .blue => 34 | .magenta => 35 | .cyan => 36 | .white => 37
  s!"\x1b[{code}m"

/-- Set background color. -/
def setBg (c : Color) : String :=
  let code := match c with
    | .black => 40 | .red => 41 | .green => 42 | .yellow => 43
    | .blue => 44 | .magenta => 45 | .cyan => 46 | .white => 47
  s!"\x1b[{code}m"

/-- Set text intensity (bold/normal). -/
def setIntensity (i : Intensity) : String :=
  match i with
  | .bold => "\x1b[1m"
  | .normal => "\x1b[22m"

/-- Wrap text with foreground color and reset. -/
def colored (c : Color) (s : String) : String :=
  setFg c ++ s ++ reset

/-- Wrap text as bold. -/
def bold (s : String) : String :=
  setIntensity .bold ++ s ++ setIntensity .normal

end System.Console.Ansi
