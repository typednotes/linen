/-
  Linen.System.Console.Ansi — ANSI terminal escape codes

  Provides constants and functions for colored terminal output — and the rule
  for when *not* to use it (`wanted`), which matters more: escape codes are
  corrupting noise everywhere except an actual terminal (a CI step summary, a
  test matching rendered lines as text).
-/
namespace System.Console.Ansi

/-- ANSI color codes. -/
inductive Color where
  | black | red | green | yellow | blue | magenta | cyan | white
deriving BEq, Repr

/-- ANSI text intensity. `faint` is SGR 2, which most terminals render
    dimmed. -/
inductive Intensity where
  | bold | faint | normal
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
  | .faint => "\x1b[2m"
  | .normal => "\x1b[22m"

/-- Wrap text with foreground color and reset. -/
def colored (c : Color) (s : String) : String :=
  setFg c ++ s ++ reset

/-- Wrap text as bold. -/
def bold (s : String) : String :=
  setIntensity .bold ++ s ++ setIntensity .normal

/-- Wrap text as faint (dimmed). -/
def dim (s : String) : String :=
  setIntensity .faint ++ s ++ setIntensity .normal

-- ── Switchable styling ────────────────────────────────────────────────

/-- The SGR parameter of a foreground colour, for `style`. -/
def Color.fgCode (c : Color) : String :=
  match c with
  | .black => "30" | .red => "31" | .green => "32" | .yellow => "33"
  | .blue => "34" | .magenta => "35" | .cyan => "36" | .white => "37"

/-- The SGR parameter of bold, for `style`. -/
def boldCode : String := "1"

/-- The SGR parameter of faint (dim), for `style`. -/
def faintCode : String := "2"

/-- Wrap `s` in one SGR parameter (`Color.fgCode`, `boldCode`, `faintCode`, or
    any other), or return it untouched when styling is off.

    Taking the switch as a parameter rather than reading the environment keeps
    this pure, so rendered output stays a value and stays testable; decide the
    switch once, with `wanted`. Moved from the sibling `infra`
    (`Infra/Core/Ansi.lean`). -/
def style (on : Bool) (code : String) (s : String) : String :=
  if on then s!"\x1b[{code}m{s}{reset}" else s

/-- Whether to colour, given the two environment variables and whether stdout
    is a terminal: `NO_COLOR` set to anything non-empty wins outright (the
    `no-color.org` convention); then `FORCE_COLOR`, for a terminal-emulating CI
    that renders escape codes; then the terminal test. A value that is only
    whitespace counts as unset. The pure half of `wanted`. -/
def shouldColor (noColor forceColor : Option String) (isTty : Bool) : Bool :=
  let nonEmpty (v : Option String) := !(v.getD "").trimAscii.isEmpty
  if nonEmpty noColor then false
  else if nonEmpty forceColor then true
  else isTty

/-- Whether output should be coloured, by the conventions people expect — see
    `shouldColor`. A pipe or a redirect gets none, which is what keeps
    `cmd | tee "$GITHUB_STEP_SUMMARY"` free of escape codes without the caller
    having to know colour exists. -/
def wanted : IO Bool := do
  return shouldColor (← IO.getEnv "NO_COLOR") (← IO.getEnv "FORCE_COLOR") (← (← IO.getStdout).isTty)

end System.Console.Ansi
