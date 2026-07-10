/-
  Data.PDF.Content.Ops — content stream operators (PDF32000-1:2008 §A)

  Ports `Pdf.Content.Ops` from Hackage's `pdf-toolbox-content`
  (https://github.com/Yuras/pdf-toolbox, `content/lib/Pdf/Content/Ops.hs`,
  fetched from
  `https://raw.githubusercontent.com/Yuras/pdf-toolbox/master/content/lib/Pdf/Content/Ops.hs`),
  module 2 of the `pdf-toolbox-content` import documented in
  `docs/imports/PdfToolboxContent/dependencies.md`.

  A PDF content stream (PDF32000-1:2008 §7.8.2) is a sequence of operands
  (plain `Object`s) followed by an operator keyword. This module ports
  upstream's closed enumeration of every operator keyword the spec defines
  (Annex A), plus the catch-all `UnknownOp` for anything else, and the
  `toOp` classifier from a raw keyword to this enumeration. No recursion is
  involved anywhere — `toOp` is a single flat pattern match — so there is
  nothing to prove terminating.

  ## Design

  `Object`/`Data.ByteString` are reused directly from the already-ported
  `Data.PDF.Core.Object`/`Data.ByteString` rather than re-declared. Upstream's
  bare `Op_f_star`/`Op_B_star`/etc. constructor names (its own workaround for
  Haskell identifiers being unable to contain a literal `*`) are kept as-is:
  they already read as ordinary Lean identifiers, and there is no more
  idiomatic Lean spelling that wouldn't lose the direct correspondence to the
  PDF spec's own `f*`/`B*`/... operator names.
-/
import Linen.Data.PDF.Core.Object

namespace Data.PDF.Content.Ops

open Data.PDF.Core.Object (Object)

/-! ── Operators ── -/

/-- Every content-stream operator keyword defined by PDF32000-1:2008 Annex A,
    grouped the same way the spec (and upstream) groups them, plus
    `UnknownOp` for any keyword outside that closed list. -/
inductive Op where
  -- Graphics State Operators
  | q | Q | cm | w | J | j | M | d | ri | i | gs
  -- Path Construction Operators
  | m | l | c | v | y | h | re
  -- Path Painting Operators
  | S | s | f | F | f_star | B | B_star | b | b_star | n
  -- Clipping Path Operators
  | W | W_star
  -- Text Object Operators
  | BT | ET
  -- Text State Operators
  | Tc | Tw | Tz | TL | Tf | Tr | Ts
  -- Text Positioning Operators
  | Td | TD | Tm | T_star
  -- Text Showing Operators
  | Tj | apostrophe | quote | TJ
  -- Type 3 Font Operators
  | d0 | d1
  -- Color Operators
  | CS | cs | SC | SCN | sc | scn | G | g | RG | rg | K | k
  -- Shading Operator
  | sh
  -- Inline Image Operators
  | BI | ID | EI
  -- XObject Operator
  | Do
  -- Marked Content Operators
  | MP | DP | BMC | BDC | EMC
  -- Compatibility Operators
  | BX | EX
  -- Unknown
  /-- A keyword outside PDF32000-1:2008 Annex A's closed operator list. -/
  | UnknownOp (bytes : Data.ByteString)
deriving BEq, Repr

/-- An operator paired with its (already-parsed) operands, in the order they
    appeared in the content stream before the operator keyword. Mirrors
    upstream's `Operator = (Op, [Object])`. -/
abbrev Operator := Op × List Object

/-- A content-stream expression: either a plain operand `Object` or an
    operator keyword. Mirrors upstream's `Expr = Obj Object | Op Op`. -/
inductive Expr where
  /-- A plain operand. -/
  | obj (o : Object)
  /-- An operator keyword. -/
  | op (o : Op)
deriving BEq, Repr

/-! ── Classification ── -/

/-- Classify a raw content-stream keyword as an `Op`, falling back to
    `UnknownOp` for anything outside PDF32000-1:2008 Annex A's closed list.
    Mirrors upstream's `toOp`. -/
def toOp (bytes : Data.ByteString) : Op :=
  match bytes.unpack with
  | [113] => .q                                          -- "q"
  | [81] => .Q                                            -- "Q"
  | [99, 109] => .cm                                      -- "cm"
  | [119] => .w                                           -- "w"
  | [74] => .J                                            -- "J"
  | [106] => .j                                           -- "j"
  | [77] => .M                                            -- "M"
  | [100] => .d                                           -- "d"
  | [114, 105] => .ri                                     -- "ri"
  | [105] => .i                                           -- "i"
  | [103, 115] => .gs                                     -- "gs"
  | [109] => .m                                           -- "m"
  | [108] => .l                                           -- "l"
  | [99] => .c                                            -- "c"
  | [118] => .v                                           -- "v"
  | [121] => .y                                           -- "y"
  | [104] => .h                                           -- "h"
  | [114, 101] => .re                                     -- "re"
  | [83] => .S                                            -- "S"
  | [115] => .s                                           -- "s"
  | [102] => .f                                           -- "f"
  | [70] => .F                                            -- "F"
  | [102, 42] => .f_star                                  -- "f*"
  | [66] => .B                                            -- "B"
  | [66, 42] => .B_star                                   -- "B*"
  | [98] => .b                                            -- "b"
  | [98, 42] => .b_star                                   -- "b*"
  | [110] => .n                                           -- "n"
  | [87] => .W                                            -- "W"
  | [87, 42] => .W_star                                   -- "W*"
  | [66, 84] => .BT                                       -- "BT"
  | [69, 84] => .ET                                       -- "ET"
  | [84, 99] => .Tc                                       -- "Tc"
  | [84, 119] => .Tw                                      -- "Tw"
  | [84, 122] => .Tz                                      -- "Tz"
  | [84, 76] => .TL                                       -- "TL"
  | [84, 102] => .Tf                                      -- "Tf"
  | [84, 114] => .Tr                                      -- "Tr"
  | [84, 115] => .Ts                                      -- "Ts"
  | [84, 100] => .Td                                      -- "Td"
  | [84, 68] => .TD                                       -- "TD"
  | [84, 109] => .Tm                                      -- "Tm"
  | [84, 42] => .T_star                                   -- "T*"
  | [84, 106] => .Tj                                      -- "Tj"
  | [39] => .apostrophe                                   -- "'"
  | [34] => .quote                                        -- "\""
  | [84, 74] => .TJ                                       -- "TJ"
  | [100, 48] => .d0                                      -- "d0"
  | [100, 49] => .d1                                      -- "d1"
  | [67, 83] => .CS                                       -- "CS"
  | [99, 115] => .cs                                      -- "cs"
  | [83, 67] => .SC                                       -- "SC"
  | [83, 67, 78] => .SCN                                  -- "SCN"
  | [115, 99] => .sc                                      -- "sc"
  | [115, 99, 110] => .scn                                -- "scn"
  | [71] => .G                                            -- "G"
  | [103] => .g                                           -- "g"
  | [82, 71] => .RG                                       -- "RG"
  | [114, 103] => .rg                                     -- "rg"
  | [75] => .K                                            -- "K"
  | [107] => .k                                           -- "k"
  | [115, 104] => .sh                                     -- "sh"
  | [66, 73] => .BI                                       -- "BI"
  | [73, 68] => .ID                                       -- "ID"
  | [69, 73] => .EI                                       -- "EI"
  | [68, 111] => .Do                                      -- "Do"
  | [77, 80] => .MP                                       -- "MP"
  | [68, 80] => .DP                                       -- "DP"
  | [66, 77, 67] => .BMC                                  -- "BMC"
  | [66, 68, 67] => .BDC                                  -- "BDC"
  | [69, 77, 67] => .EMC                                  -- "EMC"
  | [66, 88] => .BX                                       -- "BX"
  | [69, 88] => .EX                                       -- "EX"
  | _ => .UnknownOp bytes

end Data.PDF.Content.Ops
