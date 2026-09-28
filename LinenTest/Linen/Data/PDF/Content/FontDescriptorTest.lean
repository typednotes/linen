import Linen.Data.PDF.Content.FontDescriptor
import Linen.Data.Text.Encoding

open Data.PDF.Content.FontDescriptor

private def bs (s : String) : Data.ByteString := Data.Text.Encoding.encodeUtf8 s

private def mkFd (flags : UInt32) : FontDescriptor :=
  { fontName := bs "Helvetica"
    fontFamily := none
    fontStretch := none
    fontWeight := none
    flags := flags
    fontBBox := none
    italicAngle := 0
    ascent := none
    descent := none
    leading := none
    capHeight := none
    xHeight := none
    stemV := none
    stemH := none
    avgWidth := none
    maxWidth := none
    missingWidth := none
    charSet := none }

-- ── Bit positions match PDF32000-1:2008 §9.8.2, Table 123 ──

#guard FontDescriptorFlag.fixedPitch.bitPosition == 1
#guard FontDescriptorFlag.serif.bitPosition == 2
#guard FontDescriptorFlag.symbolic.bitPosition == 3
#guard FontDescriptorFlag.nonSymbolic.bitPosition == 6
#guard FontDescriptorFlag.forceBold.bitPosition == 19

-- ── `flagSet` reads the right bit ──

-- `FixedPitch` is bit 1 (value 1).
#guard flagSet (mkFd 1) FontDescriptorFlag.fixedPitch == true
#guard flagSet (mkFd 0) FontDescriptorFlag.fixedPitch == false

-- `Symbolic` is bit 3 (value 4).
#guard flagSet (mkFd 4) FontDescriptorFlag.symbolic == true
#guard flagSet (mkFd 4) FontDescriptorFlag.fixedPitch == false

-- `ForceBold` is bit 19 (value 262144).
#guard flagSet (mkFd 262144) FontDescriptorFlag.forceBold == true
#guard flagSet (mkFd 262144) FontDescriptorFlag.symbolic == false

-- Multiple flags can be set simultaneously.
#guard flagSet (mkFd 5) FontDescriptorFlag.fixedPitch == true  -- bits 1 and 3
#guard flagSet (mkFd 5) FontDescriptorFlag.symbolic == true
#guard flagSet (mkFd 5) FontDescriptorFlag.serif == false
