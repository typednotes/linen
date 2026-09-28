import Linen.Data.PDF.Content.Encoding.WinAnsi

open Data.PDF.Content.Encoding.WinAnsi

-- ── ASCII codes map to themselves (WinAnsi agrees with ASCII below 0x80) ──

#guard winAnsiEncoding[(65 : UInt8)]? == some "A"
#guard winAnsiEncoding[(32 : UInt8)]? == some " "

-- ── High byte codes map to accented/special characters ──

#guard winAnsiEncoding[(198 : UInt8)]? == some "Æ"
#guard winAnsiEncoding[(255 : UInt8)]? == some "ÿ"

-- ── The table has exactly the upstream entry count ──

#guard winAnsiEncoding.size == 216
