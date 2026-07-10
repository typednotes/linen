import Linen.Data.PDF.Content.Encoding.MacRoman

open Data.PDF.Content.Encoding.MacRoman

-- ── ASCII codes map to themselves ──

#guard macRomanEncoding[(65 : UInt8)]? == some "A"

-- ── High byte codes map to accented/special characters ──

#guard macRomanEncoding[(174 : UInt8)]? == some "Æ"
#guard macRomanEncoding[(128 : UInt8)]? == some "Ä"

-- ── The table has exactly the upstream entry count ──

#guard macRomanEncoding.size == 207
