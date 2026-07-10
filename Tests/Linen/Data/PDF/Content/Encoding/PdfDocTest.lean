import Linen.Data.PDF.Content.Encoding.PdfDoc

open Data.PDF.Content.Encoding.PdfDoc

-- ── Control codes map to themselves ──

#guard pdfDocEncoding[(0 : UInt8)]? == some "\x00"
#guard pdfDocEncoding[(65 : UInt8)]? == some "A"

-- ── Upstream's duplicate mapping for byte codes 22 and 23 is preserved
--     byte-for-byte, exactly as it appears upstream (not "fixed") ──

#guard pdfDocEncoding[(22 : UInt8)]? == some "\x17"
#guard pdfDocEncoding[(23 : UInt8)]? == some "\x17"

-- ── Codes upstream leaves undefined have no entry ──

#guard pdfDocEncoding[(127 : UInt8)]? == none
#guard pdfDocEncoding[(159 : UInt8)]? == none
#guard pdfDocEncoding[(173 : UInt8)]? == none

-- ── The table has exactly the upstream entry count (256 possible codes,
--     minus the 3 upstream leaves undefined) ──

#guard pdfDocEncoding.size == 253
