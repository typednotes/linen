/-
  Tests for `Linen.Codec.Picture.BitWriter` — checks the LSB-first, MSB-first,
  and JPEG-escaped bit readers, and the MSB (JPEG/PNG) and LSB (GIF) bit
  writers.
-/
import Linen.Codec.Picture.BitWriter

open Codec.Picture

-- ── Reader: LSB-first ──

-- a full byte read back LSB-first reproduces the byte unchanged
#guard runBoolReader (do setDecodedString (ByteArray.mk #[0xA5]); getNextBitsLSBFirst 8) == 0xA5

-- ── Reader: MSB-first ──

#guard runBoolReader (do setDecodedStringMSB (ByteArray.mk #[0xA5]); getNextBitsMSBFirst 8) == 0xA5

-- top 4 bits of 0b10110000 read MSB-first is 0b1011
#guard runBoolReader (do setDecodedStringMSB (ByteArray.mk #[0xB0]); getNextBitsMSBFirst 4) == 0xB

-- reading across a byte boundary: all of 0xFF then the top nibble of 0x00
#guard runBoolReader
    (do setDecodedStringMSB (ByteArray.mk #[0xFF, 0x00]); getNextBitsMSBFirst 12) == 0xFF0

-- ── Reader: JPEG (MSB-first with `0xFF 0x00` byte-stuffing) ──

#guard runBoolReader (do setDecodedStringJpg (ByteArray.mk #[0xB0]); getNextIntJpg 4) == 0xB

-- `0xFF 0x00` unescapes to a literal `0xFF` byte
#guard runBoolReader (do setDecodedStringJpg (ByteArray.mk #[0xFF, 0x00, 0x12]); getNextIntJpg 8)
    == 0xFF

-- `byteAlignJpg` drops the unread low bits of the current byte and resumes
-- from the following byte
#guard runBoolReader (do
    setDecodedStringJpg (ByteArray.mk #[0xB0, 0x12])
    let _ ← getNextIntJpg 4
    byteAlignJpg
    getNextIntJpg 8) == 0x12

/-- Run a `BoolWriter` action from a fresh state and return the finalized
    output bytes, using `finalize` (either `finalizeBoolWriter` for the
    MSB/JPEG writer or `finalizeBoolWriterGif` for the GIF writer). -/
def runWriter (finalize : BoolWriter ByteArray) (action : BoolWriter Unit) : ByteArray :=
  (finalize.run (action.run newWriteStateRef).2).1

-- ── Writer: MSB-first (JPEG/PNG) ──

#guard runWriter finalizeBoolWriter (writeBits' 0xA5 8) == ByteArray.mk #[0xA5]

#guard runWriter finalizeBoolWriter (writeBits' 0xB 4) == ByteArray.mk #[0xB0]

-- a full `0xFF` byte gets escaped to `0xFF 0x00`
#guard runWriter finalizeBoolWriter (writeBits' 0xFF 8) == ByteArray.mk #[0xFF, 0x00]

-- ── Writer: LSB-first (GIF), no `0xFF` escaping ──

#guard runWriter finalizeBoolWriterGif (writeBitsGif 0xA5 8) == ByteArray.mk #[0xA5]

#guard runWriter finalizeBoolWriterGif (writeBitsGif 0xFF 8) == ByteArray.mk #[0xFF]

#guard runWriter finalizeBoolWriterGif (writeBitsGif 0xB 4) == ByteArray.mk #[0xB]
