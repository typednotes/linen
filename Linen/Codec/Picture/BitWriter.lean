/-!
  Port of `Codec.Picture.BitWriter` from the `JuicyPixels` package
  (see `docs/imports/JuicyPixels/dependencies.md`, module 4 of 29). MSB-first
  bit-level reader/writer shared by the JPEG, PNG and GIF (LZW) codecs.

  Upstream threads a `BoolState`/`BoolWriteStateRef` through `ST s` actions
  (`StateT BoolState (ST s)` for the reader, hand-rolled `STRef`s for the
  writer, with a chunked buffer-list to amortise `MVector` growth). Per the
  collapsing precedent in `Linen.Data.Array.Shaped.Repr.Manifest` (also
  followed by `Linen.Codec.Picture.Types`'s `Image`), there is no need for
  any of that in Lean: `StateM` gives a pure equivalent of the `ST`-threaded
  state, and `ByteArray.push`'s amortised growth means the writer needs no
  chunk-list buffering trick to avoid repeated resizing.

  Upstream represents the remaining unconsumed bytes as a `ByteString`
  *value* (an O(1) pointer-offset slice); re-slicing a Lean `ByteArray` on
  every consumed byte would be O(n) per byte. Internal helpers below instead
  carry the remaining bytes as a `(rest : ByteArray, pos : Nat)` cursor pair,
  which is representationally different but observably identical — the
  public `setDecodedString*` functions still take a plain `ByteArray`
  (matching upstream's signatures), since that is genuinely a fresh value at
  every upstream call site.

  `getNextIntJpg`'s upstream result type is `Int32`; Lean has no fixed-width
  signed integer type, so it returns the same bit pattern as `UInt32` and
  leaves any sign reinterpretation to the (not yet ported) JPEG decoder that
  consumes it, exactly as upstream's `Int32` result is itself just a bit
  pattern until a caller applies sign correction.
-/

namespace Codec.Picture

-- ── Reader ──

/-- Current bit index into `currentByte`, the byte itself, and the remaining
    input as a `(rest, pos)` cursor. -/
structure BoolState where
  bitIdx : Int
  currentByte : UInt8
  rest : ByteArray
  pos : Nat

def emptyBoolState : BoolState := ⟨-1, 0, ByteArray.empty, 0⟩

/-- Monad used to read bits. -/
abbrev BoolReader := StateM BoolState

def runBoolReader (action : BoolReader α) : α :=
  (action.run ⟨0, 0, ByteArray.empty, 0⟩).1

def runBoolReaderWith (st : BoolState) (action : BoolReader α) : α × BoolState :=
  action.run st

def execBoolReader (st : BoolState) (reader : BoolReader α) : BoolState :=
  (reader.run st).2

private def nextByteFrom (idx : Int) (str : ByteArray) (pos : Nat) : BoolState :=
  if pos < str.size then ⟨idx, str.get! pos, str, pos + 1⟩ else ⟨idx, 0, ByteArray.empty, 0⟩

def initBoolState (str : ByteArray) : BoolState := nextByteFrom 0 str 0

/-- Bitify a string of bytes, handling JPEG's `0xFF 0x00` byte-stuffing
    escape (so should only be used when decoding JPEG). -/
private def jpgEscapeFrom (str : ByteArray) (pos : Nat) : BoolState :=
  if h : pos < str.size then
    let v := str.get! pos
    if v == 0xFF then
      if pos + 1 < str.size then
        let v2 := str.get! (pos + 1)
        if v2 == 0x00 then
          ⟨7, 0xFF, str, pos + 2⟩
        else
          jpgEscapeFrom str (pos + 2)
      else
        ⟨7, 0, ByteArray.empty, 0⟩
    else
      ⟨7, v, str, pos + 1⟩
  else
    ⟨7, 0, ByteArray.empty, 0⟩
termination_by str.size - pos
decreasing_by all_goals omega

def initBoolStateJpg (str : ByteArray) : BoolState :=
  if str.size == 0 then ⟨0, 0, ByteArray.empty, 0⟩ else jpgEscapeFrom str 0

/-- Bitify a string of bytes to decode. -/
def setDecodedString (str : ByteArray) : BoolReader Unit := set (nextByteFrom 0 str 0)

def setDecodedStringMSB (str : ByteArray) : BoolReader Unit := set (nextByteFrom 8 str 0)

def setDecodedStringJpg (str : ByteArray) : BoolReader Unit := set (jpgEscapeFrom str 0)

private def getNextBit : BoolReader Bool := do
  let s ← get
  let val := s.currentByte &&& ((1 : UInt8) <<< s.bitIdx.toNat.toUInt8) != 0
  if s.bitIdx == 7 then
    set (nextByteFrom 0 s.rest s.pos)
  else
    set { s with bitIdx := s.bitIdx + 1 }
  return val

/-- Drop all bits until the bit of index 0; useful for parsing JPEG restart
    markers, which are byte-aligned even though Huffman decoding might not
    be. -/
def byteAlignJpg : BoolReader Unit := do
  let s ← get
  if s.bitIdx != 7 then set (jpgEscapeFrom s.rest s.pos) else pure ()

def getNextBitJpg : BoolReader Bool := do
  let s ← get
  let val := s.currentByte &&& ((1 : UInt8) <<< s.bitIdx.toNat.toUInt8) != 0
  if s.bitIdx == 0 then
    set (jpgEscapeFrom s.rest s.pos)
  else
    set { s with bitIdx := s.bitIdx - 1 }
  return val

private def lsbAux : Nat → Nat → UInt32 → BoolReader UInt32
  | _, 0, acc => pure acc
  | count, n + 1, acc => do
      let bit ← getNextBit
      let nextVal := if bit then acc ||| ((1 : UInt32) <<< (count - (n + 1)).toUInt32) else acc
      lsbAux count n nextVal

def getNextBitsLSBFirst (count : Nat) : BoolReader UInt32 := lsbAux count count 0

private def msbCore (idx : Nat) (v : UInt8) (rest : ByteArray) (pos : Nat) (n : Nat)
    (acc : UInt32) : UInt32 × Nat × UInt8 × ByteArray × Nat :=
  if n == 0 then (acc, idx, v, rest, pos)
  else
    if _ : n >= idx then
      let s' := nextByteFrom 8 rest pos
      let remaining := n - idx
      let theseBits : UInt32 := v.toUInt32 <<< remaining.toUInt32
      msbCore 8 s'.currentByte s'.rest s'.pos remaining (acc ||| theseBits)
    else
      let remaining := idx - n
      let mask : UInt8 := ((1 : UInt8) <<< remaining.toUInt8) - 1
      ((v >>> remaining.toUInt8).toUInt32 ||| acc, remaining, v &&& mask, rest, pos)
termination_by 2 * n + if idx == 0 then 1 else 0
decreasing_by
  all_goals simp only [show (8 == 0) = false from rfl]
  all_goals (by_cases hidx : idx = 0 <;> simp_all <;> omega)

def getNextBitsMSBFirst (n : Nat) : BoolReader UInt32 := do
  let s ← get
  let (result, idx', v', rest', pos') := msbCore s.bitIdx.toNat s.currentByte s.rest s.pos n 0
  set (⟨Int.ofNat idx', v', rest', pos'⟩ : BoolState)
  return result

private def jpgIntCore (idx : Nat) (v : UInt8) (rest : ByteArray) (pos : Nat) (n : Nat)
    (acc : UInt32) : UInt32 × Nat × UInt8 × ByteArray × Nat :=
  if n == 0 then (acc, idx, v, rest, pos)
  else
    let leftBits := 1 + idx
    if h : n >= leftBits then
      let s' := jpgEscapeFrom rest pos
      let remaining := n - leftBits
      let mask : UInt32 := (1 <<< leftBits.toUInt32) - 1
      let finalV : UInt32 := v.toUInt32 &&& mask
      let theseBits := finalV <<< remaining.toUInt32
      jpgIntCore s'.bitIdx.toNat s'.currentByte s'.rest s'.pos remaining (acc ||| theseBits)
    else
      let remaining := leftBits - n
      let mask : UInt32 := (1 <<< n.toUInt32) - 1
      let finalV : UInt32 := v.toUInt32 >>> remaining.toUInt32
      ((finalV &&& mask) ||| acc, remaining - 1, v, rest, pos)
termination_by n
decreasing_by all_goals omega

def getNextIntJpg (n : Nat) : BoolReader UInt32 := do
  let s ← get
  let (result, idx', v', rest', pos') := jpgIntCore s.bitIdx.toNat s.currentByte s.rest s.pos n 0
  set (⟨Int.ofNat idx', v', rest', pos'⟩ : BoolState)
  return result

-- ── Writer ──

/-- Accumulated output bytes plus a partially-filled trailing byte. The
    partial-byte bit count is a `Fin 8` (rather than a bare `Nat`) so that
    `packBitsMSB`/`packBitsLSB` below carry the "fewer than 8 bits pending"
    invariant in their types instead of needing a separate state invariant
    proof to make their recursion's termination visible to `omega`. -/
structure BoolWriteState where
  buffer : ByteArray
  bitAcc : UInt8
  bitCount : Fin 8

/-- Monad used to write bits. -/
abbrev BoolWriter := StateM BoolWriteState

def newWriteStateRef : BoolWriteState := ⟨ByteArray.empty, 0, 0⟩

private def pushByte (v : UInt8) : BoolWriter Unit :=
  modify fun s => { s with buffer := s.buffer.push v }

private def setBitCount (acc : UInt8) (count : Fin 8) : BoolWriter Unit :=
  modify fun s => { s with bitAcc := acc, bitCount := count }

private def resetBitCount : BoolWriter Unit := setBitCount 0 0

private def flushLeftBits : BoolWriter Unit := do
  let s ← get
  if s.bitCount.val > 0 then pushByte (s.bitAcc <<< (8 - s.bitCount.val).toUInt8) else pure ()

def finalizeBoolWriter : BoolWriter ByteArray := do
  flushLeftBits
  return (← get).buffer

/-- MSB-first bit-packing core: packs `bitCount` low bits of `bitData` after
    the `accCount` bits already held in `acc`, returning every full byte
    produced along with the new partial accumulator. -/
private def packBitsMSB (bitData : UInt32) (bitCount : Nat) (acc : UInt8) (accCount : Fin 8) :
    Array UInt8 × UInt8 × Fin 8 :=
  let cleanMask : UInt32 := (1 <<< bitCount.toUInt32) - 1
  let cleanData := bitData &&& cleanMask
  if h1 : bitCount + accCount.val = 8 then
    (#[(acc <<< accCount.val.toUInt8) ||| cleanData.toUInt8], 0, 0)
  else if h2 : bitCount + accCount.val < 8 then
    (#[], (acc <<< accCount.val.toUInt8) ||| cleanData.toUInt8, ⟨accCount.val + bitCount, by omega⟩)
  else
    let leftBitCount := 8 - accCount.val
    let highPart := cleanData >>> (bitCount - leftBitCount).toUInt32
    let prevPart := acc.toUInt32 <<< leftBitCount.toUInt32
    let nextMask : UInt32 := (1 <<< (bitCount - leftBitCount).toUInt32) - 1
    let newData := cleanData &&& nextMask
    let newCount := bitCount - leftBitCount
    let toWrite : UInt8 := (prevPart ||| highPart).toUInt8
    let (rest, finalAcc, finalCount) := packBitsMSB newData newCount 0 0
    (#[toWrite] ++ rest, finalAcc, finalCount)
termination_by bitCount
decreasing_by all_goals omega

/-- Push a byte, escaping `0xFF` as `0xFF 0x00` (JPEG byte-stuffing). -/
private def dumpByteMSB (v : UInt8) : BoolWriter Unit := do
  if v == 0xFF then pushByte 0xFF; pushByte 0x00 else pushByte v

/-- Append `count` bits (1 to 32) of `d`, MSB-first, to the output. -/
def writeBits' (d : UInt32) (count : Nat) : BoolWriter Unit := do
  let s ← get
  let (bytes, acc, accCount) := packBitsMSB d count s.bitAcc s.bitCount
  for b in bytes do dumpByteMSB b
  setBitCount acc accCount

-- ── GIF variant (LSB-first, no `0xFF` escaping) ──

private def packBitsLSB (bitData : UInt32) (bitCount : Nat) (acc : UInt8) (accCount : Fin 8) :
    Array UInt8 × UInt8 × Fin 8 :=
  let cleanMask : UInt32 := (1 <<< bitCount.toUInt32) - 1
  let cleanData := bitData &&& cleanMask
  if h1 : bitCount + accCount.val = 8 then
    (#[acc ||| (cleanData <<< accCount.val.toUInt32).toUInt8], 0, 0)
  else if h2 : bitCount + accCount.val < 8 then
    (#[], (cleanData <<< accCount.val.toUInt32).toUInt8 ||| acc, ⟨accCount.val + bitCount, by omega⟩)
  else
    let leftBitCount := 8 - accCount.val
    let newData := cleanData >>> leftBitCount.toUInt32
    let newCount := bitCount - leftBitCount
    let toWrite : UInt8 := acc ||| (cleanData <<< accCount.val.toUInt32).toUInt8
    let (rest, finalAcc, finalCount) := packBitsLSB newData newCount 0 0
    (#[toWrite] ++ rest, finalAcc, finalCount)
termination_by bitCount
decreasing_by all_goals omega

/-- Append `count` bits (1 to 32) of `d`, LSB-first, to the output. -/
def writeBitsGif (d : UInt32) (count : Nat) : BoolWriter Unit := do
  let s ← get
  let (bytes, acc, accCount) := packBitsLSB d count s.bitAcc s.bitCount
  for b in bytes do pushByte b
  setBitCount acc accCount

private def flushLeftBitsGif : BoolWriter Unit := do
  let s ← get
  if s.bitCount.val > 0 then pushByte s.bitAcc else pure ()

def finalizeBoolWriterGif : BoolWriter ByteArray := do
  flushLeftBitsGif
  return (← get).buffer

end Codec.Picture
