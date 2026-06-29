/-
  Linen.Network.HTTP.Chunked — HTTP/1.1 chunked transfer encoding

  Frames `ByteArray` payloads in HTTP chunked transfer encoding (Haskell's
  `bsb-http-chunked`), working with core `ByteArray` directly.

  Each chunk is `<hex-length>\r\n<data>\r\n`; the terminator is `0\r\n\r\n`.

  The hex length uses core `Nat.toDigits 16` (lowercase, no leading zeros), so
  there is no hand-rolled fuel-driven digit loop.
-/

namespace Network.HTTP.Chunked

/-- The byte encoding of a natural number in lowercase hexadecimal
    (`Nat.toDigits 16`): e.g. `0 ↦ "0"`, `255 ↦ "ff"`. -/
private def natToHex (n : Nat) : ByteArray :=
  (String.ofList (Nat.toDigits 16 n)).toUTF8

/-- The CRLF separator `\r\n`. -/
private def crlf : ByteArray := ByteArray.mk #[13, 10]

/-- Wrap data in a single HTTP chunk.
    $$\text{chunkedTransferEncoding}(d) = \text{hex}(|d|) \cdot \texttt{\\r\\n} \cdot d \cdot \texttt{\\r\\n}$$

    Returns empty for empty input (no zero-length chunks — use
    `chunkedTransferTerminator` to end the transfer). -/
def chunkedTransferEncoding (data : ByteArray) : ByteArray :=
  if data.size == 0 then ByteArray.empty
  else natToHex data.size ++ crlf ++ data ++ crlf

/-- The chunked transfer encoding terminator.
    $$\text{chunkedTransferTerminator} = \texttt{0\\r\\n\\r\\n}$$ -/
def chunkedTransferTerminator : ByteArray :=
  ByteArray.mk #[48, 13, 10, 13, 10]  -- "0\r\n\r\n"

/-- Encode a list of chunks into a complete chunked transfer body.
    $$\text{encodeChunked}([c_1, \ldots, c_n]) = \text{chunk}(c_1) \cdots \text{chunk}(c_n) \cdot \text{terminator}$$ -/
def encodeChunked (chunks : List ByteArray) : ByteArray :=
  let body := chunks.foldl (fun acc chunk => acc ++ chunkedTransferEncoding chunk) ByteArray.empty
  body ++ chunkedTransferTerminator

end Network.HTTP.Chunked
