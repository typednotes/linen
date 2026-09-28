import Linen.System.IO

open System.IO

-- The header overhead is two 64-bit words.
#guard byteArrayOverhead == 16

-- `arrayPayloadSize` subtracts the overhead.
#guard arrayPayloadSize (32 * 1024) == 32 * 1024 - 16

-- The default chunk size is the 32 KB payload.
#guard defaultChunkSize == 32768 - 16
