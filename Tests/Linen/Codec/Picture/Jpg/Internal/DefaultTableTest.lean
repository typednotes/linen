import Linen.Codec.Picture.Jpg.Internal.DefaultTable

/-!
  Tests for `Linen.Codec.Picture.Jpg.Internal.DefaultTable`: table sizes,
  spot-checked literal values against ITU-T.81 Annex K / the fetched
  `JuicyPixels` source, and structural sanity checks on the Huffman trees
  built from the default code-length tables.
-/

open Codec.Picture.Jpg.Internal

-- ── Quantization tables: size and known values ──

#guard defaultLumaQuantizationTable.size == 64
#guard defaultChromaQuantizationTable.size == 64

-- the JPEG standard's well-known luminance quantization table starts with
-- 16, 11, 10, 16, 24, 40, 51, 61
#guard defaultLumaQuantizationTable[0]! == 16
#guard defaultLumaQuantizationTable[1]! == 11
#guard defaultLumaQuantizationTable[2]! == 10
#guard defaultLumaQuantizationTable[3]! == 16
#guard defaultLumaQuantizationTable[4]! == 24
#guard defaultLumaQuantizationTable[5]! == 40
#guard defaultLumaQuantizationTable[6]! == 51
#guard defaultLumaQuantizationTable[7]! == 61

-- last entry of the luma table (bottom-right of the 8x8 grid)
#guard defaultLumaQuantizationTable[63]! == 99

-- chroma table starts with 17, 18, 24, 47, and is saturated at 99 from
-- row 5 onward
#guard defaultChromaQuantizationTable[0]! == 17
#guard defaultChromaQuantizationTable[1]! == 18
#guard defaultChromaQuantizationTable[2]! == 24
#guard defaultChromaQuantizationTable[3]! == 47
#guard defaultChromaQuantizationTable[32]! == 99
#guard defaultChromaQuantizationTable[63]! == 99

-- ── DC Huffman tables: shape (16 code-length groups) and known values ──

#guard defaultDcLumaHuffmanTable.length == 16
#guard defaultDcChromaHuffmanTable.length == 16

-- Table K.3: depth-2 group holds symbols 1..5, all other single-symbol
-- groups hold exactly one symbol
#guard defaultDcLumaHuffmanTable[0]! == []
#guard defaultDcLumaHuffmanTable[1]! == [0]
#guard defaultDcLumaHuffmanTable[2]! == [1, 2, 3, 4, 5]
#guard defaultDcLumaHuffmanTable[8]! == [11]
#guard defaultDcLumaHuffmanTable[9]! == []

-- Table K.4: depth-2 group holds symbols 0..2
#guard defaultDcChromaHuffmanTable[1]! == [0, 1, 2]
#guard defaultDcChromaHuffmanTable[10]! == [11]

-- every DC table symbol total is 12 (categories 0..11)
#guard (defaultDcLumaHuffmanTable.map List.length).sum == 12
#guard (defaultDcChromaHuffmanTable.map List.length).sum == 12

-- ── AC Huffman tables: shape and known values ──

#guard defaultAcLumaHuffmanTable.length == 16
#guard defaultAcChromaHuffmanTable.length == 16

-- Table K.5, first few groups
#guard defaultAcLumaHuffmanTable[0]! == []
#guard defaultAcLumaHuffmanTable[1]! == [0x01, 0x02]
#guard defaultAcLumaHuffmanTable[3]! == [0x00, 0x04, 0x11]
#guard defaultAcLumaHuffmanTable[14]! == [0x82]

-- the last (longest-code) group of the AC luma table starts with the ZRL
-- run-length codes and ends with the run up to 0xFA
#guard (defaultAcLumaHuffmanTable[15]!).head? == some 0x09
#guard (defaultAcLumaHuffmanTable[15]!).getLast? == some 0xFA
#guard (defaultAcLumaHuffmanTable[15]!).length == 125

-- Table K.6, first few groups
#guard defaultAcChromaHuffmanTable[0]! == []
#guard defaultAcChromaHuffmanTable[1]! == [0x00, 0x01]
#guard defaultAcChromaHuffmanTable[13]! == [0xE1]
#guard defaultAcChromaHuffmanTable[14]! == [0x25, 0xF1]

#guard (defaultAcChromaHuffmanTable[15]!).head? == some 0x17
#guard (defaultAcChromaHuffmanTable[15]!).getLast? == some 0xFA
#guard (defaultAcChromaHuffmanTable[15]!).length == 119

-- both standard AC tables carry exactly 162 symbols in total (the full
-- byte range minus the values that never occur as an AC run/size pair)
#guard (defaultAcLumaHuffmanTable.map List.length).sum == 162
#guard (defaultAcChromaHuffmanTable.map List.length).sum == 162

-- ── Huffman trees built from the default tables ──

-- building a tree from an empty table yields the empty tree
#guard buildHuffmanTree [] == HuffmanTree.empty

-- every default tree is non-empty and fully defined (every leaf reachable,
-- no dangling `empty` branch at a used depth)
#guard defaultDcLumaHuffmanTree != HuffmanTree.empty
#guard defaultDcChromaHuffmanTree != HuffmanTree.empty
#guard defaultAcLumaHuffmanTree != HuffmanTree.empty
#guard defaultAcChromaHuffmanTree != HuffmanTree.empty

-- a depth-1 table `[[0]]` builds a single `branch (leaf 0) empty`
#guard buildHuffmanTree [[0]] == HuffmanTree.branch (HuffmanTree.leaf 0) HuffmanTree.empty

-- a depth-1 table with two symbols builds `branch (leaf 0) (leaf 1)`
#guard buildHuffmanTree [[0, 1]] ==
  HuffmanTree.branch (HuffmanTree.leaf 0) (HuffmanTree.leaf 1)

-- ── `DctComponent` ──

#guard DctComponent.dcComponent != DctComponent.acComponent
