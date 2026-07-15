/-
  `Linen.Text.Pandoc.Asciify` — strip diacritics to ASCII.

  ## Haskell source

  Ported from `Text.Pandoc.Asciify` in the `pandoc` package
  (v3.10, `src/Text/Pandoc/Asciify.hs`).

  Provides `toAsciiChar : Char → Option Char` and `toAsciiText : String → String`,
  which strip diacritics by keeping only the ASCII base of each accented letter.

  ### Deviations from upstream

  * Upstream is algorithmic: it applies Unicode Normalization Form D (NFD, via
    `Data.Text.Normalize`), then keeps only the ASCII characters — so any
    accented letter that decomposes into an ASCII base plus combining marks
    loses its marks, and any letter that does not so decompose (`ø`, `ł`, `æ`,
    …) is dropped (`toAsciiText`) or yields `none` (`toAsciiChar`). Lean's
    standard library has no Unicode normalization, so the equivalent
    decomposition is realised here as an explicit table (`asciiTable`) over the
    Latin-1 Supplement and Latin Extended-A accented letters that NFD
    decomposes to an ASCII base. Behaviour matches upstream on the covered
    ranges; characters outside the table with no ASCII decomposition yield
    `none`/are dropped, exactly as NFD-then-filter would.
  * The Turkish dotless `ı` (U+0131) special case (→ `i`) is preserved, as
    upstream, since it does not decompose under NFD.
-/

namespace Linen.Text.Pandoc
namespace Asciify

/-- Accented Latin letters (that NFD decomposes to an ASCII base) paired with
    that base. -/
def asciiTable : List (Char × Char) :=
  [ -- Latin-1 Supplement, uppercase
    ('À','A'), ('Á','A'), ('Â','A'), ('Ã','A'), ('Ä','A'), ('Å','A')
  , ('Ç','C'), ('È','E'), ('É','E'), ('Ê','E'), ('Ë','E')
  , ('Ì','I'), ('Í','I'), ('Î','I'), ('Ï','I'), ('Ñ','N')
  , ('Ò','O'), ('Ó','O'), ('Ô','O'), ('Õ','O'), ('Ö','O')
  , ('Ù','U'), ('Ú','U'), ('Û','U'), ('Ü','U'), ('Ý','Y')
    -- Latin-1 Supplement, lowercase
  , ('à','a'), ('á','a'), ('â','a'), ('ã','a'), ('ä','a'), ('å','a')
  , ('ç','c'), ('è','e'), ('é','e'), ('ê','e'), ('ë','e')
  , ('ì','i'), ('í','i'), ('î','i'), ('ï','i'), ('ñ','n')
  , ('ò','o'), ('ó','o'), ('ô','o'), ('õ','o'), ('ö','o')
  , ('ù','u'), ('ú','u'), ('û','u'), ('ü','u'), ('ý','y'), ('ÿ','y')
    -- Latin Extended-A (decomposable subset)
  , ('Ā','A'), ('ā','a'), ('Ă','A'), ('ă','a'), ('Ą','A'), ('ą','a')
  , ('Ć','C'), ('ć','c'), ('Ĉ','C'), ('ĉ','c'), ('Ċ','C'), ('ċ','c')
  , ('Č','C'), ('č','c'), ('Ď','D'), ('ď','d')
  , ('Ē','E'), ('ē','e'), ('Ĕ','E'), ('ĕ','e'), ('Ė','E'), ('ė','e')
  , ('Ę','E'), ('ę','e'), ('Ě','E'), ('ě','e')
  , ('Ĝ','G'), ('ĝ','g'), ('Ğ','G'), ('ğ','g'), ('Ġ','G'), ('ġ','g')
  , ('Ģ','G'), ('ģ','g'), ('Ĥ','H'), ('ĥ','h')
  , ('Ĩ','I'), ('ĩ','i'), ('Ī','I'), ('ī','i'), ('Ĭ','I'), ('ĭ','i')
  , ('Į','I'), ('į','i'), ('İ','I'), ('Ĵ','J'), ('ĵ','j')
  , ('Ķ','K'), ('ķ','k'), ('Ĺ','L'), ('ĺ','l'), ('Ļ','L'), ('ļ','l')
  , ('Ľ','L'), ('ľ','l'), ('Ń','N'), ('ń','n'), ('Ņ','N'), ('ņ','n')
  , ('Ň','N'), ('ň','n'), ('Ō','O'), ('ō','o'), ('Ŏ','O'), ('ŏ','o')
  , ('Ő','O'), ('ő','o'), ('Ŕ','R'), ('ŕ','r'), ('Ŗ','R'), ('ŗ','r')
  , ('Ř','R'), ('ř','r'), ('Ś','S'), ('ś','s'), ('Ŝ','S'), ('ŝ','s')
  , ('Ş','S'), ('ş','s'), ('Š','S'), ('š','s'), ('Ţ','T'), ('ţ','t')
  , ('Ť','T'), ('ť','t'), ('Ũ','U'), ('ũ','u'), ('Ū','U'), ('ū','u')
  , ('Ŭ','U'), ('ŭ','u'), ('Ů','U'), ('ů','u'), ('Ű','U'), ('ű','u')
  , ('Ų','U'), ('ų','u'), ('Ŵ','W'), ('ŵ','w'), ('Ŷ','Y'), ('ŷ','y')
  , ('Ÿ','Y'), ('Ź','Z'), ('ź','z'), ('Ż','Z'), ('ż','z'), ('Ž','Z'), ('ž','z') ]

/-- The ASCII equivalent of a character (its base letter with diacritics
    removed), or `none` if it has no ASCII decomposition. -/
def toAsciiChar (c : Char) : Option Char :=
  if c.toNat < 128 then some c
  else if c == 'ı' then some 'i'  -- Turkish undotted i
  else asciiTable.lookup c

/-- Strip diacritics from a string, dropping characters with no ASCII form. -/
def toAsciiText (s : String) : String :=
  String.ofList (s.toList.filterMap toAsciiChar)

end Asciify
end Linen.Text.Pandoc
