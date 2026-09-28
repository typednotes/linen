/-
  Tests for `Linen.PostgREST.RangeQuery`.
-/
import Linen.PostgREST.RangeQuery

open PostgREST.RangeQuery

namespace Tests.PostgREST.RangeQuery

/-! ### `NonnegRange` -/

#guard NonnegRange.unlimited == ({ rangeOffset := 0, rangeLimit := none } : NonnegRange)
#guard NonnegRange.unlimited.isUnlimited == true
#guard ({ rangeOffset := 0, rangeLimit := some 10 } : NonnegRange).isUnlimited == false
#guard ({ rangeOffset := 0, rangeLimit := some 10 } : NonnegRange).size == some 10
#guard NonnegRange.unlimited.size == none

#guard toString ({ rangeOffset := 0, rangeLimit := some 25 } : NonnegRange) == "0-24"
#guard toString NonnegRange.unlimited == "0-"

#guard ({ rangeOffset := 5, rangeLimit := some 20 } : NonnegRange).restrictTo 10 ==
  ({ rangeOffset := 5, rangeLimit := some 10 } : NonnegRange)
#guard ({ rangeOffset := 5, rangeLimit := some 5 } : NonnegRange).restrictTo 10 ==
  ({ rangeOffset := 5, rangeLimit := some 5 } : NonnegRange)
#guard NonnegRange.unlimited.restrictTo 10 ==
  ({ rangeOffset := 0, rangeLimit := some 10 } : NonnegRange)

/-! ### `parseRange` -/

#guard parseRange "0-24" == some ({ rangeOffset := 0, rangeLimit := some 25 } : NonnegRange)
#guard parseRange "0-" == some ({ rangeOffset := 0, rangeLimit := none } : NonnegRange)
#guard parseRange "10-19" == some ({ rangeOffset := 10, rangeLimit := some 10 } : NonnegRange)
#guard parseRange "5" == some ({ rangeOffset := 5, rangeLimit := some 1 } : NonnegRange)
#guard parseRange "  0-24  " == some ({ rangeOffset := 0, rangeLimit := some 25 } : NonnegRange)
#guard parseRange "abc" == none
#guard parseRange "5-2" == none
#guard parseRange "abc-24" == none
#guard parseRange "0-abc" == none
#guard parseRange "0-24-5" == none
#guard parseRange "" == none

/-! ### `ContentRange` / `contentRangeHeader` -/

-- Whenever `total` is a concrete `some n`, the `valid`/`h` autoParam's
-- default `by intro t ht; omega` can't invert `some n = some t` on its own,
-- so it is supplied explicitly as `by intro t ht; injection ht with ht; omega`.
-- Whenever `total` is `none`, the goal is vacuous but not always trivially
-- true, so the default can fail too; `by intro t ht; injection ht` closes it
-- directly (mismatched constructors give a contradiction regardless of the
-- goal shape). `fromNonnegRange`'s goal additionally carries unreduced
-- structure-field projections (`r.rangeOffset`, `r.rangeLimit.getD 0`) that
-- plain `omega` can't evaluate; `subst ht; decide` reduces and closes those
-- instead. Each literal is kept on a single line: a multi-line structure
-- literal here parses as two commands.
#guard contentRangeHeader { offset := 0, limit := 25, total := some 100, valid := by intro t ht; injection ht with ht; omega } == "0-24/100"
#guard contentRangeHeader { offset := 0, limit := 25, total := none, valid := by intro t ht; injection ht } == "0-24/*"
#guard contentRangeHeader { offset := 0, limit := 0, total := some 100, valid := by intro t ht; injection ht with ht; omega } == "*/100"
#guard contentRangeHeader { offset := 0, limit := 0, total := none } == "*/*"
#guard toString ({ offset := 10, limit := 5, total := some 20, valid := by intro t ht; injection ht with ht; omega } : ContentRange) == "10-14/20"

#guard ({ offset := 0, limit := 25, total := some 100, valid := by intro t ht; injection ht with ht; omega } : ContentRange) == ({ offset := 0, limit := 25, total := some 100, valid := by intro t ht; injection ht with ht; omega } : ContentRange)
#guard ({ offset := 0, limit := 25, total := some 100, valid := by intro t ht; injection ht with ht; omega } : ContentRange) != ({ offset := 0, limit := 20, total := some 100, valid := by intro t ht; injection ht with ht; omega } : ContentRange)

#guard (default : ContentRange).offset == 0
#guard (default : ContentRange).limit == 0
#guard (default : ContentRange).total == none

/-! ### `ContentRange.fromNonnegRange` -/

#guard ContentRange.fromNonnegRange { rangeOffset := 0, rangeLimit := some 25 } (some 100) (by intro t ht; injection ht with ht; subst ht; decide) == ({ offset := 0, limit := 25, total := some 100, valid := by intro t ht; injection ht with ht; omega } : ContentRange)
#guard ContentRange.fromNonnegRange NonnegRange.unlimited (some 100) (by intro t ht; injection ht with ht; subst ht; decide) == ({ offset := 0, limit := 0, total := some 100, valid := by intro t ht; injection ht with ht; omega } : ContentRange)
#guard ContentRange.fromNonnegRange { rangeOffset := 0, rangeLimit := some 25 } none (by intro t ht; injection ht) == ({ offset := 0, limit := 25, total := none, valid := by intro t ht; injection ht } : ContentRange)

end Tests.PostgREST.RangeQuery
