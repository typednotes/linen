/-
  Linen.Database.SQLite.Simple.QQ — the `sql` quasiquoter

  Module #14 of `docs/imports/sqlite-simple/dependencies.md`, on module #5
  (`Linen.Database.SQLite.Simple.Types`, for `Query`).

  ## Design

  Upstream's `sql` quasiquoter is GHC Template Haskell (`QuasiQuoter`), one
  of the libraries `AGENTS.md`'s Hackage-import precedence note records as
  having no Lean analogue by itself. But the *feature* — writing a large,
  multi-line embedded SQL string and turning it directly into a `Query`
  value, without ad hoc string concatenation — is not out of scope: Lean's
  own `syntax`/`macro_rules` elaboration mechanism is the natural, idiomatic
  substitute, and `sql "…"` below is ported using it rather than dropped.

  ### How deep is the "compile-time check", really?

  Checked directly against upstream's own source
  (`Database/SQLite/Simple/QQ.hs`): its `quoteExp` implementation is

  > `sqlExp = appE [| fromString :: String -> Query |] . stringE`

  i.e. it takes the quasiquoter's raw content string and *directly* splices
  it as a string literal into `fromString :: String -> Query`. There is no
  SQL parsing, no placeholder-count check, no syntax validation of any
  kind — the "compile-time check" is nothing more than: the quoted text
  must be well-formed enough for GHC's own quasiquoter machinery to hand
  `quoteExp` a `String` at all (which for `QuasiQuoters` is essentially
  always, since the quoted region is delimited by `[sql| … |]` and handed
  over verbatim). In other words, upstream's own "check" amounts to *no*
  SQL-specific validation whatsoever; it is purely a convenience for
  embedding a multi-line string literal without manual escaping, turned
  into a `Query` at compile time instead of at every call site.

  This port matches that precisely: `sql "…"` elaborates to
  `Query.ofString "…"`, with the only "check" performed being whatever Lean's
  own parser already requires of a string-literal token (well-formed escape
  sequences, closing quote, …) — exactly upstream's own, equally shallow,
  guarantee. Nothing here validates that the string is actually valid SQL;
  that is still only discovered when SQLite prepares the resulting `Query`
  at run time, matching upstream exactly.

  Lean string literals (unlike Haskell's, without `-XMultilineStrings`)
  already support embedded newlines directly, so `sql "SELECT * FROM t"` can
  span multiple lines exactly the way upstream's `[sql| … |]` could — the
  bracket-delimited quasiquoter syntax itself has no remaining purpose to
  port beyond that, since Lean's ordinary string literal already covers it.

  ## Haskell source
  - `Database.SQLite.Simple.QQ` (`sqlite-simple` package)
-/

import Linen.Database.SQLite.Simple.Types

namespace Database.SQLite.Simple.QQ

open Database.SQLite.Simple.Types (Query)

/-- `sql "…"` builds a `Query` directly from a string literal (see the
    module doc for exactly how little validation this — or upstream's own
    quasiquoter — actually performs at compile time). -/
syntax "sql " str : term

macro_rules
  | `(sql $s:str) => `(Query.ofString $s)

end Database.SQLite.Simple.QQ
