/-
  Linen.Database.SQLite.Simple.Time — SQLite date/time conversions

  Module #8 of `docs/imports/sqlite-simple/dependencies.md`: a thin
  re-export facade over `Linen.Database.SQLite.Simple.Time.Implementation`
  (module #7), matching upstream's own `Database.SQLite.Simple.Time`, which
  is nothing but `module Database.SQLite.Simple.Time (module Database.
  SQLite.Simple.Time.Implementation) where`.

  `Implementation.lean` already declares its contents under the namespace
  `Database.SQLite.Simple.Time` (this facade's own target namespace, not
  `…Time.Implementation`), the same "already-in-the-right-namespace"
  strategy `Linen.Data.Colour`'s module doc uses for its own facade — so
  importing this module is all that's needed for `parseDay`, `parseUTCTime`,
  `dayToString`, `utcTimeToString`, etc. to be in scope.
-/

import Linen.Database.SQLite.Simple.Time.Implementation
