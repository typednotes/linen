/-
  Tests for `Linen.PostgREST.SchemaCache.Identifiers`.
-/
import Linen.PostgREST.SchemaCache.Identifiers

open PostgREST.SchemaCache.Identifiers

namespace Tests.PostgREST.SchemaCache.Identifiers

/-! ### `QualifiedIdentifier` -/

#guard ({ qiSchema := "public", qiName := "users" } : QualifiedIdentifier) ==
  ({ qiSchema := "public", qiName := "users" } : QualifiedIdentifier)
#guard ({ qiSchema := "public", qiName := "users" } : QualifiedIdentifier) !=
  ({ qiSchema := "public", qiName := "posts" } : QualifiedIdentifier)

#guard compare ({ qiSchema := "a", qiName := "z" } : QualifiedIdentifier)
  ({ qiSchema := "b", qiName := "a" } : QualifiedIdentifier) == .lt
#guard compare ({ qiSchema := "a", qiName := "x" } : QualifiedIdentifier)
  ({ qiSchema := "a", qiName := "y" } : QualifiedIdentifier) == .lt
#guard compare ({ qiSchema := "a", qiName := "x" } : QualifiedIdentifier)
  ({ qiSchema := "a", qiName := "x" } : QualifiedIdentifier) == .eq

#guard toString ({ qiSchema := "public", qiName := "users" } : QualifiedIdentifier) == "public.users"
#guard toString ({ qiSchema := "", qiName := "users" } : QualifiedIdentifier) == "users"

/-! ### `escapeIdent` / `quoteIdent` / `quoteQi` -/

#guard escapeIdent "users" == "users"
#guard escapeIdent "a\"b" == "a\"\"b"
#guard escapeIdent "" == ""

#guard quoteIdent "users" == "\"users\""
#guard quoteIdent "a\"b" == "\"a\"\"b\""

#guard quoteQi ({ qiSchema := "public", qiName := "users" } : QualifiedIdentifier) ==
  "\"public\".\"users\""

/-! ### `toQi` -/

#guard toQi "public.users" == ({ qiSchema := "public", qiName := "users" } : QualifiedIdentifier)
#guard toQi "users" == ({ qiSchema := "public", qiName := "users" } : QualifiedIdentifier)

/-! ### `anyElement` / `isAnyElement` -/

#guard anyElement.isAnyElement == true
#guard ({ qiSchema := "public", qiName := "users" } : QualifiedIdentifier).isAnyElement == false

/-! ### `RelIdentifier` -/

#guard RelIdentifier.relId ({ qiSchema := "public", qiName := "users" } : QualifiedIdentifier) ==
  RelIdentifier.relId ({ qiSchema := "public", qiName := "users" } : QualifiedIdentifier)
#guard RelIdentifier.relAnyElement == RelIdentifier.relAnyElement
#guard RelIdentifier.relAnyElement !=
  RelIdentifier.relId ({ qiSchema := "public", qiName := "users" } : QualifiedIdentifier)

/-! ### `quoteIdent_startsWith_quote` -/

example (s : String) : (quoteIdent s).startsWith "\"" = true :=
  quoteIdent_startsWith_quote s

end Tests.PostgREST.SchemaCache.Identifiers
