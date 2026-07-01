/-
  Tests for `Linen.PostgREST.SchemaCache.Relationship`.
-/
import Linen.PostgREST.SchemaCache.Relationship

open PostgREST.SchemaCache
open PostgREST.SchemaCache.Identifiers

namespace Tests.PostgREST.SchemaCache.Relationship

/-! ### `Cardinality` -/

#guard toString Cardinality.o2m == "O2M"
#guard toString Cardinality.m2o == "M2O"
#guard toString Cardinality.o2o == "O2O"
#guard toString (Cardinality.m2m { qiSchema := "public", qiName := "user_teams" } #[] #[]) ==
  "M2M(public.user_teams)"

#guard Cardinality.o2m == Cardinality.o2m
#guard Cardinality.o2m != Cardinality.m2o
#guard (Cardinality.m2m { qiSchema := "public", qiName := "j" } #[] #[]) ==
  (Cardinality.m2m { qiSchema := "public", qiName := "j" } #[("a", "b")] #[])
#guard (Cardinality.m2m { qiSchema := "public", qiName := "j1" } #[] #[]) !=
  (Cardinality.m2m { qiSchema := "public", qiName := "j2" } #[] #[])

/-! ### `Relationship` -/

def usersToPosts : Relationship :=
  { relTable := { qiSchema := "public", qiName := "users" }
    relForeignTable := { qiSchema := "public", qiName := "posts" }
    relCardinality := .o2m
    relColumns := #[("id", "user_id")] }

#guard usersToPosts.relColumns == #[("id", "user_id")]
#guard usersToPosts.relConstraint == none
#guard usersToPosts.relIsSelf == false
#guard usersToPosts.relIsComputed == false

#guard toString usersToPosts == "public.users O2M public.posts"

#guard usersToPosts == usersToPosts
#guard usersToPosts != { usersToPosts with relCardinality := .m2o }
#guard usersToPosts == { usersToPosts with relColumns := #[] }

/-! ### `localColumns` / `foreignColumns` -/

#guard usersToPosts.localColumns == #["id"]
#guard usersToPosts.foreignColumns == #["user_id"]

def multiCol : Relationship :=
  { relTable := { qiSchema := "public", qiName := "a" }
    relForeignTable := { qiSchema := "public", qiName := "b" }
    relCardinality := .m2o
    relColumns := #[("x1", "y1"), ("x2", "y2")] }

#guard multiCol.localColumns == #["x1", "x2"]
#guard multiCol.foreignColumns == #["y1", "y2"]

end Tests.PostgREST.SchemaCache.Relationship
