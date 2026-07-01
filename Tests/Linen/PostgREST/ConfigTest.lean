/-
  Tests for `Linen.PostgREST.Config`.
-/
import Linen.PostgREST.Config

open PostgREST.Config

namespace Tests.PostgREST.Config

/-! ### `LogLevel` -/

#guard toString LogLevel.crit == "crit"
#guard toString LogLevel.error == "error"
#guard toString LogLevel.warn == "warn"
#guard toString LogLevel.info == "info"
#guard toString LogLevel.debug == "debug"

#guard LogLevel.parse "crit" == some .crit
#guard LogLevel.parse "DEBUG" == some .debug
#guard LogLevel.parse "nonsense" == none

example : ∀ l : LogLevel, LogLevel.parse (toString l) = some l :=
  LogLevel.parse_toString_roundtrip

/-! ### `OpenAPIMode` -/

#guard toString OpenAPIMode.followPriv == "follow-privileges"
#guard toString OpenAPIMode.ignorePriv == "ignore-privileges"
#guard toString OpenAPIMode.disabled == "disabled"
#guard toString OpenAPIMode.securityNone == "security-none"

#guard OpenAPIMode.parse "follow-privileges" == some .followPriv
#guard OpenAPIMode.parse "SECURITY-NONE" == some .securityNone
#guard OpenAPIMode.parse "nonsense" == none

example : ∀ m : OpenAPIMode, OpenAPIMode.parse (toString m) = some m :=
  OpenAPIMode.parse_toString_roundtrip

/-! ### `Port` -/

#guard (mkPort 3000).val == 3000
#guard toString (mkPort 80) == "80"
#guard mkPort 3000 == mkPort 3000
#guard mkPort 80 != mkPort 443

/-! ### `AppConfig` defaults -/

#guard AppConfig.default.configDbUri == "postgresql://localhost/postgres"
#guard AppConfig.default.configDbSchemas == ["public"]
#guard AppConfig.default.configDbAnonRole == "anon"
#guard AppConfig.default.configDbPoolSize == 10
#guard AppConfig.default.configServerPort == mkPort 3000
#guard AppConfig.default.configLogLevel == .error
#guard AppConfig.default.configOpenApiMode == .followPriv

/-! ### `AppConfig` queries -/

#guard AppConfig.default.hasJwtSecret == false
#guard { AppConfig.default with configJwtSecret := some "s3cr3t" }.hasJwtSecret == true

#guard AppConfig.default.hasAdminServer == false
#guard { AppConfig.default with configAdminServerPort := some (mkPort 8080) }.hasAdminServer == true

#guard AppConfig.default.hasRootSpec == false
#guard { AppConfig.default with
    configDbRootSpec := some { qiSchema := "public", qiName := "root" } }.hasRootSpec == true

#guard AppConfig.default.hasPreRequest == false

#guard AppConfig.default.mainSchema == "public"
#guard ({ configDbUri := "postgresql://localhost/postgres", configDbSchemas := ["a", "b"], configDbSchemas_nonempty := by decide } : AppConfig).mainSchema == "a"

end Tests.PostgREST.Config
