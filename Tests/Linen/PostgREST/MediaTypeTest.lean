/-
  Tests for `Linen.PostgREST.MediaType`.
-/
import Linen.PostgREST.MediaType

open PostgREST.MediaType

namespace Tests.PostgREST.MediaType

/-! ### `MTVndPlanOption` / `MTVndPlanFormat` `ToString` -/

#guard toString MTVndPlanOption.analyze == "analyze"
#guard toString MTVndPlanOption.wal == "wal"
#guard toString MTVndPlanFormat.json == "json"
#guard toString MTVndPlanFormat.text_ == "text"

/-! ### `toMime` / `ToString` -/

#guard MediaType.applicationJSON.toMime == "application/json"
#guard MediaType.textCSV.toMime == "text/csv"
#guard MediaType.textPlain.toMime == "text/plain"
#guard MediaType.textXML.toMime == "text/xml"
#guard MediaType.applicationOctetStream.toMime == "application/octet-stream"
#guard MediaType.applicationGeoJSON.toMime == "application/geo+json"
#guard MediaType.applicationOpenAPI.toMime == "application/openapi+json"
#guard MediaType.applicationVndSingularJSON.toMime == "application/vnd.pgrst.object+json"
#guard MediaType.applicationVndObject.toMime == "application/vnd.pgrst.object"
#guard (MediaType.applicationVndPlan .json [.analyze]).toMime == "application/vnd.pgrst.plan"
#guard (MediaType.other "application/custom").toMime == "application/custom"
#guard toString MediaType.applicationJSON == "application/json"

/-! ### `BEq` -/

#guard MediaType.applicationJSON == MediaType.applicationJSON
#guard MediaType.applicationJSON != MediaType.textCSV
#guard (MediaType.applicationVndPlan .json [.analyze]) == (MediaType.applicationVndPlan .text_ [])

/-! ### `toContentType` -/

#guard MediaType.applicationJSON.toContentType == "application/json; charset=utf-8"
#guard MediaType.textCSV.toContentType == "text/csv; charset=utf-8"
#guard MediaType.applicationOctetStream.toContentType == "application/octet-stream"
#guard (MediaType.applicationVndPlan .json []).toContentType == "application/vnd.pgrst.plan"

/-! ### `ofMime` -/

#guard MediaType.ofMime "application/json" == MediaType.applicationJSON
#guard MediaType.ofMime "text/csv" == MediaType.textCSV
#guard MediaType.ofMime "text/plain" == MediaType.textPlain
#guard MediaType.ofMime "text/xml" == MediaType.textXML
#guard MediaType.ofMime "application/octet-stream" == MediaType.applicationOctetStream
#guard MediaType.ofMime "application/geo+json" == MediaType.applicationGeoJSON
#guard MediaType.ofMime "application/openapi+json" == MediaType.applicationOpenAPI
#guard MediaType.ofMime "application/vnd.pgrst.object+json" == MediaType.applicationVndSingularJSON
#guard MediaType.ofMime "application/vnd.pgrst.object" == MediaType.applicationVndObject
#guard MediaType.ofMime "application/vnd.pgrst.plan" == MediaType.applicationVndPlan .json []
#guard MediaType.ofMime "application/json; charset=utf-8" == MediaType.applicationJSON
#guard MediaType.ofMime "  application/json  ; charset=utf-8" == MediaType.applicationJSON
#guard MediaType.ofMime "application/x-custom" == MediaType.other "application/x-custom"

/-! ### `Inhabited` -/

#guard (default : MediaType) == MediaType.applicationJSON

/-! ### `isJSON` / `isText` -/

#guard MediaType.applicationJSON.isJSON == true
#guard MediaType.applicationGeoJSON.isJSON == true
#guard MediaType.applicationOpenAPI.isJSON == true
#guard MediaType.applicationVndSingularJSON.isJSON == true
#guard MediaType.applicationVndObject.isJSON == true
#guard (MediaType.applicationVndPlan .json []).isJSON == true
#guard (MediaType.applicationVndPlan .text_ []).isJSON == false
#guard MediaType.textCSV.isJSON == false

#guard MediaType.textCSV.isText == true
#guard MediaType.textPlain.isText == true
#guard MediaType.textXML.isText == true
#guard MediaType.applicationJSON.isText == false

end Tests.PostgREST.MediaType
