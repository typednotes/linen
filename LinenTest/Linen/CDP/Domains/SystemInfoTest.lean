/-
  Tests for `Linen.CDP.Domains.SystemInfo`.
-/
import Linen.CDP.Domains.SystemInfo

open CDP.Domains.SystemInfo
open CDP.Internal.Utils (Command)
open Data.Json (ToJSON FromJSON)
open Data.Json.Decode (decodeAs)
open Data.Json.Encode (encode)

namespace Tests.CDP.Domains.SystemInfo

#guard decodeAs
    ("{\"vendorId\": 1, \"deviceId\": 2, \"vendorString\": \"v\", \"deviceString\": \"d\", "
      ++ "\"driverVendor\": \"dv\", \"driverVersion\": \"1.0\"}") (α := GPUDevice)
  = .ok { vendorId := 1, deviceId := 2, vendorString := "v", deviceString := "d"
        , driverVendor := "dv", driverVersion := "1.0" }
#guard encode (ToJSON.toJSON
    ({ vendorId := 1, deviceId := 2, subSysId := some 3, revision := some 4, vendorString := "v"
     , deviceString := "d", driverVendor := "dv", driverVersion := "1.0" } : GPUDevice))
  = "{\"vendorId\":1,\"deviceId\":2,\"subSysId\":3,\"revision\":4,\"vendorString\":\"v\"," ++
    "\"deviceString\":\"d\",\"driverVendor\":\"dv\",\"driverVersion\":\"1.0\"}"

#guard decodeAs "{\"width\": 1, \"height\": 2}" (α := Size) = .ok { width := 1, height := 2 }
#guard encode (ToJSON.toJSON ({ width := 1, height := 2 } : Size)) = "{\"width\":1,\"height\":2}"

#guard decodeAs
    "{\"profile\": \"VP9\", \"maxResolution\": {\"width\": 1, \"height\": 2}, \"minResolution\": {\"width\": 3, \"height\": 4}}"
    (α := VideoDecodeAcceleratorCapability)
  = .ok { profile := "VP9", maxResolution := { width := 1, height := 2 }
        , minResolution := { width := 3, height := 4 } }
#guard encode (ToJSON.toJSON
    ({ profile := "VP9", maxResolution := { width := 1, height := 2 }
     , minResolution := { width := 3, height := 4 } } : VideoDecodeAcceleratorCapability))
  = "{\"profile\":\"VP9\",\"maxResolution\":{\"width\":1,\"height\":2}," ++
    "\"minResolution\":{\"width\":3,\"height\":4}}"

#guard decodeAs
    ("{\"profile\": \"H264\", \"maxResolution\": {\"width\": 1, \"height\": 2}, "
      ++ "\"maxFramerateNumerator\": 24, \"maxFramerateDenominator\": 1}")
    (α := VideoEncodeAcceleratorCapability)
  = .ok { profile := "H264", maxResolution := { width := 1, height := 2 }
        , maxFramerateNumerator := 24, maxFramerateDenominator := 1 }
#guard encode (ToJSON.toJSON
    ({ profile := "H264", maxResolution := { width := 1, height := 2 }
     , maxFramerateNumerator := 24, maxFramerateDenominator := 1 } : VideoEncodeAcceleratorCapability))
  = "{\"profile\":\"H264\",\"maxResolution\":{\"width\":1,\"height\":2}," ++
    "\"maxFramerateNumerator\":24,\"maxFramerateDenominator\":1}"

#guard decodeAs "\"yuv420\"" (α := SubsamplingFormat) = .ok .yuv420
#guard decodeAs "\"yuv422\"" (α := SubsamplingFormat) = .ok .yuv422
#guard decodeAs "\"yuv444\"" (α := SubsamplingFormat) = .ok .yuv444
#guard encode (ToJSON.toJSON SubsamplingFormat.yuv420) = "\"yuv420\""

#guard decodeAs "\"jpeg\"" (α := ImageType) = .ok .jpeg
#guard decodeAs "\"webp\"" (α := ImageType) = .ok .webp
#guard decodeAs "\"unknown\"" (α := ImageType) = .ok .unknown
#guard encode (ToJSON.toJSON ImageType.jpeg) = "\"jpeg\""

#guard decodeAs
    ("{\"imageType\": \"jpeg\", \"maxDimensions\": {\"width\": 1, \"height\": 2}, "
      ++ "\"minDimensions\": {\"width\": 3, \"height\": 4}, \"subsamplings\": [\"yuv420\"]}")
    (α := ImageDecodeAcceleratorCapability)
  = .ok { imageType := .jpeg, maxDimensions := { width := 1, height := 2 }
        , minDimensions := { width := 3, height := 4 }, subsamplings := [.yuv420] }
#guard encode (ToJSON.toJSON
    ({ imageType := .jpeg, maxDimensions := { width := 1, height := 2 }
     , minDimensions := { width := 3, height := 4 }, subsamplings := [.yuv420] } :
       ImageDecodeAcceleratorCapability))
  = "{\"imageType\":\"jpeg\",\"maxDimensions\":{\"width\":1,\"height\":2}," ++
    "\"minDimensions\":{\"width\":3,\"height\":4},\"subsamplings\":[\"yuv420\"]}"

#guard decodeAs
    ("{\"devices\": [], \"driverBugWorkarounds\": [\"wa\"], \"videoDecoding\": [], "
      ++ "\"videoEncoding\": [], \"imageDecoding\": []}")
    (α := GPUInfo)
  = .ok { devices := [], driverBugWorkarounds := ["wa"], videoDecoding := []
        , videoEncoding := [], imageDecoding := [] }
#guard decodeAs
    ("{\"devices\": [], \"auxAttributes\": [[\"k\", \"v\"]], \"featureStatus\": [[\"f\", \"s\"]], "
      ++ "\"driverBugWorkarounds\": [], \"videoDecoding\": [], \"videoEncoding\": [], \"imageDecoding\": []}")
    (α := GPUInfo)
  = .ok { devices := [], auxAttributes := some [("k", "v")], featureStatus := some [("f", "s")]
        , driverBugWorkarounds := [], videoDecoding := [], videoEncoding := [], imageDecoding := [] }
#guard encode (ToJSON.toJSON
    ({ devices := [], auxAttributes := some [("k", "v")], driverBugWorkarounds := []
     , videoDecoding := [], videoEncoding := [], imageDecoding := [] } : GPUInfo))
  = "{\"devices\":[],\"auxAttributes\":[[\"k\",\"v\"]],\"driverBugWorkarounds\":[]," ++
    "\"videoDecoding\":[],\"videoEncoding\":[],\"imageDecoding\":[]}"

#guard decodeAs "{\"type\": \"gpu-process\", \"id\": 1, \"cpuTime\": 2.5}" (α := ProcessInfo)
  = .ok { type := "gpu-process", id := 1, cpuTime := 2.5 }
#guard encode (ToJSON.toJSON ({ type := "gpu-process", id := 1, cpuTime := 2.5 } : ProcessInfo))
  = "{\"type\":\"gpu-process\",\"id\":1,\"cpuTime\":2.500000}"

#guard encode (ToJSON.toJSON ({} : PGetInfo)) = "null"
#guard Command.commandName ({} : PGetInfo) = "SystemInfo.getInfo"
#guard decodeAs
    ("{\"gpu\": {\"devices\": [], \"driverBugWorkarounds\": [], \"videoDecoding\": [], "
      ++ "\"videoEncoding\": [], \"imageDecoding\": []}, \"modelName\": \"MacBookPro\", "
      ++ "\"modelVersion\": \"10.1\", \"commandLine\": \"chrome\"}")
    (α := GetInfo)
  = .ok
    { gpu := { devices := [], driverBugWorkarounds := [], videoDecoding := [], videoEncoding := []
              , imageDecoding := [] }
      modelName := "MacBookPro", modelVersion := "10.1", commandLine := "chrome" }

#guard encode (ToJSON.toJSON ({} : PGetProcessInfo)) = "null"
#guard Command.commandName ({} : PGetProcessInfo) = "SystemInfo.getProcessInfo"
#guard decodeAs "{\"processInfo\": [{\"type\": \"gpu-process\", \"id\": 1, \"cpuTime\": 2.5}]}"
    (α := GetProcessInfo)
  = .ok { processInfo := [{ type := "gpu-process", id := 1, cpuTime := 2.5 }] }

end Tests.CDP.Domains.SystemInfo
