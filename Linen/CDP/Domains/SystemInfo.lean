/-
  Linen.CDP.Domains.SystemInfo — the `SystemInfo` CDP domain

  Ports `CDP.Domains.SystemInfo` (see `docs/imports/cdp/dependencies.md`);
  defines methods and events for querying low-level system information. Naming
  conventions as in `CDP.Domains.CacheStorage`'s docstring.
-/
import Linen.CDP.Internal.Utils

namespace CDP.Domains.SystemInfo

open Data.Json (Value ToJSON FromJSON)
open CDP.Internal.Utils (Command)

/-- Describes a single graphics processor (GPU). -/
structure GPUDevice where
  /-- PCI ID of the GPU vendor, if available; 0 otherwise. -/
  vendorId : Float
  /-- PCI ID of the GPU device, if available; 0 otherwise. -/
  deviceId : Float
  /-- Sub sys ID of the GPU, only available on Windows. -/
  subSysId : Option Float := none
  /-- Revision of the GPU, only available on Windows. -/
  revision : Option Float := none
  /-- String description of the GPU vendor, if the PCI ID is not available. -/
  vendorString : String
  /-- String description of the GPU device, if the PCI ID is not available. -/
  deviceString : String
  /-- String description of the GPU driver vendor. -/
  driverVendor : String
  /-- String description of the GPU driver version. -/
  driverVersion : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON GPUDevice where
  parseJSON v := do
    .ok
      { vendorId := ← Value.getField v "vendorId" >>= FromJSON.parseJSON
        deviceId := ← Value.getField v "deviceId" >>= FromJSON.parseJSON
        subSysId := ← (← Value.getFieldOpt v "subSysId").mapM FromJSON.parseJSON
        revision := ← (← Value.getFieldOpt v "revision").mapM FromJSON.parseJSON
        vendorString := ← Value.getField v "vendorString" >>= FromJSON.parseJSON
        deviceString := ← Value.getField v "deviceString" >>= FromJSON.parseJSON
        driverVendor := ← Value.getField v "driverVendor" >>= FromJSON.parseJSON
        driverVersion := ← Value.getField v "driverVersion" >>= FromJSON.parseJSON }

instance : ToJSON GPUDevice where
  toJSON p := Data.Json.object <|
    [ ("vendorId", ToJSON.toJSON p.vendorId), ("deviceId", ToJSON.toJSON p.deviceId) ]
    ++ (p.subSysId.map fun v => ("subSysId", ToJSON.toJSON v)).toList
    ++ (p.revision.map fun v => ("revision", ToJSON.toJSON v)).toList
    ++ [ ("vendorString", ToJSON.toJSON p.vendorString), ("deviceString", ToJSON.toJSON p.deviceString)
       , ("driverVendor", ToJSON.toJSON p.driverVendor), ("driverVersion", ToJSON.toJSON p.driverVersion) ]

/-- Describes the width and height dimensions of an entity. -/
structure Size where
  /-- Width in pixels. -/
  width : Int
  /-- Height in pixels. -/
  height : Int
  deriving Repr, BEq, DecidableEq

instance : FromJSON Size where
  parseJSON v := do
    .ok
      { width := ← Value.getField v "width" >>= FromJSON.parseJSON
        height := ← Value.getField v "height" >>= FromJSON.parseJSON }

instance : ToJSON Size where
  toJSON p := Data.Json.object [("width", ToJSON.toJSON p.width), ("height", ToJSON.toJSON p.height)]

/-- Describes a supported video decoding profile with its associated minimum
    and maximum resolutions. -/
structure VideoDecodeAcceleratorCapability where
  /-- Video codec profile that is supported, e.g. VP9 Profile 2. -/
  profile : String
  /-- Maximum video dimensions in pixels supported for this `profile`. -/
  maxResolution : Size
  /-- Minimum video dimensions in pixels supported for this `profile`. -/
  minResolution : Size
  deriving Repr, BEq, DecidableEq

instance : FromJSON VideoDecodeAcceleratorCapability where
  parseJSON v := do
    .ok
      { profile := ← Value.getField v "profile" >>= FromJSON.parseJSON
        maxResolution := ← Value.getField v "maxResolution" >>= FromJSON.parseJSON
        minResolution := ← Value.getField v "minResolution" >>= FromJSON.parseJSON }

instance : ToJSON VideoDecodeAcceleratorCapability where
  toJSON p := Data.Json.object
    [ ("profile", ToJSON.toJSON p.profile), ("maxResolution", ToJSON.toJSON p.maxResolution)
    , ("minResolution", ToJSON.toJSON p.minResolution) ]

/-- Describes a supported video encoding profile with its associated maximum
    resolution and maximum framerate. -/
structure VideoEncodeAcceleratorCapability where
  /-- Video codec profile that is supported, e.g. H264 Main. -/
  profile : String
  /-- Maximum video dimensions in pixels supported for this `profile`. -/
  maxResolution : Size
  /-- Maximum encoding framerate in frames per second supported for this
      `profile`, as fraction's numerator ... -/
  maxFramerateNumerator : Int
  /-- ... and denominator, e.g. 24/1 fps, 24000/1001 fps, etc. -/
  maxFramerateDenominator : Int
  deriving Repr, BEq, DecidableEq

instance : FromJSON VideoEncodeAcceleratorCapability where
  parseJSON v := do
    .ok
      { profile := ← Value.getField v "profile" >>= FromJSON.parseJSON
        maxResolution := ← Value.getField v "maxResolution" >>= FromJSON.parseJSON
        maxFramerateNumerator := ← Value.getField v "maxFramerateNumerator" >>= FromJSON.parseJSON
        maxFramerateDenominator := ← Value.getField v "maxFramerateDenominator" >>= FromJSON.parseJSON }

instance : ToJSON VideoEncodeAcceleratorCapability where
  toJSON p := Data.Json.object
    [ ("profile", ToJSON.toJSON p.profile), ("maxResolution", ToJSON.toJSON p.maxResolution)
    , ("maxFramerateNumerator", ToJSON.toJSON p.maxFramerateNumerator)
    , ("maxFramerateDenominator", ToJSON.toJSON p.maxFramerateDenominator) ]

/-- YUV subsampling type of the pixels of a given image. -/
inductive SubsamplingFormat where
  | yuv420 | yuv422 | yuv444
  deriving Repr, BEq, DecidableEq

instance : FromJSON SubsamplingFormat where
  parseJSON
    | .string "yuv420" => .ok .yuv420
    | .string "yuv422" => .ok .yuv422
    | .string "yuv444" => .ok .yuv444
    | v => .error s!"failed to parse SubsamplingFormat: {repr v}"

instance : ToJSON SubsamplingFormat where
  toJSON
    | .yuv420 => .string "yuv420"
    | .yuv422 => .string "yuv422"
    | .yuv444 => .string "yuv444"

/-- Image format of a given image. -/
inductive ImageType where
  | jpeg | webp | unknown
  deriving Repr, BEq, DecidableEq

instance : FromJSON ImageType where
  parseJSON
    | .string "jpeg" => .ok .jpeg
    | .string "webp" => .ok .webp
    | .string "unknown" => .ok .unknown
    | v => .error s!"failed to parse ImageType: {repr v}"

instance : ToJSON ImageType where
  toJSON
    | .jpeg => .string "jpeg"
    | .webp => .string "webp"
    | .unknown => .string "unknown"

/-- Describes a supported image decoding profile with its associated minimum
    and maximum resolutions and subsampling. -/
structure ImageDecodeAcceleratorCapability where
  /-- Image coded, e.g. Jpeg. -/
  imageType : ImageType
  /-- Maximum supported dimensions of the image in pixels. -/
  maxDimensions : Size
  /-- Minimum supported dimensions of the image in pixels. -/
  minDimensions : Size
  /-- Optional array of supported subsampling formats, e.g. 4:2:0, if known. -/
  subsamplings : List SubsamplingFormat
  deriving Repr, BEq, DecidableEq

instance : FromJSON ImageDecodeAcceleratorCapability where
  parseJSON v := do
    .ok
      { imageType := ← Value.getField v "imageType" >>= FromJSON.parseJSON
        maxDimensions := ← Value.getField v "maxDimensions" >>= FromJSON.parseJSON
        minDimensions := ← Value.getField v "minDimensions" >>= FromJSON.parseJSON
        subsamplings := ← Value.getField v "subsamplings" >>= FromJSON.parseJSON }

instance : ToJSON ImageDecodeAcceleratorCapability where
  toJSON p := Data.Json.object
    [ ("imageType", ToJSON.toJSON p.imageType), ("maxDimensions", ToJSON.toJSON p.maxDimensions)
    , ("minDimensions", ToJSON.toJSON p.minDimensions), ("subsamplings", ToJSON.toJSON p.subsamplings) ]

/-- Provides information about the GPU(s) on the system. -/
structure GPUInfo where
  /-- The graphics devices on the system. Element 0 is the primary GPU. -/
  devices : List GPUDevice
  /-- An optional dictionary of additional GPU related attributes, as
      key/value pairs (encoded as 2-element arrays, matching upstream's
      `[(Text, Text)]` `ToJSON`/`FromJSON` instances). -/
  auxAttributes : Option (List (String × String)) := none
  /-- An optional dictionary of graphics features and their status. -/
  featureStatus : Option (List (String × String)) := none
  /-- An optional array of GPU driver bug workarounds. -/
  driverBugWorkarounds : List String
  /-- Supported accelerated video decoding capabilities. -/
  videoDecoding : List VideoDecodeAcceleratorCapability
  /-- Supported accelerated video encoding capabilities. -/
  videoEncoding : List VideoEncodeAcceleratorCapability
  /-- Supported accelerated image decoding capabilities. -/
  imageDecoding : List ImageDecodeAcceleratorCapability
  deriving Repr, BEq, DecidableEq

instance : FromJSON GPUInfo where
  parseJSON v := do
    .ok
      { devices := ← Value.getField v "devices" >>= FromJSON.parseJSON
        auxAttributes := ← (← Value.getFieldOpt v "auxAttributes").mapM FromJSON.parseJSON
        featureStatus := ← (← Value.getFieldOpt v "featureStatus").mapM FromJSON.parseJSON
        driverBugWorkarounds := ← Value.getField v "driverBugWorkarounds" >>= FromJSON.parseJSON
        videoDecoding := ← Value.getField v "videoDecoding" >>= FromJSON.parseJSON
        videoEncoding := ← Value.getField v "videoEncoding" >>= FromJSON.parseJSON
        imageDecoding := ← Value.getField v "imageDecoding" >>= FromJSON.parseJSON }

instance : ToJSON GPUInfo where
  toJSON p := Data.Json.object <|
    [("devices", ToJSON.toJSON p.devices)]
    ++ (p.auxAttributes.map fun v => ("auxAttributes", ToJSON.toJSON v)).toList
    ++ (p.featureStatus.map fun v => ("featureStatus", ToJSON.toJSON v)).toList
    ++ [ ("driverBugWorkarounds", ToJSON.toJSON p.driverBugWorkarounds)
       , ("videoDecoding", ToJSON.toJSON p.videoDecoding)
       , ("videoEncoding", ToJSON.toJSON p.videoEncoding)
       , ("imageDecoding", ToJSON.toJSON p.imageDecoding) ]

/-- Represents process info. -/
structure ProcessInfo where
  /-- Specifies process type. -/
  type : String
  /-- Specifies process id. -/
  id : Int
  /-- Specifies cumulative CPU usage in seconds across all threads of the
      process since the process start. -/
  cpuTime : Float
  deriving Repr, BEq, DecidableEq

instance : FromJSON ProcessInfo where
  parseJSON v := do
    .ok
      { type := ← Value.getField v "type" >>= FromJSON.parseJSON
        id := ← Value.getField v "id" >>= FromJSON.parseJSON
        cpuTime := ← Value.getField v "cpuTime" >>= FromJSON.parseJSON }

instance : ToJSON ProcessInfo where
  toJSON p := Data.Json.object
    [("type", ToJSON.toJSON p.type), ("id", ToJSON.toJSON p.id), ("cpuTime", ToJSON.toJSON p.cpuTime)]

/-- Parameters of the `SystemInfo.getInfo` command: returns information about
    the system. -/
structure PGetInfo where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetInfo where toJSON _ := .null

/-- Response of the `SystemInfo.getInfo` command. -/
structure GetInfo where
  /-- Information about the GPUs on the system. -/
  gpu : GPUInfo
  /-- A platform-dependent description of the model of the machine. On Mac OS,
      this is, for example, `MacBookPro`. Will be the empty string if not
      supported. -/
  modelName : String
  /-- A platform-dependent description of the version of the machine. On Mac
      OS, this is, for example, `10.1`. Will be the empty string if not
      supported. -/
  modelVersion : String
  /-- The command line string used to launch the browser. Will be the empty
      string if not supported. -/
  commandLine : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON GetInfo where
  parseJSON v := do
    .ok
      { gpu := ← Value.getField v "gpu" >>= FromJSON.parseJSON
        modelName := ← Value.getField v "modelName" >>= FromJSON.parseJSON
        modelVersion := ← Value.getField v "modelVersion" >>= FromJSON.parseJSON
        commandLine := ← Value.getField v "commandLine" >>= FromJSON.parseJSON }

instance : Command PGetInfo where
  Response := GetInfo
  commandName _ := "SystemInfo.getInfo"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `SystemInfo.getProcessInfo` command: returns information
    about all running processes. -/
structure PGetProcessInfo where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetProcessInfo where toJSON _ := .null

/-- Response of the `SystemInfo.getProcessInfo` command. -/
structure GetProcessInfo where
  /-- An array of process info blocks. -/
  processInfo : List ProcessInfo
  deriving Repr, BEq, DecidableEq

instance : FromJSON GetProcessInfo where
  parseJSON v := do .ok { processInfo := ← Value.getField v "processInfo" >>= FromJSON.parseJSON }

instance : Command PGetProcessInfo where
  Response := GetProcessInfo
  commandName _ := "SystemInfo.getProcessInfo"
  decodeResponse := FromJSON.parseJSON

end CDP.Domains.SystemInfo
