# Changelog

All notable changes to `linen` are documented here, one entry per released
version (see `version` in `lakefile.lean`). Dates are UTC, in `YYYY-MM-DD`
format.

## [0.13.0] — 2026-09-05

- `Linen.Crypto.JOSE` now supports RSA **signing**, not just verification.
- Fixed the FFI shims that call `snprintf` by including `<stdio.h>`, which
  some platforms' headers don't pull in transitively.
- Updated the docs on using FFI-backed modules.

## [0.12.0] — 2026-08-23

- Added `Linen.Data.Ini` and `Linen.Data.Yaml`: INI and YAML parsing/encoding.
- Added `Linen.Data.Float`.
- Bumped the Lean toolchain to `v4.33.1`.

## [0.11.0] — 2026-08-23

- Added `Linen.Crypto.SigV4` (AWS Signature Version 4).
- Added `Linen.Data.Hex` and `Linen.Data.Time.ISO8601`.
- Added `Linen.Network.HTTP.Client.Retry`.
- Added `Linen.Text.XML`.

## [0.10.0] — 2026-07-15

- Bumped the Lean toolchain to `v4.32.0`.

## [0.9.0] — 2026-07-15

- Version bump; no module changes.

## [0.8.0] — 2026-07-15

- Added `Linen.Control.Lens` and its `profunctors`/`indexed-traversable`
  prerequisites: a full `lens`-style profunctor-optics library
  (`Lens`/`Prism`/`Iso`/`Traversal`/`Fold`/`Getter`/`Setter`/`Review`, indexed
  variants, and `Lens` instances scattered across existing `Data.*` modules).
- Added `Linen.Data.Stream` / `Linen.Data.StreamK`: `streamly`-style fused
  streaming, plus `Data.Fold`, `Data.Scanl`, `Data.Unfold`, `Data.Parser`,
  `Data.Producer`, and `Data.Refold`.
- Added `Linen.Database.Redis`: a Redis client (cluster support, pub/sub,
  transactions, sentinel, connection pooling).
- Added `Linen.Text.Pandoc` and `Linen.Text.DocLayout`: document
  readers/writers (HTML, Markdown, Native) built on a layout engine.
- Added `Linen.Data.Array.Unboxed`, `Linen.Data.MutArray`,
  `Linen.Data.MutByteArray`, `Linen.Data.Unbox`.
- Added strict variants `Linen.Data.Either.Strict`, `Linen.Data.Maybe.Strict`,
  `Linen.Data.Tuple.Strict`.
- Added `Linen.System.IO`.

## [0.7.0] — 2026-07-13

- Added `Linen.Time.Calendar.{CalendarDiffDays,Easter,Julian,Month,Quarter}`,
  `Linen.Time.CalendarDiffTime`, `Linen.Time.Clock.TAI`, and
  `Linen.Time.UniversalTime`.

## [0.6.0] — 2026-07-13

- Added `Linen.Network.OAuth2`: OAuth 2.0 client (authorization-code, client-
  credentials, device-authorization, JWT-bearer, and resource-owner-password
  grants, plus PKCE).
- Added `Linen.Crypto.SecureRandom` and `Linen.Crypto.SHA256`.
- Added `Linen.Network.HTTP.Client.Contrib`.

## [0.5.0] — 2026-07-12

- Internal `lakefile.lean` cleanup; no module changes.

## [0.4.0] — 2026-07-12

- Added `Linen.Codec.Picture`: a JuicyPixels-style image codec (Bitmap, GIF,
  HDR, JPEG, PNG, TGA, TIFF) plus `Linen.Graphics.Image`, a `hip`-style image
  processing library (color spaces, convolution, geometric transforms,
  Fourier, Hough transform, interpolation, noise).
- Added `Linen.Database.DuckDB` (FFI bindings and a `sqlite-simple`-style
  high-level API) and `Linen.Database.SQLite` (FFI bindings and simple API).
- Added `Linen.Data.Time.Calendar` and `Linen.Data.Time.LocalTime`.

## [0.3.0] — 2026-07-11

- Added `Linen.Codec.Picture.{BitWriter,InternalHelper,Metadata,Types,
  VectorByteConversion}` (shared JuicyPixels internals ahead of the format
  decoders landing in 0.4.0).
- Added `Linen.Data.Array.Shaped`: a `repa`-style shape-indexed array library
  (delayed/manifest/partitioned/cursored representations, stencils,
  reductions, index-space operators).
- Added `Linen.Data.Colour`: a `colour`-style color library (RGB, sRGB, CIE,
  HSL/HSV color spaces, named colors).
- Added `Linen.Graphics.Netpbm`.

## [0.2.0] — 2026-07-11

- Added `Linen.CDP` (Chrome DevTools Protocol).
- Added `Linen.Data.PDF`: a `pdf-toolbox`-style PDF reader (document/page
  tree, content-stream operators, font descriptors and encodings, xref/
  object parsing, FlateDecode).
- Added `Linen.Network.WebApp`, `Linen.Network.WebSockets`, and
  `Linen.Network.URI`: a WAI/Warp-style web application interface, HTTP
  server (TLS, HTTP/2 via QUIC, gzip, static file serving) with its
  middleware stack, and a WebSockets client/server.
- Added `Linen.Web.Css` and `Linen.Web.Html`.
- Added `Linen.Crypto.AES`, `Linen.Crypto.MD5`, `Linen.Crypto.RC4`, and
  `Linen.Crypto.Zlib.FFI`.
- Added `Linen.System.Console.Ansi` and `Linen.System.Keychain`.
- Added `Linen.Data.Word8`.

## [0.1.0] — 2026-07-02

- First tagged release.
