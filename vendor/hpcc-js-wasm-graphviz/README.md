# `@hpcc-js/wasm-graphviz` 1.29.1 (vendored)

Graphviz compiled to WebAssembly, as a single ES module with the wasm inlined.
`Linen.Graphics.Graphviz.Html` embeds it (via `include_str`) in the HTML pages
it generates, so they render DOT in the browser with no network access.

| | |
|---|---|
| Package | [`@hpcc-js/wasm-graphviz`](https://www.npmjs.com/package/@hpcc-js/wasm-graphviz) 1.29.1 |
| File | `dist/index.js` of the npm tarball, unmodified |
| Tarball | `https://registry.npmjs.org/@hpcc-js/wasm-graphviz/-/wasm-graphviz-1.29.1.tgz` |
| Tarball integrity (npm) | `sha512-koAQL0wlryEMeBs4/cQC3GB5ziITv66gR9DU/SWYjjup+iQ+Nbjcqxeaef+sffN4F4cBYZ3P31B5z7+cbSABbw==` |
| `index.js` SHA-256 | `b541d7de92d53b2d86f01e4c5fecadb61ee071ad889309171ce9b9383d6cafb6` |
| Size | 819 284 bytes |
| Licence | Apache-2.0 (`LICENSE`, from the package). The Graphviz sources compiled into the wasm are EPL-1.0. |

The file is platform-independent (unlike the per-platform prebuilt binaries
`AGENTS.md` keeps out of git), which is why it is vendored rather than
downloaded at build time.

It is embedded in a `<script type="text/plain">` element and loaded from there
as a module, so it must never contain `</script` or `<!--`;
`Linen.Graphics.Graphviz.Html.bundle_safe` checks this when the module is
compiled, so an update that breaks it fails the build.

## Updating

```sh
v=1.29.1   # the new version
curl -sL "https://registry.npmjs.org/@hpcc-js/wasm-graphviz/-/wasm-graphviz-$v.tgz" -o gv.tgz
# compare `openssl dgst -sha512 -binary gv.tgz | base64` with
#   curl -s https://registry.npmjs.org/@hpcc-js/wasm-graphviz/$v | jq -r .dist.integrity
tar xzf gv.tgz && cp package/dist/index.js package/LICENSE vendor/hpcc-js-wasm-graphviz/
```

Then update this file (version, hashes, size) and `bundleVersion` in
`Linen/Graphics/Graphviz/Html.lean`, and run `lake build Tests`.
