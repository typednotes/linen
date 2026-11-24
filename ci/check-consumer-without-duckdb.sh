#!/usr/bin/env bash
# Exercise a cold linen checkout with the native development libraries installed.
set -euo pipefail
linen=$(cd "$(dirname "$0")/.." && pwd)
[[ ! -e "$linen/.lake/duckdb" ]] || { echo 'Expected a cold linen DuckDB cache' >&2; exit 1; }
scratch=$(mktemp -d "${TMPDIR:-/tmp}/linen-no-duckdb.XXXXXX")
trap 'rm -rf "$scratch"' EXIT
mkdir "$scratch/bin"
ln -s "$linen" "$scratch/linen"
cp "$linen/lean-toolchain" "$scratch"
cd "$scratch"
unset DUCKDB_PREFIX LINEN_ALLOW_UNSEALED_DUCKDB
cat > bin/curl <<'MOCK'
#!/usr/bin/env bash
printf 'unexpected download: %s\n' "$*" >&2
exit 97
MOCK
chmod +x bin/curl
export PATH="$scratch/bin:$PATH"
cat > lakefile.lean <<'LAKE'
import Lake
open Lake DSL

require linen from "linen"
package noDuckDB
lean_exe noDuckDB where
  root := `Main
LAKE
cat > Main.lean <<'LEAN'
import Linen.Data.Functor

def main : IO Unit := IO.println "linen without DuckDB"
LEAN
lake env true
[[ ! -e "$linen/.lake/duckdb" && ! -e .lake/duckdb ]]
lake build --wfail noDuckDB
[[ "$(./.lake/build/bin/noDuckDB)" == 'linen without DuckDB' ]]
case "$(uname -s)" in
  Linux) deps=$(readelf -d .lake/build/bin/noDuckDB) ;;
  Darwin) deps=$(otool -L .lake/build/bin/noDuckDB) ;;
  *) echo 'Unsupported native linkage check' >&2; exit 1 ;;
esac
if grep -qiE 'lib.*duckdb|liblinen_(postgres|zlib|keychain)' <<< "$deps"; then
  printf 'Unexpected optional native link dependency:\n%s\n' "$deps" >&2
  exit 1
fi
[[ ! -e "$linen/.lake/duckdb" && ! -e .lake/duckdb ]]
echo 'Cold consumer built without downloading or linking DuckDB'
