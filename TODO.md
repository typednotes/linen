# TODO

## Portable Lean cells

- [ ] Let Lode export Lun's compiler-attested workers with their semantic
  release names. Exercise the generated addition cell through both typed
  transports.
- [ ] Have Lun adopt existing signed variants across independent builds.
  Resolve requested versions and transitive dependencies against the
  project's authenticated catalog when checking a DAG. Persist the lock pin
  in the plan and admit the locked workers before executing placements.
  Adapt Lun's codecs to decode dependent results against the actual input.
- [ ] Store and retain named releases, immutable variants, and their
  authoring records in the authorized Typednotes project. Enforce release
  uniqueness when persisting catalog entries and coordinate trust and
  retention with Lun.

The common format and typed worker clients are documented in
[Precompiled Lean cells](docs/cell-packages.md).
