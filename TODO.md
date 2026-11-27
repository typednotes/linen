# TODO

## Portable Lean cells

- [ ] Adopt Linen's version 2 format in Lode and Lun after updating their
  Linen pins. Migrate Lode's authoring metadata into `System.Cell.Revision`
  and exercise its generated addition cell through both typed transports.
- [ ] Have Lun's checked builds attest and publish a signed worker for each
  admitted cell revision and compatible target. Configure trusted compiler
  keys and check the platform ceiling and codecs before adoption.
- [ ] Pin the admitted manifest in each checked DAG placement. Bind fresh
  invocation authority in Lun's executor and decode results against the
  actual input for dependent contracts.
- [ ] Store and retain the immutable variants and their authoring records
  in Typednotes, scoped to the authorized project.

The common format and typed worker clients are documented in
[Precompiled Lean cells](docs/cell-packages.md).
