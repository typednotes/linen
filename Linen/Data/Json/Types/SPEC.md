# JSON value types

- **S1. Representation.** An object stores its fields in order and can
  represent repeated keys. Arrays preserve element order. Numbers use
  Lean's `Float` representation.
- **S2. Equality.** Decidable equality is structural. It compares object
  fields and array elements in order and uses Lean's float equality for
  numbers.
- **S3. Field access.** Lookup returns the first matching key. Required
  access reports a missing key. Optional access maps a missing key or
  an explicit JSON null to `none`.
