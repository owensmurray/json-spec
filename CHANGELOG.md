# Changelog

## 2.0.0.0

Breaking release: the specification DSL and tuple codec API were
reworked.

- Add a textual JsonSpec language with parser
  (`Data.JsonSpec.Language.Parser`), Template Haskell quasiquoter
  (`Data.JsonSpec.Language.QQ`), and language documentation
  (`docs/language-spec.md`). Specs can use open `type` and closed
  `module` bindings, `let` frames, and backtick-escaped identifiers
  when a name collides with a keyword.
- Split tuple-based encoding/decoding out of `Data.JsonSpec` into
  `Data.JsonSpec.Codec.Tuple` (`SpecJson`, `TupleEncoding`,
  `TupleDecoding`, `Field`, etc.).
- Rename JSON acronyms to camel case (for example `SpecJSON` →
  `SpecJson`, `toJSONStructure` → `toJsonStructure`,
  `JSONStructure` → `JsonStructure`).
- Replace the old encode/decode class surface with a `Module`-centered
  binding model (`Module`, `BindingSpec`, `JsonModule`, `:=`, `::=`).

## 1.4.0.1

- Relax the `aeson` upper bound to allow 2.3.x.

## 1.4.0.0

- Add `JsonDict` for JSON objects with arbitrary string keys and values that
  all conform to one known specification.

## 1.3.0.2

- Relax upper bounds on `containers` and `time` for current Hackage
  packages.

## 1.3.0.1

- Support GHC 9.14.

## 1.3.0.0

### JsonEither now takes a type-level list

`JsonEither` now accepts a type-level list of specs (`JsonEither '[a, b, c]`)
instead of two arguments (`JsonEither a b`), so sum types with many branches
no longer require a binary tree of nested `JsonEither`s. The structural type
for `JsonEither` is nested `Either`: two or more branches map to
`Either (JStruct env a) (Either (JStruct env b) ...)`; a single branch maps
to `JStruct env spec` (no sum wrapper). Use `Left`/`Right` for construction
and pattern matching.

#### Migration guide

**Specs (example: four alternatives)**

Before:

```
JsonEither (JsonEither (JsonEither specA specB) specC) specD
```

After:

```
JsonEither '[specA, specB, specC, specD]
```

**Patterns/construction**

**Note:** The only difference in the pattern/construction may be how
the `Either`s are nested. The two examples below represent the same
four alternatives with different `Left`/`Right` nesting; the JSON and
types are equivalent.

Before (four branches):

```
Left (Left (Left val))
Left (Left (Right val))
Left (Right val)
Right val
```

After (same nesting with `Left`/`Right`; one branch = no wrapper):

```
Left val
Right (Left val)
Right (Right (Left val))
Right (Right (Right val))
```
