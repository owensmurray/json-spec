# Changelog

## Unreleased

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
