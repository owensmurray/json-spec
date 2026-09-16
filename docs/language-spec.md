# JsonSpec Language

Working language definition for a textual JSON-shape language with
open `type` bindings and closed `module` bindings.

Audience: an engineer (or AI session) implementing a parser and
elaborator. This is precise where the design is settled, and leaves
routine judgment calls to the implementer where noted. It is not an
ISO-grade formal standard.


## 1. Design summary

- A **program** is one top-level closed `module` binding; its value
  (the RHS) is the root shape.
- Specs describe JSON shapes (objects, arrays, sums, primitives,
  refs, etc.).
- `{ "k": spec, … }` is always an **object** shape.
- `let { bindings in spec }` is a **let-expression**; its value is
  `spec` under those bindings.
- `type Name = <spec>` binds a name; the RHS is elaborated **open**
  (outer names + sibling bindings in the same let are visible).
- `module Name = <spec>` binds a name the same way, except the RHS
  is elaborated **closed** (no outer names).
- Aside from closedness, `type` and `module` are the same kind of
  binding: a name denoting one spec value. Not a namespace.
- No `open` keyword. No `M.N` paths into module/type bindings.
- Closed reuse is by copying/inlining a closed RHS (or later
  parameters — out of scope for v1).


## 2. Orientation by example

### 2.1 Program and object shapes

```text
module Person = let {
  type Person = {
    "name": string,
    "age": int,
    "email"?: null string
  }
  in Person
}
```

Root shape is the `Person` object. Bare `{ … }` with string field
names is an object. Optional field: `"email"?:`.

### 2.2 Mutual recursion in one open let

```text
module Graphs = let {
  type Node = {
    "id": string,
    "edges": [Edge]
  }
  type Edge = {
    "from": Node,
    "to": Node
  }
  in Node
}
```

Sibling `type` bindings see each other. Recursion stays inside one
let frame.

### 2.3 Closed `module` vs open `type` / `let`

```text
module Demo = let {
  type Id = string

  type Money = {
    "amount": number,
    "currency": string
  }

  -- Closed: cannot see Id or Money; define what it needs locally.
  module Tax = let {
    type Rate = number
    type Line = {
      "sku": string,
      "qty": int,
      "price": {
        "amount": number,
        "currency": string
      }
    }
    in {
      "rate": Rate,
      "lines": [Line]
    }
  }

  -- Open let: may use Id, Money, Tax from the enclosing frame.
  type Invoice = let {
    type Line = {
      "sku": string,
      "qty": int,
      "price": Money
    }
    in {
      "id": Id,
      "items": [Line],
      "tax": Tax,
      "notes"?: null string
    }
  }

  in Invoice
}
```

After elaboration, `Tax` denotes the object shape from its `in`
clause — not a namespace. Use `"tax": Tax`, never `Tax.Rate`.

### 2.4 Degenerate / trivial module RHS

Any spec may appear on a `module` (or `type`) RHS:

```text
module Id = string
module Flag = bool
module Point = { "x": int, "y": int }
```

`let` is optional. Closedness still applies; for ground specs with
no free names it makes no difference.

```text
let {
  type Tag = string
  type OpenAlias = Tag       -- OK
  module ClosedAlias = Tag   -- ERROR: Tag is outer
  in OpenAlias
}
```

### 2.5 Copying for closed reuse

```text
module Company = let {
  module Shared = let {
    type Id = string
    in Id
  }

  module Billing = let {
    module Shared = let {
      type Id = string
      in Id
    }
    type Invoice = {
      "id": Shared,
      "total": number
    }
    in Invoice
  }

  in Billing
}
```

`Billing` cannot see outer `Shared`. It binds its own closed copy.
`"id": Shared` refers to that local binding (a string shape).

### 2.6 Mental model

| Construct | Role |
|-----------|------|
| `{ "k": … }` | Object shape |
| `let { … in s }` | Let; value is `s` |
| `type N = rhs` | Open binding |
| `module N = rhs` | Closed binding |
| Program | `module Main = <closed spec>` (usually a `let`) |


## 3. Lexical structure (practical)

- UTF-8 source.
- Whitespace: space, tab, newline, CR.
- Comments: `--` to end of line; nested `{- … -}` blocks.
- Identifiers: letter or `_`, then letters, digits, `_`.
  Case-sensitive.
- Keywords (reserved as bare tokens):  
  `module` `type` `let` `in` `either` `dict` `null`  
  `string` `number` `int` `bool` `datetime` `raw`
- Escaped identifiers: `` `name` `` (backticks around an
  identifier body). The body may be a keyword. Use these to bind
  or refer to a name that collides with a keyword
  (e.g. `` type `string` = int ``, then `` `string` `` as a
  ref). Bare keywords remain keywords: bare `string` is still the
  primitive, never a user ref.
- String literals: JSON-style double quotes with the usual JSON
  escapes (enough to accept field names and tag literals).
- Punctuation: `{ } [ ] ( ) , : = | ?` plus backticks
  around escaped identifiers.

Implementer judgment: Unicode letter categories, exact escape
subset, and whether to allow trailing commas — choose something
JSON-familiar and document it in the parser.


## 4. Grammar (practical EBNF)

```text
program     = module_bind ;

(* Top-level is a closed binding; name is often conventional. *)
module_bind = "module" name "=" spec ;

type_bind   = "type"   name "=" spec ;

binding     = type_bind | module_bind ;

(* Bare non-keyword, or backtick-escaped (body may be a keyword). *)
name        = ident | "`" ident_body "`" ;
ident_body  = letter_or_us { letter_or_us | digit } ;

spec        =
    "let" "{" binding { binding } "in" spec "}"
  | "either" either_branch { "|" either_branch }
  | "dict" spec
  | "null" spec
  | primary ;

either_branch = "|"? primary ;

primary     =
    "string" | "number" | "int" | "bool" | "datetime" | "raw"
  | string                          (* constant tag *)
  | name                            (* reference *)
  | object
  | array
  | "(" spec ")" ;

object      = "{" [ field { "," field } ] "}" ;
field       = string "?"? ":" spec ;

array       = "[" spec "]" ;
```

Notes for implementers:

1. **`{` disambiguation:** After `let`, `{` starts a binding block.
   Anywhere a `primary` is expected, `{` starts an **object**.
   Do not use lookahead on bare `{` to invent a let.
2. Binding blocks require at least the `in` clause; zero bindings
   is allowed (`let { in spec }`) if you want; rejecting it is
   also fine — pick one and stick to it.
3. Duplicate binding names in one let, or duplicate field names in
   one object → hard error.
4. Annotations (`@ann`) are deferred; omit in v1 unless needed.
5. `module Name { … in … }` **without** `=` is **not** in the
   grammar. Always use `module Name = …`.
6. **Keyword names:** Bare keywords cannot be binding names or
   refs. Write `` `type` `` / `` `string` `` etc. Bare
   primitives (`string`, …) still denote built-ins; an escaped
   name denotes a user binding of that spelling.


## 5. Elaboration and environments

### 5.1 Environments

An environment is a stack of frames. Each frame maps names to
spec values (with a recorded definition environment — see §5.4).

### 5.2 Open vs closed

**Open** elaboration of a `type` RHS or of the body/bindings of an
open `let` uses: current let frame (siblings) + outer frames.

**Closed** elaboration of a `module` RHS uses **only** an
environment derived from that RHS itself:

- If RHS is `let { bindings in body }`, build one frame from those
  bindings; elaborate bindings and body under `[that_frame]` only.
- If RHS is any other spec, elaborate it under the **empty**
  environment (no free name refs allowed unless the spec has none).

Outer names are never visible inside a `module` RHS.

### 5.3 Let elaboration

For `let { b1 … bn in body }` in an outer env `E` (open context):

1. Create frame `F` from `b1…bn` (mutually recursive).
2. Elaborate each binding RHS and `body` under `F ∷ E`
   (for an open let). Sibling `type`/`module` names in `F` are
   visible to each other.
3. A nested `module` binding’s RHS is still closed (§5.2), even
   when the surrounding let is open.
4. Value of the let is the elaborated `body`.

### 5.4 References and stable definition env

A bare `ident` in spec position looks up the name in the current
environment (innermost frame first).

- Bound to a spec → that reference denotes that spec.
- Unbound → error.
- There are no module namespaces to path into.

When a name denotes a binding, further structural use (encode,
decode, codegen) must use the environment active at the
**definition** of that binding, not the use site (same “stable
environment” idea as today’s `JsonRef` / `Ref` in json-spec).

### 5.5 Recursion rules

- Allowed: mutual recursion among bindings in the **same** let
  frame.
- Forbidden: a closed `module` RHS referring to outer names (so it
  cannot join an outer recursive knot via free refs).
- Practical check: after elaboration, no free names remain; reject
  cycles that would require looking across a closed boundary.

### 5.6 Shape constructors (informal semantics)

| Spec | JSON meaning |
|------|----------------|
| `string` / `number` / `int` / `bool` | usual JSON scalars (`int` = integral number) |
| `datetime` | string in ISO-8601 date-time form |
| `raw` | any JSON value |
| `{ "k": s, "o"?: t }` | object; `k` required, `o` optional |
| `[s]` | array of `s` |
| `dict s` | object with arbitrary string keys, values `s` |
| `null s` | `null` or `s` |
| `either A \| B \| …` | matches exactly one alternative |
| `"tag"` (string literal) | constant string |
| `Name` | denoted binding |
| `let { … in s }` | shape of `s` |
| `module`/`type` | not shapes themselves; only their bound values |

Unknown fields on decode, exact datetime parser, and numeric
ranges: implementer judgment; be consistent and test it.


## 6. Static errors (minimum set)

Reject when:

1. Lex/parse fails.
2. Duplicate binding name in one let, or duplicate field name in
   one object.
3. Unbound reference.
4. A `module` RHS uses an outer name (closedness violation).
5. Mutual recursion that would require crossing a closed module
   boundary (should already surface as 4 or unbound).

Useful diagnostics: span, name, and whether the context was open
or closed.


## 7. Mapping hints to current Haskell json-spec

Non-normative, for implementers bridging to `Data.JsonSpec.Spec`:

| Language | Rough AST / idea |
|----------|------------------|
| object / array / dict / null / either / primitives / tag / datetime / raw | existing `Json*` constructors |
| `let { type A = …; type B = … in s }` | `JsonLet` with open env (today’s let) |
| `module N = rhs` | closed let / former role of `JsonEmbed` around `rhs` |
| `ident` ref | `JsonRef` with stable def env |
| no paths / no `open` | do not add |

A first implementation may: parse source → internal AST matching
this doc → lower closed `module` RHS to “elaborate under empty (or
self-only) env” → reuse existing structure/codec pipelines.


## 8. Explicitly deferred

- File imports / multi-unit projects  
- Parameterized modules (`module Billing = fun (Shared) → …`)  
- Annotation syntax  
- Export lists / namespaces / `M.N` binding paths  
- `module Name { … }` sugar without `=`  
- Richer string/number constraints  

Inlining closed modules covers reuse for v1.


## 9. Implementation checklist

1. Lexer + parser per §3–§4 (`let {` vs object `{` is keyword-driven).
2. Env stack; open let vs closed module RHS per §5.
3. Mutual recursion in one frame; stable def envs for refs.
4. Reject closedness violations and unbound names.
5. Lower to existing json-spec shapes/codecs or an equivalent IR.
6. Tests: Person; Graphs recursion; Demo with nested `Tax`;
   ClosedAlias error; Company/Billing Shared copy;
   trivial `module Id = string`.
