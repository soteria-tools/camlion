# camlion

An OCaml library for parsing [Amazon Ion](https://amazon-ion.github.io/ion-docs/) data (Ion 1.0).

## Status

- Text format (`.ion`): fully supported
- Binary format (`.10n`): fully supported
- 783/783 Ion conformance tests pass

## Installation

```sh
opam install camlion
```

Or via the local opam file:

```sh
opam install .
```

## Usage

### Parsing text Ion

```ocaml
(* From a string *)
let values = Ion.Text.of_string "{ name: \"Alice\", age: 30 }" in
...

(* From a channel *)
let ic = open_in "data.ion" in
let values = Ion.Text.of_channel ic in
...
```

### Parsing binary Ion

```ocaml
let ic = open_in_bin "data.10n" in
let values = Ion.Binary.of_channel ic in
...
```

Both return `Ion.ion_value list`.

## AST

```ocaml
type ion_value = {
  annotations : string list;
  value : datum;
}

and datum =
  | Null of ion_type
  | Bool of bool
  | Int of Z.t
  | Float of float
  | Decimal of string
  | Timestamp of string
  | Symbol of string
  | SymbolId of int
  | String of string
  | Clob of string
  | Blob of string
  | List of ion_value list
  | Sexp of ion_value list
  | Struct of (string * ion_value) list
```

## Not Yet Implemented

- UTF-16 / UTF-32 BOM handling (`utf16.ion`, `utf32.ion`)
- Full external symbol table catalog resolution (unknown import SIDs appear as `"$N"`)
- Ion data model equivalence / comparison
- Serialisation (writing Ion)

## Building and Testing

```sh
dune build
dune test
dune exec test/test_ion_suite.exe
```
