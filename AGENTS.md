# camlion — Agent Session Notes

## Project Overview

OCaml library for parsing Amazon Ion (text format). No binary format support.

- **Build**: `dune build`
- **Test suite**: `dune exec test/test_ion_suite.exe`
- **Parser**: menhir (`text_parser.mly`) + ocamllex (`text_lexer.mll`)
- **ion-tests**: git submodule at `ion-tests/iontestdata/` — `good/` files must parse successfully, `bad/` files must raise an exception

## Current Status

**600/600 tests pass.** (as of last session)

Skipped tests (filtered out in `test/test_ion_suite.ml`):
- `*.10n` — binary format, not implemented
- `utf16.ion`, `utf32.ion` — require BOM-based transcoding, not implemented

## Key Files

```
lib/
  ion.ml            — AST types (datum variants, ion_value, ion_type)
  text_lexer.mll    — ocamllex lexer
  text_parser.mly   — menhir parser
  text.ml           — entry points: of_string, of_channel; post-parse validation
test/
  test_ion_suite.ml — conformance test runner
  test_camlion.ml — unit tests
```

## AST (ion.ml)

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
  | Symbol of string        (* identifier or quoted symbol, e.g. foo or 'hello world' *)
  | SymbolId of int         (* bare $N SID reference, e.g. $3 — must be range-checked *)
  | String of string
  | Clob of string
  | Blob of string
  | List of ion_value list
  | Sexp of ion_value list
  | Struct of (string * ion_value) list
```

**Important**: `Symbol "foo"` comes from both bare identifiers and quoted symbols (`'foo'`).
`SymbolId n` comes only from bare `$N` (where N is all digits), emitted as `SYMBOL_ID n` token.
This distinction matters for symbol ID range validation.

## Validation in text.ml (post-parse)

`of_string` / `of_channel` call these checks after parsing:

1. **`check_top_level_version_markers`** — rejects `$ion_N_M` where N≠1 or M≠0 (unannotated, top-level)

2. **`check_local_symbol_tables`** — for each top-level value annotated with `$ion_symbol_table`:
   - Rejects duplicate `imports` fields in the struct
   - Rejects duplicate `symbols` fields in the struct
   - Rejects `max_id` in any import struct that is null, non-integer, or negative

3. **`check_symbol_id_ranges`** — walks top-level values tracking symbol table context:
   - System symbol table defines $0..$9 (10 symbols)
   - A `$ion_symbol_table::{ imports:[...], symbols:[...] }` struct updates the count
   - Raises on any `SymbolId n` value, `$N` annotation, or `$N` field name that is out of range

## Key Lexer Details

- **`check_numeric_terminator`**: called after any numeric literal or `null.TYPE`; peeks at next char and raises `Lexer_error` if it is not a valid delimiter (whitespace, `,`, `]`, `}`, `)`, `{`, `[`, `(`, `"`, `'`, `:`). This catches `12a`, `0xfg`, `007`, `1_`, etc.
- **`$N` SID rule**: `'$' ['0'-'9']+` fires before the general `id_start id_rest*` rule and emits `SYMBOL_ID n`. Also calls `check_numeric_terminator`.
- **Surrogate pairs**: `\uD800-\uDBFF` must be followed by `\uDC00-\uDFFF`; lone surrogates raise errors.
- **Control chars in strings**: raw bytes `\x00-\x08`, `\x0E-\x1F` raise errors in short and long strings.
- **Non-ASCII in CLOBs**: bytes `\x80-\xFF` and control chars raise errors.
- **Blob validation**: `validate_blob` checks length mod 4 = 0, `=` only at end, at most 2 padding chars.
- **Timestamp validation**: `validate_timestamp_date` checks leap years and days-per-month.
- **Typed null catch-all**: `"null." id_start id_rest*` raises `Lexer_error` for unknown types.

## Parser Details

- Trailing commas in lists and structs are handled by explicit alternatives in `ion_list` and `ion_struct` rules (not in the recursive element rules, to avoid double-comma acceptance).
- `SYMBOL_ID` token is valid in: value entities, sexp entities, annotations (`:: value`), and field names.
- Long strings (`'''...'''`) are concatenated by the parser.

## What's Not Implemented

- Binary format (`.10n`)
- UTF-16 / UTF-32 BOM handling
- Full Ion symbol table resolution (imports from external catalogs)
- Ion data model equivalence / comparison
- Writing / serialisation
