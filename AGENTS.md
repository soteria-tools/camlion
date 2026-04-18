# camlion — Agent Session Notes

## Project Overview

OCaml library for parsing Amazon Ion (text and binary formats).

- **Build**: `dune build`
- **Test suite**: `dune test && dune exec test/test_ion_suite.exe`
- **Text parser**: menhir (`text_parser.mly`) + ocamllex (`text_lexer.mll`)
- **Binary parser**: Parseff-based (`binary.ml`)
- **ion-tests**: git submodule at `ion-tests/iontestdata/` — `good/` files must parse successfully, `bad/` files must raise an exception

## Current Status

**783/783 tests pass.** (600 text `.ion` + 183 binary `.10n`)

Skipped tests (filtered out in `test/test_ion_suite.ml`):
- `utf16.ion`, `utf32.ion` — require BOM-based transcoding, not implemented

## Key Files

```
lib/
  ion.ml            — AST types (datum variants, ion_value, ion_type)
  text_lexer.mll    — ocamllex lexer
  text_parser.mly   — menhir parser
  text.ml           — entry points: of_string, of_channel; post-parse validation
  binary.ml         — binary Ion parser (Parseff-based); of_channel entry point
test/
  test_ion_suite.ml — conformance test runner (dispatches .ion → Text, .10n → Binary)
  test_camlion.ml   — unit tests
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

## Binary Parser Details (binary.ml)

Uses **Parseff 0.3.0** (`Parseff.BE`, `Parseff.take`, `Parseff.position`) with a **single Parseff context** throughout — no nested `Parseff.parse` calls (which cause `Continuation_already_resumed`). Container boundaries are tracked via `Parseff.position () + computed end_pos`.

Always use `Parseff.fail` inside parsers, never bare `failwith` (regular exceptions corrupt Parseff's effect continuations).

### VarUInt / VarInt encoding
- **VarUInt**: high bit 1 = LAST byte (not "continue"), remaining 7 bits are big-endian magnitude.
- **VarInt**: first byte: bit7=last, bit6=sign, bits5-0=magnitude; subsequent bytes same as VarUInt.

### Symbol IDs
- SID 0 is valid: represents a symbol with no text → `SymbolId 0`.
- SID 0 as struct field name signals a NOP-padding pair: value must be a NOP pad, pair is silently discarded.
- Unmapped SIDs (in-range but empty slot, from external catalog imports) resolve to `"$N"` text for field names and annotations; as values they produce `SymbolId n`.

### Local Symbol Tables
- `$ion_symbol_table::{imports:[...], symbols:[...]}` at top level updates the active symbol table.
- `imports: $ion_symbol_table` means append (keep prior LST); `imports: [...]` rebuilds from system table.
- External catalog imports (unknown `name`) add `max_id` empty slots — SIDs are in-range but unmapped.
- Duplicate `imports` or `symbols` fields → error. Null/non-integer/negative `max_id` → error.

### Struct with L=1
- L=1 means ordered/sorted struct; a VarUInt length follows. If that VarUInt = 0 it is an error.
- L=0 means empty struct (no VarUInt length, no content bytes).

### Annotation wrappers
- Illegal: L=15 (null), L<3 (too short), annotation SID list length=0, nested annotation wrapper, wrapping a NOP pad, SID 0 in annotation list, content not fully consumed.

### Timestamps
- Fractional seconds coefficient is sign-magnitude: high bit of first byte is sign, remaining bits are magnitude. `0x80` = `-0` = valid zero fraction regardless of exponent.
- Valid fractional second: value in [0, 1). Exponent must be negative unless coefficient is 0.
- Validated: month 1-12, day 1-N (leap-year-aware for Feb 29), hour 0-23, minute 0-59, second 0-59.
- Fractional second ≥ 1.0 (e.g. `10 × 10^-1`) → error. Negative fractional second → error.

### BVM
- `0xE0 0x01 0x00 0xEA` must appear at the start; subsequent `0xE0` bytes at top level must also be a valid BVM or fail.

## What's Not Implemented

- UTF-16 / UTF-32 BOM handling
- Full Ion symbol table resolution (imports from external catalogs)
  - External import SIDs are accepted but resolve to `"$N"` text (unknown)
- Ion data model equivalence / comparison
- Writing / serialisation
