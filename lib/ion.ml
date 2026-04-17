(** Ion data model for Ion 1.0 text format *)

(** The Ion type system — used for typed nulls *)
type ion_type =
  | Null_type
  | Bool_type
  | Int_type
  | Float_type
  | Decimal_type
  | Timestamp_type
  | Symbol_type
  | String_type
  | Clob_type
  | Blob_type
  | List_type
  | Sexp_type
  | Struct_type

(** An Ion value: zero or more annotations wrapping a datum *)
type ion_value = {
  annotations : string list;
  value       : ion_data;
}

(** The core Ion data variants *)
and ion_data =
  | Null      of ion_type
  (** [Null Null_type] is bare [null]; [Null Bool_type] is [null.bool], etc. *)

  | Bool      of bool

  | Int       of Z.t
  (** Arbitrary-precision integer (decimal, hex, or binary in source) *)

  | Float     of float
  (** IEEE-754 double, including [nan], [infinity], [neg_infinity] *)

  | Decimal   of string
  (** Exact decimal representation, e.g. ["1.5"], ["1.5d-3"], ["-0."] *)

  | Timestamp of string
  (** Raw ISO-8601-like timestamp text, e.g. ["2003-12-01T"] *)

  | Symbol    of string
  (** Identifier or quoted symbol, e.g. [foo], ['hello world'] *)

  | SymbolId  of int
  (** Unquoted symbol ID reference, e.g. [$99] — must be resolved against symbol table *)

  | String    of string
  (** Double-quoted or triple-quoted string (already unescaped) *)

  | Clob      of string
  (** Byte string with ASCII/Latin-1 text escapes, e.g. [{{ "hello" }}] *)

  | Blob      of string
  (** Raw base64 content (not decoded), e.g. [{{ aGVsbG8= }}] *)

  | List      of ion_value list
  (** Ordered heterogeneous sequence: [ v1, v2, ... ] *)

  | Sexp      of ion_value list
  (** S-expression: ( v1 v2 ... ) — operators are Symbols *)

  | Struct    of (string * ion_value) list
  (** Ordered set of name/value pairs: { name: value, ... } *)

(* ------------------------------------------------------------------ *)
(* Convenience constructors                                             *)
(* ------------------------------------------------------------------ *)

let null          = { annotations = []; value = Null Null_type }
let bool_ b       = { annotations = []; value = Bool b }
let int_ n        = { annotations = []; value = Int n }
let float_ f      = { annotations = []; value = Float f }
let decimal s     = { annotations = []; value = Decimal s }
let timestamp s   = { annotations = []; value = Timestamp s }
let symbol s      = { annotations = []; value = Symbol s }
let string_ s     = { annotations = []; value = String s }
let clob s        = { annotations = []; value = Clob s }
let blob s        = { annotations = []; value = Blob s }
let list_ vs      = { annotations = []; value = List vs }
let sexp vs       = { annotations = []; value = Sexp vs }
let struct_ fields = { annotations = []; value = Struct fields }

let annotate anns v = { v with annotations = anns @ v.annotations }

(* ------------------------------------------------------------------ *)
(* Ion type name helpers                                                *)
(* ------------------------------------------------------------------ *)

let ion_type_to_string = function
  | Null_type      -> "null"
  | Bool_type      -> "bool"
  | Int_type       -> "int"
  | Float_type     -> "float"
  | Decimal_type   -> "decimal"
  | Timestamp_type -> "timestamp"
  | Symbol_type    -> "symbol"
  | String_type    -> "string"
  | Clob_type      -> "clob"
  | Blob_type      -> "blob"
  | List_type      -> "list"
  | Sexp_type      -> "sexp"
  | Struct_type    -> "struct"

let ion_type_of_string = function
  | "null"      -> Null_type
  | "bool"      -> Bool_type
  | "int"       -> Int_type
  | "float"     -> Float_type
  | "decimal"   -> Decimal_type
  | "timestamp" -> Timestamp_type
  | "symbol"    -> Symbol_type
  | "string"    -> String_type
  | "clob"      -> Clob_type
  | "blob"      -> Blob_type
  | "list"      -> List_type
  | "sexp"      -> Sexp_type
  | "struct"    -> Struct_type
  | s           -> failwith ("Unknown Ion type name: " ^ s)

(* ------------------------------------------------------------------ *)
(* Pretty-printing                                                      *)
(* ------------------------------------------------------------------ *)

(** Return true if the symbol text requires quoting when printed *)
let symbol_needs_quoting s =
  if String.length s = 0 then true
  else
    let ok_start c =
      (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' || c = '$'
    in
    let ok_rest c =
      ok_start c || (c >= '0' && c <= '9')
    in
    not (ok_start s.[0])
    || (let bad = ref false in
        String.iter (fun c -> if not (ok_rest c) then bad := true) s;
        !bad)

let pp_symbol buf s =
  if symbol_needs_quoting s then begin
    Buffer.add_char buf '\'';
    String.iter (fun c ->
      if c = '\'' || c = '\\' then Buffer.add_char buf '\\';
      Buffer.add_char buf c) s;
    Buffer.add_char buf '\''
  end else
    Buffer.add_string buf s

let rec pp_value buf v =
  List.iter (fun ann ->
    pp_symbol buf ann;
    Buffer.add_string buf "::") v.annotations;
  pp_data buf v.value

and pp_data buf = function
  | Null t ->
    Buffer.add_string buf "null";
    if t <> Null_type then begin
      Buffer.add_char buf '.';
      Buffer.add_string buf (ion_type_to_string t)
    end
  | Bool b ->
    Buffer.add_string buf (if b then "true" else "false")
  | Int n ->
    Buffer.add_string buf (Z.to_string n)
  | Float f ->
    if Float.is_nan f        then Buffer.add_string buf "nan"
    else if f = infinity     then Buffer.add_string buf "+inf"
    else if f = neg_infinity then Buffer.add_string buf "-inf"
    else Buffer.add_string buf (Printf.sprintf "%.17g" f)
  | Decimal s ->
    Buffer.add_string buf s
  | Timestamp s ->
    Buffer.add_string buf s
  | Symbol s ->
    pp_symbol buf s
  | SymbolId n ->
    Buffer.add_char buf '$';
    Buffer.add_string buf (string_of_int n)
  | String s ->
    Buffer.add_char buf '"';
    String.iter (fun c ->
      (match c with
       | '"'  -> Buffer.add_string buf "\\\""
       | '\\' -> Buffer.add_string buf "\\\\"
       | '\n' -> Buffer.add_string buf "\\n"
       | '\r' -> Buffer.add_string buf "\\r"
       | '\t' -> Buffer.add_string buf "\\t"
       | c    -> Buffer.add_char buf c)) s;
    Buffer.add_char buf '"'
  | Clob s ->
    Buffer.add_string buf "{{\"";
    String.iter (fun c ->
      (match c with
       | '"'  -> Buffer.add_string buf "\\\""
       | '\\' -> Buffer.add_string buf "\\\\"
       | c    -> Buffer.add_char buf c)) s;
    Buffer.add_string buf "\"}}"
  | Blob b64 ->
    Buffer.add_string buf "{{";
    Buffer.add_string buf b64;
    Buffer.add_string buf "}}"
  | List vs ->
    Buffer.add_char buf '[';
    List.iteri (fun i v ->
      if i > 0 then Buffer.add_string buf ", ";
      pp_value buf v) vs;
    Buffer.add_char buf ']'
  | Sexp vs ->
    Buffer.add_char buf '(';
    List.iteri (fun i v ->
      if i > 0 then Buffer.add_char buf ' ';
      pp_value buf v) vs;
    Buffer.add_char buf ')'
  | Struct fields ->
    Buffer.add_char buf '{';
    List.iteri (fun i (k, v) ->
      if i > 0 then Buffer.add_string buf ", ";
      pp_symbol buf k;
      Buffer.add_string buf ": ";
      pp_value buf v) fields;
    Buffer.add_char buf '}'

let to_string v =
  let buf = Buffer.create 64 in
  pp_value buf v;
  Buffer.contents buf
