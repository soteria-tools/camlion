(** Entry points for parsing Ion text format. *)

open Ion

(** If [s] matches [$ion_N_M], return [(major, minor)] as strings.
    Uses simple prefix/character scanning — no regex dependency. *)
let parse_ion_version s =
  let n = String.length s in
  if n > 6 && String.sub s 0 5 = "$ion_" then begin
    (* Find the '_' separating major and minor version *)
    match String.rindex_opt s '_' with
    | None -> None
    | Some sep ->
      if sep <= 5 then None  (* only the prefix underscore, no second _ *)
      else begin
        let major = String.sub s 5 (sep - 5) in
        let minor = String.sub s (sep + 1) (n - sep - 1) in
        (* Both must be non-empty digit strings *)
        let all_digits t =
          String.length t > 0 &&
          String.for_all (fun c -> c >= '0' && c <= '9') t
        in
        if all_digits major && all_digits minor then Some (major, minor)
        else None
      end
  end else None

(** Return Some n if s is "$N" for a non-negative integer N, else None. *)
let parse_symbol_id s =
  let n = String.length s in
  if n >= 2 && s.[0] = '$' then begin
    let rest = String.sub s 1 (n - 1) in
    if String.length rest > 0 && String.for_all (fun c -> c >= '0' && c <= '9') rest then
      (try Some (int_of_string rest) with _ -> None)
    else None
  end else None

(** Validate an import struct within a local symbol table's imports list. *)
let validate_import_struct fields =
  List.iter (fun (fname, fval) ->
    if fname = "max_id" then begin
      match fval.value with
      | Int n ->
        if Z.compare n Z.zero < 0 then
          raise (Text_lexer.Lexer_error
            "Local symbol table import max_id must be non-negative")
      | Null _ ->
        raise (Text_lexer.Lexer_error
          "Local symbol table import max_id must not be null")
      | _ ->
        raise (Text_lexer.Lexer_error
          "Local symbol table import max_id must be an integer")
    end
  ) fields

(** Validate a local symbol table struct value. *)
let validate_local_symbol_table_struct fields =
  let imports_count = ref 0 in
  let symbols_count = ref 0 in
  List.iter (fun (fname, fval) ->
    (match fname with
     | "imports" ->
       incr imports_count;
       if !imports_count > 1 then
         raise (Text_lexer.Lexer_error
           "Local symbol table has multiple 'imports' fields");
       (* Validate each import struct *)
       (match fval.value with
        | List imports ->
          List.iter (fun iv ->
            match iv.value with
            | Struct ifields -> validate_import_struct ifields
            | _ -> ()
          ) imports
        | _ -> ())
     | "symbols" ->
       incr symbols_count;
       if !symbols_count > 1 then
         raise (Text_lexer.Lexer_error
           "Local symbol table has multiple 'symbols' fields")
     | _ -> ())
  ) fields

(** Check all top-level values for local symbol table annotations. *)
let check_local_symbol_tables values =
  List.iter (fun v ->
    if List.mem "$ion_symbol_table" v.annotations then begin
      match v.value with
      | Struct fields -> validate_local_symbol_table_struct fields
      | _ -> ()
    end
  ) values

(** Ion system symbol table has symbols $0..$9 (10 symbols). *)
let system_symbol_count = 10

(** Count symbols defined by a local symbol table value (for $N tracking).
    Returns the number of new symbols added (including imported symbols
    according to max_id, plus the symbols list). *)
let count_local_symbols fields =
  let from_imports = ref 0 in
  let from_symbols = ref 0 in
  List.iter (fun (fname, fval) ->
    match fname with
    | "imports" ->
      (match fval.value with
       | List imports ->
         List.iter (fun iv ->
           match iv.value with
           | Struct ifields ->
             List.iter (fun (iname, ival) ->
               if iname = "max_id" then
                 match ival.value with
                 | Int n -> from_imports := !from_imports + Z.to_int n
                 | _ -> ()
             ) ifields
           | _ -> ()
         ) imports
       | _ -> ())
    | "symbols" ->
      (match fval.value with
       | List syms -> from_symbols := !from_symbols + List.length syms
       | _ -> ())
    | _ -> ()
  ) fields;
  !from_imports + !from_symbols

(** Check that all $N symbol IDs in a value are within range.
    [max_sid] is the total number of defined symbols. *)
let rec check_symbol_ids_in_value max_sid v =
  (* Check annotations (stored as "$N" strings) *)
  List.iter (fun ann ->
    match parse_symbol_id ann with
    | Some n when n >= max_sid ->
      raise (Text_lexer.Lexer_error
        (Printf.sprintf "Symbol ID $%d is out of range (max $%d)" n (max_sid - 1)))
    | _ -> ()
  ) v.annotations;
  (* Check datum *)
  match v.value with
  | SymbolId n when n >= max_sid ->
    raise (Text_lexer.Lexer_error
      (Printf.sprintf "Symbol ID $%d is out of range (max $%d)" n (max_sid - 1)))
  | List vs | Sexp vs -> List.iter (check_symbol_ids_in_value max_sid) vs
  | Struct fields ->
    List.iter (fun (fname, fv) ->
      (match parse_symbol_id fname with
       | Some n when n >= max_sid ->
         raise (Text_lexer.Lexer_error
           (Printf.sprintf "Field name symbol ID $%d is out of range" n))
       | _ -> ());
      check_symbol_ids_in_value max_sid fv
    ) fields
  | _ -> ()

(** Walk top-level values, tracking symbol table context, and validate $N IDs. *)
let check_symbol_id_ranges values =
  let max_sid = ref system_symbol_count in
  List.iter (fun v ->
    (* If this is a local symbol table annotation, update max_sid *)
    if List.mem "$ion_symbol_table" v.annotations then begin
      (match v.value with
       | Struct fields ->
         (* Check for $ion_symbol_table::[...] (list = new shared table) *)
         let is_lst = not (match (List.assoc_opt "imports" fields) with
           | Some { value = Symbol "$ion_symbol_table"; _ } -> true
           | _ -> false)
         in
         if is_lst then
           max_sid := system_symbol_count + count_local_symbols fields
       | _ -> ())
    end else
      check_symbol_ids_in_value !max_sid v
  ) values

let check_top_level_version_markers values =
  List.iter (fun v ->
    match v with
    | { annotations = []; value = Symbol s } ->
      (match parse_ion_version s with
       | Some (major, minor) when not (major = "1" && minor = "0") ->
         raise (Text_lexer.Lexer_error
           (Printf.sprintf "Unsupported Ion version: %s" s))
       | _ -> ())
    | _ -> ()
  ) values

let of_string s =
  let lexbuf = Lexing.from_string s in
  let values = Text_parser.top_level Text_lexer.token lexbuf in
  check_top_level_version_markers values;
  check_local_symbol_tables values;
  check_symbol_id_ranges values;
  values

let of_channel ic =
  let lexbuf = Lexing.from_channel ic in
  let values = Text_parser.top_level Text_lexer.token lexbuf in
  check_top_level_version_markers values;
  check_local_symbol_tables values;
  check_symbol_id_ranges values;
  values
