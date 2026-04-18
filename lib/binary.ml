(** Ion binary format parser (Ion 1.0). *)

open Ion

(* ------------------------------------------------------------------ *)
(* System symbol table                                                  *)
(* ------------------------------------------------------------------ *)

(** SIDs 0–9 defined by the Ion 1.0 system symbol table. *)
let system_symbols : string array = [|
  "";                         (* $0  – no text; valid but "unknown" *)
  "$ion";                     (* $1  *)
  "$ion_1_0";                 (* $2  *)
  "$ion_symbol_table";        (* $3  *)
  "name";                     (* $4  *)
  "version";                  (* $5  *)
  "imports";                  (* $6  *)
  "symbols";                  (* $7  *)
  "max_id";                   (* $8  *)
  "$ion_shared_symbol_table"; (* $9  *)
|]

let fresh_sym_table () : string array = Array.copy system_symbols

(** Resolve SID.  SID 0 is valid but has no text → returns None (caller
    decides what to do).  Out-of-range SIDs raise Failure. *)
let lookup_sym_opt (st : string array) sid =
  if sid < 0 || sid >= Array.length st then
    Parseff.fail
      (Printf.sprintf "symbol ID $%d out of range (table has %d entries)"
         sid (Array.length st))
  else
    let s = st.(sid) in
    if s = "" then None   (* SID 0 or explicitly unmapped slot *)
    else Some s

(** Resolve SID; raises a Parseff failure for SID 0 or unmapped SIDs. *)
let lookup_sym (st : string array) sid =
  match lookup_sym_opt st sid with
  | Some s -> s
  | None ->
    Parseff.fail (Printf.sprintf "symbol ID $%d is unmapped" sid)

(* ------------------------------------------------------------------ *)
(* VarUInt / VarInt                                                     *)
(* ------------------------------------------------------------------ *)

(** VarUInt: high bit 1 = last byte, low 7 bits = magnitude (big-endian). *)
let read_var_uint () =
  let v = ref 0 in
  let go = ref true in
  while !go do
    let b = Parseff.BE.any_uint8 () in
    if b land 0x80 <> 0 then begin
      v := (!v lsl 7) lor (b land 0x7F);
      go := false
    end else
      v := (!v lsl 7) lor b
  done;
  !v

(** VarInt: first byte bit7=last, bit6=sign, bits5-0=value;
    subsequent bytes same as VarUInt. *)
let read_var_int () =
  let b0 = Parseff.BE.any_uint8 () in
  let neg = b0 land 0x40 <> 0 in
  let v = ref (b0 land 0x3F) in
  if b0 land 0x80 = 0 then begin
    let go = ref true in
    while !go do
      let b = Parseff.BE.any_uint8 () in
      if b land 0x80 <> 0 then begin
        v := (!v lsl 7) lor (b land 0x7F);
        go := false
      end else
        v := (!v lsl 7) lor b
    done
  end;
  if neg then - !v else !v

(* ------------------------------------------------------------------ *)
(* Integer helpers                                                      *)
(* ------------------------------------------------------------------ *)

(** Big-endian unsigned integer from raw byte string. *)
let z_of_bytes s =
  let n = String.length s in
  let r = ref Z.zero in
  for i = 0 to n - 1 do
    r := Z.add (Z.shift_left !r 8) (Z.of_int (Char.code s.[i]))
  done;
  !r

(** Sign-magnitude integer: high bit of first byte is sign. *)
let z_sign_mag_of_bytes neg s =
  if String.length s = 0 then Z.zero
  else begin
    let first = Char.code s.[0] land 0x7F in
    let r = ref (Z.of_int first) in
    for i = 1 to String.length s - 1 do
      r := Z.add (Z.shift_left !r 8) (Z.of_int (Char.code s.[i]))
    done;
    if neg then Z.neg !r else !r
  end

(* ------------------------------------------------------------------ *)
(* UTF-8 validation                                                     *)
(* ------------------------------------------------------------------ *)

let validate_utf8 s =
  let n = String.length s in
  let i = ref 0 in
  while !i < n do
    let b = Char.code s.[!i] in
    let extra =
      if b land 0x80 = 0 then 0
      else if b land 0xE0 = 0xC0 then 1
      else if b land 0xF0 = 0xE0 then 2
      else if b land 0xF8 = 0xF0 then 3
      else Parseff.fail "invalid UTF-8 byte in string"
    in
    if !i + extra >= n then Parseff.fail "truncated UTF-8 sequence in string";
    for j = 1 to extra do
      if Char.code s.[!i + j] land 0xC0 <> 0x80 then
        Parseff.fail "invalid UTF-8 continuation byte in string"
    done;
    i := !i + extra + 1
  done

(* ------------------------------------------------------------------ *)
(* Base-64 encoder (for Blob)                                           *)
(* ------------------------------------------------------------------ *)

let base64_encode s =
  let alpha =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" in
  let n = String.length s in
  let buf = Buffer.create (n * 4 / 3 + 4) in
  let i = ref 0 in
  while !i + 2 < n do
    let b0 = Char.code s.[!i] in
    let b1 = Char.code s.[!i + 1] in
    let b2 = Char.code s.[!i + 2] in
    Buffer.add_char buf alpha.[b0 lsr 2];
    Buffer.add_char buf alpha.[((b0 land 3) lsl 4) lor (b1 lsr 4)];
    Buffer.add_char buf alpha.[((b1 land 0xF) lsl 2) lor (b2 lsr 6)];
    Buffer.add_char buf alpha.[b2 land 0x3F];
    i := !i + 3
  done;
  (match n - !i with
   | 1 ->
     let b0 = Char.code s.[!i] in
     Buffer.add_char buf alpha.[b0 lsr 2];
     Buffer.add_char buf alpha.[(b0 land 3) lsl 4];
     Buffer.add_string buf "=="
   | 2 ->
     let b0 = Char.code s.[!i] in
     let b1 = Char.code s.[!i + 1] in
     Buffer.add_char buf alpha.[b0 lsr 2];
     Buffer.add_char buf alpha.[((b0 land 3) lsl 4) lor (b1 lsr 4)];
     Buffer.add_char buf alpha.[(b1 land 0xF) lsl 2];
     Buffer.add_char buf '='
   | _ -> ());
  Buffer.contents buf

(* ------------------------------------------------------------------ *)
(* Decimal formatting                                                   *)
(* ------------------------------------------------------------------ *)

(** Parse Ion binary decimal encoding from bytes.
    Format: VarInt exponent + sign-magnitude coefficient.
    Returns a string like "3d-2" or "-0d0". *)
let format_decimal_bytes s =
  if String.length s = 0 then "0d0"
  else begin
    let len = String.length s in
    let pos = ref 0 in
    (* VarInt exponent (local reader) *)
    if !pos >= len then Parseff.fail "decimal: empty content";
    let b0 = Char.code s.[!pos] in incr pos;
    let neg_e = b0 land 0x40 <> 0 in
    let ev = ref (b0 land 0x3F) in
    if b0 land 0x80 = 0 then begin
      let go = ref true in
      while !go do
        if !pos >= len then Parseff.fail "decimal: exponent extends past content";
        let b = Char.code s.[!pos] in incr pos;
        if b land 0x80 <> 0 then begin
          ev := (!ev lsl 7) lor (b land 0x7F);
          go := false
        end else
          ev := (!ev lsl 7) lor b
      done
    end;
    let exp = if neg_e then - !ev else !ev in
    (* Coefficient: remaining bytes as sign-magnitude *)
    let coeff_s = String.sub s !pos (len - !pos) in
    let neg_c =
      String.length coeff_s > 0 && (Char.code coeff_s.[0] land 0x80 <> 0) in
    let coeff = z_sign_mag_of_bytes neg_c coeff_s in
    let sign = if neg_c then "-" else "" in
    Printf.sprintf "%s%sd%d" sign (Z.to_string (Z.abs coeff)) exp
  end

(* ------------------------------------------------------------------ *)
(* Timestamp formatting + validation                                    *)
(* ------------------------------------------------------------------ *)

(** Days per month (non-leap year). *)
let days_in_month = [| 0; 31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31 |]

let is_leap_year y =
  (y mod 4 = 0 && y mod 100 <> 0) || (y mod 400 = 0)

(** Validate and format an Ion binary timestamp from its raw bytes. *)
let format_timestamp_bytes s =
  let len = String.length s in
  if len < 2 then
    Parseff.fail
      (Printf.sprintf "timestamp: too short (len=%d, need ≥2)" len);
  let pos = ref 0 in

  let var_uint_s () =
    let v = ref 0 in
    let go = ref true in
    while !go do
      if !pos >= len then Parseff.fail "timestamp: unexpected end in VarUInt";
      let b = Char.code s.[!pos] in incr pos;
      if b land 0x80 <> 0 then begin
        v := (!v lsl 7) lor (b land 0x7F);
        go := false
      end else
        v := (!v lsl 7) lor b
    done;
    !v
  in

  let var_int_s () =
    if !pos >= len then Parseff.fail "timestamp: unexpected end in VarInt";
    let b0 = Char.code s.[!pos] in incr pos;
    let neg = b0 land 0x40 <> 0 in
    let v = ref (b0 land 0x3F) in
    if b0 land 0x80 = 0 then begin
      let go = ref true in
      while !go do
        if !pos >= len then Parseff.fail "timestamp: unexpected end in VarInt";
        let b = Char.code s.[!pos] in incr pos;
        if b land 0x80 <> 0 then begin
          v := (!v lsl 7) lor (b land 0x7F);
          go := false
        end else
          v := (!v lsl 7) lor b
      done
    end;
    if neg then - !v else !v
  in

  (* Offset: VarInt minutes; first byte 0xC0 = unknown ("–00:00") *)
  let unknown_offset = (Char.code s.[0] = 0xC0) in
  let offset = var_int_s () in
  let year = var_uint_s () in

  let buf = Buffer.create 32 in
  if !pos >= len then
    Buffer.add_string buf (Printf.sprintf "%04dT" year)
  else begin
    let month = var_uint_s () in
    if month < 1 || month > 12 then
      Parseff.fail
        (Printf.sprintf "timestamp: invalid month %d" month);
    if !pos >= len then
      Buffer.add_string buf (Printf.sprintf "%04d-%02dT" year month)
    else begin
      let day = var_uint_s () in
      let max_day =
        if month = 2 && is_leap_year year then 29
        else days_in_month.(month)
      in
      if day < 1 || day > max_day then
        Parseff.fail
          (Printf.sprintf "timestamp: invalid day %d for month %d year %d"
             day month year);
      if !pos >= len then
        Buffer.add_string buf
          (Printf.sprintf "%04d-%02d-%02dT" year month day)
      else begin
        let hour = var_uint_s () in
        let minute = var_uint_s () in
        if hour > 23 then
          Parseff.fail (Printf.sprintf "timestamp: invalid hour %d" hour);
        if minute > 59 then
          Parseff.fail (Printf.sprintf "timestamp: invalid minute %d" minute);
        Buffer.add_string buf
          (Printf.sprintf "%04d-%02d-%02dT%02d:%02d"
             year month day hour minute);
        if !pos < len then begin
          let second = var_uint_s () in
          if second > 59 then
            Parseff.fail
              (Printf.sprintf "timestamp: invalid second %d" second);
          Buffer.add_string buf (Printf.sprintf ":%02d" second);
           if !pos < len then begin
             (* Fractional seconds: VarInt scale (negative = decimal places) *)
             let scale = var_int_s () in
             let mag_bytes = String.sub s !pos (len - !pos) in
             (* Coefficient is sign-magnitude: high bit of first byte is sign *)
             let (frac_neg, mag) =
               if String.length mag_bytes = 0 then (false, Z.zero)
               else begin
                 let first = Char.code mag_bytes.[0] in
                 let neg = (first land 0x80) <> 0 in
                 let r = ref (Z.of_int (first land 0x7F)) in
                 for i = 1 to String.length mag_bytes - 1 do
                   r := Z.add (Z.shift_left !r 8) (Z.of_int (Char.code mag_bytes.[i]))
                 done;
                 (neg, !r)
               end
             in
             (* Validate: fraction must be in [0, 1) *)
             if frac_neg && Z.sign mag <> 0 then
               Parseff.fail "timestamp: negative fractional seconds";
             (* Check fraction < 1: mag * 10^scale < 1
                → mag < 10^(-scale) when scale < 0 *)
             if scale >= 0 && Z.sign mag <> 0 then
               Parseff.fail
                 "timestamp: fractional second exponent must be negative";
             let decimals = - scale in
             let limit = Z.pow (Z.of_int 10) decimals in
             if Z.geq mag limit then
               Parseff.fail
                 (Printf.sprintf
                    "timestamp: fractional seconds out of range (%s * 10^%d ≥ 1)"
                    (Z.to_string mag) scale);
             let mag_str = Z.to_string mag in
             let padded =
               let need = decimals - String.length mag_str in
               if need > 0 then String.make need '0' ^ mag_str else mag_str
             in
             let plen = String.length padded in
             let frac =
               if plen >= decimals then
                 String.sub padded (plen - decimals) decimals
               else padded
             in
             Buffer.add_char buf '.';
             Buffer.add_string buf frac
           end
        end;
        (* Offset suffix *)
        if unknown_offset then
          Buffer.add_string buf "-00:00"
        else if offset = 0 then
          Buffer.add_char buf 'Z'
        else begin
          let sign = if offset < 0 then '-' else '+' in
          let am = abs offset in
          Buffer.add_string buf
            (Printf.sprintf "%c%02d:%02d" sign (am / 60) (am mod 60))
        end
      end
    end
  end;
  Buffer.contents buf

(* ------------------------------------------------------------------ *)
(* Annotation-list parser                                               *)
(* ------------------------------------------------------------------ *)

(** Parse a sequence of VarUInt SIDs from a byte string.
    Returns list of resolved symbol strings.  SID 0 in annotation = error. *)
let parse_annotation_list (st : string array) bytes =
  let len = String.length bytes in
  let pos = ref 0 in
  let annots = ref [] in
  while !pos < len do
    let v = ref 0 in
    let go = ref true in
    while !go do
      if !pos >= len then Parseff.fail "annotation list: unexpected end";
      let b = Char.code bytes.[!pos] in incr pos;
      if b land 0x80 <> 0 then begin
        v := (!v lsl 7) lor (b land 0x7F);
        go := false
      end else
        v := (!v lsl 7) lor b
    done;
    let sym = match lookup_sym_opt st !v with
      | Some s -> s
      | None ->
        if !v = 0 then Parseff.fail "annotation SID 0 has no text"
        else Printf.sprintf "$%d" !v
    in
    annots := sym :: !annots
  done;
  List.rev !annots

(* ------------------------------------------------------------------ *)
(* Local symbol table processing                                        *)
(* ------------------------------------------------------------------ *)

(** Update sym_ref based on a parsed $ion_symbol_table::{…} struct.
    Raises a Parseff failure on invalid LST structure. *)
let process_lst (sym_ref : string array ref)
    (fields : (string * ion_value) list) =
  let imports_count = ref 0 in
  let symbols_count = ref 0 in
  let imports_val = ref None in
  let symbols_val = ref None in
  List.iter (fun (k, v) ->
    if k = "imports" then begin
      incr imports_count;
      if !imports_count > 1 then
        Parseff.fail "local symbol table has multiple 'imports' fields";
      imports_val := Some v
    end else if k = "symbols" then begin
      incr symbols_count;
      if !symbols_count > 1 then
        Parseff.fail "local symbol table has multiple 'symbols' fields";
      symbols_val := Some v
    end
  ) fields;
  let base =
    match !imports_val with
    | None ->
      Array.copy system_symbols
    | Some { value = Symbol "$ion_symbol_table"; _ } ->
      Array.copy !sym_ref
    | Some { value = List imports; _ } ->
      let slots = ref (Array.to_list system_symbols) in
      List.iter (fun import ->
        match import.value with
        | Struct ifields ->
          let max_id_opt = ref None in
          List.iter (fun (k, v) ->
            if k = "max_id" then max_id_opt := Some v
          ) ifields;
          (match !max_id_opt with
           | None -> Parseff.fail "import struct missing max_id"
           | Some { value = Null _; _ } ->
             Parseff.fail "import max_id must not be null"
           | Some { value = Int n; _ } ->
             let n = Z.to_int n in
             if n < 0 then Parseff.fail "import max_id must be non-negative";
             for _ = 1 to n do slots := !slots @ [""] done
           | Some _ -> Parseff.fail "import max_id must be an integer")
        | _ -> ()
      ) imports;
      Array.of_list !slots
    | Some _ ->
      Array.copy system_symbols
  in
  let extra =
    match !symbols_val with
    | None -> [||]
    | Some { value = List syms; _ } ->
      Array.of_list (List.map (fun sv ->
        match sv.value with
        | String s -> s
        | _ -> ""
      ) syms)
    | Some _ -> [||]
  in
  sym_ref := Array.append base extra

(* ------------------------------------------------------------------ *)
(* Core value parser (single Parseff context, position-tracked)         *)
(* ------------------------------------------------------------------ *)

(** Read the length field for a type descriptor with nibble [l].
    Handles L=14 (VarUInt follows) and L=15 (null sentinel = -1). *)
let decode_len l =
  if l = 0xF then -1           (* null *)
  else if l = 0xE then read_var_uint ()
  else l

(** Parse all values from the current position up to [end_pos].
    Returns the list of parsed ion_values (NOP pads discarded).
    This is used for list and sexp content. *)
let rec parse_seq sym_ref end_pos =
  let items = ref [] in
  while Parseff.position () < end_pos do
    let b = Parseff.BE.any_uint8 () in
    let typ = b lsr 4 in
    let l   = b land 0x0F in
    (match parse_one sym_ref typ l with
     | None -> ()
     | Some v -> items := v :: !items)
  done;
  if Parseff.position () > end_pos then
    Parseff.fail "value extends past container boundary";
  List.rev !items

(** Parse struct field (name, value) pairs from current position up to
    [end_pos].  Handles SID-0 NOP-padding pairs. *)
and parse_struct sym_ref end_pos =
  let fields = ref [] in
  while Parseff.position () < end_pos do
    let sid = read_var_uint () in
    if sid = 0 then begin
      (* SID 0 as field name = NOP padding pair; value MUST be a NOP pad *)
      let vb = Parseff.BE.any_uint8 () in
      let vtyp = vb lsr 4 in
      let vl   = vb land 0x0F in
      if vtyp <> 0x0 || vl = 0xF then
        Parseff.fail
          "struct NOP padding (SID 0 field) must have a NOP-pad value";
      let nop_len = if vl = 0xE then read_var_uint () else vl in
      ignore (Parseff.take nop_len)
      (* silently skip the NOP pair *)
    end else begin
      let fname = match lookup_sym_opt !sym_ref sid with
        | Some s -> s
        | None -> Printf.sprintf "$%d" sid
      in
      let vb = Parseff.BE.any_uint8 () in
      let vtyp = vb lsr 4 in
      let vl   = vb land 0x0F in
      (match parse_one sym_ref vtyp vl with
       | None ->
         (* NOP pad value with non-zero SID: skip the NOP, discard field *)
         ()
       | Some v ->
         fields := (fname, v) :: !fields)
    end
  done;
  if Parseff.position () > end_pos then
    Parseff.fail "struct field extends past container boundary";
  List.rev !fields

(** Parse a single typed value given its type nibble [typ] and length
    nibble [l].  Returns [None] for NOP pads. *)
and parse_one sym_ref typ l =
  match typ with

  (* 0x0 – NOP pad (L≠15) or null.null (L=15) *)
  | 0x0 ->
    if l = 0xF then
      Some { annotations = []; value = Null Null_type }
    else begin
      let len = if l = 0xE then read_var_uint () else l in
      ignore (Parseff.take len);
      None
    end

  (* 0x1 – bool *)
  | 0x1 ->
    if l = 0xF then
      Some { annotations = []; value = Null Bool_type }
    else if l > 1 then
      Parseff.fail (Printf.sprintf "invalid bool length %d (must be 0 or 1)" l)
    else
      Some { annotations = []; value = Bool (l = 1) }

  (* 0x2 – positive int *)
  | 0x2 ->
    let len = decode_len l in
    if len < 0 then Some { annotations = []; value = Null Int_type }
    else if len = 0 then Some { annotations = []; value = Int Z.zero }
    else begin
      let bytes = Parseff.take len in
      Some { annotations = []; value = Int (z_of_bytes bytes) }
    end

  (* 0x3 – negative int (magnitude > 0) *)
  | 0x3 ->
    let len = decode_len l in
    if len < 0 then Some { annotations = []; value = Null Int_type }
    else if len = 0 then
      Parseff.fail "negative integer zero (type 3, L=0) is not valid Ion"
    else begin
      let bytes = Parseff.take len in
      let mag = z_of_bytes bytes in
      if Z.equal mag Z.zero then
        Parseff.fail "negative integer zero (magnitude=0) is not valid Ion";
      Some { annotations = []; value = Int (Z.neg mag) }
    end

  (* 0x4 – float *)
  | 0x4 ->
    let len = decode_len l in
    if len < 0 then Some { annotations = []; value = Null Float_type }
    else
      let f = match len with
        | 0 -> 0.0
        | 4 -> Parseff.BE.any_float ()
        | 8 -> Parseff.BE.any_double ()
        | n ->
          Parseff.fail
            (Printf.sprintf "invalid float length %d (must be 0, 4, or 8)" n)
      in
      Some { annotations = []; value = Float f }

  (* 0x5 – decimal *)
  | 0x5 ->
    let len = decode_len l in
    if len < 0 then Some { annotations = []; value = Null Decimal_type }
    else begin
      let bytes = Parseff.take len in
      let s = format_decimal_bytes bytes in
      Some { annotations = []; value = Decimal s }
    end

  (* 0x6 – timestamp *)
  | 0x6 ->
    let len = decode_len l in
    if len < 0 then Some { annotations = []; value = Null Timestamp_type }
    else if len < 2 then
      Parseff.fail
        (Printf.sprintf "timestamp too short: length=%d (need ≥2)" len)
    else begin
      let bytes = Parseff.take len in
      let s = format_timestamp_bytes bytes in
      Some { annotations = []; value = Timestamp s }
    end

  (* 0x7 – symbol *)
  | 0x7 ->
    let len = decode_len l in
    if len < 0 then Some { annotations = []; value = Null Symbol_type }
    else begin
      let bytes = Parseff.take len in
      let sid = Z.to_int (z_of_bytes bytes) in
      (* SID 0 is valid: symbol with no text *)
      if sid < 0 || sid >= Array.length !sym_ref then
        Parseff.fail
          (Printf.sprintf "symbol ID $%d out of range (table has %d entries)"
             sid (Array.length !sym_ref))
      else if sid = 0 || !sym_ref.(sid) = "" then
        Some { annotations = []; value = SymbolId sid }
      else
        Some { annotations = []; value = Symbol !sym_ref.(sid) }
    end

  (* 0x8 – string *)
  | 0x8 ->
    let len = decode_len l in
    if len < 0 then Some { annotations = []; value = Null String_type }
    else begin
      let bytes = Parseff.take len in
      validate_utf8 bytes;
      Some { annotations = []; value = String bytes }
    end

  (* 0x9 – clob *)
  | 0x9 ->
    let len = decode_len l in
    if len < 0 then Some { annotations = []; value = Null Clob_type }
    else begin
      let bytes = Parseff.take len in
      Some { annotations = []; value = Clob bytes }
    end

  (* 0xA – blob *)
  | 0xA ->
    let len = decode_len l in
    if len < 0 then Some { annotations = []; value = Null Blob_type }
    else begin
      let bytes = Parseff.take len in
      Some { annotations = []; value = Blob (base64_encode bytes) }
    end

  (* 0xB – list *)
  | 0xB ->
    let len = decode_len l in
    if len < 0 then Some { annotations = []; value = Null List_type }
    else begin
      let end_pos = Parseff.position () + len in
      let items = parse_seq sym_ref end_pos in
      Some { annotations = []; value = List items }
    end

  (* 0xC – sexp *)
  | 0xC ->
    let len = decode_len l in
    if len < 0 then Some { annotations = []; value = Null Sexp_type }
    else begin
      let end_pos = Parseff.position () + len in
      let items = parse_seq sym_ref end_pos in
      Some { annotations = []; value = Sexp items }
    end

  (* 0xD – struct *)
  | 0xD ->
    if l = 0xF then
      Some { annotations = []; value = Null Struct_type }
    else begin
      let (end_pos, len_ok) =
        if l = 0x1 then begin
          (* Ordered struct: explicit VarUInt length *)
          let vlen = read_var_uint () in
          if vlen = 0 then
            Parseff.fail
              "ordered struct (L=1) with length=0 must have at least one field";
          (Parseff.position () + vlen, true)
        end else begin
          let len = decode_len l in
          if len < 0 then (0, false)   (* null.struct *)
          else (Parseff.position () + len, true)
        end
      in
      if not len_ok then
        Some { annotations = []; value = Null Struct_type }
      else begin
        let fields = parse_struct sym_ref end_pos in
        Some { annotations = []; value = Struct fields }
      end
    end

  (* 0xE – annotation wrapper *)
  | 0xE ->
    if l = 0xF then
      Parseff.fail "null annotation wrapper (type E, L=15) is not valid";
    if l < 3 then
      Parseff.fail
        (Printf.sprintf
           "annotation wrapper too short: L=%d (need L≥3 or L=14)" l);
    let len = if l = 0xE then read_var_uint () else l in
    let end_pos = Parseff.position () + len in
    (* Read annotation list length *)
    let annot_list_len = read_var_uint () in
    if annot_list_len = 0 then
      Parseff.fail "annotation wrapper: annotation list is empty";
    let annot_bytes = Parseff.take annot_list_len in
    let annots = parse_annotation_list !sym_ref annot_bytes in
    (* Value must occupy the rest of the wrapper *)
    if Parseff.position () >= end_pos then
      Parseff.fail "annotation wrapper: no value after annotation list";
    let vb = Parseff.BE.any_uint8 () in
    let vtyp = vb lsr 4 in
    let vl   = vb land 0x0F in
    (* Nested annotation is illegal *)
    if vtyp = 0xE then
      Parseff.fail "nested annotation wrapper is not valid";
    (* NOP pad inside annotation is illegal *)
    if vtyp = 0x0 && vl <> 0xF then
      Parseff.fail "annotation wrapping NOP pad is not valid";
    let v =
      match parse_one sym_ref vtyp vl with
      | None -> Parseff.fail "annotation content is unexpectedly NOP"
      | Some v -> v
    in
    (* Enforce that the wrapper content is fully consumed *)
    if Parseff.position () <> end_pos then
      Parseff.fail
        (Printf.sprintf
           "annotation wrapper length mismatch: expected end at %d, got %d"
           end_pos (Parseff.position ()));
    Some { v with annotations = annots @ v.annotations }

  (* 0xF – reserved / always illegal *)
  | 0xF ->
    Parseff.fail "type code 0xF is reserved and not valid"

  | _ -> assert false

(* ------------------------------------------------------------------ *)
(* Top-level stream parser                                              *)
(* ------------------------------------------------------------------ *)

(** Parse the Ion binary stream, starting after the initial BVM has
    been consumed.  Updates [sym_ref] as LSTs are encountered. *)
and parse_stream sym_ref =
  let values = ref [] in
  (* Use optional to detect end-of-stream *)
  let go = ref true in
  while !go do
    match Parseff.optional (fun () -> Parseff.BE.any_uint8 ()) () with
    | None -> go := false
    | Some b ->
      let typ = b lsr 4 in
      let l   = b land 0x0F in
      (* Top-level 0xE0 → must be a BVM *)
      if b = 0xE0 then begin
        let b1 = Parseff.BE.any_uint8 () in
        let b2 = Parseff.BE.any_uint8 () in
        let b3 = Parseff.BE.any_uint8 () in
        if not (b1 = 0x01 && b2 = 0x00 && b3 = 0xEA) then
          Parseff.fail
            (Printf.sprintf "invalid IVM: E0 %02X %02X %02X" b1 b2 b3);
        sym_ref := fresh_sym_table ()
      end else begin
        match parse_one sym_ref typ l with
        | None -> ()
        | Some v ->
          if List.mem "$ion_symbol_table" v.annotations then begin
            match v.value with
            | Struct fields -> process_lst sym_ref fields
            | _ -> ()
          end;
          values := v :: !values
      end
  done;
  List.rev !values

(* ------------------------------------------------------------------ *)
(* Public entry points                                                  *)
(* ------------------------------------------------------------------ *)

let run_source source =
  let sym_ref = ref (fresh_sym_table ()) in
  (* Must start with a BVM *)
  let result =
    Parseff.parse_source source (fun () ->
      (* Read and validate BVM *)
      let b0 = Parseff.BE.any_uint8 () in
      let b1 = Parseff.BE.any_uint8 () in
      let b2 = Parseff.BE.any_uint8 () in
      let b3 = Parseff.BE.any_uint8 () in
      if not (b0 = 0xE0 && b1 = 0x01 && b2 = 0x00 && b3 = 0xEA) then
        Parseff.fail
          (Printf.sprintf "binary Ion: invalid IVM %02X %02X %02X %02X"
             b0 b1 b2 b3);
      parse_stream sym_ref)
  in
  match result with
  | Parseff.Ok values -> values
  | Parseff.Error { error = `Failure msg; _ } ->
    failwith ("binary Ion: " ^ msg)
  | Parseff.Error { error = `Unexpected_end_of_input; _ } ->
    failwith "binary Ion: unexpected end of input"
  | Parseff.Error { error = `Expected msg; _ } ->
    failwith ("binary Ion: unexpected input, expected " ^ msg)
  | Parseff.Error { error = `Depth_limit_exceeded msg; _ } ->
    failwith ("binary Ion: nesting depth exceeded: " ^ msg)

let of_string s =
  run_source (Parseff.Source.of_string s)

let of_channel ic =
  run_source (Parseff.Source.of_channel ic)
