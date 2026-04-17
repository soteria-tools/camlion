{
(** Ion text format lexer (ocamllex).

    Produces tokens for Text_parser (menhir).

    Design notes
    ------------
    * Each "body" scanner (long strings, quoted symbols, strings, clobs, blobs)
      is an [and] lexer rule that fills a [Buffer.t], keeping line-number
      tracking correct throughout.
    * Escape sequences are processed inline while filling the buffer, so the
      parser receives already-unescaped strings.
    * [null.TYPE] is lexed as a single [NULL_TYPED] token to avoid
      shift/reduce ambiguity around [null] when it appears as a plain
      identifier/symbol.
    * Long-quoted string segments (['''...''']) emit [LONG_STRING_PART]
      tokens.  The parser concatenates adjacent parts.
    * CLOB and BLOB are both delimited by [{{ }}]; the character immediately
      following the opening [{{] (skipping whitespace) determines which.
*)

open Text_parser

exception Lexer_error of string

(* ------------------------------------------------------------------ *)
(* Escape-sequence helpers                                              *)
(* ------------------------------------------------------------------ *)

let hex_val c =
  match c with
  | '0'..'9' -> Char.code c - Char.code '0'
  | 'a'..'f' -> Char.code c - Char.code 'a' + 10
  | 'A'..'F' -> Char.code c - Char.code 'A' + 10
  | _        -> assert false

(** Encode a Unicode codepoint as UTF-8 into [buf]. *)
let utf8_of_codepoint buf cp =
  if cp <= 0x7F then
    Buffer.add_char buf (Char.chr cp)
  else if cp <= 0x7FF then begin
    Buffer.add_char buf (Char.chr (0xC0 lor (cp lsr 6)));
    Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F)))
  end else if cp <= 0xFFFF then begin
    Buffer.add_char buf (Char.chr (0xE0 lor (cp lsr 12)));
    Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
    Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F)))
  end else begin
    Buffer.add_char buf (Char.chr (0xF0 lor (cp lsr 18)));
    Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 12) land 0x3F)));
    Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
    Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F)))
  end

(* ------------------------------------------------------------------ *)
(* Integer string normalization                                         *)
(* ------------------------------------------------------------------ *)

(** Strip underscore visual separators, e.g. [1_000_000 -> "1000000"]. *)
let strip_underscores s =
  let buf = Buffer.create (String.length s) in
  String.iter (fun c -> if c <> '_' then Buffer.add_char buf c) s;
  Buffer.contents buf

(** Parse an integer literal string (decimal / hex / binary) to [Z.t]. *)
let parse_int raw =
  let s = strip_underscores raw in
  let n = String.length s in
  let neg = n > 0 && s.[0] = '-' in
  let body = if neg then String.sub s 1 (n - 1) else s in
  let v =
    if String.length body >= 2 && body.[0] = '0' then
      match body.[1] with
      | 'b' | 'B' ->
        Z.of_string_base 2 (String.sub body 2 (String.length body - 2))
      | 'x' | 'X' ->
        Z.of_string_base 16 (String.sub body 2 (String.length body - 2))
      | _ -> Z.of_string body
    else
      Z.of_string body
  in
  if neg then Z.neg v else v

(** Normalize a decimal token: strip underscores and lowercase [D] -> [d]. *)
let normalize_decimal s =
  String.map (fun c -> if c = 'D' then 'd' else c) (strip_underscores s)

(** Validate the date portion of a timestamp string (YYYY-MM-DD prefix).
    Raises [Lexer_error] for dates like 2006-02-29 (not a leap year). *)
let validate_timestamp_date s =
  let n = String.length s in
  if n >= 10 && s.[4] = '-' && s.[7] = '-' then begin
    let year  = int_of_string (String.sub s 0 4) in
    let month = int_of_string (String.sub s 5 2) in
    let day   = int_of_string (String.sub s 8 2) in
    let is_leap =
      (year mod 4 = 0 && year mod 100 <> 0) || year mod 400 = 0
    in
    let max_day = match month with
      | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
      | 4 | 6 | 9 | 11              -> 30
      | 2 -> if is_leap then 29 else 28
      | _ -> assert false
    in
    if day > max_day then
      raise (Lexer_error
        (Printf.sprintf
           "Invalid date in timestamp: day %d out of range for %04d-%02d"
           day year month))
  end

(** Check that [s] (whitespace already stripped) is valid base64.
    Rules: length divisible by 4; '=' only at the end; at most 2 '='. *)
let validate_blob s =
  let n = String.length s in
  if n mod 4 <> 0 then
    raise (Lexer_error
      (Printf.sprintf "Invalid base64 in BLOB: length %d not divisible by 4" n));
  (* Count trailing '=' *)
  let pads =
    if n >= 2 && s.[n-1] = '=' && s.[n-2] = '=' then 2
    else if n >= 1 && s.[n-1] = '=' then 1
    else 0
  in
  if pads > 2 then
    raise (Lexer_error "Invalid base64 in BLOB: more than 2 padding characters");
  (* Ensure '=' only appears at the very end *)
  for i = 0 to n - 1 - pads do
    if s.[i] = '=' then
      raise (Lexer_error "Invalid base64 in BLOB: '=' in non-terminal position")
  done

(** Check that a numeric (or keyword) literal is followed by a valid delimiter.
    Ion requires that numbers, typed nulls, booleans etc. are not immediately
    followed by alphanumeric or operator characters.  We peek at the next
    character in the lexbuf without consuming it. *)
let check_numeric_terminator lexbuf =
  let pos = lexbuf.Lexing.lex_curr_pos in
  if pos < lexbuf.Lexing.lex_buffer_len then
    match Bytes.get lexbuf.Lexing.lex_buffer pos with
    | ' ' | '\t' | '\n' | '\r' | '\x0B' | '\x0C'
    | ',' | ']' | '}' | ')' | '{' | '[' | '('
    | '"' | '\'' | ':' -> ()
    | c ->
      raise (Lexer_error
        (Printf.sprintf "Numeric literal not followed by valid delimiter: %C" c))

}

(* ------------------------------------------------------------------ *)
(* Character classes                                                    *)
(* ------------------------------------------------------------------ *)

let digit       = ['0'-'9']
let hex_digit   = ['0'-'9' 'a'-'f' 'A'-'F']
let bin_digit   = ['0' '1']
let id_start    = ['a'-'z' 'A'-'Z' '_' '$']
let id_rest     = ['a'-'z' 'A'-'Z' '0'-'9' '_' '$']
let underscore  = '_'

(* Unsigned decimal integer (no leading zeros except bare "0") *)
let dec_uint    = '0' | ['1'-'9'] (underscore? digit)*

(* Signed decimal integer *)
let dec_int     = '-'? dec_uint

(* Fractional part — dot optionally followed by digits; if digits follow,
   the first must be a digit (not underscore), preventing "._456". *)
let dec_frac    = '.' (digit (underscore? digit)*)?

(* Float exponent: E/e + optional sign + digits *)
let float_exp   = ['e' 'E'] ['+' '-']? digit+

(* Decimal exponent: D/d + optional sign + digits *)
let dec_exp     = ['d' 'D'] ['+' '-']? digit+

(* Timestamp sub-patterns *)
let ts_year   = ('0' ['0'-'9'] ['0'-'9'] ['1'-'9'])
              | ('0' ['0'-'9'] ['1'-'9'] digit)
              | ('0' ['1'-'9'] digit digit)
              | (['1'-'9'] digit digit digit)
let ts_month  = '0' ['1'-'9'] | '1' ['0'-'2']
let ts_day    = '0' ['1'-'9'] | ['1'-'2'] digit | '3' ['0'-'1']
let ts_hour   = ['0'-'1'] digit | '2' ['0'-'3']
let ts_minute = ['0'-'5'] digit
let ts_second = ['0'-'5'] digit ('.' digit+)?
let ts_offset = 'Z' | ['+' '-'] ts_hour ':' ts_minute
let ts_time   = ts_hour ':' ts_minute (':' ts_second)? ts_offset
let ts_date   = ts_year '-' ts_month '-' ts_day
let timestamp = ts_date ('T' ts_time?)?
              | ts_year '-' ts_month 'T'
              | ts_year 'T'

(* Operator characters valid inside s-expressions.
   Split to avoid the OPERATOR rule eating comment openers /* and // via
   longest-match.  We keep '/' and '*' out of the "safe after slash" set
   so that /* and // always tokenise as comments, not operators. *)
let non_slash_op       = ['!' '#' '%' '&' '*' '+' '-' ';' '<' '=' '>' '?' '@' '^' '`' '|' '~']
(* chars that may follow a leading '/' in an operator — NOT '*' or '/' *)
let after_slash_op     = ['!' '#' '%' '&' '+' '-' ';' '<' '=' '>' '?' '@' '^' '`' '|' '~']
let non_dot_op         = non_slash_op | '/'
let op_char            = non_dot_op | '.'

(* ------------------------------------------------------------------ *)
(* Main tokeniser                                                       *)
(* ------------------------------------------------------------------ *)

rule token = parse
  (* Whitespace and comments — skip *)
  | [' ' '\t' '\x0B' '\x0C']+    { token lexbuf }
  | '\n'                          { Lexing.new_line lexbuf; token lexbuf }
  | '\r' '\n'                     { Lexing.new_line lexbuf; token lexbuf }
  | '\r'                          { Lexing.new_line lexbuf; token lexbuf }
  | "//" [^ '\n' '\r']*           { token lexbuf }
  | "/*"                          { block_comment lexbuf; token lexbuf }

  (* ---------------------------------------------------------------- *)
  (* Typed nulls — must come before bare [null] and type keywords     *)
  (* ---------------------------------------------------------------- *)
  | "null.null"      { check_numeric_terminator lexbuf; NULL_TYPED Ion.Null_type      }
  | "null.bool"      { check_numeric_terminator lexbuf; NULL_TYPED Ion.Bool_type      }
  | "null.int"       { check_numeric_terminator lexbuf; NULL_TYPED Ion.Int_type       }
  | "null.float"     { check_numeric_terminator lexbuf; NULL_TYPED Ion.Float_type     }
  | "null.decimal"   { check_numeric_terminator lexbuf; NULL_TYPED Ion.Decimal_type   }
  | "null.timestamp" { check_numeric_terminator lexbuf; NULL_TYPED Ion.Timestamp_type }
  | "null.symbol"    { check_numeric_terminator lexbuf; NULL_TYPED Ion.Symbol_type    }
  | "null.string"    { check_numeric_terminator lexbuf; NULL_TYPED Ion.String_type    }
  | "null.clob"      { check_numeric_terminator lexbuf; NULL_TYPED Ion.Clob_type      }
  | "null.blob"      { check_numeric_terminator lexbuf; NULL_TYPED Ion.Blob_type      }
  | "null.list"      { check_numeric_terminator lexbuf; NULL_TYPED Ion.List_type      }
  | "null.sexp"      { check_numeric_terminator lexbuf; NULL_TYPED Ion.Sexp_type      }
  | "null.struct"    { check_numeric_terminator lexbuf; NULL_TYPED Ion.Struct_type    }
  (* null followed by dot and unrecognised type name — always an error *)
  | "null." id_start id_rest* as s
    { raise (Lexer_error (Printf.sprintf "Unknown typed null: %s" s)) }
  | "null."
    { raise (Lexer_error "null. must be followed by a valid type name") }
  | "null"           { NULL                          }

  (* ---------------------------------------------------------------- *)
  (* Bool                                                              *)
  (* ---------------------------------------------------------------- *)
  | "true"    { BOOL true  }
  | "false"   { BOOL false }

  (* ---------------------------------------------------------------- *)
  (* Special float values — before generic operator/number rules      *)
  (* ---------------------------------------------------------------- *)
  | "+inf"  { FLOAT infinity     }
  | "-inf"  { FLOAT neg_infinity }
  | "nan"   { FLOAT Float.nan   }

  (* ---------------------------------------------------------------- *)
  (* Numeric literals                                                  *)
  (* Order matters: timestamp > float > decimal > int                 *)
  (* ---------------------------------------------------------------- *)

  (* Binary integer *)
  | ('-'? '0' ['b' 'B'] bin_digit (underscore? bin_digit)*) as s
    { check_numeric_terminator lexbuf; INT (parse_int s) }

  (* Hexadecimal integer *)
  | ('-'? '0' ['x' 'X'] hex_digit (underscore? hex_digit)*) as s
    { check_numeric_terminator lexbuf; INT (parse_int s) }

  (* Timestamp — before plain decimal so e.g. 2003T isn't a decimal *)
  | timestamp as s
    { check_numeric_terminator lexbuf;
      validate_timestamp_date s;
      TIMESTAMP s }

  (* Float: integer + optional fraction + mandatory float exponent *)
  | (dec_int dec_frac? float_exp) as s
    { check_numeric_terminator lexbuf;
      FLOAT (float_of_string (strip_underscores s)) }

  (* Decimal: integer + fraction + optional decimal exponent *)
  | (dec_int dec_frac dec_exp?) as s
    { check_numeric_terminator lexbuf; DECIMAL (normalize_decimal s) }

  (* Decimal: integer + decimal exponent (no fraction) *)
  | (dec_int dec_exp) as s
    { check_numeric_terminator lexbuf; DECIMAL (normalize_decimal s) }

  (* Plain integer *)
  | dec_int as s
    { check_numeric_terminator lexbuf; INT (parse_int s) }

  (* ---------------------------------------------------------------- *)
  (* Quoted symbol  'text'                                             *)
  (* ---------------------------------------------------------------- *)
  | '\''
    { let buf = Buffer.create 32 in
      read_quoted_symbol buf lexbuf;
      QUOTED_SYMBOL (Buffer.contents buf) }

  (* ---------------------------------------------------------------- *)
  (* Long quoted string  '''text'''  (one segment; parser concatenates) *)
  (* ---------------------------------------------------------------- *)
  | "'''"
    { let buf = Buffer.create 128 in
      read_long_string buf lexbuf;
      LONG_STRING_PART (Buffer.contents buf) }

  (* ---------------------------------------------------------------- *)
  (* Short quoted string  "text"                                       *)
  (* ---------------------------------------------------------------- *)
  | '"'
    { let buf = Buffer.create 64 in
      read_short_string buf lexbuf;
      STRING (Buffer.contents buf) }

  (* ---------------------------------------------------------------- *)
  (* LOB literals  {{ ... }}                                           *)
  (* ---------------------------------------------------------------- *)
  | "{{"
    { read_lob lexbuf }

  (* ---------------------------------------------------------------- *)
  (* Punctuation                                                       *)
  (* ---------------------------------------------------------------- *)
  | "::"  { COLONCOLON }
  | ':'   { COLON      }
  | ','   { COMMA      }
  | '.'   { DOT        }
  | '['   { LBRACKET   }
  | ']'   { RBRACKET   }
  | '('   { LPAREN     }
  | ')'   { RPAREN     }
  | '{'   { LCURLY     }
  | '}'   { RCURLY     }

  (* ---------------------------------------------------------------- *)
  (* Type name keywords (also valid identifier symbols)               *)
  (* ---------------------------------------------------------------- *)
  | "bool"      { TYPE_NAME "bool"      }
  | "int"       { TYPE_NAME "int"       }
  | "float"     { TYPE_NAME "float"     }
  | "decimal"   { TYPE_NAME "decimal"   }
  | "timestamp" { TYPE_NAME "timestamp" }
  | "symbol"    { TYPE_NAME "symbol"    }
  | "string"    { TYPE_NAME "string"    }
  | "clob"      { TYPE_NAME "clob"      }
  | "blob"      { TYPE_NAME "blob"      }
  | "list"      { TYPE_NAME "list"      }
  | "sexp"      { TYPE_NAME "sexp"      }
  | "struct"    { TYPE_NAME "struct"    }

  (* ---------------------------------------------------------------- *)
  (* Symbol ID reference: $N where N is a non-negative integer       *)
  (* ---------------------------------------------------------------- *)
  | '$' (['0'-'9']+ as digits)
    { let n = int_of_string digits in
      check_numeric_terminator lexbuf;
      SYMBOL_ID n }

  (* ---------------------------------------------------------------- *)
  (* Identifier symbols                                                *)
  (* ---------------------------------------------------------------- *)
  | (id_start id_rest*) as s
    { SYMBOL s }

  (* ---------------------------------------------------------------- *)
  (* Operator characters — valid as symbols inside s-expressions.     *)
  (* We must not let the OPERATOR rule eat comment openers /* or //   *)
  (* via longest-match.  Strategy: treat '/' specially so that any    *)
  (* operator starting with '/' only matches if the next char is      *)
  (* neither '*' nor '/'.  Operators that start with a non-slash op   *)
  (* char are unrestricted.                                            *)
  (* ---------------------------------------------------------------- *)
  (* Operator starting with a non-slash op char (including '.') *)
  | (non_slash_op | '.') op_char* as s
    { OPERATOR s }
  (* Operator starting with '/', NOT followed by '*' or '/' (comment starters) *)
  | '/' (after_slash_op | '.') op_char* as s
    { OPERATOR s }
  (* Lone '/' (not followed by another op char) *)
  | '/'
    { OPERATOR "/" }

  | eof   { EOF }

  | _ as c
    { raise (Lexer_error (Printf.sprintf "Unexpected character: %C" c)) }

(* ------------------------------------------------------------------ *)
(* Block comment  /* ... */                                             *)
(* ------------------------------------------------------------------ *)
and block_comment = parse
  | "*/"  { () }
  | '\n'  { Lexing.new_line lexbuf; block_comment lexbuf }
  | '\r' '\n' { Lexing.new_line lexbuf; block_comment lexbuf }
  | '\r'  { Lexing.new_line lexbuf; block_comment lexbuf }
  | _     { block_comment lexbuf }
  | eof   { raise (Lexer_error "Unterminated block comment") }

(* ------------------------------------------------------------------ *)
(* Quoted symbol body  '...'                                           *)
(* Called after the opening single quote.                              *)
(* ------------------------------------------------------------------ *)
and read_quoted_symbol buf = parse
  | '\''        { () }    (* end of quoted symbol *)
  | '\\'        { process_escape buf false lexbuf;
                  read_quoted_symbol buf lexbuf }
  | '\n'        { Lexing.new_line lexbuf;
                  Buffer.add_char buf '\n';
                  read_quoted_symbol buf lexbuf }
  | '\r' '\n'   { Lexing.new_line lexbuf;
                  Buffer.add_char buf '\n';
                  read_quoted_symbol buf lexbuf }
  | '\r'        { Lexing.new_line lexbuf;
                  Buffer.add_char buf '\n';
                  read_quoted_symbol buf lexbuf }
  | [^ '\'' '\\' '\n' '\r']+ as s
                { Buffer.add_string buf s;
                  read_quoted_symbol buf lexbuf }
  | eof         { raise (Lexer_error "Unterminated quoted symbol") }

(* ------------------------------------------------------------------ *)
(* Short quoted string body  "..."                                      *)
(* Called after the opening double quote.                              *)
(* ------------------------------------------------------------------ *)
and read_short_string buf = parse
  | '"'         { () }    (* end of string *)
  | '\\'        { process_escape buf false lexbuf;
                  read_short_string buf lexbuf }
  | '\n' | '\r' { raise (Lexer_error "Newline inside short string") }
  | ['\x00'-'\x08' '\x0E'-'\x1F'] as c
                { raise (Lexer_error
                    (Printf.sprintf "Raw control character in string: %C" c)) }
  | [^ '"' '\\' '\n' '\r' '\x00'-'\x08' '\x0E'-'\x1F']+ as s
                { Buffer.add_string buf s;
                  read_short_string buf lexbuf }
  | eof         { raise (Lexer_error "Unterminated short string") }

(* ------------------------------------------------------------------ *)
(* Long quoted string body  '''...'''                                   *)
(* Called after the opening '''.                                       *)
(* Ocamllex longest-match handles ''' end correctly:                   *)
(*   '    -> one literal quote                                         *)
(*   ''   -> two literal quotes                                        *)
(*   '''  -> end of segment  (longest match wins)                      *)
(* ------------------------------------------------------------------ *)
and read_long_string buf = parse
  | "'''"       { () }    (* end of segment *)
  | "''"        { Buffer.add_string buf "''";
                  read_long_string buf lexbuf }
  | "'"         { Buffer.add_char buf '\'';
                  read_long_string buf lexbuf }
  | '\\'        { process_escape buf false lexbuf;
                  read_long_string buf lexbuf }
  | '\n'        { Lexing.new_line lexbuf;
                  Buffer.add_char buf '\n';
                  read_long_string buf lexbuf }
  | '\r' '\n'   { Lexing.new_line lexbuf;
                  Buffer.add_char buf '\n';
                  read_long_string buf lexbuf }
  | '\r'        { Lexing.new_line lexbuf;
                  Buffer.add_char buf '\n';
                  read_long_string buf lexbuf }
  | ['\x00'-'\x08' '\x0E'-'\x1F'] as c
                { raise (Lexer_error
                    (Printf.sprintf "Raw control character in long string: %C" c)) }
  | [^ '\'' '\\' '\n' '\r' '\x00'-'\x08' '\x0E'-'\x1F']+ as s
                { Buffer.add_string buf s;
                  read_long_string buf lexbuf }
  | eof         { raise (Lexer_error "Unterminated long string") }

(* ------------------------------------------------------------------ *)
(* Short CLOB body  "..."  inside {{ }}                                *)
(* Like short_string but clob_mode=true (no \u/\U escapes).           *)
(* ------------------------------------------------------------------ *)
and read_clob_short buf = parse
  | '"'         { () }
  | '\\'        { process_escape buf true lexbuf;
                  read_clob_short buf lexbuf }
  | '\n' | '\r' { raise (Lexer_error "Newline inside short CLOB string") }
  | ['\x00'-'\x08' '\x0E'-'\x1F'] as c
                { raise (Lexer_error
                    (Printf.sprintf "Control character in CLOB: %C" c)) }
  | ['\x80'-'\xff'] as c
                { raise (Lexer_error
                    (Printf.sprintf "Non-ASCII byte in CLOB: %C" c)) }
  | [^ '"' '\\' '\n' '\r' '\x00'-'\x08' '\x0E'-'\x1F' '\x80'-'\xff']+ as s
                { Buffer.add_string buf s;
                  read_clob_short buf lexbuf }
  | eof         { raise (Lexer_error "Unterminated CLOB string") }

(* ------------------------------------------------------------------ *)
(* Long CLOB body  '''...'''  inside {{ }}                             *)
(* ------------------------------------------------------------------ *)
and read_long_clob buf = parse
  | "'''"       { () }
  | "''"        { Buffer.add_string buf "''";
                  read_long_clob buf lexbuf }
  | "'"         { Buffer.add_char buf '\'';
                  read_long_clob buf lexbuf }
  | '\\'        { process_escape buf true lexbuf;
                  read_long_clob buf lexbuf }
  | '\n'        { Lexing.new_line lexbuf;
                  Buffer.add_char buf '\n';
                  read_long_clob buf lexbuf }
  | '\r' '\n'   { Lexing.new_line lexbuf;
                  Buffer.add_char buf '\n';
                  read_long_clob buf lexbuf }
  | '\r'        { Lexing.new_line lexbuf;
                  Buffer.add_char buf '\n';
                  read_long_clob buf lexbuf }
  | ['\x00'-'\x08' '\x0E'-'\x1F'] as c
                { raise (Lexer_error
                    (Printf.sprintf "Control character in long CLOB: %C" c)) }
  | ['\x80'-'\xff'] as c
                { raise (Lexer_error
                    (Printf.sprintf "Non-ASCII byte in long CLOB: %C" c)) }
  | [^ '\'' '\\' '\n' '\r' '\x00'-'\x08' '\x0E'-'\x1F' '\x80'-'\xff']+ as s
                { Buffer.add_string buf s;
                  read_long_clob buf lexbuf }
  | eof         { raise (Lexer_error "Unterminated long CLOB string") }

(* ------------------------------------------------------------------ *)
(* BLOB body  {{ base64 }}                                             *)
(* Called after {{ and any leading whitespace has been handled.        *)
(* ------------------------------------------------------------------ *)
and read_blob buf = parse
  | "}}"        { () }
  | [' ' '\t' '\x0B' '\x0C']+ { read_blob buf lexbuf }   (* skip ws *)
  | '\n'        { Lexing.new_line lexbuf; read_blob buf lexbuf }
  | '\r' '\n'   { Lexing.new_line lexbuf; read_blob buf lexbuf }
  | '\r'        { Lexing.new_line lexbuf; read_blob buf lexbuf }
  | ['0'-'9' 'a'-'z' 'A'-'Z' '+' '/' '=']+ as s
                { Buffer.add_string buf s; read_blob buf lexbuf }
  | eof         { raise (Lexer_error "Unterminated BLOB") }
  | _ as c      { raise (Lexer_error
                    (Printf.sprintf "Unexpected character in BLOB: %C" c)) }

(* ------------------------------------------------------------------ *)
(* Skip LOB whitespace, then dispatch to CLOB or BLOB                 *)
(* ------------------------------------------------------------------ *)
and read_lob = parse
  | [' ' '\t' '\x0B' '\x0C']+ { read_lob lexbuf }
  | '\n'        { Lexing.new_line lexbuf; read_lob lexbuf }
  | '\r' '\n'   { Lexing.new_line lexbuf; read_lob lexbuf }
  | '\r'        { Lexing.new_line lexbuf; read_lob lexbuf }
  (* Short-quoted CLOB *)
  | '"'
    { let buf = Buffer.create 64 in
      read_clob_short buf lexbuf;
      skip_lob_close lexbuf;
      CLOB (Buffer.contents buf) }
  (* Long-quoted CLOB — must start with ''' *)
  | "'''"
    { let buf = Buffer.create 128 in
      read_long_clob buf lexbuf;
      (* consume additional ''' segments *)
      read_lob_long_clob_rest buf lexbuf;
      skip_lob_close lexbuf;
      CLOB (Buffer.contents buf) }
  (* BLOB — anything else (base64 chars or immediately closing }}) *)
  | ""
    { let buf = Buffer.create 64 in
      read_blob buf lexbuf;
      let s = Buffer.contents buf in
      validate_blob s;
      BLOB s }

(* Consume additional ''' segments in a long CLOB, if any *)
and read_lob_long_clob_rest buf = parse
  | [' ' '\t' '\x0B' '\x0C']+ { read_lob_long_clob_rest buf lexbuf }
  | '\n'        { Lexing.new_line lexbuf; read_lob_long_clob_rest buf lexbuf }
  | '\r' '\n'   { Lexing.new_line lexbuf; read_lob_long_clob_rest buf lexbuf }
  | '\r'        { Lexing.new_line lexbuf; read_lob_long_clob_rest buf lexbuf }
  | "'''"
    { read_long_clob buf lexbuf;
      read_lob_long_clob_rest buf lexbuf }
  | ""          { () }   (* no more segments *)

(* Skip trailing whitespace and }} after a CLOB *)
and skip_lob_close = parse
  | [' ' '\t' '\x0B' '\x0C']+ { skip_lob_close lexbuf }
  | '\n'        { Lexing.new_line lexbuf; skip_lob_close lexbuf }
  | '\r' '\n'   { Lexing.new_line lexbuf; skip_lob_close lexbuf }
  | '\r'        { Lexing.new_line lexbuf; skip_lob_close lexbuf }
  | "}}"        { () }
  | eof         { raise (Lexer_error "Unterminated LOB: missing }}") }
  | _ as c      { raise (Lexer_error
                    (Printf.sprintf "Unexpected char after CLOB content: %C" c)) }

(* ------------------------------------------------------------------ *)
(* Escape sequence processor                                            *)
(* Called after reading a backslash.                                   *)
(* [clob_mode]: when true, \u and \U are not allowed.                 *)
(* ------------------------------------------------------------------ *)
and process_escape buf clob_mode = parse
  | 'a'   { Buffer.add_char buf '\x07' }
  | 'b'   { Buffer.add_char buf '\x08' }
  | 't'   { Buffer.add_char buf '\t'   }
  | 'n'   { Buffer.add_char buf '\n'   }
  | 'f'   { Buffer.add_char buf '\x0C' }
  | 'r'   { Buffer.add_char buf '\r'   }
  | 'v'   { Buffer.add_char buf '\x0B' }
  | '?'   { Buffer.add_char buf '?'    }
  | '0'   { Buffer.add_char buf '\x00' }
  | '\''  { Buffer.add_char buf '\''   }
  | '"'   { Buffer.add_char buf '"'    }
  | '/'   { Buffer.add_char buf '/'    }
  | '\\'  { Buffer.add_char buf '\\'   }
  | '\n'  { Lexing.new_line lexbuf     }   (* line continuation *)
  | '\r' '\n' { Lexing.new_line lexbuf }   (* line continuation *)
  | '\r'  { Lexing.new_line lexbuf     }   (* line continuation *)
  | 'x' (hex_digit hex_digit as s)
    { Buffer.add_char buf
        (Char.chr (hex_val s.[0] lsl 4 lor hex_val s.[1])) }
  | 'u' (hex_digit hex_digit hex_digit hex_digit as s)
    { if clob_mode then
        raise (Lexer_error "\\u escapes not allowed in CLOB")
      else begin
        let cp = int_of_string ("0x" ^ s) in
        if cp >= 0xD800 && cp <= 0xDBFF then begin
          (* High surrogate — must be followed by \uDC00-\uDFFF low surrogate *)
          let low = read_low_surrogate buf lexbuf in
          let full_cp = 0x10000 + (cp - 0xD800) * 0x400 + (low - 0xDC00) in
          utf8_of_codepoint buf full_cp
        end else if cp >= 0xDC00 && cp <= 0xDFFF then
          raise (Lexer_error
            (Printf.sprintf "Unpaired low surrogate: U+%04X" cp))
        else
          utf8_of_codepoint buf cp
      end }
  | 'U' ('0' '0' '0' hex_digit hex_digit hex_digit hex_digit hex_digit as s)
    { if clob_mode then
        raise (Lexer_error "\\U escapes not allowed in CLOB")
      else begin
        let cp = int_of_string ("0x" ^ s) in
        utf8_of_codepoint buf cp
      end }
  | 'U' ('0' '0' '1' '0' hex_digit hex_digit hex_digit hex_digit as s)
    { if clob_mode then
        raise (Lexer_error "\\U escapes not allowed in CLOB")
      else begin
        let cp = int_of_string ("0x" ^ s) in
        utf8_of_codepoint buf cp
      end }
  | eof   { raise (Lexer_error "Unexpected EOF in escape sequence") }
  | _ as c { raise (Lexer_error
               (Printf.sprintf "Unknown escape sequence: \\%c" c)) }

(* ------------------------------------------------------------------ *)
(* Low surrogate reader: expects \uDC00-\uDFFF after a high surrogate *)
(* ------------------------------------------------------------------ *)
and read_low_surrogate _buf = parse
  | '\\' 'u' (hex_digit hex_digit hex_digit hex_digit as s)
    { let cp = int_of_string ("0x" ^ s) in
      if cp >= 0xDC00 && cp <= 0xDFFF then cp
      else raise (Lexer_error
        (Printf.sprintf "Expected low surrogate (U+DC00..U+DFFF) but got U+%04X" cp)) }
  | eof   { raise (Lexer_error "Expected low surrogate but got EOF") }
  | _ as c { raise (Lexer_error
        (Printf.sprintf "Expected low surrogate (\\uDCxx) but got: %C" c)) }
