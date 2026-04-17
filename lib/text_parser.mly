%{
(** Ion text format parser (menhir).

    Annotations are handled via right-recursive rules that explicitly match
    [SYMBOL/TYPE_NAME/QUOTED_SYMBOL COLONCOLON value], so the COLONCOLON token
    is always the 1-token lookahead that decides "annotation vs entity".
    This avoids the LALR(1) shift/reduce conflict that arises from
    [list(annotation)] whose empty-reduction lookahead overlaps with the
    FIRST set of entity.

    Trailing commas in lists and structs are handled with left-recursive
    [list_values] / [struct_fields] rules where the trailing COMMA
    alternative has RBRACKET/RCURLY as the deciding lookahead.
*)

open Ion
%}

(* ------------------------------------------------------------------ *)
(* Token declarations                                                   *)
(* ------------------------------------------------------------------ *)

%token <Ion.ion_type> NULL_TYPED   (* null.bool, null.int, …       *)
%token                NULL         (* bare null                     *)
%token <bool>         BOOL
%token <Z.t>          INT
%token <float>        FLOAT
%token <string>       DECIMAL
%token <string>       TIMESTAMP
%token <string>       SYMBOL       (* identifier: foo, $bar, _x    *)
%token <string>       TYPE_NAME    (* bool int float … (also syms) *)
%token <string>       QUOTED_SYMBOL
%token <int>          SYMBOL_ID    (* bare $N symbol ID reference   *)
%token <string>       STRING
%token <string>       LONG_STRING_PART  (* one '''...''' segment    *)
%token <string>       CLOB
%token <string>       BLOB

%token LBRACKET RBRACKET           (* [ ]  *)
%token LPAREN   RPAREN             (* ( )  *)
%token LCURLY   RCURLY             (* { }  *)
%token COMMA                       (* ,    *)
%token COLON                       (* :    *)
%token COLONCOLON                  (* ::   *)
%token DOT                         (* .    *)
%token <string> OPERATOR           (* !#%&*+-/;<=>?@^`|~  *)

%token EOF

(* ------------------------------------------------------------------ *)
(* Entry point                                                          *)
(* ------------------------------------------------------------------ *)

%start <Ion.ion_value list> top_level

%%

(* ------------------------------------------------------------------ *)
(* Top level: a stream of Ion values                                    *)
(* ------------------------------------------------------------------ *)

top_level:
  | EOF           { [] }
  | v = value; vs = top_level  { v :: vs }

(* ------------------------------------------------------------------ *)
(* Value: zero or more annotations followed by a datum.                *)
(*                                                                      *)
(* Annotations are right-recursive to ensure the COLONCOLON token is   *)
(* always the 1-token lookahead that distinguishes annotation from      *)
(* symbol entity, avoiding LALR(1) conflicts.                          *)
(* ------------------------------------------------------------------ *)

value:
  | d = entity
    { { annotations = []; value = d } }
  | s = SYMBOL       COLONCOLON v = value
    { { v with annotations = s :: v.annotations } }
  | s = TYPE_NAME    COLONCOLON v = value
    { { v with annotations = s :: v.annotations } }
  | s = QUOTED_SYMBOL COLONCOLON v = value
    { { v with annotations = s :: v.annotations } }
  | n = SYMBOL_ID    COLONCOLON v = value
    { { v with annotations = (Printf.sprintf "$%d" n) :: v.annotations } }

(* ------------------------------------------------------------------ *)
(* Entity: the unannotated datum                                        *)
(* ------------------------------------------------------------------ *)

entity:
  | NULL              { Null Null_type }
  | t = NULL_TYPED    { Null t         }
  | b = BOOL          { Bool b         }
  | n = INT           { Int n          }
  | f = FLOAT         { Float f        }
  | s = DECIMAL       { Decimal s      }
  | s = TIMESTAMP     { Timestamp s    }
  | s = SYMBOL        { Symbol s       }
  | s = TYPE_NAME     { Symbol s       }
  | s = QUOTED_SYMBOL { Symbol s       }
  | n = SYMBOL_ID     { SymbolId n     }
  | s = STRING        { String s       }
  | s = long_string   { String s       }
  | s = CLOB          { Clob s         }
  | s = BLOB          { Blob s         }
  | v = ion_list      { List v         }
  | v = sexp          { Sexp v         }
  | v = ion_struct    { Struct v       }

(* One or more adjacent long-string segments, concatenated.            *)
(* Right-recursion is fine: after LONG_STRING_PART, shift if another   *)
(* LONG_STRING_PART follows, otherwise reduce.  No other conflict.     *)
long_string:
  | s = LONG_STRING_PART                       { s }
  | s = LONG_STRING_PART rest = long_string    { s ^ rest }

(* ------------------------------------------------------------------ *)
(* List   [ v1, v2, v3 ]   — optional trailing comma                   *)
(* Left-recursive to avoid shift/reduce on COMMA.                      *)
(* ------------------------------------------------------------------ *)

list_values:
  | v = value                           { [v]        }
  | vs = list_values COMMA v = value    { vs @ [v]   }

ion_list:
  | LBRACKET RBRACKET                          { [] }
  | LBRACKET vs = list_values RBRACKET         { vs }
  | LBRACKET vs = list_values COMMA RBRACKET   { vs }   (* trailing comma *)

(* ------------------------------------------------------------------ *)
(* S-expression   ( v1 v2 v3 )                                         *)
(* Operators are valid entities here; space-separated (no commas).    *)
(* ------------------------------------------------------------------ *)

sexp:
  | LPAREN RPAREN                       { [] }
  | LPAREN vs = sexp_elems RPAREN       { vs }

sexp_elems:
  | sv = sexp_value                     { [sv]       }
  | svs = sexp_elems sv = sexp_value    { svs @ [sv] }

(* A value inside an s-expression: same as value but operators are     *)
(* also valid entities (they become Symbol values).                    *)
sexp_value:
  | d = sexp_entity
    { { annotations = []; value = d } }
  | s = SYMBOL        COLONCOLON sv = sexp_value
    { { sv with annotations = s :: sv.annotations } }
  | s = TYPE_NAME     COLONCOLON sv = sexp_value
    { { sv with annotations = s :: sv.annotations } }
  | s = QUOTED_SYMBOL COLONCOLON sv = sexp_value
    { { sv with annotations = s :: sv.annotations } }
  | n = SYMBOL_ID     COLONCOLON sv = sexp_value
    { { sv with annotations = (Printf.sprintf "$%d" n) :: sv.annotations } }

sexp_entity:
  | NULL              { Null Null_type }
  | t = NULL_TYPED    { Null t         }
  | b = BOOL          { Bool b         }
  | n = INT           { Int n          }
  | f = FLOAT         { Float f        }
  | s = DECIMAL       { Decimal s      }
  | s = TIMESTAMP     { Timestamp s    }
  | s = SYMBOL        { Symbol s       }
  | s = TYPE_NAME     { Symbol s       }
  | s = QUOTED_SYMBOL { Symbol s       }
  | n = SYMBOL_ID     { SymbolId n     }
  | s = STRING        { String s       }
  | s = long_string   { String s       }
  | s = CLOB          { Clob s         }
  | s = BLOB          { Blob s         }
  | v = ion_list      { List v         }
  | v = sexp          { Sexp v         }
  | v = ion_struct    { Struct v       }
  | op = operator     { Symbol op      }

operator:
  | s = OPERATOR      { s   }
  | DOT               { "." }

(* ------------------------------------------------------------------ *)
(* Struct   { name: value, ... }   — optional trailing comma           *)
(* ------------------------------------------------------------------ *)

ion_struct:
  | LCURLY RCURLY                                  { [] }
  | LCURLY fs = struct_fields RCURLY               { fs }
  | LCURLY fs = struct_fields COMMA RCURLY         { fs }   (* trailing comma *)

struct_fields:
  | f = field                               { [f]      }
  | fs = struct_fields COMMA f = field      { fs @ [f] }

field:
  | name = field_name COLON v = value       { (name, v) }

(** Field names: identifier symbols, type keywords, quoted symbols,
    or plain strings (whose text becomes the field name). *)
field_name:
  | s = SYMBOL        { s }
  | s = TYPE_NAME     { s }
  | s = QUOTED_SYMBOL { s }
  | s = STRING        { s }
  | s = long_string   { s }
  | n = SYMBOL_ID     { Printf.sprintf "$%d" n }
