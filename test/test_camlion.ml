open Alcotest
open Camlion

(* ------------------------------------------------------------------ *)
(* Helpers                                                              *)
(* ------------------------------------------------------------------ *)

let parse s = Text.of_string s

let parse1 s =
  match parse s with
  | [v] -> v
  | vs  -> Alcotest.failf "Expected 1 value, got %d" (List.length vs)

let ion_value_testable =
  testable
    (fun ppf v -> Format.pp_print_string ppf (Ion.to_string v))
    (fun a b -> Ion.to_string a = Ion.to_string b)

let check_value msg expected input =
  check ion_value_testable msg expected (parse1 input)

let check_data msg expected input =
  check ion_value_testable msg
    { Ion.annotations = []; value = expected }
    (parse1 input)

let check_list msg expected input =
  check (list ion_value_testable) msg expected (parse input)

(* ------------------------------------------------------------------ *)
(* Null tests                                                           *)
(* ------------------------------------------------------------------ *)

let test_null () =
  check_data "bare null"        (Ion.Null Ion.Null_type)      "null";
  check_data "null.null"        (Ion.Null Ion.Null_type)      "null.null";
  check_data "null.bool"        (Ion.Null Ion.Bool_type)      "null.bool";
  check_data "null.int"         (Ion.Null Ion.Int_type)       "null.int";
  check_data "null.float"       (Ion.Null Ion.Float_type)     "null.float";
  check_data "null.decimal"     (Ion.Null Ion.Decimal_type)   "null.decimal";
  check_data "null.timestamp"   (Ion.Null Ion.Timestamp_type) "null.timestamp";
  check_data "null.symbol"      (Ion.Null Ion.Symbol_type)    "null.symbol";
  check_data "null.string"      (Ion.Null Ion.String_type)    "null.string";
  check_data "null.clob"        (Ion.Null Ion.Clob_type)      "null.clob";
  check_data "null.blob"        (Ion.Null Ion.Blob_type)      "null.blob";
  check_data "null.list"        (Ion.Null Ion.List_type)      "null.list";
  check_data "null.sexp"        (Ion.Null Ion.Sexp_type)      "null.sexp";
  check_data "null.struct"      (Ion.Null Ion.Struct_type)    "null.struct"

(* ------------------------------------------------------------------ *)
(* Bool tests                                                           *)
(* ------------------------------------------------------------------ *)

let test_bool () =
  check_data "true"  (Ion.Bool true)  "true";
  check_data "false" (Ion.Bool false) "false"

(* ------------------------------------------------------------------ *)
(* Int tests                                                            *)
(* ------------------------------------------------------------------ *)

let test_int () =
  check_data "zero"           (Ion.Int Z.zero)            "0";
  check_data "positive"       (Ion.Int (Z.of_int 42))     "42";
  check_data "negative"       (Ion.Int (Z.of_int (-1)))   "-1";
  check_data "underscores"    (Ion.Int (Z.of_int 1000000))"1_000_000";
  check_data "hex"            (Ion.Int (Z.of_int 255))    "0xFF";
  check_data "hex lower"      (Ion.Int (Z.of_int 255))    "0xff";
  check_data "hex negative"   (Ion.Int (Z.of_int (-255))) "-0xFF";
  check_data "binary"         (Ion.Int (Z.of_int 10))     "0b1010";
  check_data "binary neg"     (Ion.Int (Z.of_int (-10)))  "-0b1010"

(* ------------------------------------------------------------------ *)
(* Float tests                                                          *)
(* ------------------------------------------------------------------ *)

let test_float () =
  check_data "nan"      (Ion.Float Float.nan)       "nan";
  check_data "+inf"     (Ion.Float infinity)        "+inf";
  check_data "-inf"     (Ion.Float neg_infinity)    "-inf";
  check_data "1.0e0"    (Ion.Float 1.0)             "1.0e0";
  check_data "1e10"     (Ion.Float 1e10)            "1e10";
  check_data "-2.5E3"   (Ion.Float (-2500.0))       "-2.5E3"

(* ------------------------------------------------------------------ *)
(* Decimal tests                                                        *)
(* ------------------------------------------------------------------ *)

let test_decimal () =
  check_data "simple decimal"   (Ion.Decimal "1.5")     "1.5";
  check_data "decimal exp"      (Ion.Decimal "1d3")     "1d3";
  check_data "decimal exp neg"  (Ion.Decimal "1.5d-3")  "1.5d-3";
  check_data "bare dot"         (Ion.Decimal "1.")      "1.";
  check_data "negative dec"     (Ion.Decimal "-0.")     "-0."

(* ------------------------------------------------------------------ *)
(* Timestamp tests                                                      *)
(* ------------------------------------------------------------------ *)

let test_timestamp () =
  check_data "year"           (Ion.Timestamp "2003T")            "2003T";
  check_data "year-month"     (Ion.Timestamp "2003-12T")         "2003-12T";
  check_data "date"           (Ion.Timestamp "2003-12-01T")      "2003-12-01T";
  check_data "date+time"
    (Ion.Timestamp "2003-12-01T00:00:00Z")
    "2003-12-01T00:00:00Z";
  check_data "date+time+frac"
    (Ion.Timestamp "2003-12-01T00:00:00.000Z")
    "2003-12-01T00:00:00.000Z";
  check_data "date+time+offset"
    (Ion.Timestamp "2003-12-01T08:30:00+08:00")
    "2003-12-01T08:30:00+08:00"

(* ------------------------------------------------------------------ *)
(* Symbol tests                                                         *)
(* ------------------------------------------------------------------ *)

let test_symbol () =
  check_data "identifier"     (Ion.Symbol "hello")        "hello";
  check_data "in-range sid"   (Ion.SymbolId 3)            "$3";
  check_data "underscore"     (Ion.Symbol "_x")           "_x";
  check_data "quoted"         (Ion.Symbol "hello world")  "'hello world'";
  check_data "type as sym"    (Ion.Symbol "int")          "int";
  check_data "quoted empty"   (Ion.Symbol "")             "''"

(* ------------------------------------------------------------------ *)
(* String tests                                                         *)
(* ------------------------------------------------------------------ *)

let test_string () =
  check_data "simple"         (Ion.String "hello")         {|"hello"|};
  check_data "empty"          (Ion.String "")              {|""|};
  check_data "escape nl"      (Ion.String "a\nb")          {|"a\nb"|};
  check_data "escape tab"     (Ion.String "a\tb")          {|"a\tb"|};
  check_data "escape quote"   (Ion.String "a\"b")          {|"a\"b"|};
  check_data "long string"    (Ion.String "hello world")   {|'''hello world'''|};
  check_data "long concat"    (Ion.String "helloworld")    {|'''hello''' '''world'''|}

(* ------------------------------------------------------------------ *)
(* Clob tests                                                           *)
(* ------------------------------------------------------------------ *)

let test_clob () =
  check_data "short clob"    (Ion.Clob "hello")    {|{{"hello"}}|};
  check_data "clob escape"   (Ion.Clob "a\tb")     {|{{"a\tb"}}|}

(* ------------------------------------------------------------------ *)
(* Blob tests                                                           *)
(* ------------------------------------------------------------------ *)

let test_blob () =
  check_data "blob"          (Ion.Blob "aGVsbG8=")   "{{aGVsbG8=}}";
  check_data "blob ws"       (Ion.Blob "aGVsbG8=")   "{{ aGVsbG8= }}";
  check_data "empty blob"    (Ion.Blob "")            "{{}}"

(* ------------------------------------------------------------------ *)
(* List tests                                                           *)
(* ------------------------------------------------------------------ *)

let test_list () =
  check_data "empty list"    (Ion.List [])                          "[]";
  check_data "one element"   (Ion.List [Ion.int_ (Z.of_int 1)])    "[1]";
  check_data "three elems"
    (Ion.List [Ion.int_ Z.zero; Ion.bool_ true; Ion.string_ "x"])
    {|[0, true, "x"]|};
  check_data "trailing comma"
    (Ion.List [Ion.int_ (Z.of_int 1); Ion.int_ (Z.of_int 2)])
    "[1, 2,]";
  check_data "nested list"
    (Ion.List [Ion.list_ [Ion.int_ (Z.of_int 1)]])
    "[[1]]"

(* ------------------------------------------------------------------ *)
(* Sexp tests                                                           *)
(* ------------------------------------------------------------------ *)

let test_sexp () =
  check_data "empty sexp"   (Ion.Sexp [])   "()";
  check_data "sexp values"
    (Ion.Sexp [Ion.int_ (Z.of_int 1); Ion.int_ (Z.of_int 2)])
    "(1 2)";
  check_data "sexp operator"
    (Ion.Sexp [Ion.symbol "+"; Ion.int_ (Z.of_int 1); Ion.int_ (Z.of_int 2)])
    "(+ 1 2)";
  check_data "nested sexp"
    (Ion.Sexp
      [ Ion.symbol "*"
      ; Ion.sexp [Ion.symbol "+"; Ion.int_ (Z.of_int 1); Ion.int_ (Z.of_int 2)]
      ; Ion.int_ (Z.of_int 3) ])
    "(* (+ 1 2) 3)"

(* ------------------------------------------------------------------ *)
(* Struct tests                                                         *)
(* ------------------------------------------------------------------ *)

let test_struct () =
  check_data "empty struct"   (Ion.Struct [])   "{}";
  check_data "one field"
    (Ion.Struct [("name", Ion.string_ "Alice")])
    {|{name: "Alice"}|};
  check_data "two fields"
    (Ion.Struct [("a", Ion.int_ (Z.of_int 1)); ("b", Ion.bool_ true)])
    "{a: 1, b: true}";
  check_data "quoted field name"
    (Ion.Struct [("hello world", Ion.null)])
    {|{'hello world': null}|};
  check_data "string field name"
    (Ion.Struct [("foo", Ion.null)])
    {|{"foo": null}|};
  check_data "trailing comma"
    (Ion.Struct [("x", Ion.int_ (Z.of_int 1))])
    "{x: 1,}"

(* ------------------------------------------------------------------ *)
(* Annotation tests                                                     *)
(* ------------------------------------------------------------------ *)

let test_annotations () =
  check_value "one annotation"
    { Ion.annotations = ["foo"]; value = Ion.Int (Z.of_int 42) }
    "foo::42";
  check_value "two annotations"
    { Ion.annotations = ["a"; "b"]; value = Ion.Bool true }
    "a::b::true";
  check_value "quoted annotation"
    { Ion.annotations = ["hello world"]; value = Ion.Null Ion.Null_type }
    "'hello world'::null";
  check_value "type name annotation"
    { Ion.annotations = ["int"]; value = Ion.String "x" }
    {|int::"x"|}

(* ------------------------------------------------------------------ *)
(* Multi-value stream tests                                             *)
(* ------------------------------------------------------------------ *)

let test_stream () =
  check_list "two values"
    [ { Ion.annotations = []; value = Ion.Int (Z.of_int 1) }
    ; { Ion.annotations = []; value = Ion.Int (Z.of_int 2) } ]
    "1 2";
  check_list "empty stream"  []  "";
  check_list "with comments"
    [ { Ion.annotations = []; value = Ion.Bool true } ]
    "// comment\ntrue /* also comment */"

(* ------------------------------------------------------------------ *)
(* Comment tests                                                        *)
(* ------------------------------------------------------------------ *)

let test_comments () =
  check_data "inline comment"  (Ion.Int (Z.of_int 1))  "// comment\n1";
  check_data "block comment"   (Ion.Int (Z.of_int 1))  "/* hi */ 1";
  check_data "mid-struct comment"
    (Ion.Struct [("a", Ion.int_ (Z.of_int 1)); ("b", Ion.int_ (Z.of_int 2))])
    "{a: 1, /* comment */ b: 2}"

(* ------------------------------------------------------------------ *)
(* Round-trip tests (parse -> to_string -> parse -> compare)           *)
(* ------------------------------------------------------------------ *)

let roundtrip_check msg input =
  let v = parse1 input in
  let s = Ion.to_string v in
  let v2 = parse1 s in
  check ion_value_testable (msg ^ " (roundtrip)") v v2

let test_roundtrip () =
  roundtrip_check "null"        "null";
  roundtrip_check "bool"        "true";
  roundtrip_check "int"         "42";
  roundtrip_check "float"       "1.5e0";
  roundtrip_check "decimal"     "1.5";
  roundtrip_check "symbol"      "hello";
  roundtrip_check "string"      {|"world"|};
  roundtrip_check "list"        "[1, 2, 3]";
  roundtrip_check "struct"      {|{a: 1, b: "x"}|};
  roundtrip_check "annotated"   "foo::42"

(* ------------------------------------------------------------------ *)
(* Test registration                                                    *)
(* ------------------------------------------------------------------ *)

let () =
  run "Ion text parser" [
    "null",        [ test_case "all null types"      `Quick test_null       ];
    "bool",        [ test_case "true/false"           `Quick test_bool       ];
    "int",         [ test_case "integers"             `Quick test_int        ];
    "float",       [ test_case "floats"               `Quick test_float      ];
    "decimal",     [ test_case "decimals"             `Quick test_decimal    ];
    "timestamp",   [ test_case "timestamps"           `Quick test_timestamp  ];
    "symbol",      [ test_case "symbols"              `Quick test_symbol     ];
    "string",      [ test_case "strings"              `Quick test_string     ];
    "clob",        [ test_case "clobs"                `Quick test_clob       ];
    "blob",        [ test_case "blobs"                `Quick test_blob       ];
    "list",        [ test_case "lists"                `Quick test_list       ];
    "sexp",        [ test_case "s-expressions"        `Quick test_sexp       ];
    "struct",      [ test_case "structs"              `Quick test_struct     ];
    "annotations", [ test_case "annotations"          `Quick test_annotations];
    "stream",      [ test_case "multi-value stream"   `Quick test_stream     ];
    "comments",    [ test_case "comments"             `Quick test_comments   ];
    "roundtrip",   [ test_case "parse/print/parse"    `Quick test_roundtrip  ];
  ]
