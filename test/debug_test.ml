let () =
  let inputs = [
    ("hello",        "hello");
    ("dollar",       "$42");
    ("underscore",   "_x");
    ("quoted sym",   "'hello world'");
    ("type sym",     "int");
    ("empty sym",    "''");
    ("trail comma",  "[1, 2,]");
    ("struct trail", "{x: 1,}");
    ("struct quote", "{'hello world': null}");
    ("long str key", "{ '''foo''' : bar }");
    ("empty long",   "''''''");
    ("empty long key","{ '''''': bar }");
    ("126d0",        "126d0");
  ] in
  List.iter (fun (label, s) ->
    match Ocaml_ion.Text.of_string s with
    | _ -> Printf.printf "[OK]   %s\n%!" label
    | exception exn ->
      Printf.printf "[FAIL] %s => %s\n%!" label (Printexc.to_string exn)
  ) inputs

let () =
  if Array.length Sys.argv > 1 then begin
    let file = Sys.argv.(1) in
    let ic = open_in file in
    match Ocaml_ion.Text.of_channel ic with
    | _ -> print_endline "OK"
    | exception e -> Printf.printf "FAIL: %s\n" (Printexc.to_string e)
  end
