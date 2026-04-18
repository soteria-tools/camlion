(** Run the amazon-ion/ion-tests conformance suite (text + binary formats).
    Good files must parse without exception; bad files must raise. *)

open Camlion

(* -------------------------------------------------------------------------- *)
(* Helpers                                                                     *)
(* -------------------------------------------------------------------------- *)

let ion_tests_root =
  (* ION_TESTS_ROOT is set by the dune (action (setenv ...)) stanza.
     Fall back to CWD-relative path for manual runs. *)
  match Sys.getenv_opt "ION_TESTS_ROOT" with
  | Some p -> p
  | None   -> Filename.concat (Sys.getcwd ()) "ion-tests/iontestdata"

(** Recursively collect all files matching [pred] under [dir]. *)
let rec collect_files pred dir =
  let entries = Sys.readdir dir in
  Array.sort String.compare entries;
  Array.fold_left (fun acc name ->
    let path = Filename.concat dir name in
    if Sys.is_directory path then acc @ collect_files pred path
    else if pred name then acc @ [path]
    else acc
  ) [] entries

let is_ion_text name =
  Filename.check_suffix name ".ion" &&
  (* Skip UTF-16 and UTF-32 encoded files — they require BOM-based
     transcoding which is not yet implemented. *)
  not (List.mem (Filename.basename name) ["utf16.ion"; "utf32.ion"])

let is_ion_binary name =
  Filename.check_suffix name ".10n"

(** Strip [ion_tests_root] prefix for a short test name. *)
let short_name path =
  let root = ion_tests_root ^ Filename.dir_sep in
  let n = String.length root in
  if String.length path > n && String.sub path 0 n = root
  then String.sub path n (String.length path - n)
  else path

(* -------------------------------------------------------------------------- *)
(* Test builders                                                               *)
(* -------------------------------------------------------------------------- *)

let good_test path =
  let name = short_name path in
  Alcotest.test_case name `Quick (fun () ->
    let ic = open_in path in
    let parse =
      if Filename.check_suffix path ".10n" then Binary.of_channel
      else Text.of_channel
    in
    match parse ic with
    | _ -> close_in ic
    | exception exn ->
      close_in_noerr ic;
      Alcotest.failf "Expected parse success but got: %s" (Printexc.to_string exn)
  )

let bad_test path =
  let name = short_name path in
  Alcotest.test_case name `Quick (fun () ->
    let ic = open_in path in
    let parse =
      if Filename.check_suffix path ".10n" then Binary.of_channel
      else Text.of_channel
    in
    match parse ic with
    | _ ->
      close_in ic;
      Alcotest.failf "Expected parse failure but parsing succeeded"
    | exception _ ->
      close_in_noerr ic
  )

(* -------------------------------------------------------------------------- *)
(* Main                                                                        *)
(* -------------------------------------------------------------------------- *)

let () =
  let good_dir = Filename.concat ion_tests_root "good" in
  let bad_dir  = Filename.concat ion_tests_root "bad"  in
  let good_files =
    collect_files is_ion_text good_dir @
    collect_files is_ion_binary good_dir
  in
  let bad_files =
    collect_files is_ion_text bad_dir @
    collect_files is_ion_binary bad_dir
  in
  Alcotest.run "ion-tests conformance"
    [ "good", List.map good_test good_files
    ; "bad",  List.map bad_test  bad_files
    ]
