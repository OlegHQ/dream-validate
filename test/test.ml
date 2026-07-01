open Dream_validate

let check_ok name expected actual =
  Alcotest.(check (result string reject)) name (Ok expected) actual

let test_form_string () =
  let source =
    Source.of_fields [ ("username", "  alice "); ("media_id", "m1"); ("media_id", "m2") ]
  in
  check_ok "trimmed username" "alice"
    (Form.decode source
       (Form.field ~trim:true
          ~validators:[ Validation.length ~min:3 ~max:40 (); Validation.charset "username" ]
          "username"));
  Alcotest.(check (result (list string) reject))
    "repeated fields"
    (Ok [ "m1"; "m2" ])
    (Form.decode source (Form.list_field "media_id"))

let test_form_errors () =
  let source = Source.of_fields [ ("username", "!!") ] in
  match
    Form.decode source
      (Form.field ~trim:true
         ~validators:[ Validation.length ~min:3 ~max:40 (); Validation.charset "username" ]
         "username")
  with
  | Ok _ -> Alcotest.fail "expected validation errors"
  | Error errors ->
      Alcotest.(check int) "error count" 2 (List.length errors);
      Alcotest.(check string) "first field" "username" (List.hd errors).field

let test_json () =
  let source =
    match
      Json.source
        (`Assoc
          [
            ("name", `String " Poster ");
            ("count", `Int 2);
            ("tags", `List [ `String " ocaml "; `String "dream" ]);
            ("active", `Bool true);
          ])
    with
    | Ok source -> source
    | Error _ -> Alcotest.fail "expected JSON object"
  in
  check_ok "json string" "Poster" (Json.string_field ~trim:true "name" source);
  Alcotest.(check (result int reject))
    "json int" (Ok 2) (Json.int_field "count" source);
  Alcotest.(check (result (list string) reject))
    "json string list" (Ok [ "ocaml"; "dream" ])
    (Json.list_string_field ~trim:true "tags" source);
  Alcotest.(check (result bool reject))
    "json bool" (Ok true) (Json.bool_field "active" source)

let () =
  Alcotest.run "dream-validate"
    [
      ("form", [ Alcotest.test_case "string" `Quick test_form_string; Alcotest.test_case "errors" `Quick test_form_errors ]);
      ("json", [ Alcotest.test_case "fields" `Quick test_json ]);
    ]
