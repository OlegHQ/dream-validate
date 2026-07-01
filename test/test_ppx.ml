type registration_form = {
  username : string
    [@form.trim]
    [@validate.min_length 3]
    [@validate.max_length 40]
    [@validate.charset "username"];
  password : string [@validate.min_length 8];
}
[@@deriving dream_form]

type post_form = {
  body : string [@form.trim] [@validate.required] [@validate.max_length 5000];
  media_ids : string list [@form.key "media_id"];
}
[@@deriving dream_form]

type api_post = {
  body : string [@json.trim] [@validate.required] [@validate.max_length 5000];
  media_ids : string list [@json.key "mediaIds"];
  draft : bool option;
}
[@@deriving dream_json]

let source fields = Dream_validate.Source.of_fields fields

let test_registration_ok () =
  match
    registration_form_of_source
      (source [ ("username", " alice "); ("password", "password123") ])
  with
  | Ok form ->
      Alcotest.(check string) "username" "alice" form.username;
      Alcotest.(check string) "password" "password123" form.password
  | Error errors ->
      Alcotest.failf "unexpected errors: %s"
        (Dream_validate.errors_to_string errors)

let test_registration_errors () =
  match
    registration_form_of_source
      (source [ ("username", "!!"); ("password", "short") ])
  with
  | Ok _ -> Alcotest.fail "expected errors"
  | Error errors ->
      Alcotest.(check int) "error count" 3 (List.length errors)

let test_post_form () =
  match
    post_form_of_source
      (source [ ("body", " hello "); ("media_id", "m1"); ("media_id", "m2") ])
  with
  | Ok form ->
      Alcotest.(check string) "body" "hello" form.body;
      Alcotest.(check (list string)) "media ids" [ "m1"; "m2" ] form.media_ids
  | Error errors ->
      Alcotest.failf "unexpected errors: %s"
        (Dream_validate.errors_to_string errors)

let test_api_post_json () =
  match
    api_post_of_json
      (`Assoc
        [
          ("body", `String " hello ");
          ("mediaIds", `List [ `String "m1"; `String "m2" ]);
          ("draft", `Bool true);
        ])
  with
  | Ok form ->
      Alcotest.(check string) "body" "hello" form.body;
      Alcotest.(check (list string)) "media ids" [ "m1"; "m2" ] form.media_ids;
      Alcotest.(check (option bool)) "draft" (Some true) form.draft
  | Error errors ->
      Alcotest.failf "unexpected errors: %s"
        (Dream_validate.errors_to_string errors)

let test_api_post_json_errors () =
  match
    api_post_of_json (`Assoc [ ("body", `String ""); ("mediaIds", `String "m1") ])
  with
  | Ok _ -> Alcotest.fail "expected errors"
  | Error errors ->
      Alcotest.(check int) "error count" 2 (List.length errors)

let () =
  Alcotest.run "dream-validate-ppx"
    [
      ( "dream_form",
        [
          Alcotest.test_case "registration ok" `Quick test_registration_ok;
          Alcotest.test_case "registration errors" `Quick test_registration_errors;
          Alcotest.test_case "post form" `Quick test_post_form;
        ] );
      ( "dream_json",
        [
          Alcotest.test_case "api post json" `Quick test_api_post_json;
          Alcotest.test_case "api post json errors" `Quick
            test_api_post_json_errors;
        ] );
    ]
