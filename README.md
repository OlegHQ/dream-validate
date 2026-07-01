# dream-validate

Typed Dream request decoding and declarative DTO validation for OCaml web apps.

The package provides:

- `Dream_validate.Form` runtime decoders for form/query-style field lists.
- `Dream_validate.Query`, `Dream_validate.Route`, and
  `Dream_validate.Session` adapters for non-body Dream boundaries.
- `Dream_validate.Json` runtime decoders for JSON object fields and request
  bodies.
- `Dream_validate.Validation` reusable validators and structured errors.
- `dream-validate.ppx`, derivers that generate record DTO decoders from field
  attributes.

The first target use case is server-rendered Dream applications that want typed
request DTOs without handwritten `List.assoc_opt` plumbing.

## Example

```ocaml
type registration_form = {
  username : string
    [@form.trim]
    [@validate.min_length 3]
    [@validate.max_length 40]
    [@validate.charset "username"];
  password : string [@validate.min_length 8];
}
[@@deriving dream_form]

let register request =
  match registration_form_of_request request with
  | Ok form -> (* use typed DTO *)
  | Error errors -> (* render validation messages *)
```

JSON DTOs use the same validation attributes:

```ocaml
type post_json = {
  body : string [@json.trim] [@validate.required] [@validate.max_length 5000];
  media_ids : string list [@json.key "mediaIds"];
  draft : bool option;
}
[@@deriving dream_json]

let create_from_api request =
  match post_json_of_json_request request with
  | Ok dto -> (* use typed DTO *)
  | Error errors -> (* return a 400/422 response *)
```

Supported field attributes:

- `[@form.key "field_name"]`
- `[@form.trim]`
- `[@session.key "session_field_name"]`
- `[@session.csv]` for `string list` fields stored as comma-separated session
  values
- `[@json.key "field_name"]`
- `[@json.trim]`
- `[@validate.required]`
- `[@validate.min_length n]`
- `[@validate.max_length n]`
- `[@validate.charset "username"]`

Supported field types in the first release:

- `string`
- `string list`
- `int`
- `int option`
- `bool`
- `bool option`

The runtime API is intentionally ordinary OCaml so projects can use it directly
without PPX when a custom decoder is clearer.

Generated functions:

- `[@@deriving dream_form]` generates `<type>_of_source` and
  `<type>_of_request`.
- It also generates `<type>_form_fields`, `<type>_session_fields`,
  `<type>_session_csv_fields`, `<type>_of_query`, `<type>_of_route`, and
  `<type>_of_session`, so route/query/session extraction can use the DTO field
  names and attributes instead of repeating string key lists at each handler.
- `[@@deriving dream_json]` generates `<type>_of_json_source`,
  `<type>_of_json`, `<type>_of_json_string`, and `<type>_of_json_request`.

Non-body boundaries can use the generated request decoders directly:

```ocaml
type post_route = { post_id : string [@validate.required] }
[@@deriving dream_form]

let route = post_route_of_route request

type notice_query = {
  notice : string option;
  notice_type : string option [@form.key "notice_type"];
}
[@@deriving dream_form]

let query = notice_query_of_query request

type media_session = {
  media_ids : string list
    [@form.key "media_id"]
    [@session.key "composer_media_ids"]
    [@session.csv];
}
[@@deriving dream_form]

let media = media_session_of_session request

let session = user_session_of_session request
```
