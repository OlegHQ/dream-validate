# dream-validate

Typed Dream request decoding and declarative DTO validation for OCaml web apps.

The package provides:

- `Dream_validate.Form` runtime decoders for form/query-style field lists.
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
- `[@@deriving dream_json]` generates `<type>_of_json_source`,
  `<type>_of_json`, `<type>_of_json_string`, and `<type>_of_json_request`.
