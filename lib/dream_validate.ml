type error = {
  field : string;
  code : string;
  message : string;
}

type 'a result = ('a, error list) Stdlib.result

let default_message ~field ~code =
  match code with
  | "required" -> Printf.sprintf "%s is required." field
  | "invalid_int" -> Printf.sprintf "%s must be an integer." field
  | "invalid_bool" -> Printf.sprintf "%s must be true or false." field
  | "invalid_json" -> "Expected a JSON object."
  | "invalid_type" -> Printf.sprintf "%s has the wrong type." field
  | "too_short" -> Printf.sprintf "%s is too short." field
  | "too_long" -> Printf.sprintf "%s is too long." field
  | "invalid_charset" -> Printf.sprintf "%s contains invalid characters." field
  | _ -> Printf.sprintf "%s is invalid." field

let error ?message ~field ~code () =
  {
    field;
    code;
    message = Option.value message ~default:(default_message ~field ~code);
  }

let errors_to_string = function
  | [] -> ""
  | error :: _ -> error.message

module Source = struct
  type t = (string * string list) list

  let empty = []

  let add key value fields =
    match List.assoc_opt key fields with
    | None -> (key, [ value ]) :: fields
    | Some values ->
        (key, values @ [ value ]) :: List.remove_assoc key fields

  let of_fields fields =
    List.fold_left (fun acc (key, value) -> add key value acc) empty fields

  let of_query query =
    query
    |> List.filter_map (function key, Some value -> Some (key, value) | _ -> None)
    |> of_fields

  let values source key = List.assoc_opt key source |> Option.value ~default:[]

  let value source key =
    match values source key with [] -> None | value :: _ -> Some value
end

module Validation = struct
  type 'a validator = field:string -> 'a -> error list

  let pass ~field:_ _ = []

  let required ~field value =
    if String.trim value = "" then [ error ~field ~code:"required" () ] else []

  let length ?min ?max () ~field value =
    let len = String.length value in
    let too_short =
      match min with
      | Some min when len < min ->
          [
            error
              ~message:(Printf.sprintf "%s must be at least %d characters." field min)
              ~field ~code:"too_short" ();
          ]
      | _ -> []
    in
    let too_long =
      match max with
      | Some max when len > max ->
          [
            error
              ~message:(Printf.sprintf "%s must be at most %d characters." field max)
              ~field ~code:"too_long" ();
          ]
      | _ -> []
    in
    too_short @ too_long

  let username_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' -> true
    | _ -> false

  let charset name ~field value =
    let valid =
      match name with
      | "username" -> String.for_all username_char value
      | _ -> true
    in
    if valid then []
    else
      [
        error ~message:(Printf.sprintf "%s has invalid characters." field) ~field
          ~code:"invalid_charset" ();
      ]

  let all validators ~field value =
    validators |> List.concat_map (fun validator -> validator ~field value)
end

module Form = struct
  type source = Source.t
  type 'a decoder = source -> 'a result

  let decode source decoder = decoder source

  let clean ~trim value = if trim then String.trim value else value

  let validate validators ~field value =
    match Validation.all validators ~field value with
    | [] -> Ok value
    | errors -> Error errors

  let field ?key ?(trim = false) ?(validators = []) name source =
    let key = Option.value key ~default:name in
    match Source.value source key with
    | None | Some "" -> Error [ error ~field:name ~code:"required" () ]
    | Some value ->
        let value = clean ~trim value in
        validate validators ~field:name value

  let optional_field ?key ?(trim = false) ?(validators = []) name source =
    let key = Option.value key ~default:name in
    match Source.value source key with
    | None | Some "" -> Ok None
    | Some value ->
        let value = clean ~trim value in
        validate validators ~field:name value |> Result.map Option.some

  let list_field ?key ?(trim = false) ?(validators = []) name source =
    let key = Option.value key ~default:name in
    let values =
      Source.values source key
      |> List.map (clean ~trim)
      |> List.filter (fun value -> value <> "")
    in
    let errors =
      values
      |> List.concat_map (fun value -> Validation.all validators ~field:name value)
    in
    if errors = [] then Ok values else Error errors

  let parse_int ~field value =
    match int_of_string_opt value with
    | Some value -> Ok value
    | None -> Error [ error ~field ~code:"invalid_int" () ]

  let int_field ?key ?(validators = []) name source =
    let key = Option.value key ~default:name in
    match Source.value source key with
    | None | Some "" -> Error [ error ~field:name ~code:"required" () ]
    | Some value ->
        let value = String.trim value in
        let ( let* ) = Result.bind in
        let* value = parse_int ~field:name value in
        validate validators ~field:name value

  let optional_int_field ?key ?(validators = []) name source =
    let key = Option.value key ~default:name in
    match Source.value source key with
    | None | Some "" -> Ok None
    | Some value ->
        let value = String.trim value in
        let ( let* ) = Result.bind in
        let* value = parse_int ~field:name value in
        validate validators ~field:name value |> Result.map Option.some

  let parse_bool ~field value =
    match String.lowercase_ascii (String.trim value) with
    | "true" | "on" | "1" | "yes" -> Ok true
    | "false" | "off" | "0" | "no" -> Ok false
    | _ -> Error [ error ~field ~code:"invalid_bool" () ]

  let bool_field ?key ?(validators = []) name source =
    let key = Option.value key ~default:name in
    match Source.value source key with
    | None | Some "" -> Error [ error ~field:name ~code:"required" () ]
    | Some value ->
        let ( let* ) = Result.bind in
        let* value = parse_bool ~field:name value in
        validate validators ~field:name value

  let optional_bool_field ?key ?(validators = []) name source =
    let key = Option.value key ~default:name in
    match Source.value source key with
    | None | Some "" -> Ok None
    | Some value ->
        let ( let* ) = Result.bind in
        let* value = parse_bool ~field:name value in
        validate validators ~field:name value |> Result.map Option.some

  let of_request request =
    match Dream.form ~csrf:false request with
    | `Ok fields | `Expired (fields, _) -> Ok (Source.of_fields fields)
    | `Wrong_content_type ->
        Error
          [
            error ~message:"Expected a form submission." ~field:"request"
              ~code:"wrong_content_type" ();
          ]
    | _ ->
        Error
          [
            error ~message:"The submitted form could not be verified."
              ~field:"request" ~code:"invalid_form" ();
          ]

  let decode_request request decoder =
    match of_request request with Error errors -> Error errors | Ok source -> decoder source
end

module Json = struct
  type source = (string * Yojson.Safe.t) list
  type 'a decoder = source -> 'a result

  let source = function
    | `Assoc fields -> Ok fields
    | _ -> Error [ error ~field:"json" ~code:"invalid_json" () ]

  let of_string body =
    match Yojson.Safe.from_string body with
    | json -> source json
    | exception Yojson.Json_error message ->
        Error [ error ~message ~field:"json" ~code:"invalid_json" () ]

  let member source key = List.assoc_opt key source

  let clean ~trim value = if trim then String.trim value else value

  let validate validators ~field value =
    match Validation.all validators ~field value with
    | [] -> Ok value
    | errors -> Error errors

  let string_field ?key ?(trim = false) ?(validators = []) name source =
    let key = Option.value key ~default:name in
    match member source key with
    | Some (`String value) ->
        let value = clean ~trim value in
        validate validators ~field:name value
    | Some `Null | None -> Error [ error ~field:name ~code:"required" () ]
    | Some _ -> Error [ error ~field:name ~code:"invalid_type" () ]

  let optional_string_field ?key ?(trim = false) ?(validators = []) name source =
    let key = Option.value key ~default:name in
    match member source key with
    | Some (`String value) ->
        let value = clean ~trim value in
        validate validators ~field:name value |> Result.map Option.some
    | Some `Null | None -> Ok None
    | Some _ -> Error [ error ~field:name ~code:"invalid_type" () ]

  let list_string_field ?key ?(trim = false) ?(validators = []) name source =
    let key = Option.value key ~default:name in
    match member source key with
    | Some (`List values) ->
        let rec loop strings errors = function
          | [] ->
              if errors = [] then Ok (List.rev strings) else Error (List.rev errors)
          | `String value :: rest ->
              let value = clean ~trim value in
              let value_errors = Validation.all validators ~field:name value in
              if value_errors = [] then loop (value :: strings) errors rest
              else loop strings (List.rev_append value_errors errors) rest
          | _ :: rest ->
              loop strings (error ~field:name ~code:"invalid_type" () :: errors) rest
        in
        loop [] [] values
    | Some `Null | None -> Ok []
    | Some _ -> Error [ error ~field:name ~code:"invalid_type" () ]

  let int_field ?key ?(validators = []) name source =
    let key = Option.value key ~default:name in
    match member source key with
    | Some (`Int value) -> validate validators ~field:name value
    | Some `Null | None -> Error [ error ~field:name ~code:"required" () ]
    | Some _ -> Error [ error ~field:name ~code:"invalid_type" () ]

  let optional_int_field ?key ?(validators = []) name source =
    let key = Option.value key ~default:name in
    match member source key with
    | Some (`Int value) -> validate validators ~field:name value |> Result.map Option.some
    | Some `Null | None -> Ok None
    | Some _ -> Error [ error ~field:name ~code:"invalid_type" () ]

  let bool_field ?key ?(validators = []) name source =
    let key = Option.value key ~default:name in
    match member source key with
    | Some (`Bool value) -> validate validators ~field:name value
    | Some `Null | None -> Error [ error ~field:name ~code:"required" () ]
    | Some _ -> Error [ error ~field:name ~code:"invalid_type" () ]

  let optional_bool_field ?key ?(validators = []) name source =
    let key = Option.value key ~default:name in
    match member source key with
    | Some (`Bool value) -> validate validators ~field:name value |> Result.map Option.some
    | Some `Null | None -> Ok None
    | Some _ -> Error [ error ~field:name ~code:"invalid_type" () ]

  let decode json decoder =
    match source json with Error errors -> Error errors | Ok source -> decoder source

  let decode_string body decoder =
    match of_string body with Error errors -> Error errors | Ok source -> decoder source

  let of_request request = of_string (Dream.body request)

  let decode_request request decoder =
    match of_request request with Error errors -> Error errors | Ok source -> decoder source
end
