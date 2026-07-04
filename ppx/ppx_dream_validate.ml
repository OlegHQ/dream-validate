open Ppxlib

module A = Ast_builder.Default

let lid_of_parts = function
  | [] -> invalid_arg "lid_of_parts"
  | part :: parts ->
      List.fold_left
        (fun acc part -> Longident.Ldot (acc, part))
        (Longident.Lident part) parts

let lid ~loc parts = { loc; txt = lid_of_parts parts }
let ident ~loc parts = A.pexp_ident ~loc (lid ~loc parts)
let str ~loc value = A.estring ~loc value
let int ~loc value = A.eint ~loc value
let bool ~loc value = if value then [%expr true] else [%expr false]
let var ~loc name = A.evar ~loc name
let pat_var ~loc name = A.ppat_var ~loc { loc; txt = name }
let app ~loc fn args = A.pexp_apply ~loc fn args
let some ~loc expr = A.pexp_construct ~loc (lid ~loc [ "Some" ]) (Some expr)

let form_key_attr =
  Attribute.declare "form.key" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload (estring __))
    Fun.id

let form_trim_attr =
  Attribute.declare "form.trim" Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()

let session_key_attr =
  Attribute.declare "session.key" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload (estring __))
    Fun.id

let session_csv_attr =
  Attribute.declare "session.csv" Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()

let json_key_attr =
  Attribute.declare "json.key" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload (estring __))
    Fun.id

let json_trim_attr =
  Attribute.declare "json.trim" Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()

let validate_required_attr =
  Attribute.declare "validate.required" Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()

let validate_charset_attr =
  Attribute.declare "validate.charset" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload (estring __))
    Fun.id

let validate_min_length_attr =
  Attribute.declare "validate.min_length" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload (eint __))
    Fun.id

let validate_max_length_attr =
  Attribute.declare "validate.max_length" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload (eint __))
    Fun.id

let length_validator ~loc field =
  let min = Attribute.get validate_min_length_attr field in
  let max = Attribute.get validate_max_length_attr field in
  match (min, max) with
  | None, None -> []
  | _ ->
      let args =
        [ (Nolabel, [%expr ()]) ]
        @ (match min with
          | None -> []
          | Some value -> [ (Labelled "min", int ~loc value) ])
        @
        match max with
        | None -> []
        | Some value -> [ (Labelled "max", int ~loc value) ]
      in
      [ app ~loc (ident ~loc [ "Dream_validate"; "Validation"; "length" ]) args ]

let validators ~loc field =
  let required =
    match Attribute.get validate_required_attr field with
    | None -> []
    | Some () -> [ ident ~loc [ "Dream_validate"; "Validation"; "required" ] ]
  in
  let charset =
    match Attribute.get validate_charset_attr field with
    | None -> []
    | Some name ->
        [
          app ~loc (ident ~loc [ "Dream_validate"; "Validation"; "charset" ])
            [ (Nolabel, str ~loc name) ];
        ]
  in
  required @ length_validator ~loc field @ charset

let is_form_trimmed field = Option.is_some (Attribute.get form_trim_attr field)

let form_field_key field =
  Attribute.get form_key_attr field |> Option.value ~default:field.pld_name.txt

let session_field_key field =
  Attribute.get session_key_attr field
  |> Option.value ~default:(form_field_key field)

let is_session_csv field = Option.is_some (Attribute.get session_csv_attr field)

let json_field_key field =
  Attribute.get json_key_attr field
  |> Option.value
       ~default:
         (Attribute.get form_key_attr field |> Option.value ~default:field.pld_name.txt)

let is_json_trimmed field =
  Option.is_some (Attribute.get json_trim_attr field) || is_form_trimmed field

let field_type field =
  let loc = field.pld_loc in
  match field.pld_type.ptyp_desc with
  | Ptyp_constr ({ txt = Longident.Lident "string"; _ }, []) -> `String
  | Ptyp_constr ({ txt = Longident.Lident "int"; _ }, []) -> `Int
  | Ptyp_constr ({ txt = Longident.Lident "bool"; _ }, []) -> `Bool
  | Ptyp_constr ({ txt = Longident.Lident "list"; _ }, [ inner ])
  | Ptyp_constr ({ txt = Longident.Ldot (Longident.Lident "List", "t"); _ }, [ inner ])
    -> (
      match inner.ptyp_desc with
      | Ptyp_constr ({ txt = Longident.Lident "string"; _ }, []) -> `String_list
      | _ ->
          Location.raise_errorf ~loc
            "dream_validate deriving currently supports only string list fields")
  | Ptyp_constr ({ txt = Longident.Lident "option"; _ }, [ inner ])
  | Ptyp_constr ({ txt = Longident.Ldot (Longident.Lident "Option", "t"); _ }, [ inner ])
    -> (
      match inner.ptyp_desc with
      | Ptyp_constr ({ txt = Longident.Lident "string"; _ }, []) -> `String_option
      | Ptyp_constr ({ txt = Longident.Lident "int"; _ }, []) -> `Int_option
      | Ptyp_constr ({ txt = Longident.Lident "bool"; _ }, []) -> `Bool_option
      | _ ->
          Location.raise_errorf ~loc
            "dream_validate deriving supports string/int/bool option fields")
  | _ ->
      Location.raise_errorf ~loc
        "dream_validate deriving supports string, string list, int, int option, bool, and bool option fields"

let form_decoder_name = function
  | `String -> "field"
  | `String_option -> "optional_field"
  | `String_list -> "list_field"
  | `Int -> "int_field"
  | `Int_option -> "optional_int_field"
  | `Bool -> "bool_field"
  | `Bool_option -> "optional_bool_field"

let json_decoder_name = function
  | `String -> "string_field"
  | `String_option -> "optional_string_field"
  | `String_list -> "list_string_field"
  | `Int -> "int_field"
  | `Int_option -> "optional_int_field"
  | `Bool -> "bool_field"
  | `Bool_option -> "optional_bool_field"

let decoder_expr ~source ~loc field =
  let kind = field_type field in
  let field_name = field.pld_name.txt in
  let key =
    match source with `Form -> form_field_key field | `Json -> json_field_key field
  in
  let trimmed =
    match source with `Form -> is_form_trimmed field | `Json -> is_json_trimmed field
  in
  let validators = validators ~loc field in
  let args =
    [ (Nolabel, str ~loc field_name) ]
    @ (if key = field_name then [] else [ (Optional "key", some ~loc (str ~loc key)) ])
    @ (match kind with
      | `String | `String_option | `String_list ->
          if trimmed then [ (Optional "trim", some ~loc (bool ~loc true)) ] else []
      | _ -> [])
    @
    if validators = [] then []
    else [ (Optional "validators", some ~loc (A.elist ~loc validators)) ]
  in
  let module_name, decoder_name =
    match source with
    | `Form -> ("Form", form_decoder_name kind)
    | `Json -> ("Json", json_decoder_name kind)
  in
  app ~loc
    (ident ~loc [ "Dream_validate"; module_name; decoder_name ])
    args

let ensure_supported_type ~deriver td =
  if td.ptype_params <> [] then
    Location.raise_errorf ~loc:td.ptype_loc
      "%s deriving does not support parameterized types yet" deriver;
  match td.ptype_kind with
  | Ptype_record fields -> fields
  | _ ->
      Location.raise_errorf ~loc:td.ptype_loc
        "%s deriving supports record types only" deriver

let result_errors_expr ~loc binding_names =
  let error_case name =
    A.case ~lhs:[%pat? Error errors] ~guard:None ~rhs:[%expr errors]
  in
  let ok_case = A.case ~lhs:[%pat? Ok _] ~guard:None ~rhs:[%expr []] in
  binding_names
  |> List.map (fun name ->
       A.pexp_match ~loc (var ~loc name) [ error_case name; ok_case ])
  |> A.elist ~loc
  |> fun lists -> [%expr List.concat [%e lists]]

let ok_pattern ~loc fields =
  fields
  |> List.map (fun field ->
       let name = field.pld_name.txt in
       A.ppat_construct ~loc (lid ~loc [ "Ok" ]) (Some (pat_var ~loc name)))
  |> A.ppat_tuple ~loc

let record_expr ~loc fields =
  fields
  |> List.map (fun field ->
       let name = field.pld_name.txt in
       ({ loc; txt = Longident.Lident name }, var ~loc name))
  |> A.pexp_record ~loc
  |> fun make -> make None

let source_function ~source_kind td fields =
  let loc = td.ptype_loc in
  let type_name = td.ptype_name.txt in
  let source_expr = var ~loc "source" in
  let field_bindings =
    fields
    |> List.mapi (fun index field ->
         let name = Printf.sprintf "_field_%d" index in
         let expr =
           app ~loc (decoder_expr ~source:source_kind ~loc field)
             [ (Nolabel, source_expr) ]
         in
         (name, expr))
  in
  let binding_names = List.map fst field_bindings in
  let let_bindings =
    field_bindings
    |> List.map (fun (name, expr) ->
         A.value_binding ~loc ~pat:(pat_var ~loc name) ~expr)
  in
  let match_expr =
    A.pexp_match ~loc
      (A.pexp_tuple ~loc (List.map (var ~loc) binding_names))
      [
        A.case ~lhs:(ok_pattern ~loc fields) ~guard:None
          ~rhs:[%expr Ok [%e record_expr ~loc fields]];
        A.case
          ~lhs:(A.ppat_any ~loc)
          ~guard:None
          ~rhs:[%expr Error [%e result_errors_expr ~loc binding_names]];
      ]
  in
  A.pstr_value ~loc Nonrecursive
    [
      A.value_binding ~loc
        ~pat:
          (pat_var ~loc
             (match source_kind with
             | `Form -> type_name ^ "_of_source"
             | `Json -> type_name ^ "_of_json_source"))
        ~expr:
          (A.pexp_fun ~loc Nolabel None (pat_var ~loc "source")
             (A.pexp_let ~loc Nonrecursive let_bindings match_expr));
    ]

let request_function td =
  let loc = td.ptype_loc in
  let type_name = td.ptype_name.txt in
  let source_fn = var ~loc (type_name ^ "_of_source") in
  A.pstr_value ~loc Nonrecursive
    [
      A.value_binding ~loc
        ~pat:(pat_var ~loc (type_name ^ "_of_request"))
        ~expr:
          [%expr fun request ->
            Dream_validate.Form.decode_request request [%e source_fn]];
    ]

let list_of_strings ~loc values = values |> List.map (str ~loc) |> A.elist ~loc

let list_of_string_pairs ~loc values =
  values
  |> List.map (fun (left, right) ->
       A.pexp_tuple ~loc [ str ~loc left; str ~loc right ])
  |> A.elist ~loc

let form_field_names_function td fields =
  let loc = td.ptype_loc in
  let type_name = td.ptype_name.txt in
  let field_names = fields |> List.map form_field_key |> list_of_strings ~loc in
  A.pstr_value ~loc Nonrecursive
    [
      A.value_binding ~loc
        ~pat:(pat_var ~loc (type_name ^ "_form_fields"))
        ~expr:field_names;
    ]

let session_field_names_function td fields =
  let loc = td.ptype_loc in
  let type_name = td.ptype_name.txt in
  let scalar_fields, csv_fields =
    fields
    |> List.partition (fun field -> not (is_session_csv field))
    |> fun (scalar_fields, csv_fields) ->
    let csv_fields =
      csv_fields
      |> List.map (fun field ->
           if field_type field <> `String_list then
             Location.raise_errorf ~loc:field.pld_loc
               "session.csv is supported only on string list fields";
           (form_field_key field, session_field_key field))
    in
    (scalar_fields, csv_fields)
  in
  let scalar_field_names =
    scalar_fields |> List.map session_field_key |> list_of_strings ~loc
  in
  let keyed_scalar_fields =
    scalar_fields
    |> List.map (fun field -> (form_field_key field, session_field_key field))
    |> list_of_string_pairs ~loc
  in
  let csv_field_names = list_of_string_pairs ~loc csv_fields in
  A.pstr_value ~loc Nonrecursive
    [
      A.value_binding ~loc
        ~pat:(pat_var ~loc (type_name ^ "_session_fields"))
        ~expr:scalar_field_names;
      A.value_binding ~loc
        ~pat:(pat_var ~loc (type_name ^ "_session_keyed_fields"))
        ~expr:keyed_scalar_fields;
      A.value_binding ~loc
        ~pat:(pat_var ~loc (type_name ^ "_session_csv_fields"))
        ~expr:csv_field_names;
    ]

let form_boundary_functions td fields =
  let loc = td.ptype_loc in
  let type_name = td.ptype_name.txt in
  let source_fn = var ~loc (type_name ^ "_of_source") in
  let boundary name module_name =
    A.value_binding ~loc
      ~pat:(pat_var ~loc (type_name ^ name))
      ~expr:
        (A.pexp_fun ~loc Nolabel None (pat_var ~loc "request")
           (app ~loc
              (ident ~loc [ "Dream_validate"; module_name; "decode" ])
              [
                (Nolabel, var ~loc "request");
                (Nolabel, var ~loc (type_name ^ "_form_fields"));
                (Nolabel, source_fn);
              ]))
  in
  let session_boundary =
    A.value_binding ~loc
      ~pat:(pat_var ~loc (type_name ^ "_of_session"))
      ~expr:
        (A.pexp_fun ~loc Nolabel None (pat_var ~loc "request")
           (app ~loc
              (ident ~loc [ "Dream_validate"; "Session"; "decode_keyed" ])
              [
                (Nolabel, var ~loc "request");
                ( Labelled "fields",
                  var ~loc (type_name ^ "_session_keyed_fields") );
                ( Labelled "csv_fields",
                  var ~loc (type_name ^ "_session_csv_fields") );
                (Nolabel, source_fn);
              ]))
  in
  A.pstr_value ~loc Nonrecursive
    [
      boundary "_of_query" "Query";
      boundary "_of_route" "Route";
      session_boundary;
    ]

let json_function td =
  let loc = td.ptype_loc in
  let type_name = td.ptype_name.txt in
  let source_fn = var ~loc (type_name ^ "_of_json_source") in
  A.pstr_value ~loc Nonrecursive
    [
      A.value_binding ~loc
        ~pat:(pat_var ~loc (type_name ^ "_of_json"))
        ~expr:[%expr fun json -> Dream_validate.Json.decode json [%e source_fn]];
      A.value_binding ~loc
        ~pat:(pat_var ~loc (type_name ^ "_of_json_string"))
        ~expr:[%expr fun body -> Dream_validate.Json.decode_string body [%e source_fn]];
      A.value_binding ~loc
        ~pat:(pat_var ~loc (type_name ^ "_of_json_request"))
        ~expr:[%expr fun request ->
          Dream_validate.Json.decode_request request [%e source_fn]];
    ]

let sig_for_type ~source td =
  let loc = td.ptype_loc in
  let type_name = td.ptype_name.txt in
  let typ = A.ptyp_constr ~loc (lid ~loc [ type_name ]) [] in
  let source_type =
    match source with
    | `Form ->
        A.ptyp_constr ~loc
          (lid ~loc [ "Dream_validate"; "Form"; "source" ])
          []
    | `Json ->
        A.ptyp_constr ~loc
          (lid ~loc [ "Dream_validate"; "Json"; "source" ])
          []
  in
  let result_type =
    A.ptyp_constr ~loc (lid ~loc [ "Dream_validate"; "result" ]) [ typ ]
  in
  let value name type_ =
    A.psig_value ~loc
      (A.value_description ~loc ~name:{ loc; txt = name } ~type_ ~prim:[])
  in
  match source with
  | `Form ->
      [
        value (type_name ^ "_form_fields")
          (A.ptyp_constr ~loc (lid ~loc [ "list" ])
             [ A.ptyp_constr ~loc (lid ~loc [ "string" ]) [] ]);
        value (type_name ^ "_session_fields")
          (A.ptyp_constr ~loc (lid ~loc [ "list" ])
             [ A.ptyp_constr ~loc (lid ~loc [ "string" ]) [] ]);
        value (type_name ^ "_session_csv_fields")
          (A.ptyp_constr ~loc (lid ~loc [ "list" ])
             [
               A.ptyp_tuple ~loc
                 [
                   A.ptyp_constr ~loc (lid ~loc [ "string" ]) [];
                   A.ptyp_constr ~loc (lid ~loc [ "string" ]) [];
                 ];
             ]);
        value (type_name ^ "_of_source")
          (A.ptyp_arrow ~loc Nolabel source_type result_type);
        value (type_name ^ "_of_request")
          (A.ptyp_arrow ~loc Nolabel
             (A.ptyp_constr ~loc (lid ~loc [ "Dream"; "request" ]) [])
             result_type);
        value (type_name ^ "_of_query")
          (A.ptyp_arrow ~loc Nolabel
             (A.ptyp_constr ~loc (lid ~loc [ "Dream"; "request" ]) [])
             result_type);
        value (type_name ^ "_of_route")
          (A.ptyp_arrow ~loc Nolabel
             (A.ptyp_constr ~loc (lid ~loc [ "Dream"; "request" ]) [])
             result_type);
        value (type_name ^ "_of_session")
          (A.ptyp_arrow ~loc Nolabel
             (A.ptyp_constr ~loc (lid ~loc [ "Dream"; "request" ]) [])
             result_type);
      ]
  | `Json ->
      [
        value (type_name ^ "_of_json_source")
          (A.ptyp_arrow ~loc Nolabel source_type result_type);
        value (type_name ^ "_of_json")
          (A.ptyp_arrow ~loc Nolabel
             (A.ptyp_constr ~loc (lid ~loc [ "Yojson"; "Safe"; "t" ]) [])
             result_type);
        value (type_name ^ "_of_json_string")
          (A.ptyp_arrow ~loc Nolabel (A.ptyp_constr ~loc (lid ~loc [ "string" ]) [])
             result_type);
        value (type_name ^ "_of_json_request")
          (A.ptyp_arrow ~loc Nolabel
             (A.ptyp_constr ~loc (lid ~loc [ "Dream"; "request" ]) [])
             result_type);
      ]

let generate_form_str ~loc:_ ~path:_ (_rec_flag, tds) =
  tds
  |> List.concat_map (fun td ->
       let fields = ensure_supported_type ~deriver:"dream_form" td in
       [
         form_field_names_function td fields;
         session_field_names_function td fields;
         source_function ~source_kind:`Form td fields;
         request_function td;
         form_boundary_functions td fields;
       ])

let generate_form_sig ~loc:_ ~path:_ (_rec_flag, tds) =
  tds
  |> List.concat_map (fun td ->
       ignore (ensure_supported_type ~deriver:"dream_form" td);
       sig_for_type ~source:`Form td)

let generate_json_str ~loc:_ ~path:_ (_rec_flag, tds) =
  tds
  |> List.concat_map (fun td ->
       let fields = ensure_supported_type ~deriver:"dream_json" td in
       [ source_function ~source_kind:`Json td fields; json_function td ])

let generate_json_sig ~loc:_ ~path:_ (_rec_flag, tds) =
  tds
  |> List.concat_map (fun td ->
       ignore (ensure_supported_type ~deriver:"dream_json" td);
       sig_for_type ~source:`Json td)

let form_str_type_decl = Deriving.Generator.make_noarg generate_form_str
let form_sig_type_decl = Deriving.Generator.make_noarg generate_form_sig
let json_str_type_decl = Deriving.Generator.make_noarg generate_json_str
let json_sig_type_decl = Deriving.Generator.make_noarg generate_json_sig

let (_ : Deriving.t) =
  Deriving.add "dream_form" ~str_type_decl:form_str_type_decl
    ~sig_type_decl:form_sig_type_decl

let (_ : Deriving.t) =
  Deriving.add "dream_json" ~str_type_decl:json_str_type_decl
    ~sig_type_decl:json_sig_type_decl
