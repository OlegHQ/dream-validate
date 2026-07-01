type error = {
  field : string;
  code : string;
  message : string;
}

type 'a result = ('a, error list) Stdlib.result

val error : ?message:string -> field:string -> code:string -> unit -> error
val errors_to_string : error list -> string

module Source : sig
  type t

  val empty : t
  val of_fields : (string * string) list -> t
  val of_query : (string * string option) list -> t
  val values : t -> string -> string list
  val value : t -> string -> string option
end

module Validation : sig
  type 'a validator = field:string -> 'a -> error list

  val pass : 'a validator
  val required : string validator
  val length : ?min:int -> ?max:int -> unit -> string validator
  val charset : string -> string validator
  val all : 'a validator list -> 'a validator
end

module Form : sig
  type source = Source.t
  type 'a decoder = source -> 'a result

  val field :
    ?key:string ->
    ?trim:bool ->
    ?validators:string Validation.validator list ->
    string ->
    string decoder

  val optional_field :
    ?key:string ->
    ?trim:bool ->
    ?validators:string Validation.validator list ->
    string ->
    string option decoder

  val list_field :
    ?key:string ->
    ?trim:bool ->
    ?validators:string Validation.validator list ->
    string ->
    string list decoder

  val int_field :
    ?key:string ->
    ?validators:int Validation.validator list ->
    string ->
    int decoder

  val optional_int_field :
    ?key:string ->
    ?validators:int Validation.validator list ->
    string ->
    int option decoder

  val bool_field :
    ?key:string ->
    ?validators:bool Validation.validator list ->
    string ->
    bool decoder

  val optional_bool_field :
    ?key:string ->
    ?validators:bool Validation.validator list ->
    string ->
    bool option decoder

  val decode : source -> 'a decoder -> 'a result
  val of_request : Dream.request -> (source, error list) Stdlib.result
  val decode_request : Dream.request -> 'a decoder -> 'a result
end

module Json : sig
  type source
  type 'a decoder = source -> 'a result

  val source : Yojson.Safe.t -> (source, error list) Stdlib.result
  val of_string : string -> (source, error list) Stdlib.result

  val string_field :
    ?key:string ->
    ?trim:bool ->
    ?validators:string Validation.validator list ->
    string ->
    string decoder

  val optional_string_field :
    ?key:string ->
    ?trim:bool ->
    ?validators:string Validation.validator list ->
    string ->
    string option decoder

  val list_string_field :
    ?key:string ->
    ?trim:bool ->
    ?validators:string Validation.validator list ->
    string ->
    string list decoder

  val int_field :
    ?key:string ->
    ?validators:int Validation.validator list ->
    string ->
    int decoder

  val optional_int_field :
    ?key:string ->
    ?validators:int Validation.validator list ->
    string ->
    int option decoder

  val bool_field :
    ?key:string ->
    ?validators:bool Validation.validator list ->
    string ->
    bool decoder

  val optional_bool_field :
    ?key:string ->
    ?validators:bool Validation.validator list ->
    string ->
    bool option decoder

  val decode : Yojson.Safe.t -> 'a decoder -> 'a result
  val decode_string : string -> 'a decoder -> 'a result
  val of_request : Dream.request -> (source, error list) Stdlib.result
  val decode_request : Dream.request -> 'a decoder -> 'a result
end
