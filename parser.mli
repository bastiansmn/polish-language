val get_lines : string -> string list list

val parse_program : string list list -> Model.block

val parse_block : int -> string list list -> Model.position -> Model.block