type t

val create : unit -> t

val length : t -> int

val reset : t -> unit
val clear : t -> unit

val add_char : t -> char -> unit
val add_string : t -> string -> unit
val add_bytes : t -> bytes -> unit
val add_substring : t -> string -> int -> int -> unit
val add_subbytes : t -> bytes -> int -> int -> unit
val add_buffer : t -> Buffer.t -> unit
val add_aobuffer : t -> t -> unit
val nth : t -> int -> char
val blit : t -> int -> bytes -> int -> int -> unit

(* TODO:
(* val add_channel : t -> in_channel -> int -> unit *) (* TODO *)
       (* The value `sub' is required but not provided *)
*)

val output_buffer : out_channel -> t -> unit
val contents : t -> string
val to_bytes : t -> bytes

val bprintf : t -> ('a, unit, string, unit) format4 -> 'a

val formatter_of_aobuffer : t -> Format.formatter
