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

val output_buffer : out_channel -> t -> unit
val contents : t -> string
val to_bytes : t -> bytes

val formatter_of_aobuffer : t -> Format.formatter
