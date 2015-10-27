(** A buffer implementation based on a rope representation.
 * 
 * The interface is (almost) identical to the Buffer module in the
 * standard library, but this module is typically faster and has
 * significantly lower memory usage.
*)

type t
(** The type of buffers *)

val create : unit -> t
(** [create ()] creates an initially-empty buffer.

    O(1) time, allocates a couple of words. *)

val length : t -> int
(** [length buf] returns the number of characters currently contained
    in the buffer [buf].

    O(1) time, does not allocate. *)

val reset : t -> unit
(** [reset buf] empties the buffer [buf], releasing references to its
    internal storage.

    O(1) time, does not allocate. *)

val clear : t -> unit
(** [clear] is an alias for [reset]. *)

val add_char : t -> char -> unit
(** [TODO]

   O(1) time, allocates a couple of words. *)

val add_string : t -> string -> unit
(** [TODO]

    O(1) time, allocates a couple of words. *)

val add_bytes : t -> bytes -> unit
(** [TODO]

    O(1) time, allocates a couple of words and a copy of [b]. *)

val add_substring : t -> string -> int -> int -> unit
(** [TODO]

    O(1) time, allocates a couple of words and a copy of [n]
    characters from [s]. *)

val add_subbytes : t -> bytes -> int -> int -> unit
(** [TODO]

    O(1) time, allocates a couple of words and a copy of [n] characters from
    [b]. *)

val add_buffer : t -> Buffer.t -> unit
(** [TODO]

    O(1) time, allocates a couple of words and a copy of [Buffer.length buf]
    characters from [b]. *)

val add_aobuffer : t -> t -> unit
(** [TODO]

    O(n) time in the number of elements (not characters) in [v], allocates a
    couple of words for each element in [v]. *)

val nth : t -> int -> char
(** [TODO]

    O(n) time in the number of elements (not characters) in [buf], does not
    allocate. *)

val blit : t -> int -> bytes -> int -> int -> unit
(** [TODO]

    O(n) time in the number of elements (not characters) in [buf], does not
    allocate. *)

val sub : t -> int -> int -> string
(** [TODO]

    O(n) time in the number of elements (not characters) in [buf], allocates a
    string of length [n]. *)

val output_buffer : out_channel -> t -> unit
(** [TODO] *)

val contents : t -> string
(** [TODO] *)

val to_bytes : t -> bytes
(** [TODO] *)

val bprintf : t -> ('a, unit, string, unit) format4 -> 'a
(** [TODO] *)

val formatter_of_aobuffer : t -> Format.formatter
(** [TODO] *)
