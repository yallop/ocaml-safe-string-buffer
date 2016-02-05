(** A buffer implementation optimized for immutable strings.
 * 
 * The interface is (almost) identical to the Buffer module in the
 * standard library, but this module is typically faster and has
 * significantly lower memory usage.
*)

type t
(** The type of buffers *)

val create : int -> t
(** [create n] creates an initially-empty buffer.

    The argument [n] is ignored; it's only there for compatibility with
    {!Buffer.}

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
(** [add_char buf c] appends the char [c] to [buf].

    O(1) time, allocates a couple of words. *)

val add_string : t -> string -> unit
(** [add_string buf s] appends the string [s] to [buf]

    O(1) time, allocates a couple of words. *)

val add_bytes : t -> bytes -> unit
(** [add_bytes buf s] appends the string [s] to [buf]

    O(1) time, allocates a couple of words and a copy of [b]. *)

val add_substring : t -> string -> int -> int -> unit
(** [add_substring buf s offset len] appends the substring
    [String.sub s offset len] to [buf].

    O(1) time, allocates a couple of words and a copy of [len] characters from
    [s]. *)

val add_subbytes : t -> bytes -> int -> int -> unit
(** [add_substring buf s offset len] appends the subsequence
    [Bytes.sub s offset len] to [buf].

    O(1) time, allocates a couple of words and a copy of [len] characters from
    [b]. *)

val add_buffer : t -> Buffer.t -> unit
(** [add_buffer buf b] appends the contents of the buffer [b] to [buf].

    O(1) time, allocates a couple of words and a copy of [Buffer.length buf]
    characters from [b]. *)

val add_safe_string_buffer : t -> t -> unit
(** [add_safe_string_buffer buf b] appends the contents of the buffer [b] to
    [buf].

    O(n) time in the number of elements (not characters) in [v], allocates a
    couple of words for each element in [v]. *)

val add_channel : t -> in_channel -> int -> unit
(** [add_channel buf in_channel n] reads up to [n] characters from
    [in_channel] and appends them to [buf]. *)

val add_substitute : t -> (string -> string) -> string -> unit
(** [add_substitute] behaves like {!Buffer.add_substitute}. *)

val nth : t -> int -> char
(** [nth buf n] returns the [n]th character of [buf].

    O(n) time in the number of elements (not characters) in [buf], does not
    allocate. *)

val blit : t -> int -> bytes -> int -> int -> unit
(** [blit buf src_offset bytes dst_offset len] copies [len] characters from
    [buf], starting at [src_offset], into [bytes], starting at [dst_offset].

    O(n) time in the number of elements (not characters) in [buf], does not
    allocate. *)

val sub : t -> int -> int -> string
(** [sub buf offset len] extracts the substring of [buf] between [offset] and
    [offset + len].

    O(n) time in the number of elements (not characters) in [buf], allocates a
    string of length [n]. *)

val output_buffer : out_channel -> t -> unit
(** [outbut_buffer out_channel buf] writes the contents of [buf] on the
    channel [out_channel]. *)

val contents : t -> string
(** [contents buf] returns the contents of [buf] as a string. *)

val to_bytes : t -> bytes
(** [to_bytes buf] returns the contents of [buf] as a bytes sequence. *)

val bprintf : t -> ('a, unit, string, unit) format4 -> 'a
(** [bprintf] behaves like {!Printf.bprintf}. *)

val formatter_of_safe_string_buffer : t -> Format.formatter
(** [formatter_of_safe_string_buffer] behaves like
    {!Format.formatter_of_buffer}. *)
