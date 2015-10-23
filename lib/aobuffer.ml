(* Why is this faster / less allocy?

   1. No initial over-allocation
   2. No over-allocation on resizing
   3. No copying on resizing
   4. No keeping around the initial buffer (!)
   5. add_string never resizes
*)

module Check_safe_string_used =
struct
  (* The Aobuffer module must be compiled with -safe-string, since it assumes
     that strings passed in are not subsequently modified elsewhere.  This
     module (Check_safe_string_used) checks statically that -safe-string is in
     use.  If -safe-string is used then bytes and string are distinct types,
     and so the pattern match in check_safe_string_used could never match
     Is_string.  If -safe_string is not used then the Is_string case could be
     matched and so the pattern match is inexhaustive, triggering a fatal
     error. *)
  type _ safe_string_check =
      Not_string : _ safe_string_check
    | Is_string : string safe_string_check
          
  (* Check that the -safe-string is used.  See the comment at the top
     of the Check_safe_string_used module *)
  [@@@ocaml.warning "@8"]
  let check_safe_string_used : bytes safe_string_check -> unit = function
      Not_string -> ()
end

module Immutable =
struct
end

module Mutable :
sig
  type t

  val create : int -> t

  val contents : t -> string

  val to_bytes : t -> bytes

  (* val sub : t -> int -> int -> string *)

  (* val blit : t -> int -> bytes -> int -> int -> unit *)

  (* val nth : t -> int -> char *) (* TODO *)

  val length : t -> int

  val clear : t -> unit

  val add_char : t -> char -> unit

  val add_string : t -> string -> unit

  val add_bytes : t -> bytes -> unit

  val add_substring : t -> string -> int -> int -> unit

  val add_subbytes : t -> bytes -> int -> int -> unit

  val add_buffer : t -> t -> unit

  (* val add_channel : t -> in_channel -> int -> unit *)

  val output_buffer : out_channel -> t -> unit
end =
struct
  (* Kept in reversed order *)

  (* TODO: add a string Weak.t option cache.  We could even add a cache at
     each link in the chain. *)
  type t = { mutable elements: string list }

  let create _ = { elements = [] }

  let add_string buf s = buf.elements <- s :: buf.elements

  let add_bytes buf b = add_string buf (Bytes.to_string b)

  let add_char buf c = add_string buf (String.make 1 c)

  let length { elements } =
    List.fold_left (fun n s -> n + String.length s) 0 elements

  let add_subbytes buf b ofs len =
    add_string buf (Bytes.sub_string b ofs len)

  let add_substring buf b ofs len =
    add_string buf (String.sub b ofs len)

  let clear buf = buf.elements <- []

  let iter_elements f elements =
    List.fold_right (fun elem () -> ignore (f elem)) elements ()

  let iteri_elements f elements =
    List.fold_right
      (fun elem offset ->
         let () = f offset elem in
         offset + String.length elem)
      elements
      0


  let to_bytes buf =
    let len = length buf in
    let b = Bytes.create len in
    let _ = List.fold_left
        (fun i s ->
           let l = String.length s in
           Bytes.blit_string s 0 b (i - l) l;
           i - 1)
        len
        buf.elements
    in
    b

  let contents buf = Bytes.unsafe_to_string (to_bytes buf)

  let output_buffer outch {elements} =
    iter_elements (output_string outch) elements

  let add_buffer l r = l.elements <- r.elements @ l.elements
end
