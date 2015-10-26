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

module Immutable :
sig
  type t

  val empty : t

  val contents : t -> string

  val to_bytes : t -> bytes

  (* val sub : t -> int -> int -> string *)

  (* val blit : t -> int -> bytes -> int -> int -> unit *)

  (* val nth : t -> int -> char *) (* TODO *)

  val length : t -> int

  val add_char : t -> char -> t

  val add_string : t -> string -> t

  val add_bytes : t -> bytes -> t

  val add_substring : t -> string -> int -> int -> t

  val add_subbytes : t -> bytes -> int -> int -> t

  val add_aobuffer : t -> t -> t

  (* val add_channel : t -> in_channel -> int -> unit *)

  val output_buffer : out_channel -> t -> unit
end =
struct
  (* Kept in reversed order *)
  
  (* TODO: add a string Weak.t option cache.  We could even add a
     cache at each link in the chain. *)
  type t = string list

  let empty = []

  let add_string buf s = s :: buf

  let length buf =
    List.fold_left (fun n s -> n + String.length s) 0 buf

  let add_bytes buf b = add_string buf (Bytes.to_string b)

  let add_char buf c = add_string buf (String.make 1 c)

  let add_subbytes buf b ofs len =
    add_string buf (Bytes.sub_string b ofs len)

  let add_substring buf b ofs len =
    add_string buf (String.sub b ofs len)

  let to_bytes buf =
    let len = length buf in
    let b = Bytes.create len in
    let _ = List.fold_left
        (fun i s ->
           let l = String.length s in
           Bytes.blit_string s 0 b (i - l) l;
           i - l)
        len
        buf
    in
    b

  let contents = function
      [] -> ""
    | [s] -> s
    | buf -> Bytes.unsafe_to_string (to_bytes buf)

  let iter_elements f elements =
    List.fold_right (fun elem () -> ignore (f elem)) elements ()

  let output_buffer outch elements =
    iter_elements (output_string outch) elements

  let add_aobuffer = (@)
end

module Mutable =
struct
  type t = { mutable elements: Immutable.t }

  let create () = { elements = Immutable.empty }

  let add_string buf s =
    buf.elements <- Immutable.add_string buf.elements s

  let add_bytes buf b =
    buf.elements <- Immutable.add_bytes buf.elements b

  let add_char buf c =
    buf.elements <- Immutable.add_char buf.elements c

  let length { elements } = Immutable.length elements

  let add_subbytes buf b ofs len =
    add_string buf (Bytes.sub_string b ofs len)

  let add_substring buf b ofs len =
    add_string buf (String.sub b ofs len)

  let clear buf = buf.elements <- Immutable.empty

  let reset = clear

  let to_bytes buf = Immutable.to_bytes buf.elements

  let contents buf = Bytes.unsafe_to_string (to_bytes buf)

  let output_buffer outch {elements} =
    Immutable.output_buffer outch elements

  let add_aobuffer l r =
    l.elements <- Immutable.add_aobuffer r.elements l.elements

  let add_buffer buf b =
    add_string buf (Buffer.contents b)
end

module type S =
sig
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
end


module Verify (X: S) (Y: S) : S =
struct
  type t = {x: X.t; y: Y.t}

  let equal x y =
    let xcontents = X.contents x and ycontents = Y.contents y in
    if xcontents = ycontents then true
    else Printf.ksprintf failwith "%s <> %s" xcontents ycontents

  let add f g =
    fun {x;y} v ->
      begin
        assert (equal x y);
        f x v;
        g y v;
        assert (equal x y)
      end

  let add3 (f : _ -> _ -> _ -> _ -> unit) g =
    fun {x;y} a b c ->
      begin
        assert (equal x y);
        f x a b c;
        g y a b c;
        assert (equal x y)
      end

  let op1_unit f g =
    fun {x;y} ->
      begin
        assert (equal x y);
        let () = f x in
        let () = g y in
        assert (equal x y)
      end

  let op1 f g =
    fun {x; y} ->
      begin
        assert (equal x y);
        let r1 = f x in
        let r2 = g y in
        assert (r1 = r2);
        r1
      end

  let add_string   = add X.add_string Y.add_string
  let add_bytes    = add X.add_bytes  Y.add_bytes
  let add_char     = add X.add_char   Y.add_char
  let add_buffer   = add X.add_buffer Y.add_buffer
  let clear        = op1_unit X.clear  Y.clear
  let reset        = op1_unit X.reset  Y.reset
  let length       = op1  X.length Y.length
  let to_bytes     = op1  X.to_bytes Y.to_bytes
  let contents     = op1  X.contents Y.contents
  (* let add_aobuffer = add X.add_aobuffer Y.add_aobuffer *)

  let add_substring = add3 X.add_substring Y.add_substring
  let add_subbytes  = add3 X.add_subbytes Y.add_subbytes

  let create () =
    let x = X.create () in
    let y = Y.create () in
    assert (equal x y);
    {x; y}

  let output_buffer out {x;y} =
    assert (equal x y);
    X.output_buffer out x;
    assert (equal x y)

  let add_aobuffer {x;y} {x=x';y=y'} =
    assert (equal x y);
    assert (equal x' y');
    X.add_aobuffer x x';
    Y.add_aobuffer y y';
    assert (equal x' y');
    assert (equal x y);
end

module Buf =
struct
  include Buffer
  let add_aobuffer = add_buffer
  let create _ = create 100
end

(* include Verify(Buf)(Mutable) *)
include Mutable

let bprintf buf = Printf.kprintf (add_string buf)

let formatter_of_aobuffer buf =
  Format.make_formatter (add_substring buf) ignore
