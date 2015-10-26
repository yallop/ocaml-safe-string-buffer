open Aobuffer

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

  val nth : t -> int -> char
end


module Verify (X: S) (Y: S) : S =
struct
  type t = {x: X.t; y: Y.t}

  let equal x y =
    let xcontents = X.contents x and ycontents = Y.contents y in
    if xcontents = ycontents then true
    else Printf.ksprintf failwith "%s <> %s" xcontents ycontents

  let op0 f g =
    fun {x; y} ->
      begin
        assert (equal x y);
        let r1 = f x in
        let r2 = g y in
        assert (equal x y);
        assert (r1 = r2);
        r1
      end

  let op1 f g =
    fun {x; y} v ->
      begin
        assert (equal x y);
        let r1 = f x v in
        let r2 = g y v in
        assert (equal x y);
        assert (r1 = r2);
        r1
      end

  let op3 f g =
    fun {x;y} a b c ->
      begin
        assert (equal x y);
        let r1 = f x a b c in
        let r2 = g y a b c in
        assert (equal x y);
        assert (r1 = r2);
        r1
      end

  let add_string   = op1 X.add_string Y.add_string
  let add_bytes    = op1 X.add_bytes  Y.add_bytes
  let add_char     = op1 X.add_char   Y.add_char
  let add_buffer   = op1 X.add_buffer Y.add_buffer
  let clear        = op0 X.clear  Y.clear
  let reset        = op0 X.reset  Y.reset
  let length       = op0 X.length Y.length
  let to_bytes     = op0 X.to_bytes Y.to_bytes
  let contents     = op0 X.contents Y.contents
  let nth          = op1 X.nth Y.nth
  (* let add_aobuffer = op1 X.add_aobuffer Y.add_aobuffer *)

  let add_substring = op3 X.add_substring Y.add_substring
  let add_subbytes  = op3 X.add_subbytes Y.add_subbytes

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
