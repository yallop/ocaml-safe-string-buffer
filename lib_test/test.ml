open OUnit2

let test_length _ =
  let buf = Aobuffer.create () in
  begin
    assert_equal 0 (Aobuffer.length buf);

    let () = Aobuffer.add_string buf "four" in
    assert_equal 4 (Aobuffer.length buf);
    
    let () = Aobuffer.add_char buf 'a' in
    assert_equal 5 (Aobuffer.length buf);
  end


let test_create _ =
  let buf1 = Aobuffer.create ()
  and buf2 = Aobuffer.create () in
  begin
    assert_bool "create builds physically-distinct buffers"
      (buf1 != buf2);

    let s = Aobuffer.add_string buf1 "x" in

    assert_equal "" (Aobuffer.contents buf2);
    assert_equal "x" (Aobuffer.contents buf1);
  end
(*
val contents : t -> string
val to_bytes : t -> bytes
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

val formatter_of_aobuffer : t -> Format.formatter
                                 *)

let suite = "Aobuffer tests" >:::
  ["length"
    >:: test_length;

   "create"
    >:: test_create;
  ]



let _ =
  run_test_tt_main suite
