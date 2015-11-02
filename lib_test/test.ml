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

    let () = Aobuffer.add_string buf1 "x" in

    assert_equal "" (Aobuffer.contents buf2);
    assert_equal "x" (Aobuffer.contents buf1);
  end


let test_add _ =
  let buf = Aobuffer.create () in
  begin
    let () = Aobuffer.add_char buf 'a' in
    assert_equal "a" (Aobuffer.contents buf);

    let () = Aobuffer.add_string buf "bcd" in
    assert_equal "abcd" (Aobuffer.contents buf);


    let () = Aobuffer.add_bytes buf (Bytes.of_string "efg") in
    assert_equal "abcdefg" (Aobuffer.contents buf);

    let () = Aobuffer.add_substring buf "abcdefghijklmno" 7 4 in
    assert_equal "abcdefghijk" (Aobuffer.contents buf);

    let () = Aobuffer.add_subbytes buf (Bytes.of_string "abcdefghijklmno") 11 2 in
    assert_equal "abcdefghijklm" (Aobuffer.contents buf);

    let buf2 = Buffer.create 10 in
    let () = Buffer.add_string buf2 "nopq" in
    let () = Aobuffer.add_buffer buf buf2 in
    assert_equal "abcdefghijklmnopq" (Aobuffer.contents buf);

    let buf3 = Aobuffer.create () in
    let () = Aobuffer.add_string buf3 "rst" in
    let () = Aobuffer.add_aobuffer buf buf3 in
    assert_equal "abcdefghijklmnopqrst" (Aobuffer.contents buf);
  end


let test_clear _ =
  let buf = Aobuffer.create () in
  let () = Aobuffer.add_string buf "abc" in
  assert_equal "abc" (Aobuffer.contents buf);

  let () = Aobuffer.clear buf in
  assert_equal "" (Aobuffer.contents buf)

  
let test_reset _ =
  let buf = Aobuffer.create () in
  let () = Aobuffer.add_string buf "abc" in
  assert_equal "abc" (Aobuffer.contents buf);

  let () = Aobuffer.reset buf in
  assert_equal "" (Aobuffer.contents buf)


let test_formatter _ =
  let buf = Aobuffer.create () in
  let fmt = Aobuffer.formatter_of_aobuffer buf in
  let () = Format.fprintf fmt "x%az@." (fun fmt -> Format.fprintf fmt "%d") 3 in
  assert_equal "x3z\n" (Aobuffer.contents buf)


let test_bprintf _ =
  let buf = Aobuffer.create () in
  let () = Aobuffer.bprintf buf "%d%s" 3 "four" in
  assert_equal "3four" (Aobuffer.contents buf)


let test_to_bytes _ =
  let buf = Aobuffer.create () in
  let () = Aobuffer.add_string buf "a" in
  let b1 = Aobuffer.to_bytes buf in
  let b2 = Aobuffer.to_bytes buf in
  begin
    assert_equal (Bytes.of_string "a") b1;
    assert_equal (Bytes.of_string "a") b2;
    assert_bool "to_bytes allocates fresh values" (b1 != b2)
  end


let suite = "Aobuffer tests" >::: [
    "length"
    >:: test_length;
    
    "create"
    >:: test_create;
    
    "add"
    >:: test_add;

    "clear"
    >:: test_clear;

    "reset"
    >:: test_reset;

    "formatter"
    >:: test_formatter;

    "bprintf"
    >:: test_bprintf;

    "to_bytes"
    >:: test_to_bytes;
(*
TODO: sub
TODO: blit
TODO: nth
*)

  ]


let _ =
  run_test_tt_main suite
