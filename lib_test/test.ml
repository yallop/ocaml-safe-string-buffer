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


let test_sub _ =
  let buf = Aobuffer.create () in
  let () = List.iter (Aobuffer.add_string buf)
      ["abc"; ""; "def"; "ghi"] in
  let s = Aobuffer.contents buf in
  let slen = String.length s in
  begin
    for ofs = 0 to slen - 1 do
      for len = 0 to slen - ofs do
        assert_equal
          (Aobuffer.sub buf ofs len)
          (String.sub s ofs len)
      done
    done
  end


let test_sub_invalid_args _ =
  let buf = Aobuffer.create () in
  let () = List.iter (Aobuffer.add_string buf)
      ["abc"; ""; "def"; "ghi"] in
  let test_invalid_bounds ~msg ofs len =
    assert_raises ~msg (Invalid_argument "sub") @@ fun () ->
    Aobuffer.sub buf ofs len
  in
  begin
    test_invalid_bounds (-1) 1
      ~msg:"Negative offset";
    test_invalid_bounds 0 (-1)
      ~msg:"Negative length";
    test_invalid_bounds 0 (String.length (Aobuffer.contents buf) + 1)
      ~msg:"Length too large";
    test_invalid_bounds 1 (String.length (Aobuffer.contents buf))
      ~msg:"Length too large (non-zero offset)";
  end


let test_nth _ =
  let buf = Aobuffer.create () in
  begin
    Aobuffer.add_string buf "abc";
    assert_equal 'a' (Aobuffer.nth buf 0);
    assert_equal 'b' (Aobuffer.nth buf 1);
    assert_equal 'c' (Aobuffer.nth buf 2);

    Aobuffer.add_string buf "def";
    assert_equal 'a' (Aobuffer.nth buf 0);
    assert_equal 'b' (Aobuffer.nth buf 1);
    assert_equal 'c' (Aobuffer.nth buf 2);
    assert_equal 'd' (Aobuffer.nth buf 3);
    assert_equal 'e' (Aobuffer.nth buf 4);
    assert_equal 'f' (Aobuffer.nth buf 5);
  end

let test_nth_invalid_args _ =
  let buf = Aobuffer.create () in
  let test_invalid_bounds ~msg i =
    assert_raises ~msg (Invalid_argument "nth") @@ fun () ->
    Aobuffer.nth buf i
  in
  begin
    test_invalid_bounds 0
      ~msg:"accessing empty buffer";

    Aobuffer.add_string buf "abc";
    
    test_invalid_bounds (-1)
      ~msg:"negative index";

    test_invalid_bounds 3
      ~msg:"index out of range";
  end


let test_blit _ =
  let buf = Aobuffer.create () in
  let () = Aobuffer.add_string buf "abc" in

  begin
    let bytes = Bytes.make 3 '3' in
    let () = Aobuffer.blit buf 0 bytes 0 3 in
    assert_equal (Bytes.of_string "abc") bytes;

    let bytes = Bytes.make 3 '3' in
    let () = Aobuffer.blit buf 0 bytes 0 2 in
    assert_equal (Bytes.of_string "ab3") bytes;

    let bytes = Bytes.make 3 '3' in
    let () = Aobuffer.blit buf 0 bytes 1 2 in
    assert_equal (Bytes.of_string "3ab") bytes;

    let bytes = Bytes.make 3 '3' in
    let () = Aobuffer.blit buf 1 bytes 0 2 in
    assert_equal (Bytes.of_string "bc3") bytes;

    let bytes = Bytes.make 3 '3' in
    let () = Aobuffer.blit buf 1 bytes 1 2 in
    assert_equal (Bytes.of_string "3bc") bytes;
  end


let test_blit_invalid_args _ =
  let buf = Aobuffer.create () in
  let () = Aobuffer.add_string buf "abc"
  and smallbytes = Bytes.create 2
  and bigbytes = Bytes.create 10 in
  let test_invalid_bounds ~msg ~srcoff ~dst ~dstoff ~len : unit =
    assert_raises ~msg (Invalid_argument "blit") @@ fun () ->
    Aobuffer.blit buf srcoff dst dstoff len
  in
  begin
    test_invalid_bounds
      ~srcoff:(-1) ~dstoff:0 ~len:1
      ~dst:smallbytes
      ~msg:"Negative source offset";

    test_invalid_bounds
      ~srcoff:0 ~dstoff:0 ~len:4
      ~dst:bigbytes
      ~msg:"Source offset + len > length buf";

    test_invalid_bounds
      ~srcoff:1 ~dstoff:0 ~len:(-1)
      ~dst:bigbytes
      ~msg:"Negative len";

    test_invalid_bounds
      ~srcoff:0 ~dstoff:2 ~len:2
      ~dst:smallbytes
      ~msg:"Dst offset + len > length dst";

    test_invalid_bounds
      ~srcoff:0 ~dstoff:3 ~len:0
      ~dst:smallbytes
      ~msg:"Dst offset > length dst";
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

    "sub"
    >:: test_sub;

    "sub: invalid arguments"
    >:: test_sub_invalid_args;

    "blit"
    >:: test_blit;

    "blit"
    >:: test_blit_invalid_args;

    "nth"
    >:: test_nth;

    "nth"
    >:: test_nth_invalid_args;
  ]

(* TODO: add_subbytes, add_substring invalid arguments *)
(* TODO: add_channel, add_substitute *)


let _ =
  run_test_tt_main suite
