open OUnit2

let test_length _ =
  let buf = SafeStringBuffer.create () in
  begin
    assert_equal 0 (SafeStringBuffer.length buf);

    let () = SafeStringBuffer.add_string buf "four" in
    assert_equal 4 (SafeStringBuffer.length buf);
    
    let () = SafeStringBuffer.add_char buf 'a' in
    assert_equal 5 (SafeStringBuffer.length buf);
  end


let test_create _ =
  let buf1 = SafeStringBuffer.create ()
  and buf2 = SafeStringBuffer.create () in
  begin
    assert_bool "create builds physically-distinct buffers"
      (buf1 != buf2);

    let () = SafeStringBuffer.add_string buf1 "x" in

    assert_equal "" (SafeStringBuffer.contents buf2);
    assert_equal "x" (SafeStringBuffer.contents buf1);
  end


let test_add _ =
  let test_invalid_bounds ~msg name f =
    assert_raises ~msg (Invalid_argument name) f in
  let buf = SafeStringBuffer.create () in
  begin
    let () = SafeStringBuffer.add_char buf 'a' in
    assert_equal "a" (SafeStringBuffer.contents buf);

    let () = SafeStringBuffer.add_string buf "bcd" in
    assert_equal "abcd" (SafeStringBuffer.contents buf);


    let () = SafeStringBuffer.add_bytes buf (Bytes.of_string "efg") in
    assert_equal "abcdefg" (SafeStringBuffer.contents buf);

    let () = SafeStringBuffer.add_substring buf "abcdefghijklmno" 7 4 in
    assert_equal "abcdefghijk" (SafeStringBuffer.contents buf);

    let () = test_invalid_bounds "add_substring"
        ~msg:"negative offset"
        (fun () -> SafeStringBuffer.add_substring buf "abc" (-1) 1); in

    let () = test_invalid_bounds "add_substring"
        ~msg:"negative length"
        (fun () -> SafeStringBuffer.add_substring buf "abc" 1 (-1)); in

    let () = test_invalid_bounds "add_substring"
        ~msg:"length > length(string)"
        (fun () -> SafeStringBuffer.add_substring buf "abc" 0 4); in

    let () = test_invalid_bounds "add_substring"
        ~msg:"offset + length > length(string)"
        (fun () -> SafeStringBuffer.add_substring buf "abc" 1 3); in

    let () = SafeStringBuffer.add_subbytes buf (Bytes.of_string "abcdefghijklmno") 11 2 in
    assert_equal "abcdefghijklm" (SafeStringBuffer.contents buf);

    let () = test_invalid_bounds "add_subbytes"
        ~msg:"negative offset"
        (fun () -> SafeStringBuffer.add_subbytes buf (Bytes.of_string "abc") (-1) 1); in

    let () = test_invalid_bounds "add_subbytes"
        ~msg:"negative length"
        (fun () -> SafeStringBuffer.add_subbytes buf (Bytes.of_string "abc") 1 (-1)); in

    let () = test_invalid_bounds "add_subbytes"
        ~msg:"length > length(bytes)"
        (fun () -> SafeStringBuffer.add_subbytes buf (Bytes.of_string "abc") 0 4); in

    let () = test_invalid_bounds "add_subbytes"
        ~msg:"offset + length > length(bytes)"
        (fun () -> SafeStringBuffer.add_subbytes buf (Bytes.of_string "abc") 1 3); in

    let buf2 = Buffer.create 10 in
    let () = Buffer.add_string buf2 "nopq" in
    let () = SafeStringBuffer.add_buffer buf buf2 in
    assert_equal "abcdefghijklmnopq" (SafeStringBuffer.contents buf);

    let buf3 = SafeStringBuffer.create () in
    let () = SafeStringBuffer.add_string buf3 "rst" in
    let () = SafeStringBuffer.add_safe_string_buffer buf buf3 in
    assert_equal "abcdefghijklmnopqrst" (SafeStringBuffer.contents buf);

    (* test add_channel full read *)
    let buf = SafeStringBuffer.create () in
    let fd = open_in "data.txt" in
    let () = SafeStringBuffer.add_channel buf fd 4 in
    assert_equal "abc\n" (SafeStringBuffer.contents buf);

    (* test add_channel partial read *)
    let buf = SafeStringBuffer.create () in
    let fd = open_in "data.txt" in
    let () = SafeStringBuffer.add_channel buf fd 2 in
    assert_equal "ab" (SafeStringBuffer.contents buf);

    (* test add_channel short read. *)
    let buf = SafeStringBuffer.create () in
    let fd = open_in "data.txt" in
    let () =
      assert_raises End_of_file @@ fun () ->
      SafeStringBuffer.add_channel buf fd 6
    in
    (* This is currently broken due to a bug in the standard library:
       http://caml.inria.fr/mantis/view.php?id=7136 *)
    ()
    (* assert_equal "abc\n" (SafeStringBuffer.contents buf) *)
    ;

    (* Simple tests for the success case only, since the behaviour of
       add_substitute is not currently very precisely specified. *)
    let buf = SafeStringBuffer.create () in
    let () = SafeStringBuffer.add_substitute  buf
        (function "x" -> "y" | "abc" -> "def" | s -> s)
        "r$abc${x}s" in
    assert_equal "rdefys"
      (SafeStringBuffer.contents buf)
  end


let test_clear _ =
  let buf = SafeStringBuffer.create () in
  let () = SafeStringBuffer.add_string buf "abc" in
  assert_equal "abc" (SafeStringBuffer.contents buf);

  let () = SafeStringBuffer.clear buf in
  assert_equal "" (SafeStringBuffer.contents buf)

  
let test_reset _ =
  let buf = SafeStringBuffer.create () in
  let () = SafeStringBuffer.add_string buf "abc" in
  assert_equal "abc" (SafeStringBuffer.contents buf);

  let () = SafeStringBuffer.reset buf in
  assert_equal "" (SafeStringBuffer.contents buf)


let test_formatter _ =
  let buf = SafeStringBuffer.create () in
  let fmt = SafeStringBuffer.formatter_of_safe_string_buffer buf in
  let () = Format.fprintf fmt "x%az@." (fun fmt -> Format.fprintf fmt "%d") 3 in
  assert_equal "x3z\n" (SafeStringBuffer.contents buf)


let test_bprintf _ =
  let buf = SafeStringBuffer.create () in
  let () = SafeStringBuffer.bprintf buf "%d%s" 3 "four" in
  assert_equal "3four" (SafeStringBuffer.contents buf)


let test_to_bytes _ =
  let buf = SafeStringBuffer.create () in
  let () = SafeStringBuffer.add_string buf "a" in
  let b1 = SafeStringBuffer.to_bytes buf in
  let b2 = SafeStringBuffer.to_bytes buf in
  begin
    assert_equal (Bytes.of_string "a") b1;
    assert_equal (Bytes.of_string "a") b2;
    assert_bool "to_bytes allocates fresh values" (b1 != b2)
  end


let test_sub _ =
  let buf = SafeStringBuffer.create () in
  let () = List.iter (SafeStringBuffer.add_string buf)
      ["abc"; ""; "def"; "ghi"] in
  let s = SafeStringBuffer.contents buf in
  let slen = String.length s in
  begin
    for ofs = 0 to slen - 1 do
      for len = 0 to slen - ofs do
        assert_equal
          (SafeStringBuffer.sub buf ofs len)
          (String.sub s ofs len)
      done
    done
  end


let test_sub_invalid_args _ =
  let buf = SafeStringBuffer.create () in
  let () = List.iter (SafeStringBuffer.add_string buf)
      ["abc"; ""; "def"; "ghi"] in
  let test_invalid_bounds ~msg ofs len =
    assert_raises ~msg (Invalid_argument "sub") @@ fun () ->
    SafeStringBuffer.sub buf ofs len
  in
  begin
    test_invalid_bounds (-1) 1
      ~msg:"Negative offset";
    test_invalid_bounds 0 (-1)
      ~msg:"Negative length";
    test_invalid_bounds 0 (String.length (SafeStringBuffer.contents buf) + 1)
      ~msg:"Length too large";
    test_invalid_bounds 1 (String.length (SafeStringBuffer.contents buf))
      ~msg:"Length too large (non-zero offset)";
  end


let test_nth _ =
  let buf = SafeStringBuffer.create () in
  begin
    SafeStringBuffer.add_string buf "abc";
    assert_equal 'a' (SafeStringBuffer.nth buf 0);
    assert_equal 'b' (SafeStringBuffer.nth buf 1);
    assert_equal 'c' (SafeStringBuffer.nth buf 2);

    SafeStringBuffer.add_string buf "def";
    assert_equal 'a' (SafeStringBuffer.nth buf 0);
    assert_equal 'b' (SafeStringBuffer.nth buf 1);
    assert_equal 'c' (SafeStringBuffer.nth buf 2);
    assert_equal 'd' (SafeStringBuffer.nth buf 3);
    assert_equal 'e' (SafeStringBuffer.nth buf 4);
    assert_equal 'f' (SafeStringBuffer.nth buf 5);
  end

let test_nth_invalid_args _ =
  let buf = SafeStringBuffer.create () in
  let test_invalid_bounds ~msg i =
    assert_raises ~msg (Invalid_argument "nth") @@ fun () ->
    SafeStringBuffer.nth buf i
  in
  begin
    test_invalid_bounds 0
      ~msg:"accessing empty buffer";

    SafeStringBuffer.add_string buf "abc";
    
    test_invalid_bounds (-1)
      ~msg:"negative index";

    test_invalid_bounds 3
      ~msg:"index out of range";
  end


let test_blit _ =
  let buf = SafeStringBuffer.create () in
  let () = SafeStringBuffer.add_string buf "abc" in

  begin
    let bytes = Bytes.make 3 '3' in
    let () = SafeStringBuffer.blit buf 0 bytes 0 3 in
    assert_equal (Bytes.of_string "abc") bytes;

    let bytes = Bytes.make 3 '3' in
    let () = SafeStringBuffer.blit buf 0 bytes 0 2 in
    assert_equal (Bytes.of_string "ab3") bytes;

    let bytes = Bytes.make 3 '3' in
    let () = SafeStringBuffer.blit buf 0 bytes 1 2 in
    assert_equal (Bytes.of_string "3ab") bytes;

    let bytes = Bytes.make 3 '3' in
    let () = SafeStringBuffer.blit buf 1 bytes 0 2 in
    assert_equal (Bytes.of_string "bc3") bytes;

    let bytes = Bytes.make 3 '3' in
    let () = SafeStringBuffer.blit buf 1 bytes 1 2 in
    assert_equal (Bytes.of_string "3bc") bytes;
  end


let test_blit_invalid_args _ =
  let buf = SafeStringBuffer.create () in
  let () = SafeStringBuffer.add_string buf "abc"
  and smallbytes = Bytes.create 2
  and bigbytes = Bytes.create 10 in
  let test_invalid_bounds ~msg ~srcoff ~dst ~dstoff ~len : unit =
    assert_raises ~msg (Invalid_argument "blit") @@ fun () ->
    SafeStringBuffer.blit buf srcoff dst dstoff len
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


let suite = "SafeStringBuffer tests" >::: [
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

let _ =
  run_test_tt_main suite
