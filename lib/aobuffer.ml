module Check_safe_string_used =
struct
  (* The Aobuffer module must be compiled with -safe-string, since it assumes
     that strings passed in are not subsequently modified elsewhere.  The
     Check_safe_string_used module checks statically that -safe-string is in
     use. *)
  type _ safe_string_check =
    (* The index of Anything can be refined to any type *)
      Anything : _ safe_string_check
    (* The index of Is_sting can only be refined to type string *)
    | Is_string : string safe_string_check
          
  (* Check that the -safe-string is used.  Iff -safe-string is enabled then
     bytes and string are incompatible types, and so the pattern matching is
     exhaustive. *)
  [@@@ocaml.warning "@8"]
  let check_safe_string_used : bytes safe_string_check -> unit = function
      Anything -> ()
end

type t =
  { mutable elements: string list;
    (** A buffer is stored as a list of elements, kept in reverse order; that
        is, the first element on the list is the most recently added. *)
    mutable length: int; 
    (** The length field is the sum of the length of the elements. *) }

let create () = { elements = []; length = 0 }

let add_string buf s =
  begin
    buf.elements <- s :: buf.elements;
    buf.length <- buf.length + String.length s
  end

let add_bytes buf b = add_string buf (Bytes.to_string b)

let add_char buf c = add_string buf (String.make 1 c)

let length { length } = length

let add_subbytes buf b ofs len =
  if ofs < 0 || len < 0 || ofs + len > Bytes.length b
  then invalid_arg "add_subbytes"
  else add_string buf (Bytes.sub_string b ofs len)

let add_substring buf s ofs len =
  if ofs < 0 || len < 0 || ofs + len > String.length s
  then invalid_arg "add_substring"
  else if ofs = 0 && len = String.length s then add_string buf s 
  else add_string buf (String.sub s ofs len)

let clear buf =
  begin
    buf.elements <- [];
    buf.length <- 0
  end

let reset = clear

let to_bytes {elements; length} = 
  let b = Bytes.create length in
  let _ = List.fold_left
      (fun i s ->
         let l = String.length s in
         Bytes.blit_string s 0 b (i - l) l;
         i - l)
      length
      elements
  in
  b

let contents = function
  { elements = [] } -> ""
| { elements = [s] } -> s
| buf -> Bytes.unsafe_to_string (to_bytes buf)

let iter_elements f elements =
  List.fold_right (fun elem () -> ignore (f elem)) elements ()

let output_buffer outch { elements } =
  iter_elements (output_string outch) elements

let add_aobuffer l r =
  begin
    l.elements <- r.elements @ l.elements;
    l.length <- l.length + r.length
  end

let add_buffer buf b = add_string buf (Buffer.contents b)

let nth buf i =
  if i < 0 || i >= buf.length then invalid_arg "nth" else
    let rec loop length = function
        [] ->
        (* Invariant violated: the bounds check above should have
           caught this case *)
        assert false
      | s :: ss ->
        let slen = String.length s in
        let sslen = length - slen in
        if i >= sslen then s.[i - sslen]
        else loop sslen ss
    in loop buf.length buf.elements

let rec blit_loop elements elements_len srcofs dst dstofs length =
  match elements with
    [] -> ()
  | last :: firsts ->
    let last_len = String.length last in
    let first_len = elements_len - last_len in

    (* Case 1: the string to copy lies entirely within last *)
    if srcofs >= first_len then
      Bytes.blit_string last (srcofs - first_len) dst dstofs length

    (* Case 2: the string to copy lies entirely within firsts *)
    else if srcofs + length < first_len then
      blit_loop firsts first_len srcofs dst dstofs length

    (* Case 3: the string to copy lies partly within firsts and
       partly within last. *)
    else
      let nchars = srcofs + length - first_len in
      begin
        Bytes.blit_string last 0 dst (dstofs + length - nchars) nchars;
        blit_loop firsts first_len srcofs dst dstofs (length - nchars)
      end

let blit buf srcofs dst dstofs length =
  if length < 0 || srcofs < 0 || srcofs > buf.length - length
                || dstofs < 0 || dstofs > (Bytes.length dst) - length
  then invalid_arg "blit"
  else blit_loop buf.elements buf.length srcofs dst dstofs length

let sub buf ofs length =
  if length < 0 || ofs < 0 || ofs > buf.length - length
  then invalid_arg "sub"
  else
    let b = Bytes.create length in
    begin
      blit buf ofs b 0 length;
      Bytes.unsafe_to_string b
    end
    
let bprintf buf = Printf.kprintf (add_string buf)

let formatter_of_aobuffer buf =
  Format.make_formatter (add_substring buf) ignore

let add_channel buf channel n =
  let b = Buffer.create 16 in
  let () = Buffer.add_channel b channel n in
  add_buffer buf b

let add_substitute buf f s =
  let b = Buffer.create 16 in
  let () = Buffer.add_substitute b f s in
  add_buffer buf b
