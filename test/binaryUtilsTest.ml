open OUnit2

let int_list_of_bytes (byts : Bytes.t) =
  let rec loop (i : int) (bytes : Bytes.t) (bytes_as_int : int list) =
    match i with
    | 0 ->
        bytes_as_int
    | _ ->
        loop (i - 1) bytes (Char.code (Bytes.get bytes (i - 1)) :: bytes_as_int)
  in
  loop (Bytes.length byts) byts []

let test_bytes_of_int_empty _ =
  assert_equal
    (int_list_of_bytes (BarrelDB.BinaryUtils.bytes_of_int ~value:0 1))
    [0]

let test_bytes_of_int_small_value _ =
  assert_equal
    (int_list_of_bytes (BarrelDB.BinaryUtils.bytes_of_int ~value:64 8))
    [64; 0; 0; 0; 0; 0; 0; 0]

let test_bytes_of_int_max_value _ =
  assert_equal
    (int_list_of_bytes (BarrelDB.BinaryUtils.bytes_of_int ~value:65535 4))
    [255; 255; 0; 0]
