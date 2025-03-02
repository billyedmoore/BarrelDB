open OUnit2

let suite =
  "Testing Tests"
  >::: [ "test_bytes_of_int_empty" >:: BinaryUtilsTest.test_bytes_of_int_empty
       ; "test_bytes_of_int_small_value"
         >:: BinaryUtilsTest.test_bytes_of_int_small_value
       ; "test_bytes_of_int_max_value"
         >:: BinaryUtilsTest.test_bytes_of_int_max_value
       ; "test_db_get_long_value" >:: DbTest.test_get_long_value_from_db
       ; "test_db_get" >:: DbTest.test_get_from_db
       ; "test_db_delete" >:: DbTest.test_delete_from_db ]

let () = run_test_tt_main suite
