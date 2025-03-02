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
       ; "test_db_delete" >:: DbTest.test_delete_from_db
       ; "test_db_list_keys" >:: DbTest.test_list_keys_from_db
       ; "test_db_list_keys_with_removed_keys"
         >:: DbTest.test_list_keys_with_keys_removed_db
       ; "test_open_tokenize" >:: TokenizeTest.test_open_tokenize
       ; "test_brackets_tokenize" >:: TokenizeTest.test_brackets_tokenize
       ; "test_single_token_tokenize" >:: TokenizeTest.test_brackets_tokenize
       ; "test_put_tokenize" >:: TokenizeTest.test_put_tokenize ]

let () = run_test_tt_main suite
