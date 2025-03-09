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
       ; "test_put_tokenize" >:: TokenizeTest.test_put_tokenize
       ; "test_tokenize_string_not_closed"
         >:: TokenizeTest.test_tokenize_string_not_closed
       ; "test_tokenize_semicolon" >:: TokenizeTest.test_tokenize_semicolon
       ; "test_tokenize_invalid_token"
         >:: TokenizeTest.test_tokenize_invalid_token
       ; "test_parse" >:: ParseTest.test_parse
       ; "test_parse_nested" >:: ParseTest.test_parse_nested
       ; "test_parse_missing_operand" >:: ParseTest.test_parse_missing_operand
       ; "test_parse_put" >:: ParseTest.test_parse_put
       ; "test_parse_multiple_trees" >:: ParseTest.test_parse_multiple_trees
       ; "test_parse_immediate_semicolon"
         >:: ParseTest.test_parse_immediate_semicolon
       ; "test_eval_create" >:: EvalTest.test_eval_create
       ; "test_eval_multiple_trees" >:: EvalTest.test_eval_multiple_trees
       ; "test_eval_multiple_trees_open"
         >:: EvalTest.test_eval_multiple_trees_open
       ; "test_eval_multiple_trees_list"
         >:: EvalTest.test_eval_multiple_trees_list
       ; "test_eval_multiple_trees_delete"
         >:: EvalTest.test_eval_multiple_trees_delete ]

let () = run_test_tt_main suite
