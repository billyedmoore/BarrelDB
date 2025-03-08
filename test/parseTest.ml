open OUnit2

let test_parse _ =
  let asts =
    PostMixDB.Parse.parse
      (PostMixDB.Tokenize.tokenize_string "OPEN \"database.db\" ;")
  in
  let strs = List.map PostMixDB.Parse.string_of_ast asts in
  assert_equal strs ["OPEN:\n\tSTRING database.db\n"]

let test_parse_immediate_semicolon _ =
  let asts =
    PostMixDB.Parse.parse (PostMixDB.Tokenize.tokenize_string "LIST;")
  in
  let strs = List.map PostMixDB.Parse.string_of_ast asts in
  assert_equal strs ["LIST\n\t"]

let test_parse_multiple_trees _ =
  let asts =
    PostMixDB.Parse.parse
      (PostMixDB.Tokenize.tokenize_string "OPEN \"database.db\" GET \"key\"")
  in
  let strs = List.map PostMixDB.Parse.string_of_ast asts in
  assert_equal strs ["OPEN:\n\tSTRING database.db\n"; "GET:\n\tSTRING key\n"]

let test_parse_put _ =
  let asts =
    PostMixDB.Parse.parse
      (PostMixDB.Tokenize.tokenize_string "PUT \"key\" \"value\" ;")
  in
  let strs = List.map PostMixDB.Parse.string_of_ast asts in
  assert_equal strs ["PUT:\n\tSTRING key\n\tSTRING value\n"]

let test_parse_missing_operand _ =
  let parse =
   fun () -> PostMixDB.Parse.parse (PostMixDB.Tokenize.tokenize_string "OPEN ;")
  in
  assert_raises (Failure "Invalid Non-terminal Statement") parse
