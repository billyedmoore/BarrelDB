open OUnit2

let test_parse _ =
  let asts =
    Result.get_ok
      (PostMixDB.Parse.parse
         (Result.get_ok
            (PostMixDB.Tokenize.tokenize_string "OPEN \"database.db\" ;") ) )
  in
  let strs = List.map PostMixDB.Parse.string_of_ast asts in
  assert_equal strs ["OPEN:\n\tSTRING database.db\n"]

let test_parse_nested _ =
  let asts =
    Result.get_ok
      (PostMixDB.Parse.parse
         (Result.get_ok
            (PostMixDB.Tokenize.tokenize_string "OPEN GET GET \"KEY\";") ) )
  in
  let strs = List.map PostMixDB.Parse.string_of_ast asts in
  assert_equal strs ["OPEN:\n\tGET:\n\t\tGET:\n\t\t\tSTRING KEY\n"]

let test_parse_immediate_semicolon _ =
  let asts =
    Result.get_ok
      (PostMixDB.Parse.parse
         (Result.get_ok (PostMixDB.Tokenize.tokenize_string "LIST;")) )
  in
  let strs = List.map PostMixDB.Parse.string_of_ast asts in
  assert_equal strs ["LIST\n"]

let test_parse_multiple_trees _ =
  let asts =
    Result.get_ok
      (PostMixDB.Parse.parse
         (Result.get_ok
            (PostMixDB.Tokenize.tokenize_string
               "OPEN \"database.db\" GET \"key\"" ) ) )
  in
  let strs = List.map PostMixDB.Parse.string_of_ast asts in
  assert_equal strs ["OPEN:\n\tSTRING database.db\n"; "GET:\n\tSTRING key\n"]

let test_parse_put _ =
  let asts =
    Result.get_ok
      (PostMixDB.Parse.parse
         (Result.get_ok
            (PostMixDB.Tokenize.tokenize_string "PUT \"key\" \"value\" ;") ) )
  in
  let strs = List.map PostMixDB.Parse.string_of_ast asts in
  assert_equal strs ["PUT:\n\tSTRING key\n\tSTRING value\n"]

let test_parse_missing_operand _ =
  let err =
    Result.get_error
      (PostMixDB.Parse.parse
         (Result.get_ok (PostMixDB.Tokenize.tokenize_string "OPEN ;")) )
  in
  assert_equal (PostMixDB.Db.SyntaxError "Invalid Non-terminal Statement") err
