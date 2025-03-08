open OUnit2

let test_open_tokenize _ =
  let strs =
    List.map PostMixDB.Tokenize.string_of_token
      (PostMixDB.Tokenize.tokenize_string "OPEN \"database.db\"")
  in
  assert_equal strs ["OPEN"; "STRING(database.db)"]

let test_brackets_tokenize _ =
  let strs =
    List.map PostMixDB.Tokenize.string_of_token
      (PostMixDB.Tokenize.tokenize_string "OPEN (\"database.db\")")
  in
  assert_equal strs
    ["OPEN"; "OPEN_BRACKET"; "STRING(database.db)"; "CLOSE_BRACKET"]

let test_single_token_tokenize _ =
  let strs =
    List.map PostMixDB.Tokenize.string_of_token
      (PostMixDB.Tokenize.tokenize_string "LIST")
  in
  assert_equal strs ["LIST"]

let test_put_tokenize _ =
  let strs =
    List.map PostMixDB.Tokenize.string_of_token
      (PostMixDB.Tokenize.tokenize_string
         "PUT \"THIS IS A KEY\" \"THIS IS A VALUE\"" )
  in
  assert_equal strs ["PUT"; "STRING(THIS IS A KEY)"; "STRING(THIS IS A VALUE)"]

let test_tokenize_semicolon _ =
  let strs =
    List.map PostMixDB.Tokenize.string_of_token
      (PostMixDB.Tokenize.tokenize_string
        "LIST;" )
  in
  assert_equal strs ["LIST";"SEMICOLON"]

let test_tokenize_invalid_token _ =
  let f = (fun () -> PostMixDB.Tokenize.tokenize_string "BILLY" ) in
  assert_raises (Failure "Invalid Token") f

let test_tokenize_string_not_closed _ =
  let f = (fun () -> PostMixDB.Tokenize.tokenize_string "PUT \"THIS IS A KEY\" \"THIS IS A VALUE" ) in
  assert_raises (Failure "String never closed") f
