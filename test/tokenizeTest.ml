open OUnit2

let test_open_tokenize _ =
  let strs =
    List.map BarrelDB.Tokenize.string_of_token
      (BarrelDB.Tokenize.tokenize_string "OPEN \"database.db\"")
  in
  assert_equal strs ["OPEN"; "STRING(database.db)"]

let test_brackets_tokenize _ =
  let strs =
    List.map BarrelDB.Tokenize.string_of_token
      (BarrelDB.Tokenize.tokenize_string "OPEN (\"database.db\")")
  in
  assert_equal strs
    ["OPEN"; "OPEN_BRACKET"; "STRING(database.db)"; "CLOSE_BRACKET"]

let test_single_token_tokenize _ =
  let strs =
    List.map BarrelDB.Tokenize.string_of_token
      (BarrelDB.Tokenize.tokenize_string "LIST")
  in
  assert_equal strs ["LIST"]

let test_put_tokenize _ =
  let strs =
    List.map BarrelDB.Tokenize.string_of_token
      (BarrelDB.Tokenize.tokenize_string
         "PUT \"THIS IS A KEY\" \"THIS IS A VALUE\"" )
  in
  assert_equal strs ["PUT"; "STRING(THIS IS A KEY)"; "STRING(THIS IS A VALUE)"]
