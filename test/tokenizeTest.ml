open OUnit2

let test_open_tokenize _ =
  let strs =
    List.map BarrelDB.Tokenize.string_of_token
      (BarrelDB.Tokenize.tokenize_string "OPEN \"database.db\"")
  in
  assert_equal strs ["OPEN"; "QUOTE"; "STRING(database.db)"; "QUOTE"]

let test_brackets_tokenize _ =
  let strs =
    List.map BarrelDB.Tokenize.string_of_token
      (BarrelDB.Tokenize.tokenize_string "OPEN (\"database.db\")")
  in
  assert_equal strs
    [ "OPEN"
    ; "OPEN_BRACKET"
    ; "QUOTE"
    ; "STRING(database.db)"
    ; "QUOTE"
    ; "CLOSE_BRACKET" ]

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
  assert_equal strs
    [ "PUT"
    ; "QUOTE"
    ; "STRING(THIS)"
    ; "STRING(IS)"
    ; "STRING(A)"
    ; "STRING(KEY)"
    ; "QUOTE"
    ; "QUOTE"
    ; "STRING(THIS)"
    ; "STRING(IS)"
    ; "STRING(A)"
    ; "STRING(VALUE)"
    ; "QUOTE" ]
