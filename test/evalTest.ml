open OUnit2

let test_eval_create _ =
  let asts =
    PostMixDB.Parse.parse
      (PostMixDB.Tokenize.tokenize_string "CREATE \"test_eval_create.db\";")
  in
  let eval_ast ast = Result.get_ok (PostMixDB.Eval.evaluate_ast ast None) in
  match List.map eval_ast asts with
  | PostMixDB.Eval.DB_SESSION _ :: [] ->
      ()
  | _ ->
      failwith "CREATE didn't create a dbSession"

let test_eval_multiple_trees _ =
  let asts =
    PostMixDB.Parse.parse
      (PostMixDB.Tokenize.tokenize_string
         "CREATE \"test_eval_multiple_trees.db\"; PUT \"key\" \"value\"; GET \
          \"key\"" )
  in
  match Result.get_ok (PostMixDB.Eval.evaluate_asts asts None) with
  | STRING_RESULT str ->
      assert_equal str "value"
  | _ ->
      failwith "Expected the result to be STRING_RESULT but isn't"

let test_eval_multiple_trees_open _ =
  ignore
    (PostMixDB.Eval.evaluate_asts
       (PostMixDB.Parse.parse
          (PostMixDB.Tokenize.tokenize_string
             "CREATE \"test_eval_multiple_trees_open.db\";" ) )
       None ) ;
  let asts =
    PostMixDB.Parse.parse
      (PostMixDB.Tokenize.tokenize_string
         "OPEN \"test_eval_multiple_trees_open.db\";LIST;PUT \"key\" \
          \"value\"; GET \"key\"" )
  in
  match Result.get_ok (PostMixDB.Eval.evaluate_asts asts None) with
  | STRING_RESULT str ->
      assert_equal str "value"
  | _ ->
      failwith "Expected the result to be STRING_RESULT but isn't"

let test_eval_multiple_trees_list _ =
  let asts =
    PostMixDB.Parse.parse
      (PostMixDB.Tokenize.tokenize_string
         "CREATE \"test_eval_multiple_trees_list.db\";\n\
         \        PUT \"key01\" \"value\";\n\
         \        PUT \"key02\" \"value\";\n\
         \        PUT \"key03\" \"value\";\n\
         \        PUT \"key04\" \"value\";\n\
         \        PUT \"key05\" \"value\";\n\
         \        PUT \"key06\" \"value\";\n\
         \        PUT \"key07\" \"value\";\n\
         \        PUT \"key08\" \"value\";\n\
         \        PUT \"key09\" \"value\";\n\
         \        PUT \"key10\" \"value\";\n\
         \        LIST" )
  in
  match Result.get_ok (PostMixDB.Eval.evaluate_asts asts None) with
  | INFO_STRING_RESULT str ->
      assert_equal str
        "KEYS: [key01, key02, key03, key04, key05, key06, key07, key08, key09, \
         key10]"
  | _ ->
      failwith "Expected the result to be INFO_STRING_RESULT but isn't"

let test_eval_multiple_trees_delete _ =
  let asts =
    PostMixDB.Parse.parse
      (PostMixDB.Tokenize.tokenize_string
         "CREATE \"test_eval_multiple_trees_delete.db\";\n\
         \        PUT \"key01\" \"value\";\n\
         \        PUT \"key02\" \"value\";\n\
         \        PUT \"key03\" \"value\";\n\
         \        PUT \"key04\" \"value\";\n\
         \        PUT \"key05\" \"value\";\n\
         \        PUT \"key06\" \"value\";\n\
         \        PUT \"key07\" \"value\";\n\
         \        PUT \"key08\" \"value\";\n\
         \        PUT \"key09\" \"value\";\n\
         \        PUT \"key10\" \"value\";\n\
         \         DELETE \"key01\";\n\
         \         DELETE \"key02\";\n\
         \                 LIST" )
  in
  match Result.get_ok (PostMixDB.Eval.evaluate_asts asts None) with
  | INFO_STRING_RESULT str ->
      assert_equal str
        "KEYS: [key03, key04, key05, key06, key07, key08, key09, key10]"
  | _ ->
      failwith "Expected the result to be INFO_STRING_RESULT but isn't"
