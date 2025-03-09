type evaluationResult =
  | STRING_RESULT of string
  | INFO_STRING_RESULT of string
  | DB_SESSION of Db.dbSession

let string_of_evaluation_result (res : evaluationResult) =
  match res with
  | STRING_RESULT s ->
      s
  | _ ->
      failwith "EvaluationResult should be String but isn't."

let evaluate_ast (tree : Parse.ast) (db_session_opt : Db.dbSession option) :
    (evaluationResult, Db.dbError) result =
  let rec evaluate_ast_rec (tree : Parse.ast) (db_session : Db.dbSession option)
      : (evaluationResult, Db.dbError) result =
    match tree with
    | STRING str ->
        Result.ok (STRING_RESULT str)
    | OPEN _ ->
        evaluate_open tree db_session
    | CREATE _ ->
        evaluate_create tree db_session
    | GET _ ->
        evaluate_get tree db_session
    | PUT (_, _) ->
        evaluate_put tree db_session
    | DELETE _ ->
        evaluate_delete tree db_session
    | LIST ->
        evaluate_list tree db_session
    | NOP ->
        failwith "NOP should have been removed from AST."
  and evaluate_open_or_create (f : string -> (Db.dbSession, Db.dbError) result)
      (tree : Parse.ast) (db_session : Db.dbSession option) :
      (evaluationResult, Db.dbError) result =
    match tree with
    | OPEN sub_tree | CREATE sub_tree -> (
      match db_session with
      | Some _ ->
          Result.error Db.DatabaseAlreadyOpen
      | None -> (
        match evaluate_ast_rec sub_tree None with
        | Ok res -> (
          match f (string_of_evaluation_result res) with
          | Ok res ->
              Result.ok (DB_SESSION res)
          | Error err ->
              Result.error err )
        | Error err ->
            Result.error err ) )
    | _ ->
        failwith "Invalid OPEN/CREATE"
  and evaluate_open (tree : Parse.ast) (db_session : Db.dbSession option) :
      (evaluationResult, Db.dbError) result =
    evaluate_open_or_create Db.load tree db_session
  and evaluate_create (tree : Parse.ast) (db_session : Db.dbSession option) :
      (evaluationResult, Db.dbError) result =
    evaluate_open_or_create Db.create tree db_session
  and evaluate_list (tree : Parse.ast) (db_session_opt : Db.dbSession option) :
      (evaluationResult, Db.dbError) result =
    match tree with
    | LIST -> (
      match db_session_opt with
      | Some db_session -> (
        match Db.list_keys db_session with
        | Ok res ->
            Result.ok
              (INFO_STRING_RESULT ("KEYS: [" ^ String.concat ", " res ^ "]"))
        | Error err ->
            Result.error err )
      | None ->
          Result.error Db.NoDatabaseOpen )
    | _ ->
        failwith "Invalid LIST"
  and evaluate_get (tree : Parse.ast) (db_session_opt : Db.dbSession option) :
      (evaluationResult, Db.dbError) result =
    match tree with
    | GET sub_tree -> (
      match db_session_opt with
      | Some db_session -> (
        match evaluate_ast_rec sub_tree db_session_opt with
        | Ok key_result -> (
          match Db.get db_session (string_of_evaluation_result key_result) with
          | Ok key_str ->
              Result.ok (STRING_RESULT key_str)
          | Error err ->
              Result.error err )
        | Error err ->
            Result.error err )
      | None ->
          Result.error Db.NoDatabaseOpen )
    | _ ->
        failwith "Invalid GET"
  and evaluate_delete (tree : Parse.ast) (db_session_opt : Db.dbSession option)
      : (evaluationResult, Db.dbError) result =
    match tree with
    | DELETE sub_tree -> (
      match db_session_opt with
      | Some db_session -> (
        match evaluate_ast_rec sub_tree db_session_opt with
        | Ok key_result -> (
            let key = string_of_evaluation_result key_result in
            match Db.delete db_session key with
            | None ->
                Result.ok (INFO_STRING_RESULT ("DELETED: " ^ key))
            | Some err ->
                Result.error err )
        | Error err ->
            Result.error err )
      | None ->
          Result.error Db.NoDatabaseOpen )
    | _ ->
        failwith "Invalid DELETE"
  and evaluate_put (tree : Parse.ast) (db_session_opt : Db.dbSession option) :
      (evaluationResult, Db.dbError) result =
    match tree with
    | PUT (key_tree, value_tree) -> (
      match db_session_opt with
      | Some db_session -> (
        match
          ( evaluate_ast_rec key_tree (Option.some db_session)
          , evaluate_ast_rec value_tree (Option.some db_session) )
        with
        | Ok key_result, Ok value_result -> (
            let key_str = string_of_evaluation_result key_result in
            let value_str = string_of_evaluation_result value_result in
            match Db.put db_session key_str value_str with
            | None ->
                Result.ok
                  (INFO_STRING_RESULT ("PUT: " ^ key_str ^ "=" ^ value_str))
            | Some err ->
                Result.error err )
        | Error err, Ok _ | Ok _, Error err | Error err, Error _ ->
            Result.error err )
      | None ->
          Result.error Db.NoDatabaseOpen )
    | _ ->
        failwith "Invalid GET"
  in
  evaluate_ast_rec tree db_session_opt

let evaluate_asts (trees : Parse.ast list) (db_session_opt : Db.dbSession option)
    =
  let rec evaluate_asts_rec (trees : Parse.ast list)
      (db_session_opt : Db.dbSession option)
      (previous_result : (evaluationResult, Db.dbError) result) :
      (evaluationResult, Db.dbError) result =
    match trees with
    | [] ->
        previous_result
    | ast :: tail -> (
      match evaluate_ast ast db_session_opt with
      | Ok result -> (
        match result with
        | DB_SESSION db_session ->
            evaluate_asts_rec tail (Option.some db_session) (Result.ok result)
        | _ ->
            evaluate_asts_rec tail db_session_opt (Result.ok result) )
      | Error err ->
          Result.error err )
  in
  evaluate_asts_rec trees db_session_opt
    (Result.ok (INFO_STRING_RESULT "Nothing to evaluate."))
