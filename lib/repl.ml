let get_statement () =
  let rec get_statement_rec (buffer : string) : string =
    ignore
      ( match buffer with
      | "" ->
          print_string "PostMix >"
      | _ ->
          print_string "-" ) ;
    let line = read_line () in
    match String.ends_with ~suffix:";" line with
    | true ->
        buffer ^ line
    | false ->
        get_statement_rec (buffer ^ line)
  in
  get_statement_rec ""

let string_of_result (result_result : (Eval.evaluationResult, Db.dbError) result)
    : string =
  match result_result with
  | Ok result -> (
    match result with
    | STRING_RESULT str ->
        "\"" ^ str ^ "\""
    | INFO_STRING_RESULT str ->
        "INFO: " ^ str
    | DB_SESSION db_session ->
        "INFO: Opened " ^ db_session.db_name )
  | Error err ->
      "ERROR: " ^ Db.string_of_dberror err

let run_repl () =
  let rec run_repl_rec (db_session_opt : Db.dbSession option) : unit =
    let line = get_statement () in
    let tokens = Tokenize.tokenize_string line in
    let asts = Parse.parse tokens in
    let result = Eval.evaluate_asts asts db_session_opt in
    print_endline (string_of_result result) ;
    let new_db_session =
      match result with
      | Ok (DB_SESSION db) ->
          Option.some db
      | _ ->
          db_session_opt
    in
    run_repl_rec new_db_session
  in
  run_repl_rec None
