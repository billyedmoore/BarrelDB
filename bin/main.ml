let db_name = "important_data.db"

let print_result_or_error (res : (string, PostMixDB.Db.dbError) result) : unit =
  match res with
  | Error err ->
      print_endline (PostMixDB.Db.err_to_string err)
  | Ok value ->
      Printf.printf "%s\n" value

let print (toc : PostMixDB.Tokenize.token) =
  print_endline (PostMixDB.Tokenize.string_of_token toc)

let _ =
  List.iter print (PostMixDB.Tokenize.tokenize_string "OPEN \"database.db\"") ;
  match PostMixDB.Db.create db_name with
  | Error err ->
      print_endline (PostMixDB.Db.err_to_string err)
  | Ok db_session ->
      ignore (PostMixDB.Db.put db_session "key" "val") ;
      print_result_or_error (PostMixDB.Db.get db_session "key")
