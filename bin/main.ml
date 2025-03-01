let db_name = "important_data.db"

let print_result_or_error (res : (string, BarrelDB.Db.dbError) result) : unit =
  match res with
  | Error err ->
      print_endline (BarrelDB.Db.err_to_string err)
  | Ok value ->
      Printf.printf "%s\n" value

let _ =
  match BarrelDB.Db.create db_name with
  | Error err ->
      print_endline (BarrelDB.Db.err_to_string err)
  | Ok db_session ->
      ignore (BarrelDB.Db.put db_session "key" "val") ;
      print_result_or_error (BarrelDB.Db.get db_session "key")
