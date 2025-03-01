let db_name = "important_data.db"

let _ =
  match BarrelDB.Db.load db_name with
  | Error err ->
      print_endline (BarrelDB.Db.err_to_string err)
  | Ok db_session ->
      ignore (BarrelDB.Db.put db_session "key" "val")
