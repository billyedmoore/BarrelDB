let db_name = "important_data.db"

let _ =  match BarrelDB.Db.create(db_name) with 
    | Error err -> print_endline (BarrelDB.Db.err_to_string err)
    | Ok db_session -> Printf.printf "Created KV store %s with no keys\n" db_session.db_name
