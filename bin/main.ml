let db_name = "database"

let _ =  match BarrelDB.Db.create(db_name) with 
    | Some err -> print_endline (BarrelDB.Db.err_to_string err)
    | None -> print_endline "DO SOMETHING WITH THE DB."
