module StringMap = Map.Make(String)

type dbError = NotImplementedError | LoadingDatabaseNotImplementedError | DataBaseNotOpen

type keyDirEntry = {
    file_name: string;
    value_size: int;
    value_pos: int;
}

type dbSession = {
    db_name: string;
    key_dir: keyDirEntry StringMap.t;
}

let err_to_string err = match err with
    | NotImplementedError -> "NotImplementedError - Function is yet to be implemented."
    | DataBaseNotOpen -> "DataBaseNotOpen - Database you are performing actions on isn't open please use Db.create to create it."
    | LoadingDatabaseNotImplementedError -> "LoadingDatabaseNotImplementedError - Loading an existing database isn't supported yet."

let create_dir path = if not (Sys.file_exists path) then Sys.mkdir path 0o777 (* Full Permissions *)

let get_active_file_path db_name = match (Sys.file_exists (db_name ^ "/active")) with
    | true -> db_name ^ "/active"
    | false -> create_dir db_name; db_name ^ "/active"

(* External API *)
let create db_name =  match Sys.file_exists db_name with
    | true -> Result.error LoadingDatabaseNotImplementedError
    | false -> create_dir db_name; 
        Result.ok {db_name=db_name;key_dir = StringMap.empty}

let get _ _  = Result.error NotImplementedError
let put _ _ _  = Option.some NotImplementedError
let delete _ _ = Option.some NotImplementedError
let list_keys db_session = Map.fold (fun key _ acc -> key :: acc) db_session.key_dir []



