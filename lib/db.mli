type dbError = NotImplementedError | LoadingDatabaseNotImplementedError | DataBaseNotOpen

type keyDirEntry = {
    file_name: string;
    value_size: int;
    value_pos: int;
}

type dbSession = {
    db_name: string;
    key_dir: (string,keyDirEntry) Hashtbl.t;
}

(** dbError to a string **)
val err_to_string: dbError -> string

(** Open database _, returns a result either
    empty or containing an Error.**)
val create: string -> (dbSession,dbError) result

(** Get from database _ key _, returns a result 
    containing the found string or an Error.**)
val get: dbSession -> string -> (string,dbError) result

(** Put into database _ value _ at key _ returns a 
    result either empty or containing an Error.**)
val put: dbSession -> string -> string -> dbError option

(** Delete key _ from database _ returns a result either
    OK and empty or an Error.**)
val delete: dbSession -> string -> dbError option

(** List keys in databse _ returns a result containing
    either a list of keys or an Error**)
val list_keys: dbSession -> (string list,dbError) result
