type dbError =
  | NotImplementedError
  | NoDatabaseOpen
  | DatabaseNotFound
  | DatabaseExists

type keyDirEntry =
  {file_name: string; timestamp: int; value_size: int; value_pos: int}

type dbSession =
  { db_name: string
  ; key_dir: (string, keyDirEntry) Hashtbl.t
  ; active_file: string }

val err_to_string : dbError -> string
(** dbError to a string **)

val create : string -> (dbSession, dbError) result
(** Create database _, returns either the dbSession
    or an containing an Error.**)

val load : string -> (dbSession, dbError) result
(** Load database _, returns either the dbSession
    or an containing an Error.**)

val get : dbSession -> string -> (string, dbError) result
(** Get from database _ key _, returns a result 
    containing the found string or an Error.**)

val put : dbSession -> string -> string -> dbError option
(** Put into database _ value _ at key _ returns a 
    result either empty or containing an Error.**)

val delete : dbSession -> string -> dbError option
(** Delete key _ from database _ returns a result either
    OK and empty or an Error.**)

val list_keys : dbSession -> (string list, dbError) result
(** List keys in databse _ returns a result containing
    either a list of keys or an Error**)
