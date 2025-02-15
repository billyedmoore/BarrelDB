type dbError = 
    | NotImplementedError 
    | DataBaseNotOpen 

(** dbError to a string **)
val err_to_string: dbError -> string

(** Open database _, returns a result either
    empty or containing an Error.**)
val create: string -> dbError option

(** Get from database _ key _, returns a result 
    containing the found string or an Error.**)
val get: string -> string -> (string,dbError) result

(** Put into database _ value _ at key _ returns a 
    result either empty or containing an Error.**)
val put: string -> string -> string -> dbError option

(** Delete key _ from database _ returns a result either
    OK and empty or an Error.**)
val delete: string -> string -> dbError option

(** List keys in databse _ returns a result containing
    either a list of keys or an Error**)
val list_keys: string -> (string list,dbError) result
