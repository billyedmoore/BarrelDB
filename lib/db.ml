type dbError = NotImplementedError | DataBaseNotOpen

let err_to_string err = match err with 
    | NotImplementedError -> "NotImplementedError - Function is yet to be implemented."
    | DataBaseNotOpen -> "DataBaseNotOpen - Databased you are performing actions on isn't open please use Db.create to create it."

let create _ =  Option.some NotImplementedError;;
let get _ _  = Result.error NotImplementedError;;
let put _ _ _  = Option.some NotImplementedError;;
let delete _ _ = Option.some NotImplementedError;;
(*Placeholder that sets the types to match the hints*)
let list_keys db_name = if (db_name = "" ) then Ok [""] else Result.error NotImplementedError;; 



