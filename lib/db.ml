type dbError = NotImplementedError | NoDatabaseOpen | DatabaseNotFound | DatabaseExists

type keyDirEntry = {
    file_name: string;
    value_size: int;
    value_pos: int;
}

type dbSession = {
    db_name: string;
    key_dir: (string,keyDirEntry) Hashtbl.t;
    active_file: string;
}

type diskEntry = {
    timestamp: float;
    key_size: int; 
    value_size: int;
    (* The string -> bytes conversion is done when diskEntry is created *)
    key: bytes; 
    value: bytes;
}

let err_to_string err = match err with
    | NotImplementedError -> "NotImplementedError - Function is yet to be implemented."
    | NoDatabaseOpen -> "NoDatabaseOpen - No open database."
    | DatabaseNotFound -> "DatabaseNotFound - Database you tried to load could not be found."
    | DatabaseExists -> "DatabaseExists - Database already exists."

let create_dir path = if not (Sys.file_exists path) then Sys.mkdir path 0o777 (* Full Permissions *)

let get_file_size filename = match Sys.file_exists filename with 
    | true -> (Unix.stat filename).st_size
    | false -> 0

let create_disk_entry (key: string) (value: string) : diskEntry = 
    let key_bytes = Bytes.of_string key in
    let value_bytes = Bytes.of_string value in 
    {
    timestamp = Unix.time ();
    key_size = Bytes.length key_bytes;
    value_size = Bytes.length value_bytes;
    key = key_bytes;
    value = value_bytes
    }

let encode_disk_entry (disk_entry:diskEntry) = 
    let payload = Bytes.concat Bytes.empty [
    BinaryUtils.bytes_of_int ~value:(int_of_float disk_entry.timestamp) 8; (* 32 bit *)
    BinaryUtils.bytes_of_int ~value:disk_entry.key_size 4;
    BinaryUtils.bytes_of_int ~value:disk_entry.value_size 4;
    disk_entry.key;
    disk_entry.value;
    ] in 
    Bytes.concat Bytes.empty [
        BinaryUtils.bytes_of_int ~value:(BinaryUtils.crc32 payload) 4;
        payload;
    ] 

let append_disk_entry_to_file (db_session: dbSession) (disk_entry:diskEntry)= 
    (*Only creates dir if dir doesnt exist*)
    create_dir db_session.db_name;
    let filename = (db_session.db_name ^ "/" ^ db_session.active_file) in 
    let current_length = get_file_size filename in
    if current_length = 0 then close_out (open_out filename);
    let f = open_out_gen [Open_append; Open_binary] 0o600 filename in
    output_bytes f (encode_disk_entry disk_entry);
    close_out f;
    let dir_entry = {file_name = filename; value_size = disk_entry.value_size; value_pos = current_length} in
    Hashtbl.add db_session.key_dir (Bytes.to_string disk_entry.key) dir_entry  

let get_random_string (length: int): string =
    let rec loop (remaining_chars: int) (str: string) = match remaining_chars with
        | 0 -> str
        | _ -> loop (remaining_chars-1) (str ^ (Char.escaped (Char.chr ((Random.int 26) + 97)))) in 
    loop length ""

(* External API *)

let create db_name =  match Sys.file_exists db_name with
    | true -> Result.error DatabaseExists
    | false -> create_dir db_name; 
        Result.ok {db_name=db_name; key_dir=Hashtbl.create 100; active_file=(get_random_string 10)}

let load db_name = match Sys.file_exists db_name with
    | true -> Result.error NotImplementedError
    | false -> Result.error DatabaseNotFound

let get _ _  = Result.error NotImplementedError
let put db_session key value  = append_disk_entry_to_file db_session (create_disk_entry key value); Option.none
let delete _ _ = Option.some NotImplementedError
let list_keys db_session = Result.ok (Hashtbl.fold (fun key _ acc -> key :: acc) db_session.key_dir [])
