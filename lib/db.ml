exception Invalid_input

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

type diskEntry = {
    crc: Bytes.t; 
    timestamp: float;
    key_size: int; 
    value_size: int;
    (* The string -> bytes conversion is done when diskEntry is created *)
    key: bytes; 
    value: bytes;
}

let err_to_string err = match err with
    | NotImplementedError -> "NotImplementedError - Function is yet to be implemented."
    | DataBaseNotOpen -> "DataBaseNotOpen - Database you are performing actions on isn't open please use Db.create to create it."
    | LoadingDatabaseNotImplementedError -> "LoadingDatabaseNotImplementedError - Loading an existing database isn't supported yet."

let create_dir path = if not (Sys.file_exists path) then Sys.mkdir path 0o777 (* Full Permissions *)


(* TODO: consider having more than 1 file and how the name should be decided and remembered in this case *)
let get_active_file_path (db_session: dbSession) = match (Sys.file_exists (db_session.db_name ^ "/active")) with
    | true -> db_session.db_name ^ "/active"
    | false -> create_dir db_session.db_name; db_session.db_name ^ "/active"

let get_file_size filename = match Sys.file_exists filename with 
    | true -> (Unix.stat filename).st_size
    | false -> 0

(* Big Endian *)
let bytes_of_int (number:int) (number_bytes:int) = 
    let rec loop (num:int) (i:int) (n_bytes:int) (bytes:Bytes.t) = match i with
        | _ when i = n_bytes -> bytes
        | _ -> let byte = ((num lsr ((n_bytes-i-1) * 8)) land 0xFF) in
            Bytes.set bytes i (Char.chr byte);
            loop num (i+1) n_bytes bytes 
        in

    (* Check can be represented *)
    if ((float_of_int 2) ** (float_of_int (number_bytes * 8))) <= (float_of_int number) then raise Invalid_input;

    loop number 0 number_bytes (Bytes.make number_bytes (Char.chr 0))


let create_disk_entry (key: string) (value: string) : diskEntry = 
    let key_bytes = Bytes.of_string key in
    let value_bytes = Bytes.of_string value in 
    {
    crc = Bytes.create 4; (* dummy CRC*)
    timestamp = Unix.time ();
    key_size = Bytes.length key_bytes;
    value_size = Bytes.length value_bytes;
    key = key_bytes;
    value = value_bytes
    }

let encode_disk_entry (disk_entry:diskEntry) = Bytes.concat Bytes.empty [
    disk_entry.crc; (* 32 bits *) 
    bytes_of_int (int_of_float disk_entry.timestamp) 8; (* 32 bit *)
    bytes_of_int disk_entry.key_size 4;
    bytes_of_int disk_entry.value_size 4;
    disk_entry.key;
    disk_entry.value;
]

let append_disk_entry_to_file (db_session: dbSession) (disk_entry:diskEntry)= 
    let filename = (get_active_file_path db_session) in 
    let current_length = get_file_size filename in
    if current_length = 0 then close_out (open_out filename);
    let f = open_out_gen [Open_append; Open_binary] 0o600 filename in
    output_bytes f (encode_disk_entry disk_entry);
    close_out f;
    let dir_entry = {file_name = filename; value_size = disk_entry.value_size; value_pos = current_length} in
    Hashtbl.add db_session.key_dir (Bytes.to_string disk_entry.key) dir_entry  

(* External API *)
let create db_name =  match Sys.file_exists db_name with
    | true -> Result.error LoadingDatabaseNotImplementedError
    | false -> create_dir db_name; 
        Result.ok {db_name=db_name;key_dir = Hashtbl.create 100}

let get _ _  = Result.error NotImplementedError
let put db_session key value  = append_disk_entry_to_file db_session (create_disk_entry key value); Option.none
let delete _ _ = Option.some NotImplementedError
let list_keys db_session = Result.ok (Hashtbl.fold (fun key _ acc -> key :: acc) db_session.key_dir [])



