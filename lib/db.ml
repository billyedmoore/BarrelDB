type dbError = NotImplementedError | NoDatabaseOpen | DatabaseNotFound | DatabaseExists

type keyDirEntry = {
    file_name: string;
    timestamp: int;
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

let get_random_string (length: int): string =
    Random.self_init ();
    let rec loop (remaining_chars: int) (str: string) = match remaining_chars with
        | 0 -> str
        | _ -> loop (remaining_chars-1) (str ^ (Char.escaped (Char.chr ((Random.int 26) + 97)))) in 
    loop length ""

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

(* Side-effect: advance file_stream by number_bytes*)
(* Raises End_of_file if end of file is reached unexpectedly or there is no bytes to read. *)
let read_input_bytes (file_stream: in_channel) (number_bytes: int): Bytes.t =
    let buffer = Bytes.create number_bytes in
    really_input file_stream buffer (pos_in file_stream) number_bytes;
    seek_in file_stream ((pos_in file_stream) + number_bytes);
    buffer

(* Raises End_of_file if end of file is reached unexpectedly or there is no record to read *)
(* Returns (timestamp,key,start_pos) *)
let scan_record_chunk (file_stream: in_channel): (int * string * int64 * int) option = 
    let crc_size = 4 in
    let crc = read_input_bytes file_stream crc_size in
    
    let timestamp_size = 4 in
    let timestamp = read_input_bytes file_stream timestamp_size in 

    let size_size = 4 in
    let key_size = read_input_bytes file_stream size_size in
    let value_size = read_input_bytes file_stream size_size in

    let key_size_as_int = (BinaryUtils.int_of_bytes key_size) in
    let value_size_as_int = (BinaryUtils.int_of_bytes value_size) in

    let key = read_input_bytes file_stream key_size_as_int in
    let value_pos = In_channel.pos file_stream in
    let value = read_input_bytes file_stream value_size_as_int in

    let payload = Bytes.concat Bytes.empty [timestamp;key_size;value_size;key;value;] in 
    match (BinaryUtils.crc32 payload) = (BinaryUtils.int_of_bytes crc) with
        | true -> Option.some (BinaryUtils.int_of_bytes timestamp, Bytes.to_string key, value_pos, value_size_as_int)
        | false -> None 

let new_database_session (db_name: string) = {db_name=db_name; key_dir=(Hashtbl.create 100); active_file=(get_random_string 10)}

(* Side-effect: adds the keys from file_stream to the HashMap key_dir in db_session *)
let rec scan_file (db_session: dbSession) (file_stream: in_channel) (filename: string) = 
    let add_to_key_dir (filename:string) (key:string) (value_size:int) (value_pos:int) (timestamp:int) (db_session: dbSession) = 
        (Hashtbl.add db_session.key_dir key {file_name=filename; value_size=value_size; value_pos=value_pos; timestamp=timestamp}) in 
    let result = (scan_record_chunk file_stream) in
    match result with 
        | None -> ()
        | Some res -> let (timestamp,key,value_pos,value_size) = res in
                  let current_val = (Hashtbl.find_opt db_session.key_dir key) in
                  match current_val with
                    | Some value -> if (value.timestamp < timestamp) then (add_to_key_dir filename key value_size (Int64.to_int value_pos) timestamp db_session);
                    | None -> (add_to_key_dir filename key value_size (Int64.to_int value_pos) timestamp db_session); 
                        scan_file db_session file_stream filename
            
let load_file (db_session:dbSession) (filename: string): unit  = 
    match (get_file_size filename) with
        | 0 -> ()
        | _ -> let f = open_in_gen [Open_rdonly; Open_binary] 0o600 filename in
               scan_file db_session f filename;
               close_in f

let load_database (db_name: string): dbSession = 
    let db_session = (new_database_session db_name) in
    let files = Array.to_list (Sys.readdir db_name) in
    let rec load_files (files: string list) = 
    match files with
        | [] -> ()
        | file :: remaining_files -> (load_file db_session file); (load_files remaining_files)
    in
    load_files files;
    db_session

let encode_disk_entry (disk_entry:diskEntry): Bytes.t = 
    let payload = Bytes.concat Bytes.empty [
    BinaryUtils.bytes_of_int ~value:(int_of_float disk_entry.timestamp) 4; (* 32 bit *)
    BinaryUtils.bytes_of_int ~value:disk_entry.key_size 4;
    BinaryUtils.bytes_of_int ~value:disk_entry.value_size 4;
    disk_entry.key;
    disk_entry.value;
    ] in
    Bytes.concat Bytes.empty [
        BinaryUtils.bytes_of_int ~value:(BinaryUtils.crc32 payload) 4;
        payload;
    ] 

let commit_entry (db_session: dbSession) (disk_entry:diskEntry) = 
    (*Only creates dir if dir doesnt exist*)
    create_dir db_session.db_name;
    let filename = (db_session.db_name ^ "/" ^ db_session.active_file) in 
    let current_length = get_file_size filename in
    if current_length = 0 then close_out (open_out filename);
    let f = open_out_gen [Open_append; Open_binary] 0o600 filename in
    output_bytes f (encode_disk_entry disk_entry);
    close_out f;
    let dir_entry = {file_name = filename; value_size = disk_entry.value_size; 
        value_pos = current_length; timestamp = (int_of_float disk_entry.timestamp)} in
    Hashtbl.add db_session.key_dir (Bytes.to_string disk_entry.key) dir_entry  


(* External API *)

let create db_name =  match Sys.file_exists db_name with
    | true -> Result.error DatabaseExists
    | false -> create_dir db_name; 
        Result.ok (new_database_session db_name)

let load db_name = match Sys.file_exists db_name with
    | true -> Result.ok (load_database db_name)
    | false -> Result.error DatabaseNotFound

let get _ _  = Result.error NotImplementedError
let put db_session key value  = commit_entry db_session (create_disk_entry key value); Option.none
let delete _ _ = Option.some NotImplementedError
let list_keys db_session = Result.ok (Hashtbl.fold (fun key _ acc -> key :: acc) db_session.key_dir [])
