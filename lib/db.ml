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
    | DataBaseNotOpen -> "DataBaseNotOpen - Database you are performing actions on isn't open please use Db.create to create it."
    | LoadingDatabaseNotImplementedError -> "LoadingDatabaseNotImplementedError - Loading an existing database isn't supported yet."

let create_dir path = if not (Sys.file_exists path) then Sys.mkdir path 0o777 (* Full Permissions *)

let get_file_size filename = match Sys.file_exists filename with 
    | true -> (Unix.stat filename).st_size
    | false -> 0

let crc32 (input: bytes) =
    let divisor = 0xEDB88320 in
    let rec process_bytes (crc: int) (input: bytes) (index: int): int= match index with
        | _ when index = (Bytes.length input) -> crc lxor 0xFFFFFFFF
        | _ -> let byte = int_of_char (Bytes.get input index) in
            let byte_crc = crc lxor byte in
            let rec process_bits bits_crc remaining_bits = match remaining_bits with
                    | _ when remaining_bits = 0 -> bits_crc 
                    | _ -> process_bits (if (bits_crc land 1) <> 0 then (bits_crc lsr 1) lxor divisor else bits_crc lsr 1) (remaining_bits-1) in
            process_bytes (process_bits byte_crc 8) input (index+1) in

    process_bytes (0xFFFFFFFF) (input) 0


(* Big Endian *)
let bytes_of_int ~(value:int) (number_bytes:int) = 
    let rec loop (num:int) (i:int) (n_bytes:int) (bytes:Bytes.t) = match i with
        | _ when i = n_bytes -> bytes
        | _ -> let byte = ((num lsr ((n_bytes-i-1) * 8)) land 0xFF) in
            Bytes.set bytes i (Char.chr byte);
            loop num (i+1) n_bytes bytes 
        in

    (* Check can be represented *)
    if ((float_of_int 2) ** (float_of_int (number_bytes * 8))) <= (float_of_int value) then raise Invalid_input;
    (* Sense check for correctness of inputs*)
    if (number_bytes > 1000000) then raise Invalid_input;

    loop value 0 number_bytes (Bytes.make number_bytes (Char.chr 0))


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
    bytes_of_int ~value:(int_of_float disk_entry.timestamp) 8; (* 32 bit *)
    bytes_of_int ~value:disk_entry.key_size 4;
    bytes_of_int ~value:disk_entry.value_size 4;
    disk_entry.key;
    disk_entry.value;
    ] in 
    Bytes.concat Bytes.empty [
        bytes_of_int ~value:(crc32 payload) 4;
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
    | true -> Result.error LoadingDatabaseNotImplementedError
    | false -> create_dir db_name; 
        Result.ok {db_name=db_name; key_dir=Hashtbl.create 100; active_file=(get_random_string 10)}

let get _ _  = Result.error NotImplementedError
let put db_session key value  = append_disk_entry_to_file db_session (create_disk_entry key value); Option.none
let delete _ _ = Option.some NotImplementedError
let list_keys db_session = Result.ok (Hashtbl.fold (fun key _ acc -> key :: acc) db_session.key_dir [])



