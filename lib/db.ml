type dbError =
  | NotImplementedError
  | NoDatabaseOpen
  | DatabaseAlreadyOpen
  | DatabaseNotFound
  | DatabaseExists
  | KeyNotFound
  | FileReadError
  | TokenizationError of string
  | SyntaxError of string

type keyDirEntry = {filename: string; timestamp: int; value_size: int; pos: int}

type dbSession =
  { db_name: string
  ; key_dir: (string, keyDirEntry) Hashtbl.t
  ; mutable active_file: string
  ; mutex: Mutex.t }

type diskEntry =
  { timestamp: int
  ; tombstone: bool
  ; key_size: int
  ; value_size: int
  ; key: string
  ; value: string }

let string_of_dberror err =
  match err with
  | NotImplementedError ->
      "NotImplementedError - Function is yet to be implemented."
  | NoDatabaseOpen ->
      "NoDatabaseOpen - No open database."
  | DatabaseNotFound ->
      "DatabaseNotFound - Database you tried to load could not be found."
  | KeyNotFound ->
      "KeyNotFound - Key not found in database."
  | DatabaseAlreadyOpen ->
      "DatabaseAlreadyOpen - A database session is already open."
  | FileReadError ->
      "FileReadError - Error with file read."
  | DatabaseExists ->
      "DatabaseExists - Database already exists."
  | TokenizationError err ->
      "TokenizationError - " ^ err
  | SyntaxError err ->
      "SyntaxError - " ^ err

let create_dir path =
  if not (Sys.file_exists path) then Sys.mkdir path 0o777 (* Full Permissions *)

let get_file_size filename =
  match Sys.file_exists filename with
  | true ->
      (Unix.stat filename).st_size
  | false ->
      0

let get_random_string (length : int) : string =
  Random.self_init () ;
  let rec loop (remaining_chars : int) (str : string) =
    match remaining_chars with
    | 0 ->
        str
    | _ ->
        loop (remaining_chars - 1)
          (str ^ Char.escaped (Char.chr (Random.int 26 + 97)))
  in
  loop length ""

let create_disk_entry (key : string) (value : string) (tombstone : bool) :
    diskEntry =
  { timestamp= int_of_float (Unix.time ())
  ; tombstone
  ; key_size= String.length key
  ; value_size= String.length value
  ; key
  ; value }

(* Side-effect: advance file_stream by number_bytes*)
(* Raises End_of_file if end of file is reached unexpectedly or there is no bytes to read. *)
let read_input_bytes (file_stream : in_channel) (number_bytes : int) : Bytes.t =
  let buffer = Bytes.create number_bytes in
  really_input file_stream buffer 0 number_bytes ;
  buffer

(* Raises End_of_file if end of file is reached unexpectedly or there is no record to read *)
(* Returns (timestamp,key,start_pos,value_size) *)
let scan_record_chunk (file_stream : in_channel) : (diskEntry * int) option =
  let block_pos = Int64.to_int (In_channel.pos file_stream) in
  let crc_size = 4 in
  let crc = read_input_bytes file_stream crc_size in
  let timestamp_size = 4 in
  let timestamp = read_input_bytes file_stream timestamp_size in
  let tombstone = read_input_bytes file_stream 1 in
  let size_size = 4 in
  let key_size = read_input_bytes file_stream size_size in
  let value_size = read_input_bytes file_stream size_size in
  let key_size_as_int = BinaryUtils.int_of_bytes key_size in
  let value_size_as_int = BinaryUtils.int_of_bytes value_size in
  let key = read_input_bytes file_stream key_size_as_int in
  let value = read_input_bytes file_stream value_size_as_int in
  let payload =
    Bytes.concat Bytes.empty
      [timestamp; tombstone; key_size; value_size; key; value]
  in
  let disk_entry =
    { timestamp= BinaryUtils.int_of_bytes timestamp
    ; tombstone= (if BinaryUtils.int_of_bytes tombstone = 1 then true else false)
    ; key_size= key_size_as_int
    ; value_size= value_size_as_int
    ; key= Bytes.to_string key
    ; value= Bytes.to_string value }
  in
  match BinaryUtils.crc32 payload = BinaryUtils.int_of_bytes crc with
  | true ->
      Option.some (disk_entry, block_pos)
  | false ->
      None

(* Raises Not_found if key not in database *)
let get_value (db_session : dbSession) (key : string) : string =
  (* TODO: switch to results based error handling *)
  Mutex.lock db_session.mutex ;
  let key_dir_entry = Hashtbl.find db_session.key_dir key in
  let f = open_in_gen [Open_rdonly; Open_binary] 0o600 key_dir_entry.filename in
  seek_in f key_dir_entry.pos ;
  match scan_record_chunk f with
  | None ->
      Mutex.unlock db_session.mutex ;
      raise (Invalid_argument "Couldn't scan record chunk.")
  | Some disk_entry_and_pos ->
      Mutex.unlock db_session.mutex ;
      let disk_entry, _ = disk_entry_and_pos in
      disk_entry.value

let new_database_session (db_name : string) =
  { db_name
  ; key_dir= Hashtbl.create 100
  ; active_file= get_random_string 10
  ; mutex= Mutex.create () }

(* Side-effect: adds the keys from file_stream to the HashMap key_dir in db_session *)
let rec scan_file (db_session : dbSession) (file_stream : in_channel)
    (filename : string) =
  let add_to_key_dir (filename : string) (key : string) (value_size : int)
      (value_pos : int) (timestamp : int) (db_session : dbSession) =
    Hashtbl.add db_session.key_dir key
      {filename; value_size; pos= value_pos; timestamp}
  in
  let result = try scan_record_chunk file_stream with End_of_file -> None in
  match result with
  | None ->
      ()
  | Some res -> (
      let disk_entry, disk_pos = res in
      let current_val = Hashtbl.find_opt db_session.key_dir disk_entry.key in
      match current_val with
      | Some value ->
          if value.timestamp < disk_entry.timestamp then
            add_to_key_dir filename disk_entry.key disk_entry.value_size
              disk_pos disk_entry.timestamp db_session
      | None ->
          add_to_key_dir filename disk_entry.key disk_entry.value_size disk_pos
            disk_entry.timestamp db_session ;
          scan_file db_session file_stream filename )

let load_file (db_session : dbSession) (filename : string) : unit =
  match get_file_size (db_session.db_name ^ "/" ^ filename) with
  | 0 ->
      ()
  | _ ->
      let f =
        open_in_gen [Open_rdonly; Open_binary] 0o600
          (db_session.db_name ^ "/" ^ filename)
      in
      scan_file db_session f filename ;
      close_in f

let load_database (db_name : string) : dbSession =
  let db_session = new_database_session db_name in
  Mutex.lock db_session.mutex ;
  let files = Array.to_list (Sys.readdir db_name) in
  let rec load_files (files : string list) =
    match files with
    | [] ->
        ()
    | file :: remaining_files ->
        load_file db_session file ; load_files remaining_files
  in
  load_files files ;
  Mutex.unlock db_session.mutex ;
  db_session

let encode_disk_entry (disk_entry : diskEntry) : Bytes.t =
  let payload =
    Bytes.concat Bytes.empty
      [ BinaryUtils.bytes_of_int ~value:disk_entry.timestamp 4
      ; BinaryUtils.bytes_of_int ~value:(Bool.to_int disk_entry.tombstone) 1
      ; BinaryUtils.bytes_of_int ~value:disk_entry.key_size 4
      ; BinaryUtils.bytes_of_int ~value:disk_entry.value_size 4
      ; Bytes.of_string disk_entry.key
      ; Bytes.of_string disk_entry.value ]
  in
  Bytes.concat Bytes.empty
    [BinaryUtils.bytes_of_int ~value:(BinaryUtils.crc32 payload) 4; payload]

let commit_entry (db_session : dbSession) (disk_entry : diskEntry) =
  (*Only creates dir if dir doesnt exist*)
  create_dir db_session.db_name ;
  let filename = db_session.db_name ^ "/" ^ db_session.active_file in
  let current_length = get_file_size filename in
  if current_length = 0 then close_out (open_out filename) ;
  Mutex.lock db_session.mutex ;
  let f = open_out_gen [Open_append; Open_binary] 0o600 filename in
  output_bytes f (encode_disk_entry disk_entry) ;
  close_out f ;
  match disk_entry.tombstone with
  | true ->
      Hashtbl.remove db_session.key_dir disk_entry.key ;
      Mutex.unlock db_session.mutex
  | false ->
      let dir_entry =
        { filename
        ; value_size= disk_entry.value_size
        ; pos= current_length
        ; timestamp= disk_entry.timestamp }
      in
      Hashtbl.add db_session.key_dir disk_entry.key dir_entry ;
      Mutex.unlock db_session.mutex

(* External API *)

let create db_name =
  match Sys.file_exists db_name with
  | true ->
      Result.error DatabaseExists
  | false ->
      create_dir db_name ;
      Result.ok (new_database_session db_name)

let load db_name =
  match Sys.file_exists db_name with
  | true ->
      Result.ok (load_database db_name)
  | false ->
      Result.error DatabaseNotFound

let get (db_session : dbSession) (key : string) : (string, dbError) result =
  try Result.ok (get_value db_session key) with
  | Not_found ->
      Result.error KeyNotFound
  | Invalid_argument _ ->
      Result.error FileReadError

let put db_session key value =
  commit_entry db_session (create_disk_entry key value false) ;
  Option.none

let delete db_session key =
  commit_entry db_session (create_disk_entry key "" true) ;
  Option.none

let list_keys db_session =
  Mutex.lock db_session.mutex ;
  let unsorted_keys =
    Hashtbl.fold (fun key _ acc -> key :: acc) db_session.key_dir []
  in
  Mutex.unlock db_session.mutex ;
  let keys = List.sort String.compare unsorted_keys in
  Result.ok keys
