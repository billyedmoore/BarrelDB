open OUnit2

let test_get_from_db _ =
  match BarrelDB.Db.create "test_get_from_db" with
  | Error err ->
      failwith (BarrelDB.Db.err_to_string err)
  | Ok db_session -> (
    match BarrelDB.Db.put db_session "key" "value" with
    | Some err ->
        failwith (BarrelDB.Db.err_to_string err)
    | None -> (
      match BarrelDB.Db.get db_session "key" with
      | Error err ->
          failwith (BarrelDB.Db.err_to_string err)
      | Ok value ->
          assert_equal value "value" ) )

let test_delete_from_db _ =
  match BarrelDB.Db.create "test_delete_from_db" with
  | Error err ->
      failwith (BarrelDB.Db.err_to_string err)
  | Ok db_session -> (
    match BarrelDB.Db.put db_session "key" "value" with
    | Some err ->
        failwith (BarrelDB.Db.err_to_string err)
    | None -> (
        ignore (BarrelDB.Db.delete db_session "key") ;
        match BarrelDB.Db.get db_session "key" with
        | Error err ->
            assert_equal err BarrelDB.Db.KeyNotFound
        | Ok _ ->
            failwith "Get was successful after key was removed (not ideal)." ) )

let test_list_keys_from_db _ =
  match BarrelDB.Db.create "test_list_keys_from_db" with
  | Error err ->
      failwith (BarrelDB.Db.err_to_string err)
  | Ok db_session -> (
      ignore (BarrelDB.Db.put db_session "key0" "val0") ;
      ignore (BarrelDB.Db.put db_session "key1" "val1") ;
      ignore (BarrelDB.Db.put db_session "key2" "val2") ;
      ignore (BarrelDB.Db.put db_session "key3" "val3") ;
      ignore (BarrelDB.Db.put db_session "key4" "val4") ;
      ignore (BarrelDB.Db.put db_session "key5" "val5") ;
      ignore (BarrelDB.Db.put db_session "key6" "val6") ;
      ignore (BarrelDB.Db.put db_session "key7" "val7") ;
      match BarrelDB.Db.list_keys db_session with
      | Error err ->
          failwith (BarrelDB.Db.err_to_string err)
      | Ok value ->
          assert_equal
            ["key0"; "key1"; "key2"; "key3"; "key4"; "key5"; "key6"; "key7"]
            (List.sort String.compare value) )

let test_list_keys_with_keys_removed_db _ =
  match BarrelDB.Db.create "test_list_keys_from_db_with_some_keys_removed" with
  | Error err ->
      failwith (BarrelDB.Db.err_to_string err)
  | Ok db_session -> (
      ignore (BarrelDB.Db.put db_session "key0" "val0") ;
      ignore (BarrelDB.Db.put db_session "key1" "val1") ;
      ignore (BarrelDB.Db.put db_session "key2" "val2") ;
      ignore (BarrelDB.Db.delete db_session "key0") ;
      ignore (BarrelDB.Db.delete db_session "key1") ;
      ignore (BarrelDB.Db.delete db_session "key2") ;
      ignore (BarrelDB.Db.put db_session "key3" "val3") ;
      ignore (BarrelDB.Db.put db_session "key4" "val4") ;
      ignore (BarrelDB.Db.put db_session "key5" "val5") ;
      ignore (BarrelDB.Db.put db_session "key6" "val6") ;
      ignore (BarrelDB.Db.put db_session "key7" "val7") ;
      match BarrelDB.Db.list_keys db_session with
      | Error err ->
          failwith (BarrelDB.Db.err_to_string err)
      | Ok value ->
          assert_equal
            ["key3"; "key4"; "key5"; "key6"; "key7"]
            (List.sort String.compare value) )

let test_get_long_value_from_db _ =
  let known_value =
    "fsajdkldsjlajlsdjflajsdklasdkflasdjflasdksdklajsdfljfsldkfajklsdjflasjsljsdflkajsdfalsdfjals"
  in
  match BarrelDB.Db.create "test_get_long_value_from_db" with
  | Error err ->
      failwith (BarrelDB.Db.err_to_string err)
  | Ok db_session -> (
    match BarrelDB.Db.put db_session "key" known_value with
    | Some err ->
        failwith (BarrelDB.Db.err_to_string err)
    | None -> (
      match BarrelDB.Db.get db_session "key" with
      | Error err ->
          failwith (BarrelDB.Db.err_to_string err)
      | Ok value ->
          assert_equal value known_value ) )
