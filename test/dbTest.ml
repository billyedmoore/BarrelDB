open OUnit2

let test_get_from_db _ =
  match PostMixDB.Db.create "test_get_from_db" with
  | Error err ->
      failwith (PostMixDB.Db.string_of_dberror err)
  | Ok db_session -> (
    match PostMixDB.Db.put db_session "key" "value" with
    | Some err ->
        failwith (PostMixDB.Db.string_of_dberror err)
    | None -> (
      match PostMixDB.Db.get db_session "key" with
      | Error err ->
          failwith (PostMixDB.Db.string_of_dberror err)
      | Ok value ->
          assert_equal value "value" ) )

let test_delete_from_db _ =
  match PostMixDB.Db.create "test_delete_from_db" with
  | Error err ->
      failwith (PostMixDB.Db.string_of_dberror err)
  | Ok db_session -> (
    match PostMixDB.Db.put db_session "key" "value" with
    | Some err ->
        failwith (PostMixDB.Db.string_of_dberror err)
    | None -> (
        ignore (PostMixDB.Db.delete db_session "key") ;
        match PostMixDB.Db.get db_session "key" with
        | Error err ->
            assert_equal err PostMixDB.Db.KeyNotFound
        | Ok _ ->
            failwith "Get was successful after key was removed (not ideal)." ) )

let test_list_keys_from_db _ =
  match PostMixDB.Db.create "test_list_keys_from_db" with
  | Error err ->
      failwith (PostMixDB.Db.string_of_dberror err)
  | Ok db_session -> (
      ignore (PostMixDB.Db.put db_session "key0" "val0") ;
      ignore (PostMixDB.Db.put db_session "key1" "val1") ;
      ignore (PostMixDB.Db.put db_session "key2" "val2") ;
      ignore (PostMixDB.Db.put db_session "key3" "val3") ;
      ignore (PostMixDB.Db.put db_session "key4" "val4") ;
      ignore (PostMixDB.Db.put db_session "key5" "val5") ;
      ignore (PostMixDB.Db.put db_session "key6" "val6") ;
      ignore (PostMixDB.Db.put db_session "key7" "val7") ;
      match PostMixDB.Db.list_keys db_session with
      | Error err ->
          failwith (PostMixDB.Db.string_of_dberror err)
      | Ok value ->
          assert_equal
            ["key0"; "key1"; "key2"; "key3"; "key4"; "key5"; "key6"; "key7"]
            (List.sort String.compare value) )

let test_list_keys_with_keys_removed_db _ =
  match PostMixDB.Db.create "test_list_keys_from_db_with_some_keys_removed" with
  | Error err ->
      failwith (PostMixDB.Db.string_of_dberror err)
  | Ok db_session -> (
      ignore (PostMixDB.Db.put db_session "key0" "val0") ;
      ignore (PostMixDB.Db.put db_session "key1" "val1") ;
      ignore (PostMixDB.Db.put db_session "key2" "val2") ;
      ignore (PostMixDB.Db.delete db_session "key0") ;
      ignore (PostMixDB.Db.delete db_session "key1") ;
      ignore (PostMixDB.Db.delete db_session "key2") ;
      ignore (PostMixDB.Db.put db_session "key3" "val3") ;
      ignore (PostMixDB.Db.put db_session "key4" "val4") ;
      ignore (PostMixDB.Db.put db_session "key5" "val5") ;
      ignore (PostMixDB.Db.put db_session "key6" "val6") ;
      ignore (PostMixDB.Db.put db_session "key7" "val7") ;
      match PostMixDB.Db.list_keys db_session with
      | Error err ->
          failwith (PostMixDB.Db.string_of_dberror err)
      | Ok value ->
          assert_equal
            ["key3"; "key4"; "key5"; "key6"; "key7"]
            (List.sort String.compare value) )

let test_get_long_value_from_db _ =
  let known_value =
    "fsajdkldsjlajlsdjflajsdklasdkflasdjflasdksdklajsdfljfsldkfajklsdjflasjsljsdflkajsdfalsdfjals"
  in
  match PostMixDB.Db.create "test_get_long_value_from_db" with
  | Error err ->
      failwith (PostMixDB.Db.string_of_dberror err)
  | Ok db_session -> (
    match PostMixDB.Db.put db_session "key" known_value with
    | Some err ->
        failwith (PostMixDB.Db.string_of_dberror err)
    | None -> (
      match PostMixDB.Db.get db_session "key" with
      | Error err ->
          failwith (PostMixDB.Db.string_of_dberror err)
      | Ok value ->
          assert_equal value known_value ) )
