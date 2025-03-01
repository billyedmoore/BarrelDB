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
