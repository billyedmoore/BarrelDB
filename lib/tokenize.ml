type token =
  | OPEN_TOKEN
  | CREATE_TOKEN
  | PUT_TOKEN
  | GET_TOKEN
  | DELETE_TOKEN
  | LIST_TOKEN
  | SEMICOLON_TOKEN
  | STRING_TOKEN of string
  | OPEN_BRACKET_TOKEN
  | CLOSE_BRACKET_TOKEN

let token_from_buffer (buffer : char list) =
  let str = String.of_seq (List.to_seq buffer) in
  match String.lowercase_ascii str with
  | "open" ->
      OPEN_TOKEN
  | "create" ->
      CREATE_TOKEN
  | "get" ->
      GET_TOKEN
  | "put" ->
      PUT_TOKEN
  | "delete" ->
      DELETE_TOKEN
  | "list" ->
      LIST_TOKEN
  | ";" ->
      SEMICOLON_TOKEN
  | "(" ->
      OPEN_BRACKET_TOKEN
  | ")" ->
      CLOSE_BRACKET_TOKEN
  | _ ->
      failwith "Invalid Token"

let string_of_token (input : token) =
  match input with
  | OPEN_TOKEN ->
      "OPEN"
  | CREATE_TOKEN ->
      "CREATE"
  | GET_TOKEN ->
      "GET"
  | PUT_TOKEN ->
      "PUT"
  | DELETE_TOKEN ->
      "DELETE"
  | LIST_TOKEN ->
      "LIST"
  | SEMICOLON_TOKEN ->
      "SEMICOLON"
  | OPEN_BRACKET_TOKEN ->
      "OPEN_BRACKET"
  | CLOSE_BRACKET_TOKEN ->
      "CLOSE_BRACKET"
  | STRING_TOKEN s ->
      "STRING(" ^ s ^ ")"

let tokenize_string (str : string) =
  let rec chew_string (str_as_list : char list) (buffer : char list)
      (tokens : token list) : token list =
    match str_as_list with
    | [] -> (
      match buffer with [] -> tokens | _ -> tokens @ [token_from_buffer buffer]
      )
    | (' ' | '\n') :: t -> (
      match buffer with
      | [] ->
          chew_string t [] tokens
      | _ ->
          chew_string t [] (tokens @ [token_from_buffer buffer]) )
    | '"' :: t -> (
      match buffer with
      | [] ->
          handle_quote t [] tokens
      | _ ->
          handle_quote t [] (tokens @ [token_from_buffer buffer]) )
    | h :: t ->
        chew_string t (buffer @ [h]) tokens
  and handle_quote (str_as_list : char list) (buffer : char list)
      (tokens : token list) : token list =
    match str_as_list with
    | [] ->
        failwith "String never closed"
    | '"' :: tail ->
        chew_string tail []
          (tokens @ [STRING_TOKEN (String.of_seq (List.to_seq buffer))])
    | c :: tail ->
        handle_quote tail (buffer @ [c]) tokens
  in
  chew_string (List.of_seq (String.to_seq str)) [] []
