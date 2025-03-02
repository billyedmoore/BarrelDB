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
  | QUOTE_TOKEN

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
  | "\"" ->
      QUOTE_TOKEN
  | _ ->
      STRING_TOKEN str

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
  | QUOTE_TOKEN ->
      "QUOTE"
  | STRING_TOKEN s ->
      "STRING(" ^ s ^ ")"

let tokenize_string (str : string) =
  let rec chew_string (str_as_list : char list) (buffer : char list)
      (tokens : token list) : token list =
    match str_as_list with
    | [] -> (
      match buffer with [] -> tokens | _ -> tokens @ [token_from_buffer buffer]
      )
    | h :: t -> (
      match h with
      | ' ' | '\n' -> (
        match buffer with
        | [] ->
            chew_string t [] tokens
        | _ ->
            chew_string t [] (tokens @ [token_from_buffer buffer]) )
      | '"' -> (
        match buffer with
        | [] ->
            chew_string t [] (tokens @ [token_from_buffer ['"']])
        | _ ->
            chew_string t []
              (tokens @ [token_from_buffer buffer; token_from_buffer ['"']]) )
      | _ ->
          chew_string t (buffer @ [h]) tokens )
  in
  chew_string (List.of_seq (String.to_seq str)) [] []
