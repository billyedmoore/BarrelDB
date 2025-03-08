(*
PROGRAM -> STATEMENT PROGRAM | () 
STATEMENT ->    SEMICOLON_TOKEN |
		NON_TERMINAL_STATEMENT | 
	     	TERMINAL_STATEMENT
TERMINAL_STATEMENT -> 
		OPEN_TOKEN NON_TERMINAL_STATEMENT | 
		CREATE_TOKEN NON_TERMINAL_STATEMENT | 
		PUT_TOKEN NON_TERMINAL_STATEMENT NON_TERMINAL_STATEMENT |
		DELETE_TOKEN NON_TERMINAL_STATEMENT |
		LIST_TOKEN
NON_TERMINAL_STATEMENT -> 
		GET_TOKEN NON_TERMINAL_STATEMENT | 
		STRING_TOKEN
*)

type ast =
  | OPEN of ast
  | CREATE of ast
  | PUT of ast * ast
  | GET of ast
  | DELETE of ast
  | STRING of string
  | LIST
  | NOP

let string_of_ast (tree : ast) : string =
  let rec string_of_ast_rec (tree : ast) (depth : int) : string =
    let prefix = String.make depth '\t' in
    let suffix = "\n" in
    match tree with
    | STRING str ->
        prefix ^ "STRING " ^ str ^ suffix
    | OPEN sub_tree ->
        prefix ^ "OPEN:" ^ suffix ^ string_of_ast_rec sub_tree (depth + 1)
    | CREATE sub_tree ->
        prefix ^ "CREATE:" ^ suffix ^ string_of_ast_rec sub_tree (depth + 1)
    | PUT (key_sub_tree, val_sub_tree) ->
        prefix ^ "PUT:" ^ suffix
        ^ string_of_ast_rec key_sub_tree (depth + 1)
        ^ string_of_ast_rec val_sub_tree (depth + 1)
    | GET sub_tree ->
        prefix ^ "GET:" ^ suffix ^ string_of_ast_rec sub_tree (depth + 1)
    | DELETE sub_tree ->
        prefix ^ "DELETE:" ^ suffix ^ string_of_ast_rec sub_tree (depth + 1)
    | LIST ->
        prefix ^ "LIST" ^ suffix
    | NOP ->
        prefix ^ "NOP" ^ suffix
  in
  string_of_ast_rec tree 0

(* Each function handles a rule in the above grammar, they should return the 
   remaining tokens and a sub-tree *)
let rec parse_string tokens : Tokenize.token list * ast =
  match tokens with
  | Tokenize.STRING_TOKEN str :: tail ->
      (tail, STRING str)
  | _ ->
      failwith "Invalid String"

and parse_get tokens : Tokenize.token list * ast =
  match tokens with
  | Tokenize.GET_TOKEN :: tail ->
      let remaining_tokens, key = parse_non_terminal_statement tail in
      (remaining_tokens, GET key)
  | _ ->
      failwith "Invalid Get"

and parse_non_terminal_statement tokens : Tokenize.token list * ast =
  match tokens with
  | Tokenize.GET_TOKEN :: _ ->
      parse_get tokens
  | Tokenize.STRING_TOKEN _ :: _ ->
      parse_string tokens
  | _ ->
      failwith "Invalid Non-terminal Statement"

and parse_open tokens : Tokenize.token list * ast =
  match tokens with
  | Tokenize.OPEN_TOKEN :: tail ->
      let remaining_tokens, dbName = parse_non_terminal_statement tail in
      (remaining_tokens, OPEN dbName)
  | _ ->
      failwith "Invalid Open"

and parse_create tokens : Tokenize.token list * ast =
  match tokens with
  | Tokenize.OPEN_TOKEN :: tail ->
      let remaining_tokens, dbName = parse_non_terminal_statement tail in
      (remaining_tokens, CREATE dbName)
  | _ ->
      failwith "Invalid Create"

and parse_put tokens : Tokenize.token list * ast =
  match tokens with
  | Tokenize.PUT_TOKEN :: tail ->
      let tokens_after_key, key = parse_non_terminal_statement tail in
      let tokens_after_value, value =
        parse_non_terminal_statement tokens_after_key
      in
      (tokens_after_value, PUT (key, value))
  | _ ->
      failwith "Invalid Put"

and parse_delete tokens : Tokenize.token list * ast =
  match tokens with
  | Tokenize.DELETE_TOKEN :: tail ->
      let remaining_tokens, key = parse_non_terminal_statement tail in
      (remaining_tokens, DELETE key)
  | _ ->
      failwith "Invalid Delete"

and parse_list tokens : Tokenize.token list * ast =
  match tokens with
  | Tokenize.LIST_TOKEN :: tail ->
      (tail, LIST)
  | _ ->
      failwith "Invalid List"

and parse_terminal_statement tokens : Tokenize.token list * ast =
  match tokens with
  | Tokenize.OPEN_TOKEN :: _ ->
      parse_open tokens
  | Tokenize.CREATE_TOKEN :: _ ->
      parse_create tokens
  | Tokenize.PUT_TOKEN :: _ ->
      parse_put tokens
  | Tokenize.DELETE_TOKEN :: _ ->
      parse_delete tokens
  | Tokenize.LIST_TOKEN :: _ ->
      parse_list tokens
  | _ ->
      failwith "Invalid Terminal Statement"

and parse_statement tokens : Tokenize.token list * ast =
  match tokens with
  | Tokenize.SEMICOLON_TOKEN :: tail ->
      (tail, NOP)
  | _ -> (
    try parse_non_terminal_statement tokens
    with Failure _ -> parse_terminal_statement tokens )

and parse_program (tokens : Tokenize.token list) (trees : ast list) =
  match tokens with
  | [] ->
      trees
  | _ ->
      let unconsumed_tokens, tree = parse_statement tokens in
      let isnt_nop (tree : ast) = match tree with NOP -> false | _ -> true in
      parse_program unconsumed_tokens (List.filter isnt_nop (trees @ [tree]))

let parse (tokens : Tokenize.token list) : ast list = parse_program tokens []
