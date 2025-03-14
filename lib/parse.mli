type ast =
  | OPEN of ast
  | CREATE of ast
  | PUT of ast * ast
  | GET of ast
  | DELETE of ast
  | STRING of string
  | LIST
  | NOP

val parse : Tokenize.token list -> (ast list, Db.dbError) result

val string_of_ast : ast -> string
