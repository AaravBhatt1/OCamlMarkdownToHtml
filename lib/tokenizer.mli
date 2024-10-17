type token =
  | NewlineToken
  | HeaderToken of int
  | TextToken of string

val tokenize : string -> token list
