type token =
  | Newline
  | Header of int
  | Text of string

val tokenizer : string -> token list
