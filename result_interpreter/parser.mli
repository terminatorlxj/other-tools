type token =
  | M
  | S
  | STAR
  | REAL
  | DOT
  | FATAL_ERROR
  | File_end
  | Id of (string)
  | I of (int)

val input :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> float list
