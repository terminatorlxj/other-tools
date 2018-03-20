type token =
  | Semicolon
  | Dot
  | Percent
  | File_end
  | Id of (string)
  | Int of (int)

val input :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Res.res list
