
(* The type of tokens. *)

type token = 
  | Vert
  | TRUE
  | Semicolon
  | SPEC
  | RB3
  | RB1
  | Or
  | Not
  | LB3
  | LB1
  | Iden of (string)
  | FALSE
  | Exclam
  | EX
  | EU
  | EOF
  | EG
  | Comma
  | Assign
  | And
  | Amper
  | Action of (Lts.action)
  | AX
  | AR
  | AF
  | ACTION

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> ((string * Formula.formula) list)
