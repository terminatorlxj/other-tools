{
  open Parser
  open Lts
}

(* let integer = ['0'-'9']+ *)
(* let float = ['0'-'9']+ '.' ['0'-'9']* *)
let iden = ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']*
(* let str = ['a'-'z' 'A'-'Z' '0'-'9' '_' ] *)
(* let uiden = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']* *)
let nl = '\r' | '\n' | "\r\n"

rule token = 
  | "AX"  {AX}
  | "EX"  {EX}
  | "AF"  {AF}
  | "EG"  {EG}
  | "AR"  {AR}
  | "EU"  {EU}
  | "\""+.*"\"" as s  {Action (Label (String.sub s 1 (String.length s - 2)))}
  | "Tau" {Action Tau}
  | "T"   {Action T}
  | "F"   {Action F}
  | ","   {Comma}
  | ";"   {Semicolon}
  | "("   {LB1}
  | ")"   {RB1}
  | "{"   {LB3}
  | "}"   {RB3}
  | "!"   {Exclam}
  | "&"   {Amper}
  | "|"   {Vert}
  | ":="  {Assign}
  | "TRUE"  {TRUE}
  | "FALSE" {FALSE}
  | iden as s  {Iden s}
  | "/\\"   {And}
  | "\\/"   {Or} 
  | nl      {Newline}
  | "not"   {Not}
  | "Action"  {Action}
  | "Spec"    {Spec}
  | "(*"    {comment_ocaml lexbuf}
  | eof     {EOF}
and comment_ocaml =
  | "*)"  {token lexbuf}
  | eof   {EOF}
  | _     {comment_ocaml lexbuf}

(* EU(Tau, x, y, TRUE, TRUE, s)
EU("G v1 v2", x, y, TRUE, TRUE, s)
EU(T, x, y, TRUE, TRUE, s)
EU(F, x, y, TRUE, TRUE, s) *)