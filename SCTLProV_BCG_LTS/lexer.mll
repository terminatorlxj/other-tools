{
  open Parser
  open Lts

  exception InvalidToken of string * int
  let lineno = ref 1

}

(* let integer = ['0'-'9']+ *)
(* let float = ['0'-'9']+ '.' ['0'-'9']* *)
let iden = ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']*
let label = ['a'-'z' 'A'-'Z' '0'-'9' '_' '-' ' ']*
(* let str = ['a'-'z' 'A'-'Z' '0'-'9' '_' ] *)
(* let uiden = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']* *)
let space = [' ' '\t']+
let nl = '\r' | '\n' | "\r\n"

rule token = parse
  | "AX"  {AX}
  | "EX"  {EX}
  | "AF"  {AF}
  | "EG"  {EG}
  | "AR"  {AR}
  | "EU"  {EU}
  | "\""+label+"\"" as s  {Action (Label (String.sub s 1 (String.length s - 2)))}
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
  | "not"   {Parser.Not}
  | "Action"  {ACTION}
  | "Spec"    {SPEC}
  | iden as s  {Iden s}
  | "/\\"   {And}
  | "\\/"   {Or} 
  | nl      {incr lineno; token lexbuf}
  | [' ' '\t']+  {token lexbuf}
  | "(*"    {comment_ocaml lexbuf}
  | eof     {EOF}
  (* | label as s  {raise (InvalidToken (s, !lineno))} *)
and comment_ocaml = parse
  | "*)"  {token lexbuf}
  | eof   {EOF}
  | _     {comment_ocaml lexbuf}