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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Lexing
(*
//	| FATAL_ERROR I M I DOT I S {-1.0}
//	| I M I DOT I S	{[((float_of_int ($1 * 60 + $3)) +. ((float_of_int $5) /. 1000.0))]}
//	| I M I DOT I S inputs {((float_of_int ($1 * 60 + $3)) +. ((float_of_int $5) /. 1000.0)) :: $7}

*)
let error_state = ref false
# 24 "parser.ml"
let yytransl_const = [|
  257 (* M *);
  258 (* S *);
  259 (* STAR *);
  260 (* REAL *);
  261 (* DOT *);
  262 (* FATAL_ERROR *);
  263 (* File_end *);
    0|]

let yytransl_block = [|
  264 (* Id *);
  265 (* I *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\000\000"

let yylen = "\002\000\
\002\000\006\000\002\000\007\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\005\000\000\000\003\000\000\000\
\001\000\000\000\000\000\000\000\000\000\004\000"

let yydgoto = "\002\000\
\005\000\006\000"

let yysindex = "\001\000\
\251\254\000\000\251\254\002\255\000\000\254\254\000\000\253\254\
\000\000\003\255\000\255\005\255\251\254\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\004\255\000\000"

let yygindex = "\000\000\
\000\000\253\255"

let yytablesize = 11
let yytable = "\007\000\
\003\000\001\000\008\000\004\000\009\000\010\000\013\000\011\000\
\012\000\014\000\002\000"

let yycheck = "\003\000\
\006\001\001\000\001\001\009\001\007\001\009\001\002\001\005\001\
\009\001\013\000\007\001"

let yynames_const = "\
  M\000\
  S\000\
  STAR\000\
  REAL\000\
  DOT\000\
  FATAL_ERROR\000\
  File_end\000\
  "

let yynames_block = "\
  Id\000\
  I\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'inputs) in
    Obj.repr(
# 22 "parser.mly"
                       (_1)
# 95 "parser.ml"
               : float list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 27 "parser.mly"
                 (if !error_state then (error_state := false; [1200.1]) else [((float_of_int (_1 * 60 + _3)) +. ((float_of_int _5) /. 1000.0))])
# 104 "parser.ml"
               : 'inputs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'inputs) in
    Obj.repr(
# 28 "parser.mly"
                      (error_state := true; print_endline "ecountered fatal error"; _2)
# 111 "parser.ml"
               : 'inputs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'inputs) in
    Obj.repr(
# 30 "parser.mly"
                        (if !error_state then (error_state := false; ((float_of_int (_1 * 60 + _3)) +. ((float_of_int _5) /. 1000.0)) :: ((1200.1) :: (List.tl _7))) else ((float_of_int (_1 * 60 + _3)) +. ((float_of_int _5) /. 1000.0)) :: _7)
# 121 "parser.ml"
               : 'inputs))
(* Entry input *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let input (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : float list)
;;
# 39 "parser.mly"

# 148 "parser.ml"
