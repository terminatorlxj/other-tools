type token =
  | Semicolon
  | Dot
  | Percent
  | File_end
  | Id of (string)
  | Int of (int)

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Lexing
open Res
# 15 "parser.ml"
let yytransl_const = [|
  257 (* Semicolon *);
  258 (* Dot *);
  259 (* Percent *);
  260 (* File_end *);
    0|]

let yytransl_block = [|
  261 (* Id *);
  262 (* Int *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\004\000\005\000\006\000\006\000\
\007\000\008\000\009\000\010\000\011\000\012\000\013\000\014\000\
\015\000\016\000\017\000\018\000\019\000\020\000\021\000\022\000\
\023\000\024\000\000\000"

let yylen = "\002\000\
\002\000\000\000\023\000\003\000\003\000\002\000\005\000\007\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\027\000\000\000\000\000\000\000\001\000\
\000\000\000\000\004\000\000\000\000\000\000\000\005\000\006\000\
\000\000\000\000\000\000\009\000\000\000\000\000\010\000\000\000\
\000\000\000\000\011\000\000\000\000\000\007\000\012\000\000\000\
\000\000\013\000\000\000\008\000\014\000\000\000\015\000\000\000\
\016\000\000\000\017\000\000\000\018\000\000\000\019\000\000\000\
\020\000\000\000\021\000\000\000\022\000\000\000\023\000\000\000\
\024\000\000\000\025\000\000\000\026\000\000\000\003\000"

let yydgoto = "\002\000\
\004\000\005\000\006\000\010\000\014\000\018\000\021\000\024\000\
\028\000\032\000\035\000\038\000\040\000\042\000\044\000\046\000\
\048\000\050\000\052\000\054\000\056\000\058\000\060\000\062\000"

let yysindex = "\001\000\
\253\254\000\000\002\255\000\000\001\255\000\255\003\255\000\000\
\005\255\004\255\000\000\006\255\008\255\007\255\000\000\000\000\
\013\255\009\255\010\255\000\000\011\255\255\254\000\000\012\255\
\014\255\015\255\000\000\016\255\017\255\000\000\000\000\018\255\
\019\255\000\000\020\255\000\000\000\000\021\255\000\000\022\255\
\000\000\023\255\000\000\024\255\000\000\025\255\000\000\026\255\
\000\000\027\255\000\000\028\255\000\000\029\255\000\000\030\255\
\000\000\031\255\000\000\032\255\000\000\253\254\000\000"

let yyrindex = "\000\000\
\035\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\035\255\000\000"

let yygindex = "\000\000\
\000\000\202\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yytablesize = 39
let yytable = "\025\000\
\026\000\001\000\003\000\007\000\008\000\009\000\012\000\063\000\
\011\000\013\000\016\000\015\000\017\000\019\000\020\000\022\000\
\023\000\027\000\033\000\029\000\030\000\031\000\000\000\034\000\
\036\000\037\000\039\000\041\000\043\000\045\000\047\000\049\000\
\051\000\053\000\055\000\057\000\059\000\061\000\002\000"

let yycheck = "\001\001\
\002\001\001\000\006\001\002\001\004\001\006\001\002\001\062\000\
\006\001\006\001\003\001\006\001\006\001\001\001\006\001\006\001\
\006\001\006\001\002\001\006\001\006\001\006\001\255\255\006\001\
\006\001\006\001\006\001\006\001\006\001\006\001\006\001\006\001\
\006\001\006\001\006\001\006\001\006\001\006\001\004\001"

let yynames_const = "\
  Semicolon\000\
  Dot\000\
  Percent\000\
  File_end\000\
  "

let yynames_block = "\
  Id\000\
  Int\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'inputs) in
    Obj.repr(
# 16 "parser.mly"
                       (_1)
# 114 "parser.ml"
               : Res.res list))
; (fun __caml_parser_env ->
    Obj.repr(
# 19 "parser.mly"
        ([])
# 120 "parser.ml"
               : 'inputs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 22 : 'ut) in
    let _2 = (Parsing.peek_val __caml_parser_env 21 : 'st) in
    let _3 = (Parsing.peek_val __caml_parser_env 20 : 'cpup) in
    let _4 = (Parsing.peek_val __caml_parser_env 19 : 'ct) in
    let _5 = (Parsing.peek_val __caml_parser_env 18 : 'asts) in
    let _6 = (Parsing.peek_val __caml_parser_env 17 : 'auds) in
    let _7 = (Parsing.peek_val __caml_parser_env 16 : 'ass) in
    let _8 = (Parsing.peek_val __caml_parser_env 15 : 'ats) in
    let _9 = (Parsing.peek_val __caml_parser_env 14 : 'mrss) in
    let _10 = (Parsing.peek_val __caml_parser_env 13 : 'arss) in
    let _11 = (Parsing.peek_val __caml_parser_env 12 : 'mpf) in
    let _12 = (Parsing.peek_val __caml_parser_env 11 : 'mipf) in
    let _13 = (Parsing.peek_val __caml_parser_env 10 : 'vcs) in
    let _14 = (Parsing.peek_val __caml_parser_env 9 : 'ics) in
    let _15 = (Parsing.peek_val __caml_parser_env 8 : 'swaps) in
    let _16 = (Parsing.peek_val __caml_parser_env 7 : 'fsi) in
    let _17 = (Parsing.peek_val __caml_parser_env 6 : 'fso) in
    let _18 = (Parsing.peek_val __caml_parser_env 5 : 'sms) in
    let _19 = (Parsing.peek_val __caml_parser_env 4 : 'smr) in
    let _20 = (Parsing.peek_val __caml_parser_env 3 : 'sd) in
    let _21 = (Parsing.peek_val __caml_parser_env 2 : 'ps) in
    let _22 = (Parsing.peek_val __caml_parser_env 1 : 'es) in
    let _23 = (Parsing.peek_val __caml_parser_env 0 : 'inputs) in
    Obj.repr(
# 21 "parser.mly"
  (
			{
				usr_time = _1; 
				sys_time = _2; 
				cpu_percent = _3; 
				clock_time = _4;
				avrg_sh_txt_size = _5;
				avrg_unsh_data_size = _6;
				avrg_stack_size = _7;
				avrg_total_size = _8;
				max_res_size = _9;
				avrg_res_size = _10;
				major_page_faults = _11;
				minor_page_faults = _12;
				vol_contxt_switch = _13;
				invol_contxt_switch = _14;
				swaps = _15;
				file_inputs = _16;
				file_outputs = _17;
				sock_msg_sent = _18;
				sock_msg_recv = _19;
				signal_deliver = _20;
				page_size = _21;
				exit_status = _22;
			} :: _23)
# 173 "parser.ml"
               : 'inputs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 48 "parser.mly"
                (float_of_int(_1) +. ((float_of_int _3) /. 100.0))
# 181 "parser.ml"
               : 'ut))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 49 "parser.mly"
                (float_of_int(_1) +. ((float_of_int _3) /. 100.0))
# 189 "parser.ml"
               : 'st))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 50 "parser.mly"
                  (_1)
# 196 "parser.ml"
               : 'cpup))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 51 "parser.mly"
                              (float_of_int (_1 * 60 + _3) +. ((float_of_int _5) /. 100.0))
# 205 "parser.ml"
               : 'ct))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 52 "parser.mly"
                                           (float_of_int (_1 * 3600 + _3 * 60 + _5) +. ((float_of_int _7) /. 100.0))
# 215 "parser.ml"
               : 'ct))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 54 "parser.mly"
           (_1)
# 222 "parser.ml"
               : 'asts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 55 "parser.mly"
           (_1)
# 229 "parser.ml"
               : 'auds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 56 "parser.mly"
          (_1)
# 236 "parser.ml"
               : 'ass))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 57 "parser.mly"
          (_1)
# 243 "parser.ml"
               : 'ats))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 58 "parser.mly"
           (_1)
# 250 "parser.ml"
               : 'mrss))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 59 "parser.mly"
           (_1)
# 257 "parser.ml"
               : 'arss))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 60 "parser.mly"
          (_1)
# 264 "parser.ml"
               : 'mpf))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 61 "parser.mly"
           (_1)
# 271 "parser.ml"
               : 'mipf))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 62 "parser.mly"
          (_1)
# 278 "parser.ml"
               : 'vcs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 63 "parser.mly"
          (_1)
# 285 "parser.ml"
               : 'ics))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 64 "parser.mly"
            (_1)
# 292 "parser.ml"
               : 'swaps))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 65 "parser.mly"
          (_1)
# 299 "parser.ml"
               : 'fsi))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 66 "parser.mly"
          (_1)
# 306 "parser.ml"
               : 'fso))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 67 "parser.mly"
          (_1)
# 313 "parser.ml"
               : 'sms))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 68 "parser.mly"
          (_1)
# 320 "parser.ml"
               : 'smr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 69 "parser.mly"
         (_1)
# 327 "parser.ml"
               : 'sd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 70 "parser.mly"
         (_1)
# 334 "parser.ml"
               : 'ps))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 71 "parser.mly"
         (_1)
# 341 "parser.ml"
               : 'es))
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Res.res list)
;;
# 94 "parser.mly"

# 368 "parser.ml"
