%{
open Lexing
(*
//	| FATAL_ERROR I M I DOT I S {-1.0}
//	| I M I DOT I S	{[((float_of_int ($1 * 60 + $3)) +. ((float_of_int $5) /. 1000.0))]}
//	| I M I DOT I S inputs {((float_of_int ($1 * 60 + $3)) +. ((float_of_int $5) /. 1000.0)) :: $7}

*)
let error_state = ref false
%}

%token M S STAR REAL DOT FATAL_ERROR
%token File_end
%token <string>Id 
%token <int>I 


%start input
%type <float list>input

%%
input: inputs File_end	{$1}
;

inputs: 
//	| FATAL_ERROR I M I DOT I S {[-1.0]}
	| I M I DOT I S	{if !error_state then (error_state := false; [1200.1]) else [((float_of_int ($1 * 60 + $3)) +. ((float_of_int $5) /. 1000.0))]}
	| FATAL_ERROR inputs	{error_state := true; print_endline "ecountered fatal error"; $2}
//	| FATAL_ERROR I M I DOT I S inputs {(-1.0) :: $8}
	| I M I DOT I S inputs {if !error_state then (error_state := false; ((float_of_int ($1 * 60 + $3)) +. ((float_of_int $5) /. 1000.0)) :: ((1200.1) :: (List.tl $7))) else ((float_of_int ($1 * 60 + $3)) +. ((float_of_int $5) /. 1000.0)) :: $7}
; 

//single_input: 
//	| FATAL_ERROR I M I DOT I S {-1.0}
//	| I M I DOT I S	{((float_of_int ($1 * 60 + $3)) +. ((float_of_int $5) /. 1000.0))}
//	;

%%

