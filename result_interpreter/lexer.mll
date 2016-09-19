{
	open Parser
	let line_num = ref 0
}

let integer = ['0'-'9']+
let id = ['a'-'z' 'A' - 'Z'] ['a'-'z' 'A' - 'Z' '0'-'9' '_']*

rule token = parse
	| "exception encountered"	{FATAL_ERROR}
	| "Terminated"	{FATAL_ERROR}
	| "terminated by a signal" {FATAL_ERROR}
	| "Fatal error:"	{FATAL_ERROR}
	| "real"	{token lexbuf}
	| "m" 		{M}
	| "s"		{S}
	| "."		{DOT}
	| integer as s	{I (int_of_string s)}
	| '\n'		{line_num := (!line_num) + 1; dummy lexbuf}
	| [' ' '\t' '\r']	{token lexbuf}
	| eof		{File_end}
	| _		{dummy lexbuf}
and dummy = parse
	| "exception encountered"	{FATAL_ERROR}
	| "Terminated"	{FATAL_ERROR}
	| "terminated by a signal" {FATAL_ERROR}
	| "Fatal error:"	{FATAL_ERROR}
	| "real"	{token lexbuf}
	| '\n'		{line_num := (!line_num) + 1; dummy lexbuf}
	| [' ' '\t' '\r'] {dummy lexbuf}
	| eof		{File_end}
	| _		{dummy lexbuf}


