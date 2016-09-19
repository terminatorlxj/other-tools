
open Parser

let main () = 
	try
	let filename = Sys.argv.(1) in
	let time_list = Parser.input Lexer.token (Lexing.from_channel (open_in filename)) in
	let out = open_out (filename^"_time_list") in
	List.iter (fun a -> output_string out ((string_of_float a)^"\n")) time_list;
	flush out;
	close_out out
	with _ -> print_endline ("exception at line: "^(string_of_int (!(Lexer.line_num))))
	
let _ = 
	Printexc.print main ()
