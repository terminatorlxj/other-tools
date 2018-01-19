open Lexing

type t = 
    {
        start_pos: position;
        end_pos: position;
    }

let make sp ep = 
    {
        start_pos = sp;
        end_pos = ep;
    }

let str_location loc = 
    let start_pos = loc.start_pos 
    and end_pos = loc.end_pos in
    "\""^start_pos.pos_fname^"\": "^
    "Line "^(string_of_int start_pos.pos_lnum)^
    ", Column "^(string_of_int start_pos.pos_bol)^
    " ~ "^
    "Line "^(string_of_int end_pos.pos_lnum)^
    ", Column "^(string_of_int end_pos.pos_bol)