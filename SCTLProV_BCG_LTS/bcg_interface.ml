(* open Printf *)

type state = int
type label = string 
type label_visible = bool

let str_state s = string_of_int s

external read_bcg : string -> state = "read_bcg"
external trans: state -> ((label * label_visible * state) list) = "trans"
external close_bcg : unit -> unit = "close_bcg"
