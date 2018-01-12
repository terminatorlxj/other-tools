open Printf

type kstate = int

let tau = ref (-1)

exception No_next of kstate

let compare_state s1 s2 = Pervasives.compare s1 s2

module State_label_key = 
struct
	type t = kstate
	let compare = Pervasives.compare
end;;

module State_label_set = Set.Make(State_label_key)

module State_key = 
struct
  type t = int
  let compare = Pervasives.compare
end;;

module State_set = Set.Make(State_key)

let state si li = si
	
let str_kstate s = "("^(string_of_int s)^")"


type t = {
	init: kstate;
	trans: (kstate, State_set.t) Hashtbl.t;
}

let create_model s = {init = state s (-2); trans = Hashtbl.create 1;} 


let add_trans ks s1 s2 = 
	try
		Hashtbl.replace ks.trans s1 (State_set.add s2 (Hashtbl.find ks.trans s1))
	with Not_found ->
		Hashtbl.add ks.trans s1 (State_set.singleton s2)

let next ks s ignore_label only_tau = 
  let nexts = ref State_label_set.empty in
  let lts_trans = Bcg_interface.trans s in
  (* print_string (str_kstate s);
  print_string "->"; *)
  List.iter (fun (ln, lv, ns) -> 
    if only_tau then begin
      if (not lv) then
        nexts := State_label_set.add ns !nexts
    end else begin
      nexts := State_label_set.add ns !nexts
    end
  ) lts_trans;
  (* print_endline ""; *)
  !nexts


(* let has_tau s = s.label = !tau || s.label = (-2) *)
