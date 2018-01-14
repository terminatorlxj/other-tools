type lts = {init: Bcg_interface.state}

type action =  
	Tau | T | F 
	| Label of string 
	| Not of action 
	| Both of action * action 
    | Either of action * action

module State_key = 
struct
    type t = Bcg_interface.state
    let compare = Pervasives.compare
end;;

module State_set = Set.Make(State_key)

let rec match_action visible str act = 
    match act with
    | Tau -> not visible
    | T -> true
    | F -> false
    | Label labl_str -> str = labl_str
    | Not act -> not (match_action visible str act)
    | Both (act1, act2) -> (match_action visible str act1) && (match_action visible str act2)
    | Either (act1, act2) -> (match_action visible str act1) || (match_action visible str act2)

let create_lts filename = {init = Bcg_interface.read_bcg filename}

let next s act = 
    let nexts = ref State_set.empty in
    let lts_trans = Bcg_interface.trans s in
    List.iter (fun (ls, lv, ns) -> 
        if match_action lv ls act then
            nexts := State_set.add ns !nexts
      (* if only_tau then begin
        if (not lv) then
          nexts := State_label_set.add ns !nexts
      end else begin
        nexts := State_label_set.add ns !nexts
      end *)
    ) lts_trans;
    !nexts