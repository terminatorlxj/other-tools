open Type

type error = 
  | NotUnifiable of type_expr * type_expr

exception Typing_error of error

(* type pmodule_env = {
    pmodule: Parsetree.pmodule;
    env: (string, type_expr) Hashtbl.t
  } *)

type type_equation = type_expr * type_expr
(* type unifier = type_equation list *)

let unify te tmodule tmodules = 
  let rec normal_form tel = 
    match tel with
    | [] -> []
    | (Tint, Tint) :: tel' | (Tbool, Tbool) :: tel' | (Tunit, Tunit) :: tel' | (Tfloat, Tfloat) :: tel' | (Tstring, Tstring) :: tel' -> normal_form tel'
    | (Tvar i, Tvar j) :: tel' -> if i=j then normal_form te' else (Tvar i, Tvar j)::(normal_form tel')
    | (Tvar i, t) :: tel' | (t, Tvar i) :: tel' -> (Tvar i, t) :: (normal_form tel')
    | (Tlist t1, Tlist t2) :: tel' | (Tarray t1, Tarray t2) :: tel' -> normal_form ((t1,t2)::tel')
    | (Tarrow (t1, t2), Tarrow (t3, t4)) :: tel' -> normal_form ((t1,t3)::(t2,t4)::tel')
    | (Ttuple tl1, Ttuple tl2) :: tel' -> 
      normal_form ((List.rev_map2 (fun a b -> a,b) tl1 tl2)@tel')
    | (Tapply (path1, tel1), te) :: tel' -> 
      (* let type_decl = find_type_decl path1 in *)
      


(* 
let check_modules pmodules = 
  let pmenvs = Hashtbl.create 1 in  
  List.iter (fun pm -> Hashtbl.add pmenvs pm.name {pmodule = pm; env = Hashtbl.create 1;}) pmodules;
 *)
