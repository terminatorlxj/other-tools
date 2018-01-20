open Type

type error = 
  | NotUnifiable of type_expr * type_expr

exception Typing_error of error

type pmodule_env = {
    pmodule: Parsetree.pmodule;
    env: (string, type_expr) Hashtbl.t
  }

type type_equation = type_expr * type_expr
(* type unifier = type_equation list *)

let unify te current_pmname pmenvs = 
  let rec normal_form te = 
    match te with
    | [] -> []
    | (Tint, Tint) :: te' | (Tbool, Tbool) :: te' | (Tunit, Tunit) :: te' | (Tfloat, Tfloat) :: te' | (Tstring, Tstring) :: te' -> normal_form te'
    | (Tvar i, Tvar j) :: te' -> if i=j then normal_form te' else (Tvar i, Tvar j)::(normal_form te')
    | (Tvar i, t) :: te' | (t, Tvar i) :: te' -> (Tvar i, t) :: (normal_form te')
    | (Tlist t1, Tlist t2) :: te' | (Tarray t1, Tarray t2) :: te' -> normal_form ((t1,t2)::te')
    | (Tarrow (t1, t2), Tarrow (t3, t4)) :: te' -> normal_form ((t1,t3)::(t2,t4)::te')
    | (Ttuple tl1, Ttuple tl2) :: te' -> 
      normal_form ((List.rev_map2 (fun a b -> a,b) tl1 tl2)@te')
    |Tapply 


let check_modules pmodules = 
  let pmenvs = Hashtbl.create 1 in  
  List.iter (fun pm -> Hashtbl.add pmenvs pm.name {pmodule = pm; env = Hashtbl.create 1;}) pmodules;

