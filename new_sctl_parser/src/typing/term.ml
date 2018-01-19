open Pterm

type const = 
    Const_int of int
  | Const_bool of bool
  | Const_float of float
  | Const_string of string
type expr = 
  {
      expr_descr: expr_descr;
      expr_loc: Location.t;
      expr_type: Type.type_expr;
  }
and expr_descr = 
  | Expr_path of Path.t
  | Expr_const of const
  | Expr_let of pat * expr
  | Expr_apply of expr * (expr list)
  | Expr_tuple of expr list
  | Expr_constr of string * expr list
  | Expr_record of (string * expr) list
  | Expr_with of expr * ((string * expr) list)
  | Expr_list of expr list
  | Expr_array of expr list
  | Expr_sequence of expr list
  | Expr_if of expr * expr * (expr option)
  | Expr_while of expr * expr
  | Expr_for of string * expr * expr * expr
  | Expr_match of expr * ((pat * expr) list)
  | Expr_assign of expr * expr
  | Expr_constraint of expr * Types.t 
and pat = 
  {
      pat_descr: pat_descr;
      pat_loc: Location.t;
      pat_type: Type.type_expr;
  }
and pat_descr = 
  | Pat_iden of string
  | Pat_const of const
  | Pat_tuple of pat list
  (* | Ppat_record of (string * ppat) list *)
  | Pat_constr of string * (pat list)
  | Pat_list of pat list
  | Pat_listcons of pat * pat
  | Pat_array of pat list
  | Pat_or of pat * pat
  | Pat_wildcard

let make_expr pexpr = 
    let ed, et = match pexpr.pexpr_descr with
    | Pexpr_path p -> Expr_path p, Type.new_type_var ()
    | Pexpr_const (Pconst_int i) -> Expr_const (Const_int i), Type.Tint
    | Pexpr_const (Pconst_bool b) -> Expr_const (Const_bool b), Type.Tbool
    | Pexpr_const (Pconst_float f) -> Expr_const (Const_float f), Type.Tfloat
    | Pexpr_const (Pconst_string s) -> Expr_const (Const_string s), Type.Tstring
    | Pexpr_let (p, pe) -> Expr_let (make_pat p, make_expr pe), Type.new_type_var ()
    | Pexpr_apply (pe, pel) -> Expr_apply (make_expr pe, List.map (fun pe -> make_expr pe) pel), Type.new_type_var ()
    | Pexpr_tuple pel -> 
        let el = List.map (fun pe -> make_expr pe) pel in
        Expr_tuple el, Type.Ttuple (List.map (fun e -> e.expr_type) el)
    | Pexpr_constr (path, pe) -> Expr_constr (path, make_expr pe), Type.new_type_var ()
    | 
    in
    {expr_descr = ed; expr_type = et; expr_loc = pexpr.pexpr_loc}


let make_pexpr ped loc = 
  {
      pexpr_descr = ped;
      pexpr_loc = loc;
  }

let make_ppat ppatd loc = 
  {
      ppat_descr = ppatd;
      ppat_loc = loc;
  }
