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
  | Pat_record of (string * pat) list
  | Pat_constr of Path.t * (pat list)
  | Pat_list of pat list
  | Pat_listcons of pat * pat
  | Pat_array of pat list
  (* | Pat_or of pat * pat *)
  | Pat_wildcard

let rec make_expr pexpr = 
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
    | Pexpr_record (str_pe_list) -> Expr_record (List.map (fun (str,pe) -> str, make_expr pe) str_pe_list), Type.new_type_var ()
    | Pexpr_with (pe, str_pel) -> 
      let e = make_expr pe in
      Expr_with (e, List.map (fun (str,pe) -> str, make_expr pe) str_pel), e.expr_type
    | Pexpr_list pel -> 
      let el = List.map (fun pe -> make_expr pe) pel in
      if el = [] then
        Expr_list el, Type.new_type_var ()
      else
        Expr_list el, Tlist ((List.hd el).expr_type)
    | Pexpr_array pel -> 
      let el = List.map (fun pe -> make_expr pe) pel in
      if el = [] then
        Expr_array el, Type.new_type_var ()
      else
        Expr_array el, Tarray ((List.hd el).expr_type)
    | Pexpr_sequence pel -> 
      let el = List.map (fun pe -> make_expr pe) pel in
      Expr_sequence el, (Lists.last el).expr_type
    | Pexpr_if (pe1, pe2, None) ->
      let e2 = make_expr pe2 in
      Expr_if (make_expr pe1, e2, None), e2.expr_type
    | Pexpr_if (pe1, pe2, Some pe3) ->
      let e2 = make_expr pe2 in
      Expr_if (make_expr pe1, e2, Some (make_expr pe3)), e2.expr_type
    | Pexpr_while (pe1, pe2) ->
      (* let e2 = make_expr pe2 in *)
      Expr_while (make_expr pe1, make_expr pe2), Type.Tunit
    | Pexpr_for (s, pe1, pe2, pe3) ->
      Expr_for (s, make_expr pe1, make_expr pe2, make_expr pe3), Type.Tunit
    | Pexpr_match (pe1, ppat_pel) -> 
      let e1 = make_expr pe1 in
      Expr_match (e1, List.map (fun (ppat,pe) -> make_pat ppat, make_expr pe) ppat_pel), e1.expr_type
    | Pexpr_assign (pe1, pe2) -> Expr_assign (make_expr pe1, make_expr pe2), Type.Tunit
    | Pexpr_constraint (pe, pt) -> Expr_constraint (make_expr pe, Type.make_type pt)
    in
    {expr_descr = ed; expr_type = et; expr_loc = pexpr.pexpr_loc}
and make_pat ppat = 
  let patd, patt = match ppat.ppat_descr with
  | Ppat_iden s -> Pat_iden s, Type.new_type_var ()
  | Ppat_const (Pconst_int i) -> Pat_const (Const_int i), Type.Tint
  | Ppat_const (Pconst_bool b) -> Pat_const (Const_bool b), Type.Tbool
  | Ppat_const (Pconst_float f) -> Pat_const (Const_float f), Type.Tfloat
  | Ppat_const (Pconst_string s) -> Pat_const (Const_string s), Type.Tstring
  | Ppat_tuple ppatl -> 
    let patl = List.map (fun ppat -> make_pat ppat) ppatl in
    Pat_tuple patl, Type.Ttuple (List.map (fun pat -> pat.pat_type) patl)
  | Ppat_record str_ppatl -> Pat_record (List.map (fun (s,ppat) -> s, make_pat ppat) str_ppatl), Type.new_type_var ()
  | Ppat_constr (path, ppatl) -> Pat_constr (path, List.map (fun ppat -> make_pat ppat) ppatl), Type.new_type_var ()
  | Ppat_list ppatl -> 
    let patl = List.map (fun ppat -> make_pat ppat) ppatl in
    if patl = [] then
      Pat_list patl, Type.new_type_var ()
    else
      Pat_list patl, Type.Tlist ((List.hd patl).pat_type)
  | Ppat_listcons (ppat1, ppat2) ->
    let pat2 = make_pat ppat2 in
    Pat_listcons (make_pat ppat1, pat2), pat2.pat_type
  | Ppat_array ppatl ->
    let patl = List.map (fun ppat -> make_pat ppat) ppatl in
    if patl = [] then
      Pat_array patl, Type.new_type_var ()
    else
      Pat_array patl, Type.Tarray ((List.hd patl).pat_type)
  | Ppat_wildcard -> Pat_wildcard, Type.new_type_var ()
  in
  {pat_descr = patd; pat_type = patt; pat_loc = ppat.ppat_loc}
(* 
let make_pexpr ped loc = 
  {
      pexpr_descr = ped;
      pexpr_loc = loc;
  }

let make_ppat ppatd loc = 
  {
      ppat_descr = ppatd;
      ppat_loc = loc;
  } *)
