type pconst = 
      Pconst_int of int
    | Pconst_bool of bool
    | Pconst_float of float
    | Pconst_string of string
type pexpr = 
    {
        pexpr_descr: pexpr_descr;
        pexpr_loc: Location.t;
    }
and pexpr_descr = 
    | Pexpr_path of Path.t
    | Pexpr_const of pconst
    | Pexpr_let of ppat * pexpr
    | Pexpr_apply of pexpr * (pexpr list)
    | Pexpr_tuple of pexpr list
    | Pexpr_variant of Path.t * (pexpr list)
    | Pexpr_record of (string * pexpr) list
    | Pexpr_with of pexpr * ((string * pexpr) list)
    | Pexpr_list of pexpr list
    | Pexpr_array of pexpr list
    | Pexpr_sequence of pexpr list
    | Pexpr_if of pexpr * pexpr * (pexpr option)
    | Pexpr_while of pexpr * pexpr
    | Pexpr_for of string * pexpr * pexpr * pexpr
    | Pexpr_match of pexpr * ((ppat * pexpr) list)
    | Pexpr_assign of pexpr * pexpr
    | Pexpr_constraint of pexpr * Ptype.ptype
and ppat = 
    {
        ppat_descr: ppat_descr;
        ppat_loc: Location.t;
    }
and ppat_descr = 
    | Ppat_iden of string
    | Ppat_const of pconst
    | Ppat_tuple of ppat list
    | Ppat_record of (string * ppat) list
    | Ppat_variant of Path.t * ppat
    | Ppat_list of ppat list
    | Ppat_listcons of ppat * ppat
    | Ppat_array of ppat list
    (* | Ppat_or of ppat * ppat *)
    | Ppat_wildcard

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

let rec str_ppat ?(prefix="") pt  = 
    prefix^begin
        match pt.ppat_descr with
        | Ppat_iden str -> str
        | Ppat_const const -> Constants.str_constant const
        | Ppat_tuple ppts -> 
            assert (List.length ppts <> 0);
            "("^(str_ppat (List.hd ppts))^(List.fold_left (fun str pt -> str^", "^(str_ppat pt)) "" (List.tl ppts))^")" 
        (* | Ppat_record  *)
        | Ppat_constr (str, ppts) -> str^(List.fold_left (fun str ppt -> str^" "^(str_ppat ppt)) "" ppts)
        | Ppat_list ppts -> "["^(List.fold_left (fun str ppt -> if str="" then (str_ppat ppt) else str^"; "^(str_ppat ppt)) "" ppts)^"]"
        | Ppat_listcons (ppt1, ppt2) -> (str_ppat ppt1)^" :: "^(str_ppat ppt2)
        | Ppat_array ppts -> "[|"^(List.fold_left (fun str ppt -> if str="" then (str_ppat ppt) else str^"; "^(str_ppat ppt)) "" ppts)^"|]"
        | Ppat_or (ppt1, ppt2) -> (str_ppat ppt1)^" | "^(str_ppat ppt2)
        | Ppat_wildcard -> "_"
    end
let rec str_pexpr ?(prefix="") pe =
    prefix^begin
        match pe.pexpr_descr with
        | Pexpr_path p -> Path.str_path p
        | Pexpr_const const -> Constants.str_constant const
        | Pexpr_let (pt1, pe1) -> "let "^(str_ppat pt1)^" = "^(str_pexpr pe1)
        | Pexpr_apply (pe1, pes) -> (str_pexpr pe1)^(List.fold_left (fun str pe -> str^" "^(str_pexpr pe)) "" pes)
        | Pexpr_tuple pes -> 
            assert (List.length pes <> 0);
            "("^(str_pexpr (List.hd pes))^(List.fold_left (fun str pe -> str^", "^(str_pexpr pe)) "" (List.tl pes))^")" 
        | Pexpr_constr (str, pes) -> str^(List.fold_left (fun str pe -> str^" "^(str_pexpr pe)) "" pes)
        | Pexpr_record str_pes -> 
            assert (List.length str_pes <> 0);
            "{"^(List.fold_left (fun str (str1, pe) -> str^str1^"="^(str_pexpr pe)^";") "" (str_pes))^"}" 
        | Pexpr_with (pe1, str_pes) -> (str_pexpr pe1)^" with {"^(List.fold_left (fun str (str1,pe) -> str^str1^"="^(str_pexpr pe)^";") "" str_pes)^"}"
        | Pexpr_list pes -> "["^(List.fold_left (fun str pe -> if str="" then str_pexpr pe else str^";"^(str_pexpr pe)) "" pes)^"]"
        | Pexpr_array pes -> "[|"^(List.fold_left (fun str pe -> if str="" then str_pexpr pe else str^";"^(str_pexpr pe)) "" pes)^"|]"
        | Pexpr_sequence (pe1, pe2) -> (str_pexpr pe1)^";\n"^(str_pexpr ~prefix pe2)
        | Pexpr_if (pe1, pe2, None) -> "if "^(str_pexpr pe1)^" then \n"^(str_pexpr ~prefix:(prefix^"\t") pe2)
        | Pexpr_if (pe1, pe2, Some pe3) -> "if "^(str_pexpr pe1)^" then "^(str_pexpr pe2)^" else "^(str_pexpr pe3)
        | Pexpr_while (pe1, pe2) -> "while "^(str_pexpr pe1)^"\n"^(str_pexpr ~prefix:("\t"^prefix) pe2)
        | Pexpr_for (str, pe1, pe2, pe3) -> "for "^str^" in "^(str_pexpr pe1)^"..."^(str_pexpr pe2)^"\n"^(str_pexpr ~prefix:("\t"^prefix) pe3)
        | Pexpr_match (pe1, ppat_pes) -> "match "^(str_pexpr pe1)^" with"^(List.fold_left (fun str (ppat, pe) ->str^"\n"^(str_ppat ~prefix:("\t"^prefix) ppat)^" -> "^(str_pexpr pe)) "" ppat_pes)
        | Pexpr_constraint (pe, t) -> (str_pexpr pe)^": "^(Types.str_type t)
    end