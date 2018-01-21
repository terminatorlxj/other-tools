type tfml_fair = tfml * (tfml list)

and tfml = 
    {
        tfml_descr: tfml_descr;
        tfml_loc: Location.t;
    }
and tfml_descr = 
    | Ttop
    | Tbottom
    | Tatomic of string * (tstate list)
    | Tneg of tfml
    | Tand of tfml * tfml
    | Tor of pfml * pfml
    | TAX of string * tfml * tstate
    | TEX of string * tfml * tstate
    | TEG of string * tfml * tstate
    | TAF of string * tfml * tstate
    | TEU of string * string * tfml * tfml * tstate
    | TAR of string * string * tfml * tfml * tstate
and tstate =
    {
        tstate_descr: tstate_descr;
        tstate_loc: Location.t;        
    }
and tstate_descr = 
    | Tsvar of string
    | Tstate of expr

let make_tstate pst =
    match pst with
    | Psvar s -> Tsvar s
    | Pstate pexpr -> Tstate (make_expr pexpr)

let rec make_tfml pfml = 
    match pfml with
    | Ptop -> Ttop
    | Pbottom -> Tbottom
    | Patomic (s, psts) -> Tatomic (s, List.map (fun pst -> make_tstate pst) psts)
    | Pneg pfml1 -> Tneg (make_tfml pfml1)
    | Pand (pfml1, pfml2) -> Tand (make_tfml pfml1, make_tfml pfml2)
    | Por (pfml1, pfml2) -> Tor (make_tfml pfml1, make_tfml pfml2)
    | PAX (s, pfml1, pst) -> TAX (s, make_tfml pfml1, make_tstate pst)
    | PEX (s, pfml1, pst) -> TEX (s, make_tfml pfml1, make_tstate pst)
    | PAF (s, pfml1, pst) -> TAF (s, make_tfml pfml1, make_tstate pst)
    | PEG (s, pfml1, pst) -> TEG (s, make_tfml pfml1, make_tstate pst)
    | PAR (s1, s2, pfml1, pfml2, pst) -> TAR (s1, s2, make_tfml pfml1, make_tfml pfml2, make_tstate pst)
    | PEU (s1, s2, pfml1, pfml2, pst) -> TEU (s1, s2, make_tfml pfml1, make_tfml pfml2, make_tstate pst)

let make_tfml_fair (pfml, pfmll) = (make_tfml pfml, List.map (fun pfml -> make_tfml pfml) pfmll)

type fml_fair = fml * (fml list)

and fml = 
    | Ttop
    | Tbottom
    | Tatomic of string * (state list)
    | Tneg of fml
    | Tand of fml * fml
    | Tor of fml * fml
    | TAX of string * fml * state
    | TEX of string * fml * state
    | TEG of string * fml * state
    | TAF of string * fml * state
    | TEU of string * string * fml * fml * state
    | TAR of string * string * fml * fml * state
and state =
    | Svar of string
    | State of value