%{
    open Parsetree
%}

%token <int>Int 
%token <float>Float
%token <string>Iden UIden
%token Import Datatype Vertical Value Let Match With Underline Model Next If Then Else For In While Do Done
%token LB1 RB1 LB2 RB2 LB3 RB3 Equal Non_Equal LT GT LE GE Comma Semicolon Dot DotDot Arrow EOF Add AddDot Minus MinusDot Mult MultDot
%token Negb Ando Oro And Or Neg LArrow Colon ColonColon Top Bottom AX EX AF EG AR EU True False Function
%token TList TFloat TArray TInt TBool TUnt At Init Transition Atomic Spec Fairness Assigno
%start <Parsetree.pmodule>program
%%

program: 
      imports definitions EOF {({filename=""; pimported=$1; pdefinitions=$2, pspecs=[]})}
    | imports definitions specs EOF {{filename=""; pimported=$1; pdefinitions=$2, pspecs=$3}}
;

imports: {[]}
    | Import UIden imports {$1::$3}
;

definitions: {[]}
    | definition definitions {$1::$2}
;

definition:
      Value p=pattern ot=option(typ) Equal e=expr {Pdef_value (p,opt,e)}
    | Datatype tname=Iden Equal t=typ {Pdef_type (tname, {params=[];arity=0;kind=PTKalias t;ptype_decl_loc=Location.make $startpos(t) $endpos(t)})}
    | Datatype tname=Iden targ=Iden Equal t=typ {Pdef_type (tname, {params=[targ];arity=1;kind=PTKalias t;ptype_decl_loc=Location.make $startpos(t) $endpos(t)})}
    | Datatype tname=Iden LB1 targs=separated_list(Comma, Iden) RB1 Equal t=typ {Pdef_type (tname, {params=targs;arity=List.length targs;kind=PTKalias t;ptype_decl_loc=Location.make $startpos(t) $endpos(t)})}
    | Datatype tname=Iden Equal tk=type_kind {Pdef_value (tname, {params=[];arity=0;kind=tk;ptype_decl_loc=Location.make $startpos(tk) $endpos(tk)})}
;


typ:
;

type_kind:
;


%%