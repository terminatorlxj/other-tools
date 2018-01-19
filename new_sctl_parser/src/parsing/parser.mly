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
      Value id=Iden option(type) Equal e = expr {}
    | Datatype 


%%