%{
  open Formula
  
%}

%token <action> Action 
%token <string> Iden
%token TRUE FALSE AX EX AF EG AR EU And Or Not
%token Comma LB1 RB1 Newline
%token EOF

%start <Formula.fml list>spec
%left Or
%left And
%right Not
%%
spec: fmls EOF  {$1};

fmls: {[]}
  | fml Newline spec {$1 :: $3}
  ;

fml: TRUE {Top}
  | FALSE {Bottom}
  | Not fml {Neg $2}
  | EX LB1 Action Comma Iden Comma fml Comma Iden RB1 {EX ($3, $5, $7, $9)}
  | AX LB1 Action Comma Iden Comma fml Comma Iden RB1 {AX ($3, $5, $7, $9)}
  | EG LB1 Action Comma Iden Comma fml Comma Iden RB1 {EG ($3, $5, $7, $9)}
  | AF LB1 Action Comma Iden Comma fml Comma Iden RB1 {AF ($3, $5, $7, $9)}
  | EU LB1 Action Comma Iden Comma Iden Comma fml Comma fml Comma Iden RB1 {EU ($3, $5, $7, $9, $11, $13)}
  | AR LB1 Action Comma Iden Comma Iden Comma fml Comma fml Comma Iden RB1 {AR ($3, $5, $7, $9, $11, $13)}

