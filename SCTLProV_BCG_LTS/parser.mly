%{
  open Lts
  open Formula
  
  let actions = Hashtbl.create 1
%}

%token <action> Action 
%token <string> Iden
%token TRUE FALSE AX EX AF EG AR EU And Or Not
%token Comma Semicolon LB1 RB1 LB3 RB3 Newline ACTION SPEC Exclam Amper Vert Assign
%token EOF

%start <(string * formula) list>program

%left Or
%left And
%right Not

%left Vert
%left Amper
%right Exclam
%%

program: actions specs EOF {$2}
  | specs   {$1}
  | EOF     {[]}
;

actions: ACTION LB3 action_bindings RB3 {};
specs: SPEC LB3 spec_bindings RB3 {$3};

action_bindings:  {()}
  | Iden Assign action Semicolon action_bindings {Hashtbl.add actions $1 $3}
;

spec_bindings: {[]}
  | Iden Assign fml Semicolon spec_bindings {($1, $3)::$5}
;

action: Iden  {Hashtbl.find actions $1}
  | Action  {$1}
  | Exclam action {Not $2}
  | action Amper action {Both ($1, $3)}
  | action Vert action  {Either ($1, $3)}
;

fml: TRUE {Top}
  | FALSE {Bottom}
  | Not fml {Neg $2}
  | EX LB1 action Comma Iden Comma fml Comma Iden RB1 {EX ($3, $5, $7, $9)}
  | AX LB1 action Comma Iden Comma fml Comma Iden RB1 {AX ($3, $5, $7, $9)}
  | EG LB1 action Comma Iden Comma fml Comma Iden RB1 {EG ($3, $5, $7, $9)}
  | AF LB1 action Comma Iden Comma fml Comma Iden RB1 {AF ($3, $5, $7, $9)}
  | EU LB1 action Comma Iden Comma Iden Comma fml Comma fml Comma Iden RB1 {EU ($3, $5, $7, $9, $11, $13)}
  | AR LB1 action Comma Iden Comma Iden Comma fml Comma fml Comma Iden RB1 {AR ($3, $5, $7, $9, $11, $13)}

