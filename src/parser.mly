%token EOL EOF
%token LPAREN RPAREN COMMA
%token <string> NAME
%token AND OR NOT IMPLIES
%token TRUE FALSE
%token FORALL EXISTS
%right IMPLIES
%right OR NOT
%right AND
%start parse
%type <Logic_formulas.formula> parse
%%
parse :
    | formula EOL { $1 }
    | EOL parse { $2 }
    | formula EOF { $1 }
    | EOF { Logic_formulas.True }
    ;

formula :
    | LPAREN formula RPAREN { $2 }
    | NOT formula { Logic_formulas.Not($2) }
    | formula AND formula { Logic_formulas.And($1,$3) }
    | formula OR formula { Logic_formulas.Or($1,$3) }
    | formula IMPLIES formula { Logic_formulas.Impl($1,$3) }
    | FORALL NAME formula { Logic_formulas.Forall($2,$3) }
    | EXISTS NAME formula { Logic_formulas.Exists($2,$3) }
    | NAME LPAREN args RPAREN { Logic_formulas.Predicate($1,$3) }
    | NAME { Logic_formulas.Var($1) }
    | TRUE { Logic_formulas.True }
    | FALSE { Logic_formulas.False }
    ;

args :
    | NAME LPAREN args RPAREN { [Logic_formulas.Predicate($1,$3)] }
    | NAME { [Logic_formulas.Var($1)] }
    | NAME LPAREN args RPAREN COMMA args{ [Logic_formulas.Predicate($1,$3)]@($6) }
    | NAME COMMA args { [Logic_formulas.Var($1)]@($3) }
    ;