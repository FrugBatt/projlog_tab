{
  open Parser
  exception SyntaxError of string
}

rule token = parse
    |[' ' '\t']           { token lexbuf }
    |['\n']               { EOL }  
    |"and"                { AND }
    |"or"                 { OR }
    |"not"                { NOT }
    |"true"               { TRUE }
    |"false"              { FALSE }
    |"("                  { LPAREN }
    |")"                  { RPAREN }
    |"=>"                 { IMPLIES }
    |"forall"             { FORALL }
    |"exists"             { EXISTS }
    |['A'-'Z']['a'-'z']*  { PROP(Lexing.lexeme lexbuf) }
    |['a'-'z']+           { VAR(Lexing.lexeme lexbuf) }
    |","                  {COMMA}
    |eof                  { EOF }
    | _                   { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }



