%{
  open DJINNast
%}

%token <string> IDENT
%token <DJINNast.const> CONST 
%token IF THEN ELSE
%token FOR IN DO
%token PRINT 
%token EOF INT
%token LPAR RPAR LBRACES RBRACES COMMA EQUAL COLON SCOLON POINTS
%token PLUS MINUS MUL DIV SETTER
%token AND OR NOT SMALLER SEQUAL LARGER LEQUAL NOTEQUAL EQUALS
%token TYPE ARRAY FILLED BY OF VAR

%left OR
%left AND
%nonassoc NOT
%nonassoc EQUALS SMALLER SEQUAL LARGER LEQUAL NOTEQUAL
%left PLUS MINUS
%left MUL DIV
%nonassoc unary_minus

%start prog

%type <DJINNast.prog> prog

%%

prog:
 | sl = stmt+ EOF                                 {Block sl}
;

expr:
| c = CONST                                       {Const c}
| id = ident                                      {Ident id}
| MINUS e=expr %prec unary_minus                  {Unop (Neg,e)}
| NOT e = expr                                    {Unop (Not,e)}
| e1 = expr op = binop e2 = expr                  {Binop (op,e1,e2)}
| e1 = expr op = boolop e2 = expr                 {Boolop (op,e1,e2)}
| LPAR e = expr RPAR                              { e }
;

stmt:
| IF LPAR e = expr RPAR THEN LBRACES s = stmt RBRACES                                     {IfCondition(e,s)}
| IF LPAR e = expr RPAR THEN LBRACES s1 = stmt RBRACES ELSE LBRACES s2 = stmt RBRACES     {IfElseCondition (e,s1,s2)}
| FOR id = ident IN e1 = expr POINTS e2 = expr DO LBRACES s = stmt RBRACES                {Foreach (id,e1,e2,s)}
| PRINT LPAR e = expr RPAR SCOLON                                                         {Print (e)}
| SETTER id = ident COLON INT EQUAL e=expr SCOLON                                         {Setter (id,e)}
| id = ident COLON EQUAL e = expr SCOLON                                                  {Operation (id,e)}
| SETTER id = ident COLON ARRAY FILLED BY e = expr SCOLON                                 {Array (id,e)}
| SETTER id = ident COLON id2 = ident FILLED BY e=expr SCOLON                             {FilledBy (id,id2,e)}
| TYPE id = ident EQUAL e1=expr POINTS e2=expr SCOLON                                     {TypeInt (id,e1,e2)}
| TYPE id=ident COLON ARRAY e=expr OF INT SCOLON                                          {TypeArray (id,e)}
;

%inline binop:
| PLUS            {Plus}
| MINUS           {Minus}
| MUL             {Mul}
| DIV             {Div}
;

%inline boolop:
| AND               {And}
| OR                {Or}
| NOT               {Not}
| EQUALS            {Equals}
| LARGER            {Larger}
| LEQUAL            {Lequal}
| SMALLER           {Smaller}
| SEQUAL            {Sequal}
| NOTEQUAL          {Notequal}
;

ident:
  id = IDENT {id}
;