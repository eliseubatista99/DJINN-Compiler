type ident = string

type unop = 
  Neg
  | Not

type binop = 
  Plus 
  | Minus 
  | Mul 
  | Div

type boolop = 
  And 
  | Or 
  | Not 
  | Larger 
  | Smaller 
  | Lequal 
  | Sequal 
  | Equals 
  | Notequal

type const = 
  | B of bool
  | I of int

type expr = 
  | Const of const
  | Ident of ident
  | Binop of binop * expr * expr
  | Boolop of boolop * expr * expr
  | Unop of unop * expr

type stmt =
  | IfCondition of expr * stmt
  | IfElseCondition of expr * stmt * stmt
  | Print of expr
  | Foreach of ident * expr * expr * stmt
  | Block of stmt list
  | Setter of ident * expr
  | Operation of ident * expr
  | Array of ident * expr
  | TypeInt of ident * expr * expr
  | TypeArray of ident * expr
  | FilledBy of ident * ident * expr

and prog = stmt