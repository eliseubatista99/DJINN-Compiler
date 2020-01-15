open DJINNast
open Format

exception Error of string
let error s = raise (Error s)

let functions = (Hashtbl.create 17 : (string, ident list * stmt) Hashtbl.t)

type value =
  | None
  | Bool of bool
  | Int of int 
  | Arrays of value array

let rec print_value = function 
  | Bool true -> printf "True"
  | Bool false -> printf "False"
  | Int n -> printf "%d" n
  | Arrays a -> let n = Array.length a in
      printf "[";
      for i=0 to n-1 do print_value a.(i); if i < n-1 then printf ", " done;
      printf "]"

let get_len = function
  | Arrays a -> let n = Array.length a in n

let is_false = function
  | Arrays [||] -> true
  | Bool false -> false 
  | Int n -> n = 0
  | _ -> true

let is_true v = (is_false v)

let neg v = not (is_false v)

let rec compare_list a1 n1 a2 n2 i =
  if i = n1 && i = n2 then 0
  else if i = n1 then -1
  else if i = n2 then 1
  else let c = compare_values a1.(i) a2.(i) in
       if c <> 0 then c else compare_list a1 n1 a2 n2 (i + 1)

and compare_values v1 v2 = match v1, v2 with
  | Arrays a1, Arrays a2 -> compare_list a1 (Array.length a1) a2 (Array.length a2) 0
  | Bool b1, Int _ -> compare_values (Int (if b1 then 1 else 0)) v2
  | Int _ , Bool b2 -> compare_values v1 (Int (if b2 then 1 else 0))
  | _ -> compare v1 v2

let rec compare_value v1 v2 = match v1, v2 with
  | Arrays a1, Arrays a2 -> compare_list a1 (Array.length a1) a2 (Array.length a2) 0
  | Bool b1, Int _ -> compare_value (Int (if b1 then 1 else 0)) v2
  | Int _ , Bool b2 -> compare_value v1 (Int (if b2 then 1 else 0))
  | _ -> compare v1 v2

let binop op v1 v2 = match op, v1, v2 with
  | Plus, Int n1, Int n2 -> Int (n1 + n2)
  | Minus, Int n1, Int n2 -> Int (n1 - n2)
  | Mul, Int n1, Int n2 -> Int (n1 * n2)
  | Div, Int _, Int 0 -> error "dividing by zero"
  | Div, Int n1, Int n2 -> Int (n1 / n2)
  | _ -> error "unsuported operand types"

let boolop op v1 v2 = match op, v1, v2 with
  | Equals, _, _ -> Bool (compare_value v1 v2 = 0)
  | Notequal, _, _ -> Bool (compare_value v1 v2 <> 0)
  | Smaller, _, _ -> Bool (compare_value v1 v2 < 0)
  | Sequal, _, _ -> Bool (compare_value v1 v2 <= 0)
  | Larger, _, _ -> Bool (compare_value v1 v2 > 0)
  | Lequal, _, _ -> Bool (compare_value v1 v2 >= 0)
  | _ -> error "unsuported operand types"

type ctx = (string, value) Hashtbl.t

let rec expr ctx = function
  | Const (B b) -> Bool b
  | Const (I n) -> Int n
  | Boolop (And, e1, e2) -> 
      let v1 = expr ctx e1 in
      if is_true v1 then expr ctx e2 else v1
  | Boolop (Or, e1, e2) ->
      let v1 = expr ctx e1 in
      if not (is_true v1) then expr ctx e2 else v1
  | Binop (Plus | Minus | Mul | Div as op, e1, e2) ->
      binop op (expr ctx e1) (expr ctx e2)
  | Boolop (Equals | Notequal | Smaller | Sequal | Larger | Lequal as op, e1, e2) ->
      boolop op (expr ctx e1) (expr ctx e2)
  | Unop (Neg, e1) ->
      begin match expr ctx e1 with
        | Int n -> Int (-n)
        | _ -> error "Unsuported operand types" 
      end
  | Unop (Not, e1) ->
      Bool (neg (expr ctx e1))
  | Ident id ->
      if not (Hashtbl.mem ctx id) then error "unbound variable";
      Hashtbl.find ctx id

and expr_int ctx e = match expr ctx e with
  | Int n -> n
  | _ -> error "integer expected"

and expr_array ctx e = match expr ctx e with
  | Arrays n -> n
  | _ -> error "array expected"

and stmt ctx = function 
  | IfCondition (e, s) -> if is_true (expr ctx e) then stmt ctx s
  | IfElseCondition (e, s1, s2) -> if is_true (expr ctx e) then stmt ctx s1 else stmt ctx s2
  | Foreach (x, e1, e2, s) -> for i=(expr_int ctx e1) to (expr_int ctx e2) do (stmt ctx s) done;
  | Block bl -> block ctx bl
  | Setter (id,e) -> Hashtbl.replace ctx id (expr ctx e)
  | Print e -> 
      print_value (expr ctx e); printf "@."
  | Operation(id,e) -> if not (Hashtbl.mem ctx id) then error "unbound variable";
      Hashtbl.replace ctx id (expr ctx e)
  | TypeInt (id, e1, e2) -> Hashtbl.replace ctx id (Int(expr_int ctx e2 - expr_int ctx e1))
  | TypeArray (id, e) -> Hashtbl.replace ctx id (Arrays(Array.make (expr_int ctx e) None))
  | FilledBy (id, id2, e) -> Hashtbl.replace ctx id (Arrays(Array.make (get_len (Hashtbl.find ctx id2)) (Int(expr_int ctx e))))

and block ctx = function
  | [] -> ()
  | s :: sl -> stmt ctx s; block ctx sl

let prog s =
  stmt (Hashtbl.create 17) s 
