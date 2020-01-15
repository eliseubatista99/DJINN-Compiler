open Format
open X86_64
open DJINNast

(* alocação das variáveis *)

exception VarUndef of string

let (genv : (string, unit) Hashtbl.t) = Hashtbl.create 17

module Smap = Map.Make(String)

type local_env = ident Smap.t

let frame_size = ref 0

let rec alloc_expr env next = function
  | Const i ->
    Const i, next
  | _ -> assert false

let alloc_stmt = function

  | Print e ->
    Print e
  | _ -> assert false

let alloc = List.map alloc_stmt

(******************************************************************************)
(* produção de código *)

let popn n = addq (imm n) (reg rsp)
let pushn n = subq (imm n) (reg rsp)

module StrMap = Map.Make(String)

let rec compile_expr =

    let rec comprec env next = function
      | Const (I i) ->
        movq (imm i) (reg rax) ++
        pushq rax

      | Binop (Div, e1, e2) ->
      comprec env next e1 ++
      comprec env next e2 ++
      movq (imm 0) (reg rdx) ++
      popq rbx ++
      popq rax ++
      idivq (reg rbx) ++
      pushq rax

      | Binop (o, e1, e2)->
          let op = match o with
            | Plus -> addq
            | Minus -> subq
            | Mul -> imulq
            | _ -> assert false
          in
          comprec env next e1 ++
          comprec env next e2 ++
          popq rbx ++
          popq rax ++
          op (reg rbx) (reg rax) ++
          pushq rax
    
      | _ -> assert false

      in comprec StrMap.empty 0

let rec compile_stmt = function

  | Print e -> 
    compile_expr e ++
    popq rdi ++
    call "print_int"

  | Block bl ->
    block bl

  | _ -> assert false
  
and block = function
  | [a] -> compile_stmt a
  | s :: sl ->  nop; compile_stmt s; (++); block sl
  | _ -> assert false

let start p =
  match p with
  | Block sb -> sb

let compile_program p ofile =
  let code = List.map compile_stmt (start p) in let code = List.fold_right (++) code nop in
  let p =
    { text =
        glabel "main" ++
        subq (imm !frame_size) (reg rsp) ++
        leaq (ind ~ofs:(!frame_size -8) rsp) rbp ++ 
        code ++
        addq (imm !frame_size) (reg rsp) ++
        movq (imm 0) (reg rax) ++
        ret ++
        label "print_int" ++
        movq (reg rdi) (reg rsi) ++
        movq (ilab ".Sprint_int") (reg rdi) ++
        movq (imm 0) (reg rax) ++
        ret;
      data =
        Hashtbl.fold (fun x _ l -> label x ++ dquad [1] ++ l) genv
          (label ".Sprint_int" ++ string "%d\n")
    }
  in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  X86_64.print_program fmt p;
  fprintf fmt "@?";
  close_out f

