{
  open Lexing
  open DJINNast
  open DJINNparser

exception Lexing_error of string

let id_or_kwd =
  let h = Hashtbl.create 32 in
  List.iter (fun (s,tok) -> Hashtbl.add h s tok)
    [
      "print", PRINT;
      "if", IF;
      "then", THEN;
      "else", ELSE;
      "foreach", FOR;
      "in", IN;
      "do", DO;
      "and", AND;
      "or", OR;
      "not", NOT;
      "True", CONST (B true);
      "False", CONST (B false);
      "int", INT;
      "type", TYPE;
      "var", SETTER;
      "array", ARRAY;
      "filled", FILLED;
      "by", BY;
      "of", OF;
    ];
  fun s -> try Hashtbl.find h s with Not_found -> IDENT s

  let string_buffer = Buffer.create 1024
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = letter (letter | digit | '_')*
let integer = digit+
let space = [' ' '\t']
let comment = "//" [^'\n']*

rule next_tokens = parse 
| '\n'    { new_line lexbuf; next_tokens lexbuf }
| (space | comment)+ { next_tokens lexbuf }
| ident as id { [id_or_kwd id] }
| '+' { [PLUS] }
| '-' { [MINUS] }
| '*' { [MUL] }
| '/' { [DIV] }
| '=' { [EQUAL] }
| "==" { [EQUALS] }
| "!=" { [NOTEQUAL] }
| "<" { [SMALLER] }
| "<=" { [SEQUAL] }
| ">" { [LARGER] }
| ">=" { [LEQUAL] }
| '(' { [ LPAR ] }
| ')' { [ RPAR ] }
| '{' { [ LBRACES ] }
| '}' { [ RBRACES ] }
| ',' { [ COMMA ] }
| ':' { [ COLON ] }
| ';' { [ SCOLON ] }
| ".." { [ POINTS ] }
| integer as s 
          {
            try [CONST (I (int_of_string s))]
            with _ -> raise (Lexing_error ("constant is too large: " ^ s))
          }
| eof { [EOF] }
| _ as c { raise (Lexing_error ("illegal character: " ^ String.make 1 c)) } 

and indentation = parse
  | (space | comment)* '\n'
      { new_line lexbuf; indentation lexbuf }
  | space* as s
      { String.length s }

{

  let next_token =
    let tokens = Queue.create () in 
    fun lb -> 
      if Queue.is_empty tokens then begin 
    let l = next_tokens lb in
    List.iter (fun t -> Queue.add t tokens) l
      end;
      Queue.pop tokens

}