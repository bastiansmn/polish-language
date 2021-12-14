(* TODO Reprint *)

open Model
open Printf

(* Print une expression sous forme prefixe *)
let rec print_expr expression =
   match expression with
   | Num(i) -> string_of_int i ^ " "
   | Var(name) -> name ^ " "
   | Op(op, l, d) -> (match op with
                     | Mul -> "( * " ^ print_expr(l) ^ print_expr(d) ^ ")"
                     | Add -> "( + " ^ print_expr(l) ^ print_expr(d) ^ ")"
                     | Sub -> "( - " ^ print_expr(l) ^ print_expr(d) ^ ")"
                     | Div -> "( / " ^ print_expr(l) ^ print_expr(d) ^ ")"
                     | Mod -> "( % " ^ print_expr(l) ^ print_expr(d) ^ ")")

(* Print une condition  *)
let print_cond cond = 
   let print_comp comp = 
      match comp with
      | Eq -> "="
      | Ne -> "<>"
      | Lt -> "<"
      | Le -> "<="
      | Gt -> ">"
      | Ge -> ">="
   in match cond with
   | (a, b, c) -> print_expr(a) ^ " " ^ print_comp(b) ^ " " ^ print_expr(c)

let rec getindentstring indentation =
   if indentation = 0 then "-"
   else " " ^ getindentstring(indentation-1)

let print_program program =
   printf "%s" (print_block program 0)

let rec print_block block indent =
   match block with
   | [] -> ""
   | (_, instr)::l -> getindentstring(indent) ^ print_instr(instr)(indent) ^ "\n" ^ print_block(l)(indent)
   
let print_instr instr indent =
   match instr with
   | Set(name, expr) -> sprintf "%s := %s" (name) (print_expr expr)
   | Read(name) -> sprintf "READ %s" (name)
   | Print(expr) -> sprintf "PRINT %s" (print_expr expr)
   | If(cond, instr1, instr2) ->	sprintf "IF %s\n%sELSE\n%s" (print_cond cond) (print_block instr1 (indent+2)) (print_block instr2 (indent+2))
   | While(cond, instr) -> sprintf "WHILE %s\n%s" (print_cond cond) (print_block instr (indent+2))