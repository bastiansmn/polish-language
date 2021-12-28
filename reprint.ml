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
   if indentation = 0 then ""
   else " " ^ getindentstring(indentation-1)

let print_pos pos =
   let log10ofpos = String.length (string_of_int pos) in
   let rec aux n =
      if n = 0 then ""
      else " " ^ aux (n-1)
   in string_of_int pos ^ aux (2 - log10ofpos) ^ " | "


let rec print_block block indent =
   let print_instr instr indent pos =
      match instr with
      | Set(name, expr) -> sprintf "%s%s := %s\n" (getindentstring indent) (name) (print_expr expr)
      | Read(name) -> sprintf "%sREAD %s\n" (getindentstring indent) (name)
      | Print(expr) -> sprintf "%sPRINT %s\n" (getindentstring indent) (print_expr expr)
      | If(cond, instr1, instr2) ->	sprintf "%sIF %s\n%s%s%sELSE\n%s" (getindentstring indent) (print_cond cond) (print_block instr1 (indent+2)) (print_pos (pos+List.length(instr1)+1)) (getindentstring indent) (print_block instr2 (indent+2))
      | While(cond, instr) -> sprintf "%sWHILE %s\n%s" (getindentstring indent) (print_cond cond) (print_block instr (indent+2))
   in match block with
   | [] -> ""
   | (pos, instr)::l -> (print_pos pos) ^ print_instr(instr)(indent)(pos) ^  print_block(l)(indent)

let print_program program =
   printf "%s" (print_block program 0)
   
