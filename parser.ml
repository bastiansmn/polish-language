open Model


let split_words line =
   String.split_on_char(' ')(line)

let rec print_list l =
   match l with
   | [] -> ()
   | e::l -> print_endline e; print_list l

let rec print_file lines =
   match lines with
   | [] -> ()
   | e::l -> print_list e; print_endline "--NEW LINE--"; print_file l

let parse_block lines =
   failwith "TODO"
   (* Lit chaque lignes,
   si on remarque une indentation différente de celle courante (créer fonction aux ?), alors créer nouveau block avec les lignes qui ont cette même indentation (parse_block rec) (Peut etre ajouter un i = indentation pour le comparer aux blocs suivants)
   sinon (si on est dans la même indentation que le block courant), la ligne courante est une instruction (parse_instr)
   Exemple :
   n := 12           <- lecture d'un nv block (on lancera parse_block avec 0 ?)
   IF n = 2
      n = 12         <- lecture d'un nv block (parse_block avec 2 ?)
   ELSE
      IF n = 1			<- lecture d'un nv block (2)
         n = 14      <- lecture d'un nv block (4)
      ELSE
         n = 11 		<- idem
   return n
   *)

let parse_instr lines =
   failwith "TODO"
   (* Lit chaques lignes
   Si on lit un mot clé d'instruction (READ, SET, ...), alors on transf cette expr en ce qu'elle est censé devenir (appel à des fonctions pour chaque instr ?)
   sinon (il devrait y avoir une erreur ou un comm)
   *)

(* TODO : fonction spécifiques à chaque instructions *)

let parse_while lines =
   failwith "TODO"
   (* Lire la première ligne -> parse_cond
   Ensuite parse_block pour le reste
    *)

let parse_comp comp =
   if comp = "=" then Eq
   else if comp = "<>" then Ne
   else if comp = "<" then Lt
   else if comp = "<=" then Le
   else if comp = ">" then Gt
   else if comp = ">=" then Ge
   else raise (Failure "Unexpected operator") 

let is_comp comp = (comp = "=" || comp = "<>"  || comp = "<" || comp = "<=" || comp = ">" || comp = ">=")
      
let is_int str =
   let rec aux i =
      try (
         let ch = String.get(str)(i)
         in if Char.code ch <= 57 && Char.code ch >= 48 || (ch = '-' && i = 0) then aux(i+1)
         else false
      ) with Invalid_argument(i) -> true
   in aux 0

let is_op op =
   if op = "+" || op = "-"  || op = "*" || op = "/" || op = "%" then true
   else false

let parse_op op =
   if op = "+" then Add
   else if op = "-" then Sub
   else if op = "*" then Mul
   else if op = "/" then Div
   else if op = "%" then Mod
   else raise (Failure "Unexpected operand")

let parse_expr words =
   let rec listtostack list acc =
      match list with
      | [] -> acc
      | e::l -> let _ = Stack.push(e)(acc) in listtostack l acc 
   in let stack = listtostack (List.rev(words)) (Stack.create())
   in let rec aux () =
      let wd = Stack.pop(stack)
      in if is_op wd then
         Op(parse_op(wd), aux (), aux ())
      else if is_int wd then 
         Num(int_of_string(wd))
      else
         Var(wd)
   in let res = aux ()
   in let rec invertOp op =
      match op with
      | Num(i) -> Num(i)
      | Var(s) -> Var(s)
      | Op(op, e1, e2) -> Op(op, invertOp e2, invertOp e1)
   in if not(Stack.is_empty(stack)) then raise (Failure "Expression not available")
      else invertOp(res)

let parse_cond line =
   let rec aux_cond wrd acc =
      match wrd with
      | [] -> raise (Failure "Unexpected syntaxe line") 
      | e::l -> if is_comp e then (parse_expr (acc), parse_comp e, parse_expr l)
         else aux_cond l (acc@[e]) 
   in aux_cond line []

let get_lines filename =
   let ic = open_in filename
   in let try_read () =
      try
         Some(input_line ic)
      with End_of_file -> None
   in let rec aux acc =
      match try_read () with
      | None -> acc
      | Some(line) -> aux(acc @ [split_words line])
   in aux []

let read_polish (filename:string) =
   print_file (get_lines filename)