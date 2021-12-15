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

let pop stack =
   match stack with
   | [] -> raise (Failure "Empty stack")
   | e::l -> (e, l)

let parse_expr words =
   let rec aux words_stack =
      let split_stack = pop(words_stack)
      in let wd = fst(split_stack)
      in let stack = snd(split_stack)
      in if is_op wd then
         let res_next_call = aux(stack)
         in let new_stack = snd(res_next_call)
         in let right_res = aux(new_stack)
         in (Op(parse_op(wd), fst(res_next_call), fst(right_res)), snd(right_res))
      else if is_int wd then
         (Num(int_of_string wd), stack)
      else
         (Var(wd), stack)
   in let res = aux words
   in if List.length (snd(res)) > 0 then raise (Failure "Expression not available")
      else fst(res)

let parse_cond line =
   let rec aux_cond wrd acc =
      match wrd with
      | [] -> raise (Failure "Unexpected syntaxe line") 
      | e::l -> if is_comp e then (parse_expr (acc), parse_comp e, parse_expr l)
         else aux_cond l (acc@[e]) 
   in aux_cond line []
   (* La fonction parcours l'element line si un element cond est trouvé alors elle renvoie l'object avec
   l'expression contenu en accumulateur, la condition courante et le reste de la list
   Si on arrive à la fin du parcours, c'est qu'il y'a eu un probleme de syntaxe
    *)

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

(* Renvoie le nombre d'espaces en début de ligne *)
let getindentation line =
   let rec aux line acc =
      match line with 
      | [] -> 0
      | e::l -> if e = "" then aux (l) (acc+1)
               else acc
   in aux line 0

(* Prend des une ligne (sans READ) et renvoie Read(var) ssi il n'y a qu'une seule var après READ *)
let parse_read line =
   if List.length line = 1 then Read(List.hd line)
   else raise (Failure "Cannot read multiple variables")

(* Renvoie Print(expr) *)
let parse_print line =
   Print(parse_expr line)

(* Renvoie la ligne 'line' désindentée *)
let unindent line =
   let rec aux line =
      match line with
      | [] -> line
      | e::l -> if e = "" then aux l
                else line
   in aux line

(* Parse set ssi il n'y a qu'une var avant := et une expr après *)
let parse_set line =
   print_list (unindent line);
   let rec aux_set line acc =
      match line with
      | [] -> raise (Failure "Unexpected syntaxe line") 
      | e::l -> if e = ":=" then 
                  if List.length acc = 1 then Set(List.hd(acc), parse_expr l)
                  else raise (Failure "Only one variable name expected")
                else aux_set l (e::acc)
   in aux_set (unindent line) []

(* TODO : Vérifier qu'un else n'est pas seul
   Actuellement :
   WHILE cond
      ...
   ELSE
      ...
   est accepté
 *)
(* Renvoie toutes les lignes de line tant qu'il y a une indentation=ind (ligne de ELSE exclus) *)
let getlines_with_indent lines ind =
   let rec aux lines acc =
      match lines with
      | [] -> acc
      | e::l -> if getindentation e = ind || List.hd(unindent e) = "ELSE" then aux l (acc@[e])
                else acc
   in aux lines []

(* Sépare des lignes en 2 listes de lignes à l'endroit où on lit ELSE 
   Ex :
   split_in_blocs ["  PRINT 2";"  PRINT 3";"ELSE";"  PRINT 4"]
   renvoie (["  PRINT 2";"  PRINT 3"], ["  PRINT 4"])
   Permet de séparer les deux blocs d'une instruction IF
 *)
let split_in_blocs lines =
   let rec aux line bloc1 =
      match line with 
      | [] -> raise ( Failure "Cannot parse IF and ELSE blocs" )
      | e::l -> if List.hd(e) = "ELSE" then (bloc1, l)
                else aux l (bloc1@[e])
   in aux lines []

(* Parse un programme et appel parse les blocs intérieurs récursivement *)
let parse_program lines =
   let rec parse_block indent lines iline =
      match lines with 
      | [] -> []
      | line::rest -> if getindentation line = indent then
                        match unindent(line) with
                        | [] -> raise (Failure "Unexpected indent")
                        | wd::r ->  if wd = "READ" then 
                                       (iline, parse_read (r))::(parse_block indent rest (iline+1))
                                    else if wd = "PRINT" then
                                       (iline, parse_print (r))::(parse_block indent rest (iline+1))
                                    else if wd = "COMMENT" then
                                       parse_block (indent) (rest) (iline+1)
                                    else if wd = "IF" then
                                       let blocs = split_in_blocs ( getlines_with_indent rest (indent+2))
                                       in (iline, 
                                          If(parse_cond r,
                                              parse_block (indent+2) (fst(blocs)) (iline+1),
                                              parse_block (indent+2) (snd(blocs)) (iline+1)
                                          ))::(parse_block indent rest (iline+1))
                                    else if wd = "WHILE" then
                                       (iline,
                                          While(parse_cond (r), parse_block (indent+2) (getlines_with_indent rest (indent+2)) (iline+1))
                                       )::(parse_block indent rest (iline+1))
                                    else 
                                       (iline, parse_set (wd::r))::(parse_block indent rest (iline+1))
                      else raise (Failure ( Printf.sprintf 
                                    "Unexpected indentation : line=%d ('%s')\nGot %d expected %d" iline (String.concat " " line) (getindentation line) indent))
   in parse_block 0 lines 1