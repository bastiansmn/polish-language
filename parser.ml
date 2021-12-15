open Model

let split_words line = String.split_on_char(' ')(line)

let list_comp = [(Eq, "="); (Ne, "<>"); (Lt, "<"); (Le, "<="); (Gt, ">"); (Ge, ">=")]

let parse_comp comp = try(List.assoc comp list_comp) with Not_found -> raise (Failure "Unexpected comparator") 

let is_comp comp = List.mem comp list_comp
      
let is_int str = try (int_of_string str; true) with Invalid_argument (str) -> false
                  
let list_op = [(Eq, "="); (Ne, "<>"); (Lt, "<"); (Le, "<="); (Gt, ">"); (Ge, ">=")]
let is_op op = List.mem comp list_comp

let parse_op op = try(List.assoc op list_op) with Not_found -> raise (Failure "Unexpected operand") 

let parse_expr tokens =
   let rec listtostack list acc =
      match list with
      | [] -> acc
      | e::l -> let _ = Stack.push(e)(acc) in listtostack l acc 
   in let stack = listtostack (List.rev(tokens)) (Stack.create())
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
      | Some(line) -> aux(split_words line::acc)
   in List.rev (aux [])

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
      | [] -> (acc, [])
      | e::l -> if getindentation e >= ind && getindentation e mod 2 = 0 then aux l (acc@[e])
                else (acc, e::l) 
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
      | line::rest -> 
         if getindentation line = indent then
            match unindent(line) with
            | [] -> [] (* TODO Vérifier si on devrait renvoyer err *)
            | wd::r ->  if wd = "READ" then 
                           (iline, parse_read (r))::(parse_block indent rest (iline+1))
                        else if wd = "PRINT" then
                           (iline, parse_print (r))::(parse_block indent rest (iline+1))
                        else if wd = "COMMENT" then
                           parse_block (indent) (rest) (iline+1)
                        else if wd = "IF" then
                           let blocif_and_rest = getlines_with_indent (rest) (indent+2) 
                           in match snd(blocif_and_rest) with
                              | [] -> []
                              | line::rest ->
                                 if List.hd(unindent line) = "ELSE" && getindentation(line) = indent then
                                    let blocelse_and_rest = getlines_with_indent (rest) (indent+2)
                                    in let blockif = parse_block(indent+2)(fst(blocif_and_rest))(iline+1)
                                    in if List.length blockif = 0 then raise (Failure "IF cannot be empty")
                                       else let blockelse = parse_block(indent+2)(fst(blocelse_and_rest))(iline+1)
                                          in (iline, If(
                                             parse_cond(r), blockif, blockelse
                                          ))::(parse_block (indent) (snd(blocelse_and_rest)) (iline+1))
                                 else if getindentation(line) = indent then
                                    let blockif = parse_block (indent+2) (fst(blocif_and_rest)) (iline+1)
                                    in if List.length blockif = 0 then raise (Failure "IF cannot be empty")
                                       else (iline, If(
                                          parse_cond(r), blockif, []
                                       ))::((parse_block (indent) (snd(blocif_and_rest)) (iline+1)))
                                 else 
                                    raise ( Failure "Unexpected indentation" )
                        else if wd = "WHILE" then
                           let blocwhile_and_rest = getlines_with_indent (rest) (indent+2)
                           in let blockwhile = parse_block(indent+2)(fst(blocwhile_and_rest))(iline+1)
                           in (iline, While(
                              parse_cond(r), blockwhile
                           ))::(parse_block (indent) (snd(blocwhile_and_rest)) (iline+1))
                        else 
                           (iline, parse_set (wd::r))::(parse_block indent rest (iline+1))
         else raise (Failure ( Printf.sprintf 
                        "Unexpected indentation : line=%d ('%s')\nGot %d expected %d" iline (String.concat " " line) (getindentation line) indent))
   in parse_block 0 lines 1
