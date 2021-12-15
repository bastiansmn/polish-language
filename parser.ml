open Model

exception WrongOperator of string
exception WrongComparator of string
exception ExpressionNotAvailable of string
exception ConditionMissing of string
exception ExpressionMissing of string
exception MultipleVariables of string
exception UnexpectedIndentation of string

let split_words line = String.split_on_char(' ')(line)

let list_comp = [(Eq, "="); (Ne, "<>"); (Lt, "<"); (Le, "<="); (Gt, ">"); (Ge, ">=")]

let parse_comp comp = try(List.assoc comp list_comp) with Not_found -> raise (WrongComparator "Unexpected comparator")

let is_comp comp = List.mem comp list_comp
      
let is_int str = try (int_of_string str; true) with Invalid_argument (str) -> false
                  
let list_op = [(Eq, "="); (Ne, "<>"); (Lt, "<"); (Le, "<="); (Gt, ">"); (Ge, ">=")]
let is_op op = List.mem comp list_comp

let parse_op op = try(List.assoc op list_op) with Not_found -> raise (WrongOperator "Unexpected operator")

(* Renvoie la liste des mots dans une ligne *)
(* 
   Entrée : 
      - une ligne line
   Sortie : 
      - Une liste de mots
 *)
let split_words line =
   String.split_on_char(' ')(line)
  
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
   in if not(Stack.is_empty(stack)) then raise (ExpressionNotAvailable "Expression not available")
      else invertOp(res)

let parse_cond words =
   let rec aux_cond wrd acc =
      match wrd with
      | [] -> raise (ConditionMissing "Unexpected syntaxe line") 
      | e::l -> if is_comp e then (parse_expr (acc), parse_comp e, parse_expr l)
         else aux_cond l (acc@[e]) 
   in aux_cond words []

(* Renvoie le fichier séparés en lignes, puis chaque lignes séparées en mots *)
let get_lines filename =
   let ic = open_in filename
   in let try_read () =
      try
         Some(input_line ic)
      with End_of_file -> None
   in let rec aux acc =
      match try_read () with
      | None -> acc
      | Some(line) -> if String.length line <> 0 then aux((split_words line)::acc)
                      else aux(acc)
   in List.rev(aux [])

(* Renvoie le nombre d'espaces en début de ligne *)
let getindentation line =
   let rec aux line acc =
      match line with 
      | [] -> 0
      | e::l -> if e = "" then aux (l) (acc+1)
               else acc
   in aux line 0

(* Prend une ligne (sans READ) et renvoie Read(var) ssi il n'y a qu'une seule var après READ *)
let parse_read line =
   if List.length line = 1 then Read(List.hd line)
   else raise (MultipleVariables "Cannot read multiple variables")

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
      | [] -> raise (ExpressionMissing "Unexpected syntaxe line") 
      | e::l -> if e = ":=" then 
                  if List.length acc = 1 then Set(List.hd(acc), parse_expr l)
                  else raise (MultipleVariables "Only one variable name expected")
                else aux_set l (e::acc)
   in aux_set (unindent line) []

(* Renvoie toutes les lignes de line tant qu'il y a une indentation>=ind et vérifie au passage si l'indentation est paire *)
let getlines_with_indent lines ind =
   let rec aux lines acc =
      match lines with
      | [] -> (acc, [])
      | e::l -> if getindentation e >= ind && getindentation e mod 2 = 0 then aux l (acc@[e])
                else (acc, e::l) 
   in aux lines []

(* Associe à chaque éléments de la liste son indice (partant de 1) *)
let map_index list =
   let rec aux list i =
      match list with
      | [] -> []
      | e::l -> (i, e)::(aux l (i+1))
   in aux list 1

(* 
   Entrées: 
      - une indentation courante 
      - Des lignes à parser 
   Sortie : Le bloc correspondant
*)
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
                           parse_if (rest) (indent) (r) (iline+1)
                        else if wd = "WHILE" then
                           parse_while (rest) (indent) (r) (iline+1)
                        else 
                           (iline, parse_set (wd::r))::(parse_block indent rest (iline+1))
         else raise (UnexpectedIndentation ( Printf.sprintf 
                        "Unexpected indentation : line=%d ('%s')\nGot %d expected %d" iline (String.concat " " line) (getindentation line) indent))


(* Parse un if avec son bloc correspondant, puis son bloc ELSE si il existe *)
(* 
   Entrée :
      - Des lignes restantes après le if (rest)
      - Une indentation courante
      - Une condition à parser
   Sorite : If dans la synthaxe abstraite
 *)
and parse_if rest indent cond iline =
   let blocif_and_rest = getlines_with_indent (rest) (indent+2) 
      in match snd(blocif_and_rest) with
      (* line rest indent cond=r *)
         | [] -> []
         | line::rest ->
            if List.hd(unindent line) = "ELSE" && getindentation(line) = indent then
               let blocelse_and_rest = getlines_with_indent (rest) (indent+2)
               in let blockif = parse_block (indent+2) (fst(blocif_and_rest)) (iline+1)
               in if List.length blockif = 0 then raise (Failure "IF cannot be empty")
                  else let blockelse = parse_block (indent+2) (fst(blocelse_and_rest))(iline+1)
                     in (iline, If(
                        parse_cond(cond), blockif, blockelse
                     ))::(parse_block (indent) (snd(blocelse_and_rest)) (iline+1))
            else if getindentation(line) = indent then
               let blockif = parse_block (indent+2) (fst(blocif_and_rest)) (iline+1)
               in if List.length blockif = 0 then raise (Failure "IF cannot be empty")
                  else (iline, If(
                     parse_cond(cond), blockif, []
                  ))::((parse_block (indent) (snd(blocif_and_rest)) (iline+1)))
            else 
               raise ( UnexpectedIndentation "Unexpected indentation" )


(* Parse un while ainsi que son bloc suivant *)
(* 
   Entrée :
      - Des lignes restantes après le if (rest)
      - Une indentation courante
      - Une condition à parser
   Sortie : While dans la synthaxe abstraite
 *)
and parse_while rest indent cond iline =
   let blocwhile_and_rest = getlines_with_indent (rest) (indent+2)
   in let blockwhile = parse_block(indent+2)(fst(blocwhile_and_rest))(iline+1)
   in (iline, While(
      parse_cond(cond), blockwhile
   ))::(parse_block (indent) (snd(blocwhile_and_rest)) (iline+1))


(* Parse un programme et appel parse les blocs intérieurs récursivement *)
let parse_program lines =
   parse_block 0 (List.map snd (map_index lines)) 1
