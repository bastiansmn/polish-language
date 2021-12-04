
(** Projet Polish -- Analyse statique d'un mini-langage impératif *)

(** Note : cet embryon de projet est pour l'instant en un seul fichier
    polish.ml. Il est recommandé d'architecturer ultérieurement votre
    projet en plusieurs fichiers source de tailles raisonnables *)

(*****************************************************************************)
(** Syntaxe abstraite Polish (types imposés, ne pas changer sauf extensions) *)

(** Position : numéro de ligne dans le fichier, débutant à 1 *)
type position = int

(** Nom de variable *)
type name = string

(** Opérateurs arithmétiques : + - * / % *)
type op = Add | Sub | Mul | Div | Mod

(** Expressions arithmétiques *)
type expr =
  | Num of int
  | Var of name
  | Op of op * expr * expr

(** Opérateurs de comparaisons *)
type comp =
| Eq (* = *)
| Ne (* Not equal, <> *)
| Lt (* Less than, < *)
| Le (* Less or equal, <= *)
| Gt (* Greater than, > *)
| Ge (* Greater or equal, >= *)

(** Condition : comparaison entre deux expressions *)
type cond = expr * comp * expr

(** Instructions *)
type instr =
  | Set of name * expr
  | Read of name
  | Print of expr
  | If of cond * block * block
  | While of cond * block
and block = (position * instr) list

(** Un programme Polish est un bloc d'instructions *)
type program = block


(***********************************************************************)

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

let parse_cond	line = 
	failwith "TODO"
	(* Lit la ligne (line), et parse_expr * parse_comp * parse_expr 
	Il faut avancer dans les lettre de la ligne jsq voir un caractère de condition, sinon on est dans une expression (parse_expr).
	*)

let parse_expr words =
	(* words est l'ensembre des mots lu qui sont censés créer l'expression *)

let parse_num word =
	(* Lit word et décide s'il est un num ou non, le renvoie si oui *)

let parse_var word =
	(* Créé si la var n'existe pas, rééassigne sa valeur sinon (liste de variables) *)

let parse_op words =
	(* Renvoie un op (Add ou Sub, ... avec les valeurs qu'il opère) *)

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

let print_polish (p:program) : unit = failwith "TODO"

let eval_polish (p:program) : unit = failwith "TODO"

let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage: à documenter (TODO)\n"

let main () =
  match Sys.argv with
  | [|_;"--reprint";file|] -> read_polish file
  | [|_;"--eval";file|] -> read_polish file
  | _ -> usage ()

(* lancement de ce main *)
let () = main ()
