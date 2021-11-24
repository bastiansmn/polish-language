
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

(* TODO :
	0 - Supprimer les espaces inutiles (sauf tabulations début de lignes)
	1 - Créer des blocs si nombre pair d'espaces en début de ligne
	2 - Clean les fonctions inutiles
 *)

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

let read_polish (filename:string) = 
	let get_lines file =
		let ic = open_in filename 
		in let try_read () =
			try
				Some(input_line ic)
			with End_of_file -> None
		in let rec aux acc =
			match try_read () with
			| None -> acc
			| Some(line) -> aux(acc @ [String.split_on_char(' ')(line)])
		in aux []
	in print_file (get_lines filename)

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
