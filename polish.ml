
(** Projet Polish -- Analyse statique d'un mini-langage impératif *)

(** Note : cet embryon de projet est pour l'instant en un seul fichier
    polish.ml. Il est recommandé d'architecturer ultérieurement votre
    projet en plusieurs fichiers source de tailles raisonnables *)

(***********************************************************************)

open Model
open Parser
open Simplifie

let usage () =
  print_string "\nPolish : analyse statique d'un mini-langage\n";
  print_string "usage: \n";
  print_string "\t./run --reprint path/to/file.p\n";
  print_string "\t\tAffiche le fichier Polish tel qu'il est vu pas eval.ml\n\n";
  print_string "\t./run --eval path/to/file.p\n";
  print_string "\t\tExecute le programme Polish\n\n"

let main () =
  match Sys.argv with
  | [|_;"--reprint";file|] -> Reprint.print_program ( parse_program ( get_lines file ) )
  | [|_;"--simpl";file|] -> Reprint.print_program ( simpl ( parse_program ( get_lines file ) ) )
  | [|_;"--eval";file|] -> Eval.eval ( parse_program ( get_lines file ) )
  | _ -> usage ()

(* lancement de ce main *)
let () = main ()
