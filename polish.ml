
(** Projet Polish -- Analyse statique d'un mini-langage impératif *)

(** Note : cet embryon de projet est pour l'instant en un seul fichier
    polish.ml. Il est recommandé d'architecturer ultérieurement votre
    projet en plusieurs fichiers source de tailles raisonnables *)

(***********************************************************************)

open Model
open Parser

let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage: à documenter (TODO)\n"

let main () =
  match Sys.argv with
  | [|_;"--reprint";file|] -> Reprint.print_program ( parse_program ( get_lines file ) )
  | [|_;"--eval";file|] -> Eval.eval ( parse_program ( get_lines file ) )
  | _ -> usage ()

(* lancement de ce main *)
let () = main ()
