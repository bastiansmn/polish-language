
(** Projet Polish -- Analyse statique d'un mini-langage impératif *)

(** Note : cet embryon de projet est pour l'instant en un seul fichier
    polish.ml. Il est recommandé d'architecturer ultérieurement votre
    projet en plusieurs fichiers source de tailles raisonnables *)

(***********************************************************************)

let print_polish (p:Model.program) : unit = failwith "TODO"

let eval_polish (p:Model.program) : unit = failwith "TODO"

let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage: à documenter (TODO)\n"

let main () =
  match Sys.argv with
  | [|_;"--reprint";file|] -> Parser.read_polish file
  | [|_;"--eval";file|] -> Parser.read_polish file
  | _ -> usage ()

(* lancement de ce main *)
let () = main ()
