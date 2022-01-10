open Parser

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

let parse_program_simplifie lines = parse_block 0 (List.map snd (map_index lines)) 1
