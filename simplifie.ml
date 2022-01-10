open Parser

| 
let rec calcul_expr env expr = 
  match expr with
  | Num (int) -> int
  | Var (name) -> if (isInEnv env name) then getValueEnv env name
                  else raise (Division_by_zero) (* TODO : Pas la bonne exception*)
  | Op (op, expr_fst, expr_snd) ->
     let expr_gauche = calcul_expr env expr_fst in
     let expr_droite = calcul_expr env expr_snd in
     match op with
     | Add -> (expr_gauche + expr_droite)
     | Sub -> (expr_gauche - expr_droite)
     | Mul -> (expr_gauche *  expr_droite)
     | Div -> if expr_droite <> 0 then (expr_gauche / expr_droite)
              else raise (Division_by_zero)
     | Mod -> if expr_droite <> 0 then (expr_gauche mod expr_droite)
              else raise (Division_by_zero)

let function_verifie_condition (expr_fst, comp, expr_snd) =
  let expr_gauche = calcul_expr expr_fst in
  let expr_droite = calcul_expr expr_snd in
  match comp with
  | Eq -> expr_gauche = expr_droite
  | Ne -> expr_gauche <> expr_droite
  | Lt -> expr_gauche < expr_droite
  | Le -> expr_gauche <= expr_droite
  | Gt -> expr_gauche > expr_droite
  | Ge -> expr_gauche >= expr_droite

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
   in 
   if((iline, While(parse_cond(cond), blockwhile))::(parse_block (indent) (snd(blocwhile_and_rest)) (iline+1))


(* Parse un programme et appel parse les blocs intérieurs récursivement *)

let parse_program_simplifie lines = parse_block 0 (List.map snd (map_index lines)) 1
