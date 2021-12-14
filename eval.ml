(*Fonction pour pour l'env *)

let rec isInEnv env name = match env with
  | [] -> false
  | element::restant -> if(equal (fst (element)) name ) then true
                        else isInEnv restant name

let rec getValueEnv env name = match env with
  | [] -> -99999
  | element::restant -> if(equal (fst (element)) name ) then snd(element)  
                        else getValueEnv restant name

let rec calcul_expr env expr = 
  match expr with
  | Num (int) -> int
  | Var (name) -> if (isInEnv env name) then getValueEnv env name
                  else raise (Division_by_zero)
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


(*On demande à l'utilisateur "<nom de la variable> ?" pour lire read_int et on l'associe 
 dans le tableau (l'environment)*)
let function_read env name =
  Printf.printf "%s ?" name ;
  let variable_int = read_int() in
  (name, variable_int)::env


(*On calcul la valeur avec la fonction calcule_expr et on affiche sur la sortie standard de la sorte
    "<calcul> = <expresion_calculé> \n"*)
let function_print env expr = Printf.printf "<calcul> = %d \n" (calcul_expr env expr)



(*Entre cond :   expr_fst * comp * expr_snd et verification que tout est correct.
 Sortie:  True ou False  *)
let function_verifie_condition env (expr_fst, comp, expr_snd) =
  match comp with
  | Eq -> calcul_expr env expr_fst = calcul_expr env expr_snd
  | Ne -> calcul_expr env expr_fst <> calcul_expr env expr_snd
  | Lt -> calcul_expr env expr_fst < calcul_expr env expr_snd
  | Le -> calcul_expr env expr_fst <= calcul_expr env expr_snd
  | Gt -> calcul_expr env expr_fst > calcul_expr env expr_snd
  | Ge -> calcul_expr env expr_fst >= calcul_expr env expr_snd

let rec eval_block env block =
  match block with
  | [] -> env
  | (_, element)::list -> 
     let env' = eval_instruction env element in
     eval_block (env') list
and  eval_instruction env instruction =
  match instruction with 
  | Set (name , expr) -> (name, (calcul_expr env expr))::env
  | Read (name) -> function_read env name
  | Print (expr) -> function_print env expr; env
  | If (cond, block_if, block_else) -> 
     if function_verifie_condition env cond then eval_block env block_if 
     else eval_block env block_else 
  | While (cond , block_while) ->
     if function_verifie_condition env cond 
     then eval_instruction (eval_block env block_while) instruction 
     else env

let eval program  = eval_block [] program

;;
eval abs;;
eval factors;;