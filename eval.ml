
(* Tableau de (string*int) *)

let eval environnement program  = eval_block environnement program

let eval_block environnement block = 


let eval_instruction environnement instruction =
    match instruction with 
    | Set (name , expr) -> function_set environnement name expr
    | Read (name) -> function_read environnement name
    | Print (expr) -> function_print environnement expr
    | If (cond, block, block_else) -> 
        if verifie_condition cond then eval_block block 
        else eval_block block_else 
    | While (cond , block) ->
        if verifie_condition cond then eval_block block 
        else (*arreter le programme *)

let rec calcul_expr environnement expr = 
    match expr with
    | Num (int) -> int
    | Var (name) -> if (isInEnv environnement name) then getValueEnv environnement name
                    else raise (Invalid)
    | Op (op, expr_fst, expr_snd) -> let expr_gauche = calcul_expr expr_fst in
                                     let expr_droit = calcul_expr expr_snd in
                                          match op with
                                         | Add -> (expr_gauche + expr_droite)
                                         | Sub -> (expr_gauche - expr_droite)
                                         | Mul -> (expr_gauche *  expr_droite)
                                         | Div -> if expr_droite != 0 then (expr_gauche / expr_droite)
                                                  else raise (Division_by_zero)
                                         | Mod -> if expr_droite != 0 then (expr_gauche % expr_droite)
                                                  else raise (Division_by_zero)

 (*On attribue le calcul de expr et cette valeur est associé à name*)
let function_set environnement name expr = environnement::(name, (calcul_expr expr))


 (*On demande à l'utilisateur "<nom de la variable> ?" pour lire read_int et on l'associe 
 dans le tableau (l'environment)*)
let function_read environnement name = printf "%s ?" name ; let variable_int = read_int() in environnement::(name, variable_int)


(*On calcul la valeur avec la fonction calcule_expr et on affiche sur la sortie standard de la sorte
    "<calcul> = <expresion_calculé> \n"*)
let function_print expr = printf "<calcul> = %d \n" (calcule_expr expr)



(*Entre cond :   expr_fst * comp * expr_snd et verification que tout est correct.
 Sortie:  True ou False  *)
let verifie_condition cond =
     match cond with
     |_ (expr_fst, comp, expr_snd) -> match comp with
                                        | Eq -> calcul_expr expr_fst = calcul_expr expr_snd
                                        | Ne -> calcul_expr expr_fst <> calcul_expr expr_snd
                                        | Lt -> calcul_expr expr_fst < calcul_expr expr_snd
                                        | Le -> calcul_expr expr_fst <= calcul_expr expr_snd
                                        | Gt -> calcul_expr expr_fst > calcul_expr expr_snd
                                        | Ge -> calcul_expr expr_fst >= calcul_expr expr_snd

(*Fonction pour pour l'environnement *)

let rec isInEnv environnement name = match environnement with
                                | [] -> false
                                | element::restant -> if(equal (fst (element)) name ) then snd(element) 
                                                     else isInEnv l name

let rec getValueEnv environnement name = match environnement with
                                | [] -> -99999
                                | element::restant -> if(equal (fst (element)) name ) then true 
                                                     else getValueEnv l name