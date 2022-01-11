open Model

let rec is_simpl expr env =
   match expr with
   | Var(v) -> (try let _ = List.assoc v env in true with Not_found -> false)
   | Num(_) -> true
   | Op(_, e1, e2) -> is_simpl e1 env && is_simpl e2 env

let is_simpl_c cond =
   let rec aux expr = 
      match expr with
      | Var(_) -> false
      | Num(_) -> true
      | Op(_, e1, e2) -> aux e1 && aux e2
   in match cond with
   | (e1, _, e2) -> aux e1 && aux e2

let rec simpl_expr expr env =
   match expr with
   | Var(n) -> (try let value = List.assoc n env in Num(value) with Not_found -> Var(n))
   | Num(i) ->  Num(i)
   | Op(op, e1, e2) -> Op(op, simpl_expr e1 env, simpl_expr e2 env)
                       

let rec compute_expr expr =
match expr with
   | Var(n) -> 1
   | Num(i) ->  i
   | Op(op, e1, e2) -> let ve1 = compute_expr e1 in
                        let ve2 = compute_expr e2 in
                        (match op with
                        | Add -> if ve1 = 0 then ve2
                                 else if ve2 = 0 then ve1
                                 else (ve1) + (ve2)
                        | Sub -> if ve1 = 0 then - ve2
                                 else if ve2 = 0 then ve1
                                 else (ve1) - (ve2)
                        | Mul -> if ve1 = 0 || ve2 = 0 then 0
                                 else if ve1 = 1 then ve2
                                 else if ve2 = 1 then ve1
                                 else (ve1) * (ve2)
                        | Div -> if ve1 = 0 then 0
                                 else (ve1) / (ve2)
                        | Mod -> if ve1 = 0 then 0
                                 else (ve1) mod (ve2))

let simpl_cond cond env =
   match cond with
   | (e1, cmp, e2) -> (simpl_expr e1 env, cmp, simpl_expr e2 env)

let compute_cond cond =
   match cond with 
   | (e1, cmp, e2) -> let e1res = compute_expr e1 in let e2res = compute_expr e2
                      in match cmp with
                        | Eq -> e1res = e2res
                        | Ne -> e1res <> e2res
                        | Lt -> e1res < e2res
                        | Le -> e1res <= e2res
                        | Gt -> e1res > e2res
                        | Ge -> e1res >= e2res

let rec simpl_block block env =
   match block with
   | [] -> []
   | (pos, instr)::rest -> match instr with 
                         | Print(expr) -> if is_simpl expr env then (pos, Print(simpl_expr expr env))::simpl_block rest env
                                       else (pos, Print(expr))::simpl_block rest env
                         | Read(name) -> (pos, Read(name))::simpl_block rest env
                         | Set(name, expr) -> if is_simpl expr env then simpl_block (rest) ( (name, compute_expr expr )::env )
                                              else (pos, Set(name, expr))::simpl_block rest env
                         | If(cond, b1, b2) -> (pos, If (
                            simpl_cond cond env,
                            simpl_block b1 env,
                            simpl_block b2 env
                         ))::simpl_block rest env
                         | While(cond, b) -> (pos, While (
                            simpl_cond cond env,
                            simpl_block b env
                         ))::simpl_block rest env

let progpag_vars program =
   simpl_block program []

let rec delete_unused_blocks block =
   match block with
   | [] -> []
   | (pos, instr)::rest -> match instr with
                         | Print(e) -> (pos, Print(e))::delete_unused_blocks rest
                         | Read(name) -> (pos, Read(name))::delete_unused_blocks rest
                         | Set(name, expr) -> (pos, Set(name, expr))::delete_unused_blocks rest
                         | If(cond, b1, b2) -> if is_simpl_c cond then 
                                                let b = compute_cond cond
                                                in if b then delete_unused_blocks b1@delete_unused_blocks rest
                                                else delete_unused_blocks b2@delete_unused_blocks rest
                                             else (pos, If(cond, b1, b2))::delete_unused_blocks rest
                         | While(cond, b) -> if is_simpl_c cond then 
                                                let res = compute_cond cond
                                                in if res then (pos, While(cond, b))::delete_unused_blocks rest
                                                else delete_unused_blocks rest
                                             else (pos, While(cond, b))::delete_unused_blocks rest

let simpl program =
   delete_unused_blocks (progpag_vars program)



