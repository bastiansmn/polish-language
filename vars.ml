open Model

module Names = Set.Make(String)

let rec vars_of_expr expr = 
   match expr with
   | Var(v) -> [v]
   | Op(_, e1, e2) -> (vars_of_expr e1) @ (vars_of_expr e2)
   | _ -> []

let vars_of_cond cond = 
   match cond with
   | (e1, _, e2) -> (vars_of_expr e1) @ (vars_of_expr e2)


let rec vars_of_block block =
   match block with
   | [] -> (Names.empty, Names.empty)
   | (_, instr)::rest -> let res = vars_of_instr instr in
                         let next_block = vars_of_block (rest) in
                           (
                              Names.union (fst res) (fst next_block),
                              Names.union (snd res) (snd next_block)
                           )

and vars_of_instr instr =
   match instr with
   | Print(e) -> (Names.of_list(vars_of_expr e), Names.empty)
   | Set(name, expr) -> (Names.of_list(name::(vars_of_expr expr)), Names.singleton(name))
   | Read(name) -> (Names.singleton(name), Names.singleton(name))
   | If(cond, b1, b2) -> (
         Names.union (Names.of_list (vars_of_cond cond)) (Names.union (fst(vars_of_block b1)) (fst(vars_of_block b2))),
         Names.inter (snd(vars_of_block b1)) (snd(vars_of_block b2))
      )
   | While(cond, b) -> (
         Names.union (Names.of_list (vars_of_cond cond)) (fst(vars_of_block b)),
         Names.empty
      )

let vars program =
   let res = vars_of_block(program)
   in let init = snd res
   in let all = fst res
   in let uninit = Names.diff all init
   in Printf.printf "ALL : ";
      Names.iter (Printf.printf "%s ") all;
      Printf.printf "\nUNINIT : ";
      Names.iter (Printf.printf "%s ") uninit;
      Printf.printf "\n"