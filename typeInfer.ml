open Syntax
open Results
       

let rec typeInfer env expr = match expr with
    
    (* Rule SUM *)
	(e1 Sum e2)->let (t1, c1) = collect(env, e1) in 
        let (t2, c2) = collect(env, e2) in 
        env.add(e1 Sum e2, TyInt);
        C=c1;
		C.append(c2);
        C.add(t1,TyInt);
        C.add(t2,TyInt);
                
  (* Rule BS-BOOL *)
  | Vbool(expr) -> expr
