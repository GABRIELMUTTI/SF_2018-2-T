open Syntax
open Results
     
type expType = TyBool 
			| TyNat  
			|TyImplication of expType * expType
			

type typeEquation= TyAssign of expType * expType

let rec collect (typeEnv : (expr * expType) list) (expr: Syntax.expr) = (*let typeEnv = (expr * extType) list*)
	match expr with
    (* Rule SUM *)
	Binop(Sum, e1, e2)->let typeEnv= (Binop(Sum, e1, e2), TyNat)::typeEnv in
		let (t1, c1) = collect typeEnv e1 in 
        let (t2, c2) = collect typeEnv e2 in 
		let typeEquations=[TyAssign(t1, TyNat);TyAssign(t2,TyNat)] in
		let typeEquations= typeEquations@c1 in
		let typeEquations= typeEquations@c2 in
		(TyNat,typeEquations);;
		
		
                

