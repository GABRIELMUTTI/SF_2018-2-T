open Syntax
open Results
     
type expType = TyBool 
			| TyNat  
			|TyImplication of expType * expType
			
type typeVar = (int * expType)

type typeEquation= TyAssign of typeVar * expType

let rec collect (typeEnv : (expr * expType) list) (expr: Syntax.expr) = (*let typeEnv = (expr * extType) list*)
	match expr with
    (* Rule SUM *)
	Binop(Sum, e1, e2)->let (t1, c1) = collect typeEnv e1 in 
        let (t2, c2) = collect typeEnv, e2 in 
		let typeEnv= (Binop(Sum, e1, e2), TyNat)::typeEnv in (*return type of expression later*)
        let C=c1 in
		let C=c2@c1 in
		let C=TyAssign(t1,TyNat)::C in
        let C=TyAssign(t2,TyNat)::C in(*let C=[TyAssign(t1,TyNat);TyAssign(t2,TyNat)] in*)
		(TyNat,C)
                

