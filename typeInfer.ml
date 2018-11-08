open Syntax
open Results
  
exception UndefinedVariableExpr of Syntax.expr
exception UnifyTypeNotMeet


type expType = TyBool 
			| TyNat  
			|TyImplication of expType * expType
			|TyTuple of expType * expType
			| TyVariable of int
			|TyList of expType
			

type typeEquation= TyAssign of expType * expType

let varID= ref 0;;(*let varID=varID+1 in*)

let rec findTypeInEnv (typeEnv : (expr * expType) list) (expr: Syntax.expr) =
	match typeEnv with
	(e1, t1) :: tl ->
		if e1 = expr then
			t1
		else
			findTypeInEnv tl expr
	| _ -> raise (UndefinedVariableExpr expr)

let rec collect (typeEnv : (expr * expType) list) (expr: Syntax.expr) = (*let typeEnv = (expr * extType) list*)
	match expr with
	|Ncte(n)->(TyNat,[])
	|Bcte(b)->(TyBool,[])
	
	|Binop((Sum|Sub|Mult|Div), e1, e2)->
		let (t1, c1) = collect typeEnv e1 in 
        let (t2, c2) = collect typeEnv e2 in 
		let typeEquations=[TyAssign(t1, TyNat);TyAssign(t2,TyNat)] in
		let typeEquations= typeEquations@c1 in
		let typeEquations= typeEquations@c2 in
		(TyNat,typeEquations)
		
	(*let typeEnv= (expr,TyBool)::typeEnv in*)
	|Binop((And|Or), e1, e2)->
		let (t1, c1) = collect typeEnv e1 in 
        let (t2, c2) = collect typeEnv e2 in 
		let typeEquations=[TyAssign(t1, TyBool);TyAssign(t2,TyBool)] in
		let typeEquations= typeEquations@c1 in
		let typeEquations= typeEquations@c2 in
		(TyBool,typeEquations)
		
	|Binop((Eq|Df|Lt|Le|Gt|Ge), e1, e2)->
		let (t1, c1) = collect typeEnv e1 in 
        let (t2, c2) = collect typeEnv e2 in 
		let typeEquations=[TyAssign(t1, TyNat);TyAssign(t2,TyNat)] in
		let typeEquations= typeEquations@c1 in
		let typeEquations= typeEquations@c2 in
		(TyBool,typeEquations)
		
	|Unop(Not,e1)->let (t1, c1) = collect typeEnv e1 in 
		let typeEquations=[TyAssign(t1, TyBool)] in
		let typeEquations= typeEquations@c1 in
		(TyBool,typeEquations)
		
	|Pair(e1,e2)->let (t1, c1) = collect typeEnv e1 in 
        let (t2, c2) = collect typeEnv e2 in 
		let typeEquations= c1 in
		let typeEquations= typeEquations@c2 in
		(TyTuple(t1,t2),typeEquations)
		
	|If(e1,e2,e3)->let (t1, c1) = collect typeEnv e1 in 
        let (t2, c2) = collect typeEnv e2 in 
		let (t3, c3) = collect typeEnv e3 in 
		let typeEquations=[TyAssign(t1, TyBool);TyAssign(t1, t2)] in
		let typeEquations= typeEquations@c1 in
		let typeEquations= typeEquations@c2 in
		let typeEquations= typeEquations@c3 in
		(t2,typeEquations)
		
	|Var(x)-> let t1 = findTypeInEnv typeEnv (Var(x)) in
		(t1,[])
		
	|App(e1, e2)->let xTypeVar=TyVariable(!varID)in
		varID:= !varID+1;
		let typeEnv= (App(e1, e2), xTypeVar)::typeEnv in
		let (t1, c1) = collect typeEnv e1 in 
        let (t2, c2) = collect typeEnv e2 in 
		let typeEquations=[TyAssign(t1, TyImplication(t2,xTypeVar))] in
		let typeEquations= typeEquations@c1 in
		let typeEquations= typeEquations@c2 in
		(xTypeVar,typeEquations)
		
	|Lam(x, e)->let xTypeVar=TyVariable(!varID)in
		varID:= !varID+1;
		let typeEnv= (Var(x), xTypeVar)::typeEnv in
		let (t1, c1) = collect typeEnv e in 
		(TyImplication(xTypeVar,t1),c1)
		
	|Let(x,e1,e2)->let xTypeVar=TyVariable(!varID)in
		varID:= !varID+1;
		let (t1, c1) = collect typeEnv e1 in 
		let typeEnv = (Var(x), xTypeVar)::typeEnv in
        let (t2, c2) = collect typeEnv e2 in 
		let typeEquations=[TyAssign(xTypeVar, t1)] in
		let typeEquations= typeEquations@c1 in
		let typeEquations= typeEquations@c2 in
		(t2,typeEquations)
	
    |Lrec(f,y,e1,e2)->let fTypeVar=TyVariable(!varID) in
		varID:= !varID+1;
		let yTypeVar=TyVariable(!varID) in
		varID:= !varID+1;
		let typeEnv = (Var(f), fTypeVar)::typeEnv in
        let (t2, c2) = collect typeEnv e2 in 
		let typeEnv = (Var(y), yTypeVar)::typeEnv in
        let (t1, c1) = collect typeEnv e1 in 
		let typeEquations=[TyAssign(fTypeVar, TyImplication(yTypeVar,t1))] in
		let typeEquations= typeEquations@c1 in
		let typeEquations= typeEquations@c2 in
		(t2,typeEquations)
		
	| Nil->let xTypeVar=TyVariable(!varID)in
		varID:= !varID+1;
		(TyList(xTypeVar),[])
		
    | Cons(e1,e2)->let (t1, c1) = collect typeEnv e1 in 
        let (t2, c2) = collect typeEnv e2 in 
		let typeEquations=[TyAssign(TyList(t1),t2)] in
		let typeEquations= typeEquations@c1 in
		let typeEquations= typeEquations@c2 in
		(t2,typeEquations)
		
    | IsEmpty(e1)-> let xTypeVar=TyVariable(!varID)in
		varID:= !varID+1;
		let (t1, c1) = collect typeEnv e1 in 
		let typeEquations=[TyAssign(t1,TyList(xTypeVar))] in
		let typeEquations= typeEquations@c1 in
		(TyBool,typeEquations)
		
    | Hd(e1)->let xTypeVar=TyVariable(!varID)in
		varID:= !varID+1;
		let (t1, c1) = collect typeEnv e1 in 
		let typeEquations=[TyAssign(t1,TyList(xTypeVar))] in
		let typeEquations= typeEquations@c1 in
		(xTypeVar,typeEquations)
		
    | Tl(e1)->let xTypeVar=TyVariable(!varID)in
		varID:= !varID+1;
		let (t1, c1) = collect typeEnv e1 in 
		let typeEquations=[TyAssign(t1,TyList(xTypeVar))] in
		let typeEquations= typeEquations@c1 in
		(TyList(xTypeVar),typeEquations)
		
    | Raise->let xTypeVar=TyVariable(!varID)in
		varID:= !varID+1;
		(xTypeVar,[])
		
    | Try(e1,e2)->let (t1, c1) = collect typeEnv e1 in 
        let (t2, c2) = collect typeEnv e2 in 
		let typeEquations=[TyAssign(t1,t2)] in
		let typeEquations= typeEquations@c1 in
		let typeEquations= typeEquations@c2 in
		(t2,typeEquations)




let rec VerifyInside (xTypeVar:int) (t: expType) =
  match t with
  |TyNat -> false
  |TyBool -> false
  |TyList(t1) -> VerifyInside xTypeVar t1 
  |TyImplication(t1,t2) -> 
      VerifyInside xTypeVar t1 || VerifyInside xTypeVar t2
  |TyTuple(t1,t2) -> VerifyInside xTypeVar t1 || VerifyInside xTypeVar t2
  |TyVariable(yTypeVar) -> xTypeVar = yTypeVar
  |_ -> raise UnifyTypeNotMeet



let rec change (varID: int) (t: expType) (element: expType) = 
  match element with
  |TyNat -> TyNat
  |TyBool -> TyBool
  |TyList(t1) -> TyList(change varID t t1)
  |TyImplication(t1, t2) -> 
      TyImplication((change varID t t1), (change varID t t2))
  |TyTuple(t1, t2) -> 
      TyTuple((change varID t t1), (change varID t t2))
  |TyVariable(yVarID) -> if varID = yVarID then t else TyVariable(yVarID)
  |_ -> raise UnifyTypeNotMeet


let ChangeOccurrences (varID: int) (t:expType) (clist:(expType*expType)list) =
  List.map (fun (element1, element2) -> 
    ((change varID t element1),(change varID t element2))) clist



let rec Unify sigma  (ctypeEquations: (expType * expType) list) =
  match ctypeEquations with
  |[] -> sigma
  |(TyNat, TyNat)::tl -> Unify sigma tl
  |(TyBool, TyBool)::tl -> Unify sigma tl
  |(TyList(t1), TyList(t2))::tl -> Unify sigma (t1,t2)::tl
  |(TyImplication(t1, t2), TyImplication(t3, t4))::tl -> 
                                   Unify sigma (t1,t3)::(t2,t4)::tl
  |(TyTuple(t1,t2), TyTuple(t3,t4))::tl ->  
                                   Unify sigma (t1,t3)::(t2,t4)::tl
  |(TyVariable(xTypeVar), t)::tl -> 
      if VerifyInside xTypeVar t 
        then raise UnifyTypeNotMeet
        else Unify sigma@[(TyVariable(xTypeVar),t)] ChangeOccurrences xTypeVar t tl
  |(t, TyVariable(xTypeVar))::tl ->
      if VerifyInside xTypeVar t 
        then raise UnifyTypeNotMeet
        else Unify sigma@[(TyVariable(xTypeVar),t)] ChangeOccurrences xTypeVar t tl
  |(_,_) -> raise UnifyTypeNotMeet

