open Syntax
open Results
open Support
open Functions
      
exception NoRuleApplies
exception UndefinedVariable of string
exception DivisionByZero
exception EmptyList
exception WrongType

(* Searches the environment for a variable, returning its value if it is defined. *)
let rec get_value env var = match env with
    (id, value) :: tl ->
     if (compare id var) == 0 then
       value
     else
       get_value tl var
  | _ -> raise (UndefinedVariable var)


(* Executes a generic binary operation. *)
let generic_binop op v1 v2 =
  match op with
  | Eq -> Vbool(v1 == v2)
  | Df -> Vbool(v1 != v2)
  | _ -> raise NoRuleApplies

(* Executes a numerical binary operation. *)
let numerical_binop op n1 n2 =
  match op with
  | Sum -> Vnum(n1 + n2)
  | Sub -> Vnum(n1 - n2)
  | Mult -> Vnum(n1 * n2)
  | Div ->
     if n2 != 0 then
       Vnum(n1 / n2)
     else
       RRaise
  | Gt -> Vbool(n1 > n2)
  | Ge -> Vbool(n1 >= n2)
  | Lt -> Vbool(n1 < n2)
  | Le -> Vbool(n1 <= n2)
  | _ -> generic_binop op n1 n2

(* Executes a boolean binary operation. *)
let boolean_binop op b1 b2 =
  match op with
  | And -> Vbool(b1 && b2)
  | Or -> Vbool(b2 || b2)
  | _ -> generic_binop op b1 b2



(* Big-Step function. *)
let rec big_step (env : (variable * result) list ) expr = match expr with
    
    (* Rule BS-NUM *)
    Ncte(expr) -> Vnum (expr)
                
  (* Rule BS-BOOL *)
  | Bcte(expr) -> Vbool (expr)

  (* Rule BS-ID: replaces a variable with its corresponding value, defined in the environment.*)
  | Var(var) -> get_value env var

  | Raise -> RRaise
              
  (* Rule BS-IFTR *)
  | If(Bcte(true), e2, e3) -> big_step env e2

  (* Rule BS-IFFLS *)                         
  | If(Bcte(false), e2, e3) -> big_step env e3

  (* If(e1, e2, e3) *)
  | If(e1, e2, e3) ->
     let v1 = big_step env e1 in
     (match v1 with
     | Vbool(b1) ->
        let e = If(Bcte(b1), e2, e3) in
        big_step env e
     | RRaise -> RRaise
     | _ -> raise WrongType)
     
  (* BS-FN *)
  | Lam(x, e) -> Vclos(x, e, env)

  (* BS-LET *)
  | Let(x, e1, e2) ->
     let v1 = big_step env e1 in
     (match v1 with
      | RRaise -> RRaise
      | _ ->
         let env' = (x, v1) :: env in
         big_step env' e2)
     
                             
  (* BS-LETREC *)
  | Lrec(f, x, e1, e2) ->
     let env' = (f, Vrclos(f, x, e1, env)) :: env in
     big_step env' e2

  (* App(e1, e2) *)
  | App(e1, e2) ->
     let v1 = big_step env e1 in
     let v2 = big_step env e2 in
     (match v1, v2 with
      | RRaise, _ -> RRaise
      | _, RRaise -> RRaise
      | _ ->
         let (e, env') =
           (match v1 with
              
            (* BS-APP *)
            | Vclos(x, e, env'') ->
               (e, (x, v2) :: env'')
              
            (* BS-APPREC *)
            | Vrclos(f, x, e, env'') ->
               (e, (x, v2) :: (f, Vrclos(f, x, e, env'')) :: env'')
            | _ -> raise NoRuleApplies) in
    
       big_step env' e)

  | Binop(op, e1, e2) ->
     let v1 = big_step env e1 in
     let v2 = big_step env e2 in
     (match v1, v2 with
     | Vnum(n1), Vnum(n2) -> numerical_binop op n1 n2
     | Vbool(b1), Vbool(b2) -> boolean_binop op b1 b2
     | RRaise, _ -> RRaise
     | _, RRaise -> RRaise
     | _ -> raise NoRuleApplies)
     
  (* Cons(e1, e2) *)
  | Cons(e1, e2) ->
     let v1 = big_step env e1 in
     let v2 = big_step env e2 in
     Vcons(v1, v2)
     
  (* Hd(e) *)
  | Hd(e) ->
     let v = big_step env e in
     (match v with
     | Vpair(v1, v2) -> v1
     | Vcons(v1, v2) -> v1
     | _ -> RRaise)
     
  (* Tl(e) *)
  | Tl(e) ->
     let v = big_step env e in
     (match v with
     | Vpair(v1, v2) -> v2
     | Vcons(v1, v2) -> v2
     | _ -> RRaise)
     
  (* IsEmpty(e) *)
  | IsEmpty(e) ->
     let list = big_step env e in
     Vbool(list == Vnil)

  | Nil -> Vnil
     
  (* Pair(e1, e2) *)
  | Pair(e1, e2) ->
     let v1 = big_step env e1 in
     let v2 = big_step env e2 in
     Vpair(v1, v2)

  (* Try(e1, e2) *)
  | Try(e1, e2) ->
     let v1 = big_step env e1 in

     (* BS-TRYRS *)
     if v1 == RRaise then
       big_step env e2

     (* BS-TRY *)
     else
       v1

  (* No rule applies, raise exception. *)
  | _ -> raise NoRuleApplies




    
(* Evaluates an expression and returns a result. *)
let main () =
  try
    let expr = l1_factorial () in
    big_step [] (App(expr, Ncte(10)))
    
  with
    NoRuleApplies ->
     Printf.printf "Evaluation halted: no rule applies for some subexpression.\n";
     RRaise
  | UndefinedVariable(var) ->
     let str = "Evaluation halted: undefined variable \"" ^ var ^ "\".\n" in
     Printf.printf "%s" str;
     RRaise
     
     
(* Calls main and prints the result value. *)
let _ =
  let value = main () in
  Printf.printf "%s\n" (result_to_string value)
    
