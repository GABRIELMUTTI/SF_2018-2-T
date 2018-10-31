open Syntax
open Results
       
exception NoRuleApplies
exception UndefinedVariable

let rec get_value env var = match env with
    (var, result) :: tl -> result
  | (_, result) :: tl -> get_value env tl
  | _ -> raise UndefinedVariable
  
        
let rec big_step env expr = match expr with
    
    (* Rule BS-NUM *)
    Vnum(expr) -> expr
                
  (* Rule BS-BOOL *)
  | Vbool(expr) -> expr

  (* Rule BS-ID: replaces a variable with its corresponding value, defined in the environment.*)
  | variable -> get_value env expr

  (* Rule BS-IFTR *)
  | If(Vbool(true), e2, e3) -> big_step env e2

  (* Rule BS-IFFLS *)                         
  | If(Vbool(false), e2, e3) -> big_step env e3

  (* If(e1, e2, e3) *)
  | If(e1, e2, e3) -> big_step env If(big_step env e1, e2, e3)

  (* BS-FN *)
  | Lam(variable, e) -> Vclos(variable, e, env)

  (* BS-LET *)
  | Let(x, e1, e2) ->
     let v1 = big_step env e1 in
     let env' = (x, v1) :: env in
     big_step env' e2
                             
  (* BS-LETREC *)
  | Lrec(f, x, e1, e2) ->
     let env' = (f, x, e1, env) :: env in
     big_step env' e2
              
  (* BS-APP *)
  | App(Vclos(x, e, env'), e2) ->
     let v2 = big_step env e2 in
     let env'' = (x, v2) :: env' in
     big_step env'' e

  (* BS-APPREC *)
  | App(Vrclos(f, x, e, env'), e2) ->
     let v2 = big_step env e2 in
     let env'' = (x, v2) :: (f, Vrclos(f, x, e, env')) :: env' in
     big_step env'' e
              
  (* No rule applies, raise exception. *)
  | _ -> raise NoRuleApplies

       
let main () =
  try
    big_step (Vbool(true))
  with
    NoRuleApplies ->
     Printf.printf "Evaluation halted: no rule applies for some subexpression.\n";
     RRaise()
  | UndefinedVariable ->
     Printf.printf "Evaluation halted: undefined variable.";
     RRaise()
     
let _ = main ()
