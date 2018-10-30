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
                              
  (* No rule applies, raise exception. *)
  | _ -> raise NoRuleApplies

       
let main () =
  try
    big_step (Vbool(true))
  with
    NoRuleApplies ->
     Printf.printf "Evaluation halted: no rule applies for some subexpression.\n";
     0
  | UndefinedVariable ->
     Printf.printf "Evaluation halted: undefined variable.";
     0
     
let _ = main ()
