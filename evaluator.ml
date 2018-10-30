open Syntax
open Results

exception NoRuleApplies
   
let big_step expr = match expr with
    
    (* Case Vnum: checks if expr is a value. *)
    Vnum(expr) -> expr
  | _ -> raise NoRuleApplies

let main () =
  try
    big_step (Vbool(true))
  with
    NoRuleApplies -> Printf.printf "Evaluation halted: no rule applies for some subexpression.\n"

let _ = main ()
