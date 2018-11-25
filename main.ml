open Syntax
open Results
open Evaluator
open Functions
open Support
open TypeInfer

let main () =
  let f = Functions.l1_fib () in
  let expr = App(f, Ncte(15)) in
  try
    let (expected_type, equations) = collect [] expr in
    let unify_result = unify [] equations in
    let final_type = apply_subs unify_result expected_type in
    let value = big_step [] expr in
    Printf.printf "Expected type: %s\n" (type_to_string final_type);
    Printf.printf "Resulting value: %s\n" (result_to_string value)
  with
  | UnifyTypeNotMeet -> Printf.printf "Type Infer error: no solution to type equations.\n"
  | NoRuleApplies -> Printf.printf "Evaluation error: no rules applies.\n"
  | UndefinedVariable(var) -> Printf.printf "Evaluation error: undefined variable \"%s\".\n" var
  | UndefinedVariableExpr(e) -> Printf.printf "Type Infer error: undefined variable in expression \"%s\"\n" (expr_to_string e)
       
let _ =
  main ()
