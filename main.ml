open Syntax
open Results
open Evaluator
open Functions
open Support
open TypeInfer


let rec test_functions functions =  
  match functions with
  | [] -> []
  | (name, fn) :: tl ->
     let expr = fn in
     Printf.printf "\nCurrent function: %s\n" name;
     try
       let (expected_type, equations) = collect [] expr in
       let unify_result = unify [] equations in
       let final_type = apply_subs unify_result expected_type in
       let value = big_step [] expr in
       Printf.printf "Expected type: %s\n" (type_to_string final_type);
       Printf.printf "Resulting value: %s\n" (result_to_string value);
       (expected_type, value) :: test_functions tl
     with
     | UnifyTypeNotMeet ->
        Printf.printf "Type Infer error: no solution to type equations.\n";
        test_functions tl
     | NoRuleApplies ->
        Printf.printf "Evaluation error: no rules applies.\n";
        test_functions tl
     | UndefinedVariable(var) ->
        Printf.printf "Evaluation error: undefined variable \"%s\".\n" var;
        test_functions tl
     | UndefinedVariableExpr(e) ->
        Printf.printf "Type Infer error: undefined variable in expression \"%s\"\n" (expr_to_string e);
        test_functions tl
  
   
let main () =
  let fns = [(("fib", App(Functions.l1_fib (), Ncte(12))))] in
  let fns = ("factorial", App(Functions.l1_factorial (), Ncte(6))) :: fns in
  let fns = ("sub_till_zero", App(Functions.l1_sub_till_zero (), Ncte(20))) :: fns in
  let fns = ("identity", App(Functions.l1_identity (), Bcte(true))) :: fns in
  let fns = ("rec_function", App(Functions.l1_rec (), Bcte(false))) :: fns in
  let fns = ("function", App(Functions.l1_simple_function (), Bcte(true))) :: fns in
  let fns = ("raise", Functions.l1_raise ()) :: fns in
  test_functions fns
  
let _ =
  main ()
