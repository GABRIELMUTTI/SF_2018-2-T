open Syntax
open Results
       
exception NoRuleApplies
exception UndefinedVariable

let rec get_value env var = match env with
    (id, value) :: tl ->
     if (compare id var) == 0 then
       value
     else
       get_value tl var
  | _ -> raise UndefinedVariable
    
   
        
let rec big_step (env : (variable * result) list ) expr = match expr with
    
    (* Rule BS-NUM *)
    Ncte(expr) -> Vnum (expr)
                
  (* Rule BS-BOOL *)
  | Bcte(expr) -> Vbool (expr)

  (* Rule BS-ID: replaces a variable with its corresponding value, defined in the environment.*)
  | Var(var) -> get_value env var
                      
  (* Rule BS-IFTR *)
  | If(Bcte(true), e2, e3) -> big_step env e2

  (* Rule BS-IFFLS *)                         
  | If(Bcte(false), e2, e3) -> big_step env e3

  (* If(e1, e2, e3) *)
  | If(e1, e2, e3) ->
     let Vbool(v1) = big_step env e1 in   
     let e = If(Bcte v1, e2, e3) in
     big_step env e
     
  (* BS-FN *)
  | Lam(variable, e) -> Vclos(variable, e, env)

  (* BS-LET *)
  | Let(x, e1, e2) ->
     let v1 = big_step env e1 in
     let env' = (x, v1) :: env in
     big_step env' e2
                             
  (* BS-LETREC *)
  | Lrec(f, x, e1, e2) ->
     let env' = (f, Vrclos(f, x, e1, env)) :: env in
     big_step env' e2

  (* App(e1, e2) *)
  | App(e1, e2) ->
     let v1 = big_step env e1 in
     let v2 = big_step env e2 in
     let (e, env') = match v1 with

       (* BS-APP *)
       | Vclos(x, e, env') ->
          let env'' = (x, v2) :: env' in
          (e, env'')

       (* BS-APPREC *)
       | Vrclos(f, x, e, env') ->
          let env'' = (f, Vrclos(f, x, e, env')) :: env' in
          (e, env'') in
     
     big_step env' e
         
  (* No rule applies, raise exception. *)
  | _ -> raise NoRuleApplies



(* Function to print a result value. *)
let rec result_to_string result =
  let rec env_to_string env =
    match env with
    | (id, value) :: tl ->
       let value_string = result_to_string value in
       let rest_string = env_to_string tl in
       "(" ^ id ^ ", " ^ value_string ^ ") :: " ^ rest_string in
  
  match result with
  | Vnum(num) -> string_of_int num
  | Vbool(bool) -> string_of_bool bool
  | Vpair(v1, v2) ->
     let v1_string = result_to_string v1 in
     let v2_string = result_to_string v2 in
     "(" ^ v1_string ^ ", " ^ v2_string ^ ")"
  | Vnil -> "nil"
  | Vcons(v1, v2) ->
     let v1_string = result_to_string v1 in
     let v2_string = result_to_string v2 in
     v1_string ^ " :: " ^ v2_string
  | Vclos(var, e, env) ->
     let env_string = env_to_string env in
     "(" ^ var ^ ", " ^ env_string ^ ")"
  | Vrclos(f, var, e, env) ->
     let env_string = env_to_string env in
     "(" ^ "f" ^ ", " ^ var ^ ", " ^ env_string ^ ")"
  | RRaise -> "raise"
  | _ -> "Undefined"

(* Evaluates an expression and returns a result. *)
let main () =
  try
    let expr1 = If(Bcte(true), Ncte(5), Var("lul")) in
    let expr = If(Bcte(false), expr1, Var("xdsada")) in
    big_step [("xdsada", Vbool(true)); ("lul", Vnum(432))] expr
  with
    NoRuleApplies ->
     Printf.printf "Evaluation halted: no rule applies for some subexpression.\n";
     RRaise
  | UndefinedVariable ->
     Printf.printf "Evaluation halted: undefined variable.\n";
     RRaise


(* Calls main and prints the result value. *)
let _ =
  let value = main () in
  Printf.printf "%s\n" (result_to_string value)
