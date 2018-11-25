open Results
open Syntax
open TypeInfer
   
(* Function to print a result value. *)
let rec result_to_string result =

  (* Gets a string of the environment. *)
  let rec env_to_string env =
    let str = "(" in
    match env with
    | (id, value) :: [] ->
       let value_str = result_to_string value in
       str ^ id ^ ", " ^ value_str ^ ")"
       
    | (id, value) :: tl ->
       let value_str = result_to_string value in
       let rest_str = env_to_string tl in
       str ^ id ^ ", " ^ value_str ^ "::" ^ rest_str
    | _ -> "" in

  
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
     if env == [] then
       "<fn " ^ var ^ ">"
     else
     "<fn " ^ var ^ ", " ^ env_string ^ "() >"
  | Vrclos(f, var, e, env) ->
     let env_string = env_to_string env in
     if env == [] then
       "<" ^ "rec fn " ^ f ^ ", " ^ var ^ ", ()>"
     else
       "<" ^ "rec fn "^ f ^ ", " ^ var ^ ", " ^ env_string ^ ">"
  | RRaise -> "raise"
 
let rec type_to_string aType =
  match aType with
  | TyBool -> "bool"
  | TyNat -> "nat"
  | TyImplication(e1, e2) ->
     (type_to_string e1) ^ " -> " ^ (type_to_string e2)
  | TyTuple(e1, e2) ->
     "(" ^ (type_to_string e1) ^ ", " ^ (type_to_string e2) ^ ")"
  | TyVariable(var) -> "T" ^ (string_of_int var)
  | TyList(e) -> (type_to_string e) ^ " list"


let rec expr_to_string expr =
  match expr with
  | Ncte(n) -> string_of_int n
  | Bcte(b) -> string_of_bool b
  | Binop(op, e1, e2) -> "Binop(" ^ (expr_to_string e1) ^ ", " ^ (expr_to_string e2) ^ ")"
  | Unop(op, e) -> "Unop(" ^ (expr_to_string e) ^ ")"
  | Pair(e1, e2) -> "Pair(" ^ (expr_to_string e1) ^ ", " ^ (expr_to_string e2) ^ ")"
  | If(e1, e2, e3) -> "If("  ^ (expr_to_string e1) ^ ", " ^ (expr_to_string e2) ^ (expr_to_string e2) ^ ")"
  | Var(var) -> "Var(" ^ var ^ ")"
  | App(e1, e2) -> "App(" ^ (expr_to_string e1) ^ ", " ^ (expr_to_string e2) ^ ")"
  | Lam(var, e) -> "Lam(" ^ var ^ ", " ^ (expr_to_string e) ^ ")"
  | Let(var, e1, e2) -> "Let(" ^ var ^ ", " ^ (expr_to_string e1) ^ ", " ^ (expr_to_string e2) ^")"
  | Lrec(f, var, e1, e2) -> "Lrec(" ^ var ^ ", " ^ var ^ ", " ^ (expr_to_string e1) ^ ", " ^ (expr_to_string e1) ^")"
  | Nil -> "Nil"
  | Cons(e1, e2) -> "Cons(" ^ (expr_to_string e1) ^ ", " ^ (expr_to_string e2) ^ ")"
  | IsEmpty(e) -> "IsEmpty(" ^ (expr_to_string e) ^ ")"
  | Hd(e) -> "Hd(" ^ (expr_to_string e) ^ ")"
  | Tl(e) -> "Tl(" ^ (expr_to_string e) ^ ")"
  | Raise -> "Raise"
  | Try(e1, e2) -> "Try(" ^ (expr_to_string e1) ^ ", " ^ (expr_to_string e2) ^ ")"


  
