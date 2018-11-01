open Results
open Syntax

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
 
