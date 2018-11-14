(* Function to print a result value. *)
let rec result_to_string result =
  let rec env_to_string env =
    match env with
    | (id, value) :: tl ->
       let value_string = result_to_string value in
       let rest_string = env_to_string tl in
       "(" ^ id ^ ", " ^ value_string ^ ") :: " ^ rest_string
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
     "(" ^ var ^ ", " ^ env_string ^ ")"
  | Vrclos(f, var, e, env) ->
     let env_string = env_to_string env in
     "(" ^ "f" ^ ", " ^ var ^ ", " ^ env_string ^ ")"
  | RRaise -> "raise"
  | _ -> "Undefined"

let rec get_type_str expType =
  match expType with
  | TyBool -> "bool"
  | TyNat -> "nat"
  | TyImplication(t1, t2) -> "(" ^ (get_type_str t1) ^ " -> " ^ (get_type_str t2) ^ ")"
  | TyTuple(t1, t2) -> "(" ^ (get_type_str t1) ^ " * " ^ (get_type_str t2) ^ ")"
  | TyVariable(value) -> "T" ^ (string_of_int value)
  | TyList(t) -> (get_type_str t) ^ " list"
  | _ -> "error!"

let rec print_type_equations equations =
  match equations with
  | [] -> ()
  | TyAssign(t1, t2) :: tl ->
     Printf.printf "%s = %s\n" (get_type_str t1) (get_type_str t2);
     print_type_equations tl
  | _ -> ()

