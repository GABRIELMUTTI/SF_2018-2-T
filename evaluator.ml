open Syntax
open Results

let big_step env expr = match expr with
    (* Case Value: checks if expr is a value. *)
    Vnum(expr) -> expr
  | _ -> 0

let main () =
  let e = Vnum(1) in
  big_step [] e


let _ = Printf.printf "%d" main ()
