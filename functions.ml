open Syntax

(* Calculates nth fibonacci number using a naive algorithm. *)
let l1_fib () =
  Lrec("fib",
       "n",
       If(Binop(Eq,
                Var("n"),
                Ncte(0)),
          Ncte(0),
          If(Binop(Eq,
                   Var("n"),
                   Ncte(1)),
             Ncte(1),
             Binop(Sum,
                   App(Var("fib"),
                       Binop(Sub,
                             Var("n"),
                             Ncte(1))),
                   App(Var("fib"),
                       Binop(Sub,
                             Var("n"),
                             Ncte(2)))))),
       Var("fib"))

(* Decrements a number until it reaches zero. *)
let l1_sub_till_zero () =
  Lam("n",
      Lrec("lul",
           "x",
           If(Binop(Le,
                    Var("x"),
                    Ncte(0)),
              Var("x"),
              App(Var("lul"),
                  Binop(Sub,
                        Var("x"),
                        Ncte(1)))),
           App(Var("lul"),
               Var("n"))))

(* Returns the argument. *)
let l1_identity () =
  Lam("x",
      Var("x"))

(* Calculates the factorial of a number. *)
let l1_factorial () =
  Lrec("fact",
       "n",
       If(Binop(Eq,
                Var("n"),
                Ncte(0)),
          Ncte(1),
          If(Binop(Eq,
                   Var("n"),
                   Ncte(1)),
             Ncte(1),
             Binop(Mult,
                   Var("n"),
                   App(Var("fact"),
                       Binop(Sub,
                             Var("n"),
                             Ncte(1)))))),
         Var("fact"))

let l1_rec () =
  Lrec("test",
       "x",
       If(Var("x"),
          Bcte(true),
          Bcte(false)),
       Var("test"))
  
let l1_simple_function () =
  Lam("x",
      If(Var("x"),
         Ncte(0),
         Ncte(1)))

let l1_raise () =
  App(l1_simple_function (), Raise)
