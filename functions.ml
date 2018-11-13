open Syntax

(* Calculates nth fibonacci number using a naive algorithm. *)
let l1_naive_fib () =
  Lrec("fib",
       "n",
       If(Binop(Eq,
                Var("n"),
                Ncte(0)),
          Ncte(1),
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

let l1_fast_fib () =
  Lam("n",
      Let("val",
          Ncte(1),
          Let("prev",
              Ncte(0),
              Lrec("fib",
                   "n",
                   If(Binop(Eq,
                            Var("n"),
                            Ncte(0)),
                      Binop(Sum,
                            Var("val"),
                            Var("prev")),
                      Let("tmp",
                          Var("val"),
                          Let("val",
                              Binop(Sum,
                                    Var("val"),
                                    Var("prev")),
                              Let("prev",
                                  Var("val"),
                                  App(Var("fib"),
                                      Binop(Sub,
                                            Var("n"),
                                            Ncte(1))))))),
                     Var("fib")))))
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


(* Returns the last argument of a list. *)
let l1_last_element_list () =
  Lrec("last_element_list",
       "list",
       If(IsEmpty(Tl(Var("list"))),
          Hd(Var("list")),
          App(Var("last_element_list"),
              Tl(Var("list")))),
       Var("last_element_list"))

(* Calculates the factorial of a number. *)
let l1_factorial () =
  Lam("n",
      Lrec("aux",
           "tuple",
           Let("result",
               Hd(Var("tuple")),
               Let("current",
                   Tl(Var("tuple")),
                   If(Binop(Ge,
                            Var("current"),
                            Ncte(1)),
                      App(Var("aux"),
                          Pair(Binop(Mult,
                                     Var("result"),
                                     Var("current")),
                               Binop(Sub,
                                     Var("current"),
                                     Ncte(1)))),
                      Var("result")))),
           App(Var("aux"),
               Pair(Ncte(1),
                    Var("n")))))



let l1_mapply () =
  Lam("fn",
      Lrec("mapply",
           "list",
           If(IsEmpty(Var("list")),
              Nil,
              Cons(App(Var("fn"),
                       (Hd(Var("list")))),
                   App(Var("mapply"),
                       (Tl(Var("list")))))),
           Var("mapply")))


let l1_rec_function () =
  Lam("y",
      Lrec("rec",
           "x",
           Binop(Sum,
                 Var("x"),
                 Var("y")),
           Var("rec")))
  
let l1_simple_function () =
  Lam("x",
      If(Var("x"),
         Ncte(0),
         Ncte(1)))

let l1_raise () =
  App(l1_simple_function (), Raise)
