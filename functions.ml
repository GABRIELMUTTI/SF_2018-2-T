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


(* Returns the last item of a list. *)
let l1_last_element_list () =
  Lrec("fun",
       "l",
       App(Var("fun"),
           Tl(Var("l"))),
       Var("fun"))



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


let l1_mapply () =
  Lrec("mapply",
       "fn",
       Lam("list",
           If(IsEmpty(Var("list")),
              Nil,
              Cons(App(Var("fn"),
                       Hd(Var("list"))),
                   App(App(Var("mapply"),
                           Var("fn")),
                       Tl(Var("list")))))),
       Var("mapply"))

let l1_list_if () =
  If(Bcte(true),
     Nil,
     Cons(Bcte(false), Nil))

let l1_rec_function () =
  Lam("y",
      Lrec("rec",
           "x",
           Binop(Sum,
                 Var("x"),
                 Var("y")),
           Var("rec")))

let l1_rec () =
  Lrec("test",
       "x",
       Var("x"),
       Var("test"))
  
let l1_simple_function () =
  Lam("x",
      If(Var("x"),
         Ncte(0),
         Ncte(1)))

let l1_raise () =
  App(l1_simple_function (), Raise)
