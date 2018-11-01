open Syntax

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

let l1_raise () =
  App(l1_factorial (), Raise)


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
