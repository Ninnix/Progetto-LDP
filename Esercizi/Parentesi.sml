(*Implementazione delle liste di liste*)

datatype rlist = empty | cons of rlist*rlist;

fun cont empty = "" 
  | cont (cons(empty,empty)) = "()"
  | cont (cons(m,empty)) = "(),"^cont(m)
  | cont (cons(empty,m)) = "("^cont(m)^")"
  | cont (cons(m,n)) = "("^cont(n)^"),"^cont(m);


fun eval empty = "()" 
  | eval (cons(empty,empty)) = "(())"
  | eval (cons(m,empty)) = "((),"^ cont(m) ^")"
  | eval (cons(empty,m)) = "("^eval(m)^")"
  | eval (cons(m,n)) = "("^eval(n)^","^cont(m)^")";


  (*alcuni test*)
val x = cons(cons(empty,cons(empty,empty)),cons(empty,empty)); (*  ((()),(()))  *)
val y = cons(empty, empty); (* (())  *)
val z = cons(y, empty); (*  ((),())  *)
val u = cons(cons(cons(cons(empty,empty),empty),empty),empty); (* ((),(),(),()) *)
val t = cons(empty,cons(empty,cons(empty,cons(empty,empty)))); (*  ((((()))))  *)

