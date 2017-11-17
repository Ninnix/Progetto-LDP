datatype rlist = empty | cons of rlist*rlist;

(* concatenazione in lista *)
fun eval empty = "()" | eval (cons (m, n)) = "(" ^ eval(m) ^ "," ^ eval(n) ^ ")";

(* da il contenuto della lista *)
fun contenuto empty = "" | contenuto (cons(m,n)) = eval(m) ^ eval(n);

(* vero cons *)
fun eval2 empty = "()" | eval2 (cons (m,n)) = "(" ^ eval2(n) ^ "," ^ contenuto(m) ^ ")";

val x = cons(cons(empty,cons(empty,empty)),cons(empty,empty));
val y = cons(empty, empty);
val z = cons(y, empty);

eval2 x;
eval2 y;