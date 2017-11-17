datatype rlist = empty | cons of rlist*rlist;

(* concatenazione in lista *)
fun eval empty = "()" | eval (cons (m, n)) = "(" ^ eval(m) ^ "," ^ eval(n) ^ ")";

(* da il contenuto della lista *)
fun contenuto empty = "" | contenuto (cons(m,n)) = if m = empty then eval2(n) else eval2(n)^ "," ^ contenuto(m);

(* vero cons, inserisce il secondo elemento nel primo*)
fun eval2 empty = "()" | eval2 (cons (m,n)) = if m = empty then "(" ^ eval2(n) ^ ")"
           else "(" ^ eval2(n) ^ "," ^ contenuto(m) ^ ")";        

(*alcuni test*)
val x = cons(cons(empty,cons(empty,empty)),cons(empty,empty));
val y = cons(empty, empty);
val z = cons(y, empty);
val u = cons(cons(cons(cons(empty,empty),empty),empty),empty);
val t =  cons(empty,cons(empty,cons(empty,cons(empty,empty))));
