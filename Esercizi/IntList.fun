(*datatype delle liste di interi*)
datatype IntList = empty | cons of  IntList*int ;

(*utility di eval*)
fun separator empty= ")" | separator (cons(m,n)) =  "," ^ Int.toString n ^ separator(m) ;
(*valutazione di IntLIst*)
fun eval empty= "()" | eval (cons(m,n)) =  "(" ^ Int.toString n  ^ separator(m) ; 

(*concatenazione di liste, passandogli due liste ne crea una nuova in cui la seconda lista e' posta di seguito alla prima*)
fun conc empty = (fn empty => empty | (cons(v,w)) => (cons(v,w)))  | 
conc (cons(a,b)) = (fn empty => (cons(a,b)) | (cons(v,w)) => (cons(conc a (cons(v,w)), b))) ;                                 

(*reverse di liste*)                                     
fun rev empty= empty | rev (cons(a,b)) = conc (rev a) (cons(empty,b));


(*lista  (5,3,2,4) *)
 val x= cons(cons(cons(cons(empty,4),2),3),5);
(*lista (1,6,7)*)
 val y= cons(cons(cons(empty,7),6),1);