(* PROGRAMMA 2

Esempio di emulazione comportamento dell'ownership con oggetto memorizzato nell'heap in rust (Vector)

in RUST:

fn main() {
    let mut x = Vec::new();
    let mut y = 0;
    let mut vec = vec![10, 20, 30];
    vec.pop();
    vec.push(42);
    vec.push(50);

    x = vec;
    y = x[2];
    
    println!("{}", y);  //stampa 42

    //vec.pop();     //errore = use of moved value: `vec`
}

*)

val programma2 = intVector("x",[], 
                 mylet("y",myvalue(myint(0)),
                 intVector("vec",[myvalue(myint(10)),myvalue(myint(20)),myvalue(myint(30))],
                 conc( popVector("vec"),
                 conc( pushVector("vec",myvalue(myint(42))),
                 conc( pushVector("vec",myvalue(myint(50))),
                 conc( assign("x",myvar("vec")) ,
                 conc( assign("y",getIndex("x",myvalue(myint(2)))), 
                 conc( stampaEnv, 
                 conc( stampaStore,
                       popVector("vec") 
                 ))))))))));
                           
val result = evalp(programma2, Env, Store);

(*
Env: { (moved, < 10  9  11  12 >)  (y, < 7 >)  (x, < 10  9  11  12 >) }
Store: { (7, 42)  (12, 50)  (11, 42)  (10, 10)  (9, 20)  (8, 30)  (7, 0) }

uncaught exception Fail [Fail: variable not found]
  raised at: C:\Users\Samuele\Documents\SML prove\Progetto_Linguaggi\Main.sml:57.16-57.41
*)
