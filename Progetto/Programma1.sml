(* PROGRAMMA 1

Esempio di emulazione comportamento dell'ownership con oggetto memorizzato nello stack in rust, cioè un tipo primitivo (bool)

in RUST:

fn main() {
    let x = 8 + 5 < 3;
    let y = x;

    fn fun_1(w:bool) -> bool{
        return w;
    }

    let z = fun_1(x);
    
    if z {;}
    else {
        println!("{}", x); //stampa false 
        println!("{}", y); //stampa false 
        println!("{}", z); //stampa false 
    }
}

*)

val programma2 = mylet("x", 
                 less( sum(myvalue(myint(8)), myvalue(myint(5))),  myvalue(myint(3))), 
                 mylet("y", myvar("x"),
                 mylet("z", appl( funzio("w",myvar("w")), myvar("x") ),
                 myif( myvar("z"), skip, conc(stampaEnv,stampaStore))
                 )));
                           
val result = evalp(programma2, Env, Store);

(*
Env: { (z, < 3 >)  (y, < 2 >)  (x, < 1 >) }
Store: { (3, false)  (2, false)  (1, false) }
*)