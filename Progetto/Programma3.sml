(* PROGRAMMA 3

Esempio di emulazione comportamento dell'ownership, uso del clone()

in RUST:

fn main() {
    let mut x = 0;
    let mut y = 0;
    let mut vec1 = Vec::new();
    let mut i = 0;

    while i<5 {
        vec1.push(i);
        i += 1;
    }

    let mut vec2 = vec1.clone();
    x = vec1[3];
    y = vec2[3];
    
    println!("{}", x);   //stampa 3
    println!("{}", y);   //stampa 3
}

*)

val programma3 = mylet("x",myvalue(myint(0)), 
                 mylet("y",myvalue(myint(0)),
                 intVector("vec1",[],
                 intVector("vec2",[],
                 mylet("i",myvalue(myint(0)),
                 conc( mywhile( less(myvar("i"),myvalue(myint(5))) , 
                                conc(pushVector("vec1",myvar("i")),
                                assign("i",sum(myvar("i"),myvalue(myint(1)))))),
                 conc(clone("vec2","vec1"),
                 conc(assign("x",getIndex("vec1",myvalue(myint(3)))),
                 conc(assign("y",getIndex("vec2",myvalue(myint(3)))),
                 conc(stampaEnv,
                      stampaStore)
                 )))))))));
                 
val result = evalp(programma3, Env, Store);

(*
Env: { (i, < 47 >)  (vec2, < 57  56  55  54  53 >)  (vec1, < 48  49  50  51  52 >)  (y, < 46 >)  (x, < 45 >) }
Store: { (46, 3)  (45, 3)  (57, 0)  (56, 1)  (55, 2)  (54, 3)  (53, 4)  (47, 5)  (52, 4)  (47, 4)  
         (51, 3)  (47, 3)  (50, 2)  (47, 2)  (49, 1)  (47, 1)  (48, 0)  (47, 0)  (46, 0)  (45, 0) }
*)         


