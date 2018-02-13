
(*************************** Ultimi test ***************************)

val v1 :value = myint(5);
val v2 :value = myint(3);
val x :var = "x";
val y :var = "y";
val l1 :loc = 42;
val l2 :loc = 65;
val vl :varloc = (ref x,ref [l1]);
val vl2 :varloc = (ref y,ref [l2]);
val lv :locval = (l1,v1);
val lv2 :locval = (l2,v2);
val Env :varloc list = vl::Env;
val Env :varloc list = vl2::Env;
val Store :locval list = lv::Store;
val Store :locval list = lv2::Store;
(*New Test*)

(*Vector test (creazione,accesso e uso dei metodi push e pop)*)
val z :var = "z";
val p1:p = intVector(z,[myvalue(myint(5)),myvalue(myint(8)),myvalue(myint(1))],assign(x,getIndex(z,myvalue(myint(1)))));
val S2: locval list = evalp(p1,Env,Store);
evalM(myvar(x),Env,S2);
(* push int *)
val p2:p = intVector(z,[myvalue(myint(97))] ,conc(pushVector(z, myvalue(myint(47))),conc(pushVector(z, myvalue(myint(49))),assign(x,getIndex(z,myvalue(myint(2)))))));
val S3: locval list = evalp(p2,Env,Store);
evalM(myvar(x),Env,S3);
(* push bool *)
val p3:p = boolVector(z,[myvalue(mybool(true))] ,conc(pushVector(z, myvalue(mybool(false))),conc(pushVector(z, myvalue(mybool(true))),assign(x,getIndex(z,myvalue(myint(2)))))));
val S4: locval list = evalp(p3,Env,Store);
evalM(myvar(x),Env,S4);
(* pop *)
val p4:p = boolVector(z,[myvalue(mybool(true)),myvalue(mybool(false)),myvalue(mybool(true))], conc(popVector(z), popVector(z)));

(*Ownership test(verifica che l'assegnamento tra variabili contenenti array funzioni come previsto)*)
val vl3 : varloc = (ref z, ref [~1,86,87,88]);
val lv3 :locval list = [(86,myint(11)),(87,myint(3)),(88,myint(37))];  (*inserimento in ambiente e store dell'array con valori: [11,3,37]*)
val Env :varloc list = vl3::Env;
val Store :locval list = lv3 @ Store;

(*val p4:p = intVector(z,[myvalue(myint(11)),myvalue(myint(3)),myvalue(myint(37))], assign(x, myvar(z)) );*)
(*
val p5: p = assign(x, myvar(z));
evalp(p5,Env,Store);
Env; *) (*per apprezzare le modifiche*)

(*pop*)
(*
val p6: p = conc(popVector(x), popVector(x));
evalp(p6,Env,Store);
Env;*)

(*clone*)
val p7 : p = clone(x,z);
evalp(p7,Env,Store);
Env;

(************************** Vecchi Test ***************************************)

(*val p1 : p = skip ;
evalp(p1,Env,Store);
val p2 : p = assign(x,myvalue(myint(~3)));
val p3 : p = assign(y,myvalue(myint(77)));
evalM(myvar(x),Env,evalp(p2,Env,Store));
val p4 :p = myif(myvalue(mybool(true)),p2,p3);
val p5 :p = myif(myvalue(mybool(false)),p2,p3);
val Store2:locval list = evalp(p4,Env,Store);
val Store3:locval list = evalp(p5,Env,Store);
evalM(myvar(x),Env,Store2);
evalM(myvar(y),Env,Store2);
evalM(myvar(x),Env,Store3);
evalM(myvar(y),Env,Store3);
val p6 : p = conc(p2,p3);
val Store4 : locval list= evalp(p6,Env,Store);
evalM(myvar(x),Env,Store4);
evalM(myvar(y),Env,Store4);*)

(*while test*)
(*val m1 : M = less(myvalue(myint(0)), myvar(y));
val pdec : p = assign(y,sum(myvar(y),myvalue(myint(~1))));
val pinc : p = assign(x,sum(myvar(x),myvalue(myint(1))));
val p7 : p = mywhile(m1,conc(pdec,pinc));
val Store5 : locval list = evalp(p7,Env,Store);
evalM(myvar(y),Env,Store5);
evalM(myvar(x),Env,Store5);*)
print(printEnv(Env)^"\n");
print(printStore(Store)^"\n");

val m1 : M = less(myvalue(myint(0)), myvar(y));
val pdec : p = assign(y,sum(myvar(y),myvalue(myint(~1))));
val pinc : p = assign(x,sum(myvar(x),myvalue(myint(1))));
val p7:p = mywhile(m1, conc(pdec,pinc)) 
val Store5 : locval list = evalp(p7,Env,Store);
evalM(myvar(y),Env,Store5);
evalM(myvar(x),Env,Store5);

(*mylet test*)
val m2 : M = myvalue(myint(6));
val m3 : M = myvalue(myint(2));
val z : var = "z";
val t : var = "t";
val Env = ("t",[35])::Env;
val p8: p = mylet(z,m2,assign(z,myvalue(myint(9))));
val Store : locval list = [];
val Store6 : locval list = evalp(p8,Env,Store);   
(*evalp(assign(z,myvalue(myint(0))), Env, Store); *)

(*Test appl*)
(*val f : M = funzio(z,sum(myvar(z),myvalue(myint(3))));
evalM(appl(f,myvalue(myint(5))),Env,Store);

val m4 : M = myvalue(mybool(true));
val m5 : M = myvalue(mybool(false));
val z2 : var = "z2";
val t2 : var = "t2";*)

(* f3: fn x -> fn y -> x y          f4: fn z2 -> x < 6            p: (f3 f4) 4          torna true, YEAH !   *)  
(*val f3 : M = funzio(x,funzio(y,appl(myvar(x),myvar(y))));
val f4 : M = funzio(z2,less(myvar(z2),myvalue(myint(6))));
evalM(appl(appl(f3,f4),myvalue(myint(4))),Env,Store);    *)           

(*f1: z2->fn t2-> fn h3-> (z2 t2) h3             f2: x ->fn y -> x<y       p: ((f1 f2)7)5         torna false, YUPPIE!  *)

(*val f1 : M = funzio(z2,funzio(t2, funzio("h3",appl(appl(myvar(z2),myvar(t2)),myvar("h3")))));
val f2 : M = funzio(x,funzio(y,less(myvar(x),myvar(y))));
evalM(appl(appl(appl(f1,f2),myvalue(myint(7))),myvalue(myint(5))),Env,Store);*)

(*test stampe*)
(*
val x:value = mybool(true);
print(printValue(x)^"\n");

val x:value = closure("c",myvalue(myint(4)),Env,Store);
print(printValue(x)^"\n");


val x:value = closure("c",myvalue(myint(4)),Env,Store);
val y: M = myvalue(mybool(true));
print(printM(y)^"\n");

val x:value = closure("u",myvalue(myint(4)),Env,Store);
print(printValue(x)^"\n");

val y: M = myvalue(myint(4));
print(printM(y)^"\n");

val y: M = myvar("pollo");
print(printM(y)^"\n");

val y: M = sum(sum(myvalue(myint(2)),myvar("y")),myvalue(myint(6)));

print(printM(y)^"\n");
val y: M = less(myvar("p"),myvalue(myint(2)));

print(printM(y)^"\n");
val y: M = less(less(myvar("p"),myvalue(myint(4))),myvalue(myint(2)));

print(printM(y)^"\n");
val y: M = funzio("z",sum(myvar("z"),myvalue(myint(3))));

print(printM(y)^"\n");
val y = funzio("z",funzio("v",funzio("h",sum(myvar("z"),sum(myvar("v"),myvar("h"))))));

print(printM(y)^"\n");
val y = appl(funzio("x",myvar("x")),myvalue(myint(7)));

print(printM(y)^"\n");
val y = appl(funzio("x",appl(myvar("x"),myvalue(myint(3)))),funzio("y",sum(myvar("y"),myvalue(myint(1)))));

print(printM(y)^"\n");

print(printEnv(Env)^"\n");
print(printStore(Store)^"\n");*)

val z:var = "z";
val w:p = arr(z,[myvalue(myint(9)),myvalue(myint(4)),myvalue(myint(5))],assign(t,myvar(z))) ;
val w4: locval list= evalp(w,Env,Store);
evalM(myvar(t),Env,w4);
evalM(myvar(z),Env,w4);
  