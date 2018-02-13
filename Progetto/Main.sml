
(******************** Progetto Linguaggi di Programmazione *********************
     
     Implementazione in SML di un sottolinguaggio interessante di RUST. 
     Abbiamo scelto di riprodurre i meccanismi dell'ownership utilizzati da RUST,
     in particolare abbiamo messo in risalto le differenze tra i meccanismi 
     utilizzati per dati memorizzati nello Stack (interi e booleani) e 
     nell'Heap (vector). Infine abbiamo implementato il metodo Clone() di RUST
     che consente di "aggirare" alcuni degli effetti dell'ownership.
     
     Gruppo formato da:
     Samuele Corsalini     1705696
     Nicolò D'Evangelista  1698229
     Domenico Fedele       1706550
     
*)

(*Tipo delle Variabili*)
type var = string;
(*Tipo delle Locazioni*)
type loc = int;
(*Ritorna la locazione sotto forma di stringa*)
fun printLoc (l:loc) = Int.toString l;
(*Utilizzati per la creazione e gestione delle locazioni*)
val currentloc: int ref = ref 0;
fun newloc () : loc =  (currentloc :=  !currentloc + 1; !currentloc ) ;

(*Tipo delle coppie Variabile e Locazione memorizzate nell'ambiente, utilizziamo
riferimenti per porterli poi manipolare dopo la loro creazione*)
type varloc = ((var ref)*((loc list) ref));

(*Ambiente*)
val Env:varloc list = [];

(*Ritorna la stringa che rappresenta una lista di locazioni*)
fun printLocList( locList: loc list ) : string = 
    let
    val outstr = ref "<" 
    in 
    (List.app  ( fn x => ( if(x <> ~1 andalso x <> ~2) then outstr := (!outstr^" "^printLoc(x)^" ") else ()) ) locList; 
    outstr := (!outstr^">"); !outstr )
    end;

(*Ritorna la stinga che rappresenta l'ambiente*)
fun printEnv ( env: varloc list ) : string = 
    let
    val stringEnv = ref "{" 
    in 
    (List.app  (fn x =>  (stringEnv := (!stringEnv^" ("^ ( !(#1 x)) ^", "^printLocList( !(#2 x))^") ")) ) env; 
    stringEnv := (!stringEnv^"}"); !stringEnv )
    end;

(*Dichiarazione tipi di dato delle espressioni(M) e dei valori(value)*)
datatype value = myint of int | mybool of bool | closure of var*M*(varloc list)*((loc*value) list)
and M = myvalue of value | myvar of var | sum of M*M | less of M*M | funzio of var*M | appl of M*M | getIndex of var*M;

(*Meccanismi elementari di tipaggio dei valori*)
datatype tipo = i32 | r_bool | r_closure;
fun typeValue (myint(i)) = i32 | typeValue (mybool(b)) = r_bool |typeValue (closure(v,m,vl,lv)) = r_closure;

(*Funzioni che ritornano la rapprensentazione in stringa del relativo elemento del linguaggio *)
fun printValue (myint(s))= Int.toString s | printValue (mybool(b)) = Bool.toString b | 
    printValue (closure(v,m,env,str)) = " ("^v^", "^printM(m)^", "^printEnv(env)^", "^printStore(str)^" ) "
and printM (myvalue(v)) = printValue(v) | printM (myvar(v1)) = v1 | printM (sum(m1,m2)) = printM(m1) ^ "+" ^ printM(m2) |
    printM (less(m1,m2)) = printM(m1) ^ "<" ^ printM(m2) | printM (funzio(v,m)) = "fn " ^ v ^ " => " ^ printM(m) |
    printM (appl(m1,m2)) = "(" ^ printM(m1) ^ ")("^ printM(m2) ^ ")" | printM (getIndex(v,M)) =  v^"["^ printM(M) ^ "]" 
and printStore ( store: (loc*value) list ) : string = 
    let
    val stringSt = ref "{" 
    in 
    (List.app  (fn x =>  (stringSt := (!stringSt^" ("^ printLoc(#1 x)^", "^printValue(#2 x)^") ")) ) store;  
    stringSt := (!stringSt^"}"); !stringSt )
    end;

(*Tipo delle coppie Locazione Valore memorizzate nello store*)
type locval = loc*value;

(* Store *)
val Store:locval list = [];

(*Funzione che, data una variabile, ritorna la lista di locazioni ad essa associata nell'ambiente*)
fun findloc (v:var , E: varloc list) : loc list = 
    if( List.exists (fn y => !(#1 y) = v) E ) 
    then !(#2 (Option.valOf (List.find (fn x => !(#1 x) = v ) E)))
    else raise Fail "variable not found";
    
(*Funzione che, data una locazione, ritorna il valore ad essa associata nello store*)
fun findValueOfLoc (l:loc, S: locval list ) : value =
    if( List.exists (fn x => (#1 x) = l) S )
    then #2 (Option.valOf (List.find (fn x => (#1 x) = l) S) )
    else raise Fail "location not found in the store";

(*Funzione di somma tra interi*)
fun mysum (v1:value, v2:value) : value =
    case v1 of
    mybool(b) => raise Fail "Invalid Argument"
    |closure(v,m,vl,lv) => raise Fail "Invalid Argument"
    |myint(i) => case v2 of
                 mybool(b2) => raise Fail "Invalid Argument"
                 |closure(v,m,vl,lv) => raise Fail "Invalid Argument"
                 |myint(i2) =>  myint(i+i2);
                 
(* Funzione confronto tra interi (minore) *)                 
fun myless (v1:value, v2:value) : value =
    case v1 of
    mybool(b) => raise Fail "Invalid Argument"
    |closure(v,m,vl,lv) => raise Fail "Invalid Argument"
    |myint(i) => case v2 of
                 mybool(b2) => raise Fail "Invalid Argument"
                 |closure(v,m,vl,lv) => raise Fail "Invalid Argument"
                 |myint(i2) => mybool(i<i2);

(*Funzione di valutazione di espressioni*)                  
fun evalM(m:M, E:varloc list, S:locval list) : value =
    case m of
    myvalue(v) => v
    |myvar(w) => if( List.exists (fn y => !(#1 y) = w) E ) 
                 then (if (List.hd (findloc(w,E)) = ~1 orelse List.hd (findloc(w,E)) = ~2 )
                       then raise Fail "array value not defined"
                       else findValueOfLoc( List.hd (findloc(w, E)), S ))
                 else raise Fail "variable not found"
    |sum(m1,m2) => mysum(evalM(m1,E,S), evalM(m2,E,S))
    |less(m1,m2) => myless(evalM(m1,E,S), evalM(m2,E,S))
    |funzio(v,m) => closure(v,m,E,S)
    |getIndex(v,m) =>  if( List.exists (fn y => !(#1 y) = v) E ) 
                       then 
                            (if (List.hd (findloc(v,E)) ) = ~1 orelse (List.hd (findloc(v,E)) ) = ~2
                            then ( case (evalM(m,E,S)) of
                                   myint(i) => if ((i<0) orelse (i > ((length (findloc(v,E)) )-2)) ) 
                                        then raise Fail "Index OutOfBound Exception"
                                        else #2 (Option.valOf (List.find (fn y => (#1 y)= List.nth((findloc(v,E)),i+1) ) S))
                                   |  _     => raise Fail "Indice array di tipo non intero" )
                            else raise Fail "expexted vector")
                       else  raise Fail "variable not found"
    |appl(m1,m2) => case evalM(m1,E,S) of
                    closure(v,m3,env,sto) =>  evalM(m3,( (ref v), (ref [newloc ()]) )::env,( !currentloc ,evalM(m2,E,S))::sto)
                    |    _      =>  raise Fail "expected fun as first parameter";

(*Dichiarazione tipi di dato dei programmi*)                    
datatype p = skip | conc of p*p | myif of M*p*p | mywhile of M*p | mylet of var*M*p |assign of var*M 
             | intVector of var*(M list)*p | boolVector of var*(M list)*p | pushVector of var*M | popVector of var
             | clone of var*var | stampaEnv | stampaStore;

(*Utility usata dal costruttore assign di evalp per rimuovere dall'ambiente le occorrenze 
di variabili di tipo array che vanno fuori scoope*)
fun removeArr (v1:var,x:varloc) = 
    if ( !(#1 x)=v1 ) andalso ( (List.hd ( !(#2 x))) = ~1 orelse (List.hd ( !(#2 x))) = ~2)
    then (#1 x) := "moved"
    else () ;
   
(*Funzione di valutazione dei programmi*) 
fun evalp(pr:p, E:varloc list, S:locval list) =
    case pr of
    skip => S
    |conc(p1,p2) =>  evalp(p2,E,evalp(p1,E,S)) 
    |myif(m,p1,p2) => if printValue(evalM(m,E,S)) = "true"
                      then evalp(p1,E,S)
                      else if printValue(evalM(m,E,S)) = "false" 
                      then evalp(p2,E,S)
                      else raise Fail "expected bool"
    |mywhile(m,p1) => if printValue(evalM(m,E,S)) = "false" 
                      then S
                      else if printValue(evalM(m,E,S)) = "true" 
                      then evalp(mywhile(m,p1),E,evalp(p1,E,S))
                      else raise Fail "expected bool"
    |mylet(v1,m,p1) => evalp( p1, ( (ref v1) , (ref ([newloc ()])) )::E, ( !currentloc, evalM(m,E,S) )::S)
    |assign(v,m) => (case m of
                    myvar(v1) => ( if (List.hd (findloc(v1,E)) = ~1 orelse List.hd (findloc(v1,E)) = ~2 )
                                   then( 
                                     (#2 (Option.valOf (List.find (fn y => !(#1 y) = v) E))) := findloc(v1,E);
                                     List.app (fn x => removeArr(v1, x)) E;
                                     S
                                     )
                                   else ( (List.hd (findloc(v,E))) ,evalM(m,E,S))::S )
                    |    _    => (( (List.hd (findloc(v,E))) ,evalM(m,E,S))::S) ) 
    |intVector(v1,ml,p1) => if List.exists (fn x=> case (evalM(x,E,S)) of 
                                                  myint(i) => false
                                                  |   _    => true 
                                                  ) ml
                          then raise Fail "expected int"
                          else (let   
                              val n:int ref = ref ((List.length ml)-1) 
                              and listloc : (loc list) ref = ref [] 
                              and lv : (locval list) ref = ref [] 
                           in 
                              while !n >= 0 do 
                                  ( 
                                  listloc := (newloc ()) :: !listloc ;
                                  lv := ( !currentloc , evalM( (List.nth(ml, !n)) , E ,S) ) :: !lv ; 
                                  n := !n -1
                                  );
                              (evalp(p1, ( (ref v1), (ref ( ~1 :: !listloc)) ) :: E , ( !lv @ S)) ) 
                           end
                          )
    |boolVector(v1,ml,p1) => if List.exists (fn x=> case (evalM(x,E,S)) of 
                                                  mybool(b) => false
                                                  |    _    => true 
                                                  ) ml
                          then raise Fail "expected bool"
                          else (let   
                              val n:int ref = ref ((List.length ml)-1) 
                              and listloc : (loc list) ref = ref [] 
                              and lv : (locval list) ref = ref [] 
                          in 
                              while !n >= 0 do 
                              ( 
                              listloc := (newloc ()) :: !listloc ;
                              lv := ( !currentloc , evalM( (List.nth(ml, !n)) , E ,S) ) :: !lv ; 
                              n := !n -1
                              );
                          (evalp(p1, ( (ref v1), (ref ( ~2 :: !listloc)) ) :: E , (!lv @ S)) ) 
                          end
                       )
    |pushVector(v,m) => (if ( ~1 = List.hd(findloc(v,E)) )
                         then (
                              case evalM(m,E,S) of
                              myint(i) =>   ((#2 (Option.valOf (List.find (fn x => !(#1 x) = v) E))) := 
                                            !(#2 (Option.valOf (List.find (fn x => !(#1 x) = v) E))) @ [(newloc ())];
                                            ( !currentloc,evalM(m,E,S) ) ::S)
                              |  _     =>   raise Fail "expected int argument"
                              )
                         else if ( ~2 = List.hd(findloc(v,E)) )
                              then (
                              case evalM(m,E,S) of
                              mybool(b) =>   ((#2 (Option.valOf (List.find (fn x => !(#1 x) = v) E))) := 
                                            !(#2 (Option.valOf (List.find (fn x => !(#1 x) = v) E))) @ [(newloc ())];
                                            ( !currentloc,evalM(m,E,S) ) ::S)
                              |  _     =>   raise Fail "expected bool argument"
                              )
                              else raise Fail "expected vector")
    |popVector(v)    => if ( ( ( ~1 = List.hd(findloc(v,E)) ) orelse ~2 = List.hd(findloc(v,E))) andalso List.length (findloc(v,E)) > 1)
                         then (
                              (#2 (Option.valOf (List.find (fn x => !(#1 x) = v) E))) := 
                                 List.take(findloc(v,E), (List.length (findloc(v,E)))-1);
                              S
                              )
                         else raise Fail "expected vector"
    |clone(v1,v2)    =>  if (List.hd (findloc(v2,E)) = ~1 orelse List.hd (findloc(v2,E)) = ~2)
                         then let 
                                  val n: int ref = ref ((List.length (findloc(v2,E)))-1)
                                  and listlocv2: (loc list) ref = ref (findloc(v2,E))
                                  and v : value ref = ref  (myint(0))
                                  and listloc : (loc list) ref = ref []
                                  and listlocval : (locval list) ref = ref []
                               in 
                                  while !n > 0 do 
                                  ( 
                                    v := findValueOfLoc( List.nth( !listlocv2, !n) , S) ;
                                    listloc := (newloc ()) :: !listloc;
                                    listlocval := ( (List.hd ( !listloc)) , !v ) :: !listlocval;
                                    n := !n - 1
                                  );
                                  ( if List.hd (findloc(v2,E)) = ~1 
                                    then (#2 (Option.valOf (List.find (fn y => !(#1 y) = v1) E))) := ~1::( !listloc)
                                    else (#2 (Option.valOf (List.find (fn y => !(#1 y) = v1) E))) := ~2::( !listloc);
                                    !listlocval @ S
                                  )
                              end
                          else raise Fail "attempted to take value of method `clone` on invalid type"
    |stampaEnv     =>   (print ("Env: "^printEnv(E)^"\n"); S)
    |stampaStore   =>   (print ("Store: "^printStore(S)^"\n"); S);
     
