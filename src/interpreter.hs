--INTERPRETE SECD COMPLETO in Haskell
module Interpreter(
  Valore(..),
  execute
)
where

import Lexer
import Parser
import Compiler
{-
import LexerTest
import SyntaxTest
import ParserTest
-}

--------------------------------------------------------------------------------
--Datatype

-- tipo che modella gli R-valori delle variabili.
-- Si tratta dei valori da mettere nella pila S e nell'ambiente dinamico E.
-- CLO modella le chiusure
data Valore = V  LKC                    -- prende come argomenti {NUM, BOO, STRI, NIL}
            | OGA                       -- placeholder per l'ambiente ricorsivo
            | CLO [Secdexpr] [[Valore]] -- chiusura body_funzione ambiente_dinamico
            | VLISTA [Valore]           -- lista di parametri attuali
            deriving(Show,Eq)

-- datatype per i valori del Dump
data Dump = CONTR  [Secdexpr]                     -- controllo : serve per immagazzinare IF THEN ELSE della SEL
          | TRIPLA [Valore] [[Valore]] [Secdexpr] -- serve per immagazzinare (S E C)
          | DUMMY
          deriving(Show,Eq)


--------------------------------------------------------------------------------
--Funzioni di utilità

-- crea l'ambiente dinamico ricorsivo necessario per il trattamento
-- della ricorsione. Serve nel caso Rap => lazyE vl2 vl2 // passa 2 copie della stessa lista
--
-- []         => produce la lista di parametri vuota
-- altrimenti =>
--              a:b :=  vl2 := CLO [corpo della chiusura] [ambiente dinamico]
--              c   :=  vl2 := CLO [corpo della chiusura] [ambiente dinamico]
--              trasforma il parametro "a" tramite lazyClo e continua ricorsivamente sugli altri parametri
--              mettendo in testa il risultato mantiene l'ordine che avevano in input
--              gli elementi della lista
lazyE:: [Valore] -> [Valore] -> [Valore]
lazyE [] _      = []
lazyE (a : b) c = lazyClo a c : lazyE b c

-- creazione di una chiusura
-- c := CLO [corpo della chiusura] [ambiente dinamico]
-- CLO => ha trovato una chiusura nella lista dei parametri
--        a := body
--        b := E (ambiente dinamico) // [OGA] inizialmente, nel caso di chiusure ricorsive
--        => produce una nuova chiusura con :
--           1) stesso body di prima
--           2) (tutte le chiusure presenti nella lista rese ricorsive) : (ambiente dinamico precedente)  // l'ambiente dinamico iniziale è [OGA]
lazyClo:: Valore -> [Valore] -> Valore
lazyClo (CLO a b) c   = CLO a (lazyE c c : tail b) --lascio il nome senza valutarlo;
lazyClo (V x) _       = V x                        -- tail b perchè elimino [OGA], altrimenti 
lazyClo (VLISTA x) _  = VLISTA x                   -- lasciandolo invaliderei gli indirizzi delle variabili globali
lazyClo x _           = error ("LazyClo trova valore incompatibile" ++ (show x)) -- acceduti internamente alle chiusura


lazyClo (CLO a b) c = (CLO a ((lazyE c c):(tail b))) in modo da togliere OGA
--funzioni per la ricerca degli R-valori dati i loro indirizzi: usate da Ld
--eseguono concretamente il codice, infatti estrae un Valore

-- ritorna l'elemento in posizione n-sima della lista data
index:: Integer -> [a] -> a
index 0 s   = head s
index n s   = index (n - 1) (tail s)

-- ritorna l'elemento in posizione (a, b) nell'ambiente [[Valore]] cioè in "e"
locate:: (Integer, Integer) -> [[Valore]] -> Valore
locate (a, b) e   = index b (index a e)

-- estrae una costante intera
extract_int :: Valore -> Integer
extract_int (V (NUM x)) = x
extract_int x           = error ("trovato altro da intero" ++ (show x))

--funzioni per le liste di Valori VLISTA

-- ritorna il primo elemento della lista
vhd :: Valore -> Valore
vhd (VLISTA (a:b))  = a
vhd (VLISTA [])     = error "vhd trovata lista vuota"
vhd _               = error "vhd non trova VLISTA"

-- ritorna la coda della lista
vtl :: Valore -> Valore
vtl (VLISTA (a:b))  = VLISTA b
vtl (VLISTA [])     = error "vtl trovata lista vuota";
vtl _               = error "vtl non trova VLISTA"

vatom :: Valore -> Valore
vatom (V _)   = V (BOO True)
vatom _       = V (BOO False)

bool2s_espressione:: Bool -> LKC
bool2s_espressione b = if b then (BOO True) else (BOO False)

-- test di uguaglianza per il tipo Valore,
-- si adatta ai tipi dei parametri con cui viene invocata
eqValore:: Valore -> Valore -> Bool
eqValore a@(V _) b      = eqV a b
eqValore a@(VLISTA _) b = eqVLISTA a b
eqValore a b            = error ("uguaglianza tra chiusure"++ (show a) ++ (show b))

-- test di uguaglianza per il tipo VLISTA
eqVLISTA:: Valore -> Valore -> Bool
eqVLISTA (VLISTA []) (VLISTA [])           = True
eqVLISTA (VLISTA (a : b)) (VLISTA (c : d)) = eqValore a c && eqVLISTA (VLISTA b) (VLISTA d)
eqVLISTA _ _                               = False

-- test di uguaglianza per valori NON lista
eqV :: Valore -> Valore -> Bool
eqV (V a) (V b)   = a==b
eqV _ _           = False


--------------------------------------------------------------------------------
--Interprete
-- s := stack      (ambiente statico)
-- e := enviroment (ambiente dinamico)
-- c := controllo  (esecuzione del codice)
-- d := dump       (mantiene dump temporaneo di istruzioni)
interprete:: [Valore] -> [[Valore]] -> [Secdexpr] -> [Dump] -> Valore
--[Ldc NIL,Ldc (NUM 5),Cons,Ldf [Ld (0,0),Rtn],Ap,Stop]
interprete s e c d = case (head c) of
                                      Ld(b, n) -> let
                                                    x = (locate (b,n) e)
                                                  in
                                                    interprete (x:s) e (tail c) d
                                      (Ldc k) -> case k of
                                                          NIL -> interprete ((VLISTA []):s) e (tail c) d --NIL indica l'inizio di una lista di parte dx binders
                                                          _   -> interprete ((V k):s) e (tail c) d
                                      Add -> let
                                                 operand1 = extract_int (head s)
                                                 operand2 = extract_int (head (tail s))
                                             in
                                                 interprete ((V(NUM (operand1 + operand2))) : (tail (tail s))) e (tail c)  d

                                      Sub -> let
                                                 operand1 = extract_int (head s)
                                                 operand2 = extract_int (head (tail s))
                                             in
                                                 interprete ((V(NUM (operand1 - operand2))) : (tail (tail s))) e (tail c)  d
                                      Mult -> let
                                                  operand1 = extract_int (head s)
                                                  operand2 = extract_int (head (tail s))
                                              in
                                                  interprete ((V(NUM (operand1 * operand2))):(tail (tail s))) e (tail c)  d
                                      Div -> let
                                                 operand1 = extract_int (head s)
                                                 operand2 = extract_int (head (tail s))
                                             in
                                                 interprete ((V(NUM (operand1 `div` operand2))):(tail (tail s))) e (tail c)  d
                                      Rem -> let
                                                 operand1 = extract_int (head s)
                                                 operand2 = extract_int (head (tail s))
                                             in
                                                 interprete ((V(NUM (operand1 `mod` operand2))):(tail (tail s))) e (tail c)  d
                                      Leq -> let
                                                 operand1 = extract_int (head s)
                                                 operand2 = extract_int (head (tail s))
                                             in
                                                 interprete ((V(bool2s_espressione (operand1 <= operand2))):(tail (tail s))) e (tail c)  d
                                      Eq -> case s of
                                              (w1:w2:w3) -> interprete ((V (bool2s_espressione (eqValore w1 w2))):w3) e (tail c) d
                                              _          -> error "manca un argomento in Eq"
                                      Car -> interprete ((vhd(head s) ):(tail s)) e (tail c) d
                                      Cdr -> interprete ((vtl(head s) ):(tail s)) e (tail c) d
                                      Cons -> case head (tail s) of --ATTENZIONE: analizzo "s" e NON "c"
                                                  (VLISTA x)   -> interprete  (VLISTA ((head s):x):(tail (tail s))) e (tail c) d
                                                  x            -> error ("CONS: il secondo argomento non e' una lista" ++ (show  x))
                                      Atom -> interprete ((vatom (head s)):(tail s)) e (tail c) d

                                      Sel sl1 sl2 -> case head s of -- controlla il risultato dell'espressione nell'IF
                                                          (V (BOO True))  -> interprete (tail s) e sl1 ((CONTR (tail c)):d)
                                                          (V (BOO False)) -> interprete (tail s) e sl2 ((CONTR (tail c)):d)
                                                          _               -> error "non c'e' bool su s quando si esegue SEL"

                                      Join -> case (head d) of
                                                  (CONTR c1) -> interprete s e c1 (tail d)
                                                  _          -> error "JOIN: il dump non contiene controllo"
                                      Ldf sl -> interprete ((CLO sl e):s) e (tail c) d
                                      Ap -> case (head s) of
                                                  (CLO c1 e1) -> case (head (tail s)) of
                                                                     VLISTA x -> interprete [] (x:e1) c1 ((TRIPLA (tail(tail s)) e (tail c)):d)
                                                                     _           -> error "AP senza lista dei parametri"
                                                  _  -> error "AP senza chiusura su s"

                                      Rtn ->  case (head d) of
                                                  (TRIPLA s1 e1 c1) -> interprete ((head s):s1) e1 c1 (tail d)
                                                  _                 ->  error  "RTN: non trovata TRIPLA su dump"
                                      Rap -> case (head s) of -- CLO (body, E)
                                                  (CLO c1 e1) ->  case e1 of --se è una chiusura allora l'ambiente deve iniziare con [OGA] perchè sto controllando le chiusure ricorsive (Rap)
                                                                        ([OGA]:re)   -> case (head (tail s)) of --nello stack, sotto la chiusura (CLO) appena controllata, ci deve essere la lista della definizione della chiusura (parte dx di LETREC) (parte LETREC "fun= lambda.." )
                                                                                        --invocazione => stack = lo svuoto, dinamico = lista_parametri_attuali:ambiente_chiusura, controllo = chiamata a chiusura con parametri attuali(parte "in" di LETREC), dump = dump dello stato attuale della macchina
                                                                                        (VLISTA vl2) -> interprete [] ((lazyE vl2 vl2):re) c1 ((TRIPLA (tail (tail s)) (tail e) (tail c)):d)
                                                                                        --sosituisco ad [OGA] la lista dei parametri attuali resi circolari da lazyE
                                                                                        _            -> error "manca lista dei parametri dopo OGA";
                                                                        _            -> error "non trovata [OGA] nell'ambiente di chiusura ricorsiva"
                                                  _          -> error "RAP: non trovata chiusura su s"
                                      Push -> interprete s  ([OGA]:e)  (tail c)  d
                                      Stop -> (head s)
                                      _  -> error "operazione non riconosciuta"

--se x e'  programma Secdexpr da eseguire. Si aggiunge Stop alla fine.
execute :: [Secdexpr] -> Valore
execute secdexprl = interprete [] [] (secdexprl ++ [Stop]) []

-- Mostra che si puo' usare letrec anche con binders non-funzionali. Le var a sinistra non devono apparire a destra

e = "let z=2 in letrec x= 2+z and y= 2*z in x*y*z end end $"

-- distribuisce FACT su una lista di interi *)

--val S="letrec  FACT = lambda ( X ) if  eq ( X, 0 ) then 1 else  X * FACT(  X - 1 )"^
--" and G = lambda ( H  L ) if  eq ( nil, L ) then L else cons( H(car( L ) ), G ( H , cdr ( L ) ))"^
--" in G ( FACT, cons( 6 ,cons( 7, cons( 8 , nil))) ) end $";

--(*considera liste di liste Z e produce una lista semplice che contiene tanti interi quante sono le liste contenute in Z e l'intero
--corrispondente ad una lista contenuta in Z � la somma dei fattoriali dei suoi elementi: f2=fattoriale, f1=calcola somma dei fattori--ali degli elementi di una
--lista di interi e f0 distribuisce f1 sulle liste contenute in Z *)

f="letrec f0 = lambda ( x ) "++
          "letrec f1 = lambda(y) " ++
                 "letrec f2=lambda (z) if eq(z , 1) then 1 else z * f2( z - 1 ) " ++
                 "in if eq( y , nil ) then 0 else f2 ( car ( y ) ) + f1 ( cdr (y)) end " ++
          "in if eq(x , nil) then nil else cons (f1 ( car ( x )),f0 ( cdr ( x ) ) ) end " ++
   "in f0( cons (cons (3 , cons (3 , nil)), cons( cons (3 , nil), nil))) end $"


--(* esempio di funzione che restituisce una funzione locale *)

g="let f1 = lambda() letrec f2=lambda (z) if eq(z , 1) then 1 else z * f2( z - 1 ) in f2 end in let x=f1() in x(8) end end $"


--Tok=lexi(explode S)

--(k1,k2)=PROG(Tok)
