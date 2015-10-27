# Compiler - Lispkit

Il repository contiene un compilatore per ils
linguaggio funzionale (minimale) Lispkit.
Il compilatore è stato realizzato come progetto per il corso
*Linguaggi di Programmazione* AA 2015/2016.

## Componenti

Il compilatore è stato implementato per componenti, realizzando
dapprima l'analizzatore lessicale (lexer) e successivamente le restanti parti.

### Analizzatore lessicale - Lexer

Dato in input un programma P genera la serie di token T ad esso relativi.
Non importa se il programma non ha alcun senso dal punto di vista semantico: l'importante è che esistano dei token, relativi agli elementi del programma,
che permettano di tradurlo.

### Analizzatore sintattico

Data in input una stringa s viene costruito l'albero di parsing della stringa s
per la grammatica G.  

#### Grammatica

G e' una grammatica libera da contesto (GLC), essenzialmente una grammatica descrive la sintassi del linguaggio da analizzare. In particolare la GLC scelta  
è una grammatica LL(1): significa che il parsing dell'input s viene eseguito da sinistra a destra esaminando una sola volta tutta la stringa e considerando un simbolo del programma per volta.  

### Analizzatore semantico

### Generatore di codice target
