% ####################################
% Calentando motores
% ####################################

%%% Ejercicio 1

% listaNats(+LInf,+LSup,?Nats), que unifica la lista Nats con los naturales en el rango [LInf, LSup], o una lista vacía si LSup < LInf.

listaNats(INF,SUP,LNATS):- SUP<INF, LNATS=[]. 
listaNats(INF,SUP,LNATS):- SUP>=INF, LNATS=[INF|B], INF2 is INF+1, listaNats(INF2,SUP,B).


%%% Ejercicio 2

% nPiezasDeCada(+Cant, +Tamaños, -Piezas), que instancia a Piezas con una lista que contiene 
%  una cantidad Cant de cada tamaño en la lista Tamaños.
	
nPiezasDeCada(0,_,[]).
nPiezasDeCada(_,[],[]).
nPiezasDeCada(CANT,[T|TAMS],PIEZAS):- PIEZAS=[pieza(T,CANT)|P], nPiezasDeCada(CANT,TAMS,P). 

%%% Ejercicio 3

% resumenPiezas(+SecPiezas, -Piezas), que permite instanciar Piezas con la lista de
%  piezas incluidas en SecPiezas. 

resumenPiezas([],[]).
resumenPiezas(SECP,PIEZAS):- msort(SECP,SECORD), nPiezasDeCada(1,SECORD,P), juntar(P,PIEZAS).

juntar([],[]).
juntar([X],[X]).
juntar([pieza(T1,C1),pieza(T2,C2)|PS],PIEZAS):- T1=\=T2, juntar([pieza(T2,C2)|PS],P), PIEZAS=[pieza(T1,C1)|P].
juntar([pieza(T,C1),pieza(T,C2)|PS],PIEZAS):- C3 is C1+C2, juntar([pieza(T,C3)|PS],PIEZAS).



% ####################################
% Enfoque naïve
% ####################################

%%% Ejercicio 4

% generar(+Total,+Piezas,-Solución), donde Solución representa una lista de piezas
%  cuyos valores suman Total. Aquí no se pide controlar que la cantidad de cada pieza
%  esté acorde con la disponibilidad.

generar(0,_,[]).
generar(T,PIEZAS,SOL):- T>0, tamanios(PIEZAS,TP), member(TAM,TP), T1 is T-TAM, generar(T1,PIEZAS,SOL1), SOL=[TAM|SOL1].

tamanios([],[]).
tamanios([pieza(T,_)|L],LNATS):- tamanios(L,D), LNATS=[T|D]. 

%%% Ejercicio 5 

% cumpleLímite(+Piezas,+Solución) será verdadero cuando la cantidad de piezas utilizadas en Solución 
%  no exceda las cantidades disponibles indicadas en Piezas

cumpleLimite(PIEZAS,SOL):- resumenPiezas(SOL,SOLPIEZAS), cumpleLimiteAux(PIEZAS,SOLPIEZAS).

cumpleLimiteAux(_,[]).
cumpleLimiteAux(PIEZAS,[S|SOL]):- noSuperaLimite(S,PIEZAS), cumpleLimiteAux(PIEZAS,SOL).

noSuperaLimite(pieza(T,C),[pieza(T,C2)|_]):- C2>=C, !.
noSuperaLimite(pieza(T,C),[pieza(T2,_)|PIEZAS]):- T=\=T2, noSuperaLimite(pieza(T,C),PIEZAS).


%%% Ejercicio 6

% construir1(+Total,+Piezas,-Solución), donde Solución representa una lista de piezas cuyos valores 
%  suman Total y, además, las cantidades utilizadas de cada pieza no exceden los declarados en Piezas.

construir1(T,P,SOL):- generar(T,P,SOL), cumpleLimite(P,SOL).


% ####################################
% Enfoque dinámico
% ####################################

%%% Ejercicio 7

% construir2(+Total,+Piezas,-Solución), cuyo comportamiento es identico a construir1/3 pero que utiliza 
%  definiciones dinámicas para persistir los cálculos auxiliares realizados y evitar repetirlos. 
%  No se espera que las soluciones aparezcan en el mismo orden entre construir1/3 y construir2/3, pero sí, sean las mismas.

construir2(T,P,SOL):- retractall(lookUp(_,_,_,_)), generar2(T,P,T,SOL), cumpleLimite(P,SOL).

generar2(0,_,_,[]).
generar2(T,P,K,SOL):- T>0, lookUp(T,P,K,SOL), !.
generar2(T,P,K,SOL):- T>0, K>0, L is K-1, generar2(T,P,L,SOL), asserta(lookUp(T,P,L,SOL)).
generar2(T,P,K,SOL):- T>0, dameMax(P,K,L), M is T-L, between(0,M,T1), T2 is T-L-T1, L2 is L-1, generar2(T1,P,L2,SOL1), generar2(T2,P,L,SOL2), append(SOL1, [K|SOL2], SOL), asserta(lookUp(T,P,K,SOL)).
 

:- dynamic lookUp/4.

dameMax(P,K,L):- tamanios(P,TS), sort(TS,TSORD), reverse(TSORD, REV), dameMaxAux(REV,K,L).

dameMaxAux([X|_],K,L):- X=<K, L=X, !. 
dameMaxAux([X|REV],K,L):- X>K, dameMaxAux(REV,K,L).

% ####################################
% Comparación de resultados y tiempos
% ####################################

%%% Ejercicio 8

% todosConstruir1(+Total, +Piezas, -Soluciones, -N), donde Soluciones representa una lista con todas las
%  soluciones de longitud Total obtenidas con construir1/3, y N indica la cantidad de soluciones totales.

todosConstruir1(T,P,SOL,N):- aggregate_all(count,construir1(T,P,SOL),N). 


%%% Ejercicio 9

% todosConstruir2(+Total, +Piezas, -Soluciones, -N), donde Soluciones representa una lista con todas 
%  las soluciones de longitud Total obtenidas con construir2/3, y N indica la cantidad de soluciones totales.

todosConstruir2(T,P,SOL,N):- aggregate_all(count,todosConstruir2(T,P,SOL),N). 


% ####################################
% Patrones
% ####################################

%%% Ejercicio 10

% construirConPatron(+Total, +Piezas, ?Patrón, -Solución) será verdadero cuando Solución sea una solución factible 
%  en los términos definidos anteriormente y, además, sus piezas respeten el patrón indicado en Patrón. 
%  Se sugiere definir un predicado tienePatrón(+Lista, ?Patrón) que decida si Lista presenta el Patrón especificado.

construirConPatron(_, _, _, _):- fail.


%%%%%%%%%
% TESTS %
%%%%%%%%%

ejemplo(e2, P) :- listaNats( 1, 20, D), nPiezasDeCada( 5, D, P).
ejemplo(e1, P) :- listaNats( 1, 3, D_1_3), nPiezasDeCada( 2, D_1_3, P_1_3),
					listaNats( 4, 6, D_4_6), nPiezasDeCada( 5, D_4_6, P_4_6), append(P_1_3, P_4_6, P).