% ####################################
% Calentando motores
% ####################################

%%% Ejercicio 1

% listaNats(+LInf,+LSup,?Nats), que unifica la lista Nats con los naturales en el rango [LInf, LSup], o una lista vacía si LSup < LInf.

listaNats(INF,SUP,LNATS):- SUP<INF, LNATS=[]. 
listaNats(INF,SUP,LNATS):- SUP>=INF, LNATS=[INF|B], INF2 is INF+1, listaNats(INF2,SUP,B).

%%%%%%%%%%%%%%%%%%%%%%%% 
%% Detalle
%%%%%%%%%%%%%%%%%%%%%%%%
%% Para crear la listaNats se hace un checkeo sobre los limites de la lista, de modo tal que 
%% se genere una lista vacia si los limites estan invertidos.
%% Luego se recursiona sobre el limite inferior, encabezandolo en la lista e incrementandolo
%% secuencialmente.
%%%%%%%%%%%%%%%%%%%%%%%%
%% Ejemplo de uso
%%%%%%%%%%%%%%%%%%%%%%%% 
%% ?- listaNats(2,1,X).
%% X = [] .
%%
%% ?- listaNats(1,5,C).
%% C = [1, 2, 3, 4, 5] .
%%%%%%%%%%%%%%%%%%%%%%%%

%%% Ejercicio 2

% nPiezasDeCada(+Cant, +Tamaños, -Piezas), que instancia a Piezas con una lista que contiene 
%  una cantidad Cant de cada tamaño en la lista Tamaños.
	
nPiezasDeCada(0,_,[]).
nPiezasDeCada(_,[],[]).
nPiezasDeCada(CANT,[T|TAMS],PIEZAS):- PIEZAS=[pieza(T,CANT)|P], nPiezasDeCada(CANT,TAMS,P). 

%%%%%%%%%%%%%%%%%%%%%%%% 
%% Detalle
%%%%%%%%%%%%%%%%%%%%%%%%
%% Para construir las piezas, recursionamos sobre la lista de tamanios, creando en cada paso 
%% recursivo Cant piezas de tamaño head(Tamaños).
%%%%%%%%%%%%%%%%%%%%%%%%
%% Ejemplo de uso
%%%%%%%%%%%%%%%%%%%%%%%%
%% ?- nPiezasDeCada(3,[1,2,3],P).
%% P = [pieza(1, 3), pieza(2, 3), pieza(3, 3)] .
%%%%%%%%%%%%%%%%%%%%%%%%

%%% Ejercicio 3

% resumenPiezas(+SecPiezas, -Piezas), que permite instanciar Piezas con la lista de
%  piezas incluidas en SecPiezas. 

resumenPiezas([],[]).
resumenPiezas(SECP,PIEZAS):- msort(SECP,SECORD), nPiezasDeCada(1,SECORD,P), juntar(P,PIEZAS).

% juntar(+PiezasOrdenadas,-PiezasJuntadas), funcion que dada una lista de piezas ordenadas en tamaño retorna 
% una nueva lista de piezas pero juntando aquellas que tienen el mismo tamaño, de manera que se crea una nueva 
% pieza con el tamaño original cuya cantidad es la suma de las cantidades de las piezas originales.

juntar([],[]).
juntar([X],[X]).
juntar([pieza(T1,C1),pieza(T2,C2)|PS],PIEZAS):- T1=\=T2, juntar([pieza(T2,C2)|PS],P), PIEZAS=[pieza(T1,C1)|P].
juntar([pieza(T,C1),pieza(T,C2)|PS],PIEZAS):- C3 is C1+C2, juntar([pieza(T,C3)|PS],PIEZAS).

%%%%%%%%%%%%%%%%%%%%%%%% 
%% Detalle
%%%%%%%%%%%%%%%%%%%%%%%%
%% resumenPiezas ordena la secuencia de Naturales SecPiezas, aun con repetidos. Teniendo esta secuencia 
%% ordenada crea una pieza con cada tamaño y luego junta piezas de igual tamaño. Es necesario ordenar pues
%% juntar tiene como precondicion que las piezas le lleguen ordenadas por tamaño.
%%
%% juntar recursiona sobre la secuencia de piezas ordenadas, tomando de a pares en cada paso recursivo y 
%% sumando cantidades si los tamaños son iguales.
%%%%%%%%%%%%%%%%%%%%%%%%
%% Ejemplo de uso
%%%%%%%%%%%%%%%%%%%%%%%% 
%% ?- resumenPiezas([2,2,1,2,3,10,1,5,1,10], P).
%% P = [pieza(1, 3), pieza(2, 3), pieza(3, 1), pieza(5, 1), pieza(10, 2)]
%%%%%%%%%%%%%%%%%%%%%%%%

% ####################################
% Enfoque naïve
% ####################################

%%% Ejercicio 4

% generar(+Total,+Piezas,-Solución), donde Solución representa una lista de piezas
%  cuyos valores suman Total. Aquí no se pide controlar que la cantidad de cada pieza
%  esté acorde con la disponibilidad.

generar(0,_,[]).
generar(T,PIEZAS,SOL):- T>0, tamanios(PIEZAS,TP), member(TAM,TP), T1 is T-TAM, generar(T1,PIEZAS,SOL1), SOL=[TAM|SOL1].

% tamanios(+Piezas,-Tamanios), funcion que dada una lista de piezas retorna una lista con los Tamaños
% de cada pieza.

tamanios([],[]).
tamanios([pieza(T,_)|L],LNATS):- tamanios(L,D), LNATS=[T|D]. 

%%%%%%%%%%%%%%%%%%%%%%%% 
%% Detalle
%%%%%%%%%%%%%%%%%%%%%%%%
%% generar toma una pieza entre las posibles, la agrega a la solucion y luego recursiona disminuyendo a T tanto como
%% el tamaño de la pieza agregada. Puesto que puede superar el umbral que queremos, requiere pedir T>0.
%% Utiliza tamanios como inversa de resumenPiezas, generando una secuencia de tamaños. De esta forma maneja las 
%% cantidades como repeticiones.
%%%%%%%%%%%%%%%%%%%%%%%%
%% Ejemplo de uso
%%%%%%%%%%%%%%%%%%%%%%%%
%% ?- nPiezasDeCada(2,[1,2,3],P), generar(5,P,SOL).
%% P = [pieza(1, 2), pieza(2, 2), pieza(3, 2)],
%% SOL = [1, 1, 1, 1, 1] ;
%% P = [pieza(1, 2), pieza(2, 2), pieza(3, 2)],
%% SOL = [1, 1, 1, 2] ;
%% P = [pieza(1, 2), pieza(2, 2), pieza(3, 2)],
%% SOL = [1, 1, 2, 1] ;
%% P = [pieza(1, 2), pieza(2, 2), pieza(3, 2)],
%% SOL = [1, 1, 3] ;
%% P = [pieza(1, 2), pieza(2, 2), pieza(3, 2)],
%% SOL = [1, 2, 1, 1] ;
%% P = [pieza(1, 2), pieza(2, 2), pieza(3, 2)],
%% SOL = [1, 2, 2] ;
%% P = [pieza(1, 2), pieza(2, 2), pieza(3, 2)],
%% SOL = [1, 3, 1] ;
%% P = [pieza(1, 2), pieza(2, 2), pieza(3, 2)],
%% SOL = [2, 1, 1, 1] ;
%% P = [pieza(1, 2), pieza(2, 2), pieza(3, 2)],
%% SOL = [2, 1, 2] ;
%% P = [pieza(1, 2), pieza(2, 2), pieza(3, 2)],
%% SOL = [2, 2, 1] ;
%% P = [pieza(1, 2), pieza(2, 2), pieza(3, 2)],
%% SOL = [2, 3] ;
%% P = [pieza(1, 2), pieza(2, 2), pieza(3, 2)],
%% SOL = [3, 1, 1] ;
%% P = [pieza(1, 2), pieza(2, 2), pieza(3, 2)],
%% SOL = [3, 2] ;
%% false.
%%%%%%%%%%%%%%%%%%%%%%%%

%%% Ejercicio 5 

% cumpleLímite(+Piezas,+Solución) será verdadero cuando la cantidad de piezas utilizadas en Solución 
%  no exceda las cantidades disponibles indicadas en Piezas

cumpleLimite(PIEZAS,SOL):- resumenPiezas(SOL,SOLPIEZAS), cumpleLimiteAux(PIEZAS,SOLPIEZAS).

% cumpleLimiteAux(+Piezas,+PiezasSolucion), será verdadero cuando la cantidad de piezas utilizadas en PiezasSolucion 
% no exceda las cantidades disponibles indicadas en Piezas. Asumimos que una pieza del mismo tamaño no aparece dos veces en la solucion.
% Es decir, la solucion no va a tener algo del estilo S=[pieza(1,1), pieza(1,1)] sino que va a ser S=[pieza(1,2)]. 

cumpleLimiteAux(_,[]).
cumpleLimiteAux(PIEZAS,[S|SOL]):- noSuperaLimite(S,PIEZAS), cumpleLimiteAux(PIEZAS,SOL).

% noSuperaLimite(+PiezaAChequear,+Piezas), sera verdadero cuando la cantidad de piezas de piezaAChequear con tamaño T sea <=
% a la cantidad disponible de piezas de tamaño T. 

noSuperaLimite(pieza(T,C),[pieza(T,C2)|_]):- C2>=C, !.
noSuperaLimite(pieza(T,C),[pieza(T2,_)|PIEZAS]):- T=\=T2, noSuperaLimite(pieza(T,C),PIEZAS).

%%%%%%%%%%%%%%%%%%%%%%%% 
%% Detalle
%%%%%%%%%%%%%%%%%%%%%%%%
%% simplemente verifica que no se utilicen mas piezas de las disponibles recursionando sobre las piezas utilizadas
%% en la solucion, aprovechando que por tamaño tenemos la cantidad de piezas asociadas. Como se dijo, se asume que
%% todas las piezas del mismo tamaño T estan consideradas en una sola CANT en pieza(T,CANT).
%% Luego, una vez que se encuentra la pieza tamaño T en las piezas disponibles, si se utilizan a los sumo tantas piezas
%% como se disponga, se hace un cut para eliminar ramas que ya no interesan.
%%%%%%%%%%%%%%%%%%%%%%%%
%% Ejemplo de uso
%%%%%%%%%%%%%%%%%%%%%%%%
%% ejemploCumpleLimite1 :- nPiezasDeCada(2,[3,1,2],P), cumpleLimite(P,[3,2]).
%%
%% Resultado:
%% True
%%
%% ejemploCumpleLimite2 :- nPiezasDeCada(2,[3,1,2],P), cumpleLimite(P,[1,1,1,1,1]).
%%
%% Resultado:
%% False
%%%%%%%%%%%%%%%%%%%%%%%%

%%% Ejercicio 6

% construir1(+Total,+Piezas,-Solución), donde Solución representa una lista de piezas cuyos valores 
%  suman Total y, además, las cantidades utilizadas de cada pieza no exceden los declarados en Piezas.

construir1(T,P,SOL):- generar(T,P,SOL), cumpleLimite(P,SOL).

%%%%%%%%%%%%%%%%%%%%%%%% 
%% Detalle
%%%%%%%%%%%%%%%%%%%%%%%%
%% sin demasiado detalle, construir1 al mejor estilo generate&test genera todas las posibles soluciones con los
%% tamaños que disponemos y luego checkea por cada solucion que cumpla con la disponibilidad.
%%%%%%%%%%%%%%%%%%%%%%%%
%% Ejemplo de uso
%%%%%%%%%%%%%%%%%%%%%%%% 
%% ejemploConstruir1(SOL) :- nPiezasDeCada(2,[3,1,2],P), construir1(5,P,SOL).
%%
%% Resultado:
%% SOL = [3, 1, 1] ;
%% SOL = [3, 2] ;
%% SOL = [1, 3, 1] ;
%% SOL = [1, 1, 3] ;
%% SOL = [1, 2, 2] ;
%% SOL = [2, 3] ;
%% SOL = [2, 1, 2] ;
%% SOL = [2, 2, 1] ;
%% false.
%%%%%%%%%%%%%%%%%%%%%%%%

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
generar2(T,P,K,SOL):- T>0, K>0, L is K-1, generar2(T,P,L,SOL), assert(lookUp(T,P,K,SOL)).
generar2(T,P,K,SOL):- T>0, dameMax(P,K,L), M is T-L, between(0,M,T1), T2 is T-L-T1, L2 is L-1, generar2(T1,P,L2,SOL1), generar2(T2,P,L,SOL2), append(SOL1, [K|SOL2], SOL), assert(lookUp(T,P,K,SOL)).


:- dynamic lookUp/4.

dameMax(P,K,L):- tamanios(P,TS), sort(TS,TSORD), reverse(TSORD, REV), dameMaxAux(REV,K,L).

dameMaxAux([X|_],K,L):- X=<K, L=X, !. 
dameMaxAux([X|REV],K,L):- X>K, dameMaxAux(REV,K,L).

%%%%%%%%%%%%%%%%%%%%%%%% 
%% Detalle
%%%%%%%%%%%%%%%%%%%%%%%%
%% En esta solucion alternativa para construir, se desglosa el generar2 en tres casos:
%% - la solucion ya existe en la base de conocimientos
%% - la solucion utiliza una pieza de a lo sumo tamaño K, a izquierda las piezas son mas chicas
%% - la solucion no utiliza una pieza de tamaño a lo sumo K y por ende se recursiona con K-1
%%%%%%%%%%%%%%%%%%%%%%%%
%% Ejemplo de uso
%%%%%%%%%%%%%%%%%%%%%%%% 
%% 
%%%%%%%%%%%%%%%%%%%%%%%%

% ####################################
% Comparación de resultados y tiempos
% ####################################

%%% Ejercicio 8

% todosConstruir1(+Total, +Piezas, -Soluciones, -N), donde Soluciones representa una lista con todas las
%  soluciones de longitud Total obtenidas con construir1/3, y N indica la cantidad de soluciones totales.

todosConstruir1(T,P,SOL,N):- aggregate_all(count,construir1(T,P,SOL),N). 

%%%%%%%%%%%%%%%%%%%%%%%% 
%% Detalle
%%%%%%%%%%%%%%%%%%%%%%%%
%% utiliza el predicado aggregate_all para contar las soluciones de construir1 de tamaño T en N
%%%%%%%%%%%%%%%%%%%%%%%%
%% Ejemplo de uso
%%%%%%%%%%%%%%%%%%%%%%%% 
%% ejemploTodosConstruir1(N) :- nPiezasDeCada(2,[3,1,2],P), todosConstruir1(5,P,_,N).
%% Resultado:
%% N = 8 ;
%% false.
%%%%%%%%%%%%%%%%%%%%%%%%

%%% Ejercicio 9

% todosConstruir2(+Total, +Piezas, -Soluciones, -N), donde Soluciones representa una lista con todas 
%  las soluciones de longitud Total obtenidas con construir2/3, y N indica la cantidad de soluciones totales.

todosConstruir2(T,P,SOL,N):- aggregate_all(count,construir2(T,P,SOL),N). 

%%%%%%%%%%%%%%%%%%%%%%%% 
%% Detalle
%%%%%%%%%%%%%%%%%%%%%%%%
%% utiliza el predicado aggregate_all para contar las soluciones de construir2 de tamaño T en N 
%%%%%%%%%%%%%%%%%%%%%%%%
%% Ejemplo de uso
%%%%%%%%%%%%%%%%%%%%%%%% 
%% ejemploTodosConstruir2(N) :- nPiezasDeCada(2,[3,1,2],P), todosConstruir2(5,P,_,N).
%% Resultado:
%% N = 8 ;
%% false.
%%%%%%%%%%%%%%%%%%%%%%%%

% ####################################
% Patrones
% ####################################

%%% Ejercicio 10

% construirConPatron(+Total, +Piezas, ?Patrón, -Solución) será verdadero cuando Solución sea una solución factible 
%  en los términos definidos anteriormente y, además, sus piezas respeten el patrón indicado en Patrón. 
%  Se sugiere definir un predicado tienePatrón(+Lista, ?Patrón) que decida si Lista presenta el Patrón especificado.

construirConPatron(T,P,PATRON,SOL):- construir1(T,P,SOL), tienePatron(SOL,PATRON). 

% tienePatron(+Lista,?Patron). La idea es guardar el patron en caso de que la lista tenga mayor longitud.

tienePatron(LISTA,PATRON) :- tienePatronAux(PATRON,PATRON,LISTA).

% tienePatronAux(?Patron,?Patron,+Lista). La idea es consumir el patron y la lista en simultaneo.
% Si el elemento del patron esta instanciado entonces hay que verificar que coincida con el elemento
% de la lista. Caso contrario se unifica el elemento del patron con el elemento de la lista.
% Si la lista con el patron fue consumida pero la lista no se encuentra vacia entonces volvemos a comenzar
% con lo que queda de patron.
 
tienePatronAux(_,[],[]).
tienePatronAux(PATRON,[],LISTA) :- tienePatronAux(PATRON,PATRON,LISTA).
tienePatronAux(PATRON,[P|PS],[X|XS]):- tienePatronAux(PATRON,PS,XS), asignarOChequear(P,X).

% asignarOChequear(?P,+X). Unifica X con P si P no esta instanciada. Compara X con P si
% P esta instanciado.

asignarOChequear(P,X):- var(P), X=P.
asignarOChequear(P,X):- not(var(P)), X==P. 

%%%%%%%%%%%%%%%%%%%%%%%% 
%% Detalle
%%%%%%%%%%%%%%%%%%%%%%%%
%% Para realizar camino3 se tomo como base camino2. Se hace uso de dos predicados dinamicos.
%% Para tener de forma dinamica la logitud del camino mas chico encontrado en cada momento 
%% se utiliza el predicado 'caminoMinimo' que solo tiene un parametro y ese valor.
%% Para realizar el calculo de la distancia de cada camino, es necesario el punto de inicio
%% y este se define en el predicado 'posInicial'. Se opto por este predicado para no pasar
%% mas parametros en otros predicados.
%% Para lograr una reduccion drastica del espacio de busqueda se utiliza el predicado 'evalRec'
%% que compara la longitud del camino parcial encontrado con la longitud del camino mas corto.
%% En caso de ser mayor el camino parcial, este predicado falla reduciendo el espacio de busqueda.
%% En caso que aun no se haya encontrado el primer camino y por ende no este definido 'caminoMinimo',
%% se continua con la busqueda.
%% Finalmente, cuando se encuentra un camino se actualiza la distancia del mejor camino segun
%% corresponda utilizando el predicado 'actualizaCamMin'.
%% La actualizacion solo se produce si la posicion pasada como parametro coincide con la posicion
%% inicial del camino (es decir no es un camino parcial) y la longitud pasada como parametro
%% no estaba definida o es menor que la anterior.
%%%%%%%%%%%%%%%%%%%%%%%%
%% Ejemplo de uso
%%%%%%%%%%%%%%%%%%%%%%%% 
%% ?- tablero(3,3,T), ocupar(pos(0,1),T), camino3(pos(0,0),pos(0,2),T,C).
%% C = [pos(0, 0), pos(1, 0), pos(1, 1), pos(1, 2), pos(0, 2)] ;
%% false.
%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%
% TESTS %
%%%%%%%%%

ejemploListaDeNats1(P) :- listaNats( 1, 20, D), nPiezasDeCada( 5, D, P).
ejemploListaDeNats2(P) :- listaNats( 1, 3, D_1_3), nPiezasDeCada( 2, D_1_3, P_1_3),
					listaNats( 4, 6, D_4_6), nPiezasDeCada( 5, D_4_6, P_4_6), append(P_1_3, P_4_6, P).

% Resultado:
% P = [pieza(3, 2), pieza(1, 2), pieza(2, 2)] 

ejemploNPiezasDeCada(P) :- nPiezasDeCada(2,[3,1,2],P).

% Resultado:
% P = [pieza(1, 2), pieza(2, 2), pieza(3, 2), pieza(4, 5), pieza(5, 5), pieza(6, 5)] ;

ejemploResumenPiezas(P) :- resumenPiezas([2,2,1,2,3,10,1,5,1,10], P).

% Resultado:
% P = [pieza(1, 3), pieza(2, 3), pieza(3, 1), pieza(5, 1), pieza(10, 2)] 

ejemploGenerar(SOL) :- nPiezasDeCada(2,[3,1,2],P), generar(5,P,SOL).

% Resultado:
% SOL = [3, 1, 1] ;
% SOL = [3, 2] ;
% SOL = [1, 3, 1] ;
% SOL = [1, 1, 3] ;
% SOL = [1, 1, 1, 1, 1] ;
% SOL = [1, 1, 1, 2] ;
% SOL = [1, 1, 2, 1] ;
% SOL = [1, 2, 1, 1] ;
% SOL = [1, 2, 2] ;
% SOL = [2, 3] ;
% SOL = [2, 1, 1, 1] ;
% SOL = [2, 1, 2] ;
% SOL = [2, 2, 1] ;
% false.

ejemploCumpleLimite1 :- nPiezasDeCada(2,[3,1,2],P), cumpleLimite(P,[3,2]).

% Resultado:
% True

ejemploCumpleLimite2 :- nPiezasDeCada(2,[3,1,2],P), cumpleLimite(P,[1,1,1,1,1]).

% Resultado:
% False

ejemploConstruir1(SOL) :- nPiezasDeCada(2,[3,1,2],P), construir1(5,P,SOL).

% Resultado:
% SOL = [3, 1, 1] ;
% SOL = [3, 2] ;
% SOL = [1, 3, 1] ;
% SOL = [1, 1, 3] ;
% SOL = [1, 2, 2] ;
% SOL = [2, 3] ;
% SOL = [2, 1, 2] ;
% SOL = [2, 2, 1] ;
% false.

ejemploConstruir2(SOL) :- nPiezasDeCada(2,[3,1,2],P), construir2(5,P,SOL).

% Resultado: FALTA!

ejemploTodosConstruir1(N) :- nPiezasDeCada(2,[3,1,2],P), todosConstruir1(5,P,_,N).

% Resultado:
% N = 8 ;
% false.

ejemploTodosConstruir2(N) :- nPiezasDeCada(2,[3,1,2],P), todosConstruir2(5,P,_,N).

% Resultado:
% N = 8 ;
% false.

ejemploTimeConstruir1 :- time(ejemploConstruir1(_)).

% Resultado: FALTA!

ejemploTimeConstruir2 :- time(ejemploConstruir2(_)).

% Resultado: FALTA!

