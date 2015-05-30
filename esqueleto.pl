%%%%%%%%%%%%%%%%%%%%%%%%
%% Tablero
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 1
%% tablero(+Filas,+Columnas,-Tablero) instancia una estructura de tablero en blanco
%% de Filas x Columnas, con todas las celdas libres.
tablero(1,C,T) :- length(COLS,C), T=[COLS].
tablero(F,C,T) :- F>1, C>0, F2 is F-1, length(COLS,C), tablero(F2,C,T2), T=[COLS|T2].

%% Ejercicio 2
%% ocupar(+Pos,?Tablero) será verdadero cuando la posición indicada esté ocupada.
ocupar(pos(F,C),T) :- nth0(F,T,FILA), nth0(C,FILA,CELDA), CELDA=ocupada.

%% Ejercicio 3
%% vecino(+Pos, +Tablero, -PosVecino) será verdadero cuando PosVecino sea
%% un átomo de la forma pos(F', C') y pos(F',C') sea una celda contigua a
%% pos(F,C), donde Pos=pos(F,C). Las celdas contiguas puede ser a lo sumo cuatro
%% dado que el robot se moverá en forma ortogonal.
vecino(pos(F,C),T,pos(NORTE,C)) :-  NORTE is F-1, nth0(NORTE,T,FIL), nth0(C,FIL,_).
vecino(pos(F,C),T,pos(SUR,C)) :-  SUR is F+1, nth0(SUR,T,FIL), nth0(C,FIL,_).
vecino(pos(F,C),T,pos(F,ESTE)) :-  ESTE is C+1, nth0(F,T,FIL), nth0(ESTE,FIL,_).
vecino(pos(F,C),T,pos(F,OESTE)) :-  OESTE is C-1, nth0(F,T,FIL), nth0(OESTE,FIL,_).

%% Ejercicio 4
%% vecinoLibre(+Pos, +Tablero, -PosVecino) idem vecino/3 pero además PosVecino
%% debe ser una celda transitable (no ocupada) en el Tablero
vecinoLibre(pos(F,C),T,pos(F2,C2)) :- vecino(pos(F,C),T,pos(F2,C2)), nth0(F2,T,FIL), nth0(C2,FIL,CEL), CEL\==ocupada.


%%%%%%%%%%%%%%%%%%%%%%%%
%% Definicion de caminos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 5
%% camino(+Inicio, +Fin, +Tablero, -Camino) será verdadero cuando Camino sea una lista
%% [pos(F1,C1), pos(F2,C2),..., pos(Fn,Cn)] que denoten un camino desde Inicio
%% hasta Fin pasando solo por celdas transitables.
%% Además se espera que Camino no contenga ciclos.
%% Notar que la cantidad de caminos es finita y por ende se tiene que poder recorrer
%% todas las alternativas eventualmente.
%% Consejo: Utilizar una lista auxiliar con las posiciones visitadas
camino(_,_,_,_).

%% Ejercicio 6
%% cantidadDeCaminos(+Inicio, +Fin, +Tablero, ?N) que indique la cantidad de caminos
%% posibles sin ciclos entre Inicio y Fin.
cantidadDeCaminos(_,_,_,_).

%% Ejercicio 7
%% camino2(+Inicio, +Fin, +Tablero, -Camino) ídem camino/4 pero se espera una heurística
%% que mejore las soluciones iniciales.
%% No se espera que la primera solución sea necesariamente la mejor.
%% Una solución es mejor mientras menos pasos se deba dar para llegar a
%% destino (distancia Manhattan). Por lo tanto, el predicado deberá devolver de a uno,
%% todos los caminos pero en orden creciente de longitud.
camino2(_,_,_,_).

%% Ejercicio 8
%% camino3(+Inicio, +Fin, +Tablero, -Camino) ídem camino2/4 pero se espera que
%% se reduzca drásticamente el espacio de búsqueda.
%% En el proceso de generar los potenciales caminos, se pueden ir sacando algunas conclusiones.
%% Por ejemplo, si se está en la celda (3,4) y se dieron ya 6 pasos desde el Inicio,
%% entonces no tiene sentido seguir evaluando cualquier camino que implique llegar a la celda (3,4)
%% desde Inicio en más de 6 pasos.
%% Notar que dos ejecuciones de camino3/4 con los mismos argumentos deben dar los mismos resultados.
%% En este ejercicio se permiten el uso de predicados: dynamic/1, asserta/1, assertz/1 y retractall/1.
camino3(_,_,_,_).


%%%%%%%%%%%%%%%%%%%%%%%%
%% Tableros simultáneos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 9
%% caminoDual(+Inicio, +Fin, +Tablero1, +Tablero2, -Camino) será verdadero
%% cuando Camino sea un camino desde Inicio hasta Fin pasando al mismo tiempo
%% sólo por celdas transitables de ambos tableros.
%% Nota: Es posible una implementación que resuelva en forma inmediata casos en los que trivialmente no existe camino dual posible.
caminoDual(_,_,_,_,_).


%%%%%%%%%%%%%%%%%%%%%%%%
%% Tableros de ejemplo
%%%%%%%%%%%%%%%%%%%%%%%%

%% Vacio de 3x3
tablero(ej1,T) :- tablero(3,3,T).

%% Ocupado solo el centro de 3x3
tablero(ej2,T) :- tablero(3,3,T), ocupar(pos(1,1),T).

%% Figura 1 de 5x5
tablero(ej5x5,T) :- tablero(5,5,T), ocupar(pos(1,1),T), ocupar(pos(1,2),T).
