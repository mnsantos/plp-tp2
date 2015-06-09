%%%%%%%%%%%%%%%%%%%%%%%%
%% Tablero
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 1
%% tablero(+Filas,+Columnas,-Tablero) instancia una estructura de tablero en blanco
%% de Filas x Columnas, con todas las celdas libres.
tablero(1,C,T) :- length(COLS,C), T=[COLS].
tablero(F,C,T) :- F>1, C>0, F2 is F-1, length(COLS,C), tablero(F2,C,T2), T=[COLS|T2].
%%%%%%%%%%%%%%%%%%%%%%%%
%% Ejemplo de uso
%%%%%%%%%%%%%%%%%%%%%%%%
%% ?- tablero(3,3,T).
%% T = [[_G4763, _G4766, _G4769], [_G4772, _G4775, _G4778], [_G4781, _G4784, _G4787]] ;
%% false.
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 2
%% ocupar(+Pos,?Tablero) será verdadero cuando la posición indicada esté ocupada.
ocupar(pos(F,C),T) :- nth0(F,T,FILA), nth0(C,FILA,CELDA), CELDA=ocupada.
%%%%%%%%%%%%%%%%%%%%%%%%
%% Ejemplo de uso
%%%%%%%%%%%%%%%%%%%%%%%%
%% ?- tablero(3,3,T), ocupar(pos(0,1),T).
%% T = [[_G529, ocupada, _G535], [_G538, _G541, _G544], [_G547, _G550, _G553]] ;
%% false.
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 3
%% vecino(+Pos, +Tablero, -PosVecino) será verdadero cuando PosVecino sea
%% un átomo de la forma pos(F', C') y pos(F',C') sea una celda contigua a
%% pos(F,C), donde Pos=pos(F,C). Las celdas contiguas puede ser a lo sumo cuatro
%% dado que el robot se moverá en forma ortogonal.
vecino(pos(F,C),T,pos(NORTE,C)) :-  NORTE is F-1, nth0(NORTE,T,FIL), nth0(C,FIL,_).
vecino(pos(F,C),T,pos(SUR,C)) :-  SUR is F+1, nth0(SUR,T,FIL), nth0(C,FIL,_).
vecino(pos(F,C),T,pos(F,ESTE)) :-  ESTE is C+1, nth0(F,T,FIL), nth0(ESTE,FIL,_).
vecino(pos(F,C),T,pos(F,OESTE)) :-  OESTE is C-1, nth0(F,T,FIL), nth0(OESTE,FIL,_).
%%%%%%%%%%%%%%%%%%%%%%%%
%% Ejemplo de uso
%%%%%%%%%%%%%%%%%%%%%%%%
%% ?- tablero(3,3,T), ocupar(pos(0,1),T), vecino(pos(0,0),T,V).
%% V = pos(1, 0) ;
%% V = pos(0, 1) ;
%% false.
%% ?- tablero(3,3,T), ocupar(pos(0,1),T), vecino(pos(1,1),T,V).
%% V = pos(0, 1) ;
%% V = pos(2, 1) ;
%% V = pos(1, 2) ;
%% V = pos(1, 0) ;
%% false.
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 4
%% vecinoLibre(+Pos, +Tablero, -PosVecino) idem vecino/3 pero además PosVecino
%% debe ser una celda transitable (no ocupada) en el Tablero
vecinoLibre(P,T,pos(F,C)) :- vecino(P,T,pos(F,C)), nth0(F,T,FIL), nth0(C,FIL,CEL), CEL\==ocupada.
%%%%%%%%%%%%%%%%%%%%%%%%
%% Ejemplo de uso
%%%%%%%%%%%%%%%%%%%%%%%%
%% ?- tablero(3,3,T), ocupar(pos(0,1),T), vecinoLibre(pos(1,1),T,V).
%% V = pos(2, 1) ;
%% V = pos(1, 2) ;
%% V = pos(1, 0) ;
%% false.
%%%%%%%%%%%%%%%%%%%%%%%%

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
ocupada(pos(F,C),T) :- nth0(F,T,FIL), nth0(C,FIL,CEL), CEL==ocupada.
camino(P,P,T,[P]) :- not(ocupada(P,T)).
camino(INI,FIN,T,CAM) :- INI\==FIN, ocupar(INI,T), vecinoLibre(INI,T,VECINO), camino(VECINO,FIN,T,CAM2), CAM=[INI|CAM2].
%%%%%%%%%%%%%%%%%%%%%%%%
%% Ejemplo de uso
%%%%%%%%%%%%%%%%%%%%%%%%
%% ?- tablero(3,3,T), ocupar(pos(0,1),T), camino(pos(0,0),pos(0,2),T,C).
%% C = [pos(0, 0), pos(1, 0), pos(2, 0), pos(2, 1), pos(1, 1), pos(1, 2), pos(0, 2)] ;
%% C = [pos(0, 0), pos(1, 0), pos(2, 0), pos(2, 1), pos(2, 2), pos(1, 2), pos(0, 2)] ;
%% C = [pos(0, 0), pos(1, 0), pos(1, 1), pos(2, 1), pos(2, 2), pos(1, 2), pos(0, 2)] ;
%% C = [pos(0, 0), pos(1, 0), pos(1, 1), pos(1, 2), pos(0, 2)] ;
%% false.
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 6
%% cantidadDeCaminos(+Inicio, +Fin, +Tablero, ?N) que indique la cantidad de caminos
%% posibles sin ciclos entre Inicio y Fin.
cantidadDeCaminos(INI,FIN,T,N) :- aggregate_all(count,camino(INI,FIN,T,_),N). 
cantidadDeCaminos2(INI,FIN,T,N) :- aggregate_all(count,camino2(INI,FIN,T,_),N). 
cantidadDeCaminos3(INI,FIN,T,N) :- aggregate_all(count,camino3(INI,FIN,T,_),N).
cantidadDeCaminoDual(INI,FIN,T1,T2,N) :- aggregate_all(count,caminoDual(INI,FIN,T1,T2,_),N).
%%%%%%%%%%%%%%%%%%%%%%%%
%% Ejemplo de uso
%%%%%%%%%%%%%%%%%%%%%%%%

%% ?- tablero(3,3,T), ocupar(pos(0,1),T), cantidadDeCaminos(pos(0,0),pos(0,2),T,N).
%% N = 4 ;
%% false.
%% ?- tablero(3,3,T), ocupar(pos(0,1),T), cantidadDeCaminos(pos(0,0),pos(0,2),T,5).
%% false.
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 7
%% camino2(+Inicio, +Fin, +Tablero, -Camino) ídem camino/4 pero se espera una heurística
%% que mejore las soluciones iniciales.
%% No se espera que la primera solución sea necesariamente la mejor.
%% Una solución es mejor mientras menos pasos se deba dar para llegar a
%% destino (distancia Manhattan). Por lo tanto, el predicado deberá devolver de a uno,
%% todos los caminos pero en orden creciente de longitud.
camino2(P,P,T,[P]) :- not(ocupada(P,T)).
camino2(INI,FIN,T,CAM) :- INI\==FIN, ocupar(INI,T), mejorVecinoLibre(INI,FIN,T,VECINO), camino2(VECINO,FIN,T,CAM2), CAM=[INI|CAM2].

%% distancia(+Pos1, +Pos2, -Distancia)
distancia(pos(F1,C1), pos(F2,C2), D) :- abs(F1-F2,DIFF1), abs(C1-C2,DIFF2), D is DIFF1+DIFF2. 
  
%% mejorVecinoLibre(+Pos, +PosFinCamino, +Tablero, -PosVecinoLibre)
mejorVecinoLibre(POS,FIN,T,VL) :- findall(V,vecinoLibre(POS,T,V),LISTAVL), map_list_to_pairs(distancia(FIN),LISTAVL,PARESVL), 
                                  keysort(PARESVL,ORDENVL), pairs_values(ORDENVL,VECINOS), member(VL,VECINOS).
%%%%%%%%%%%%%%%%%%%%%%%%
%% Ejemplo de uso
%%%%%%%%%%%%%%%%%%%%%%%% 
%% ?- tablero(3,3,T), ocupar(pos(0,1),T), camino2(pos(0,0),pos(0,2),T,C).
%% C = [pos(0, 0), pos(1, 0), pos(1, 1), pos(1, 2), pos(0, 2)] ;
%% C = [pos(0, 0), pos(1, 0), pos(1, 1), pos(2, 1), pos(2, 2), pos(1, 2), pos(0, 2)] ;
%% C = [pos(0, 0), pos(1, 0), pos(2, 0), pos(2, 1), pos(1, 1), pos(1, 2), pos(0, 2)] ;
%% C = [pos(0, 0), pos(1, 0), pos(2, 0), pos(2, 1), pos(2, 2), pos(1, 2), pos(0, 2)] ;
%% false.
%%%%%%%%%%%%%%%%%%%%%%%%                                

%% Ejercicio 8
%% camino3(+Inicio, +Fin, +Tablero, -Camino) ídem camino2/4 pero se espera que
%% se reduzca drásticamente el espacio de búsqueda.
%% En el proceso de generar los potenciales caminos, se pueden ir sacando algunas conclusiones.
%% Por ejemplo, si se está en la celda (3,4) y se dieron ya 6 pasos desde el Inicio,
%% entonces no tiene sentido seguir evaluando cualquier camino que implique llegar a la celda (3,4)
%% desde Inicio en más de 6 pasos.
%% Notar que dos ejecuciones de camino3/4 con los mismos argumentos deben dar los mismos resultados.
%% En este ejercicio se permiten el uso de predicados: dynamic/1, asserta/1, assertz/1 y retractall/1.
camino3(INI,FIN,T,CAM) :- retractall(caminoMinimo(_)), retractall(posInicial(_)), assert(posInicial(INI)), cam3(INI,FIN,T,CAM,1).

%% cam3(+Inicio, +Fin, +Tablero, -Camino, +LongitudCamino)
cam3(P,P,_,[P],_).
cam3(INI,FIN,T,CAM,L) :- INI\==FIN, evalRec(L), ocupar(INI,T), mejorVecinoLibre(INI,FIN,T,VECINO), L2 is L+1, cam3(VECINO,FIN,T,CAM2,L2), CAM=[INI|CAM2], length(CAM,LEN), actualizaCamMin(INI,LEN).

%% evalRec(+LongitudCamino)
evalRec(L) :- caminoMinimo(LEN), LEN=<L, fail.
evalRec(L) :- caminoMinimo(LEN), LEN>L.
evalRec(_) :- not(caminoMinimo(_)).

%% actualizaCamMin(+PosInicioCamino, +LongitudCamino)
actualizaCamMin(P,_) :- caminoMinimo(_), not(posInicial(P)).
actualizaCamMin(P,L) :- caminoMinimo(LEN), posInicial(P), LEN>=L, retract(caminoMinimo(LEN)), assert(caminoMinimo(L)).
actualizaCamMin(P,_) :- not(caminoMinimo(_)), not(posInicial(P)).
actualizaCamMin(P,L) :- not(caminoMinimo(_)), posInicial(P), assert(caminoMinimo(L)).

%% posInicial(?PosInicial)
:- dynamic posInicial/1.

%% caminoMinimo(?LongitudCamino)
:- dynamic caminoMinimo/1.
%%%%%%%%%%%%%%%%%%%%%%%%
%% Ejemplo de uso
%%%%%%%%%%%%%%%%%%%%%%%% 
%% ?- tablero(3,3,T), ocupar(pos(0,1),T), camino3(pos(0,0),pos(0,2),T,C).
%% C = [pos(0, 0), pos(1, 0), pos(1, 1), pos(1, 2), pos(0, 2)] ;
%% false.
%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%
%% Tableros simultáneos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 9
%% caminoDual(+Inicio, +Fin, +Tablero1, +Tablero2, -Camino) será verdadero
%% cuando Camino sea un camino desde Inicio hasta Fin pasando al mismo tiempo
%% sólo por celdas transitables de ambos tableros.
%% Nota: Es posible una implementación que resuelva en forma inmediata casos en los que trivialmente no existe camino dual posible.
caminoDual(P,P,_,_,[P]).
caminoDual(INI,FIN,T1,T2,CAM) :- INI\==FIN, ocupar(INI,T1), mejorVecinoLibre(INI,FIN,T1,VECINO), posLibre(VECINO,T2), caminoDual(VECINO,FIN,T1,T2,CAM2), CAM=[INI|CAM2].

posLibre(pos(F,C),T) :- nth0(F,T,FIL), nth0(C,FIL,CEL), CEL\==ocupada.
%%%%%%%%%%%%%%%%%%%%%%%%
%% Ejemplo de uso
%%%%%%%%%%%%%%%%%%%%%%%%
%% ?- tablero(3,3,T), ocupar(pos(0,1),T), tablero(3,3,T2), ocupar(pos(2,1),T2), caminoDual(pos(0,0),pos(0,2),T,T2,C).
%% C = [pos(0, 0), pos(1, 0), pos(1, 1), pos(1, 2), pos(0, 2)] ;
%% false.
%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%
%% Tableros de ejemplo
%%%%%%%%%%%%%%%%%%%%%%%%

%% Vacio de 3x3
tablero(ej1,T) :- tablero(3,3,T).

%% Ocupado solo el centro de 3x3
tablero(ej2,T) :- tablero(3,3,T), ocupar(pos(1,1),T).

%% Ocupado el centro y derecha centro de 3x3
tablero(ej3,T) :- tablero(3,3,T), ocupar(pos(1,1),T), ocupar(pos(1,2),T).

%% Figura 1 de 5x5
tablero(ej5x5,T) :- tablero(5,5,T), ocupar(pos(1,1),T), ocupar(pos(1,2),T).

%% Figura 1 de 5x5 con un ocupado mas
tablero(ej5x5b,T) :- tablero(5,5,T), ocupar(pos(1,1),T), ocupar(pos(1,2),T), ocupar(pos(1,3),T).

%% Tablero 5x5 vacio
tablero(ej5x5vacio,T) :- tablero(5,5,T).

%% Tablero 2x2 Ocupado
tablero(ejOcupado,T) :- tablero(2,2,T), ocupar(pos(0,0),T), ocupar(pos(0,1),T), ocupar(pos(1,0),T), ocupar(pos(1,1),T).
