:- consult(esqueleto).

:- begin_tests(esqueleto).

%% Tableros de test
tableroTest(tableroTest1,T) :- tablero(5,5,T), ocupar(pos(1,1),T), ocupar(pos(1,2),T).

%% Test tablero
test(tablero) :- tablero(2,2,T), length(T,2), nth0(0,T,F1), length(F1,2), nth0(1,T,F2), length(F2,2), !.

%% Test ocupar
test(ocupar) :- tablero(2,2,T), ocupar(pos(0,0),T), nth0(0,T,F1), nth0(0,F1,CELDA), CELDA==ocupada, !.

%% Test vecino
test(vecino) :- tableroTest(tableroTest1,T), findall(V,vecino(pos(0,0),T,V),VS), length(VS,2), member(pos(0,1),VS), member(pos(1,0),VS), !.
test(vecino) :- tableroTest(tableroTest1,T), findall(V,vecino(pos(3,1),T,V),VS), length(VS,4), member(pos(2,1),VS), member(pos(4,1),VS), member(pos(3,0),VS), member(pos(3,2),VS), !.

%% Test vecinoLibre
test(vecinoLibre) :- tableroTest(tableroTest1,T), findall(V,vecinoLibre(pos(0,1),T,V),VS), length(VS,2), member(pos(0,0),VS), member(pos(0,2),VS), !.

%% Tests camino
%% Verifica que la cantidad de caminos de camino sea correcta.
test(camino) :- tableroTest(tableroTest1,T), cantidadDeCaminos(pos(0,0), pos(2,3), T, N), N is 287, !.

%% Tests camino2
%% Verifica que la cantidad de caminos de camino2 sea correcta.
test(camino2) :- tableroTest(tableroTest1,T), cantidadDeCaminos2(pos(0,0), pos(2,3), T, N), N is 287, !.

%% Tests camino3
%% Verifica que camino3 no devuelva soluciones peores a las ya encontradas.
test(camino3) :- tableroTest(tableroTest1,T), camino3(pos(0,0), pos(2,3), T, C), length(C,L), not(L>6), !.

:- end_tests(esqueleto).

:- run_tests.

%% tableroTest(tableroTest1, T) :- tablero(5,5,T), ocupar(pos(1,3),T), ocupar(pos(2,3),T), ocupar(pos(3,3),T), ocupar(pos(4,3),T).
%% solucion(solucionTest1, C) :- C = [pos(0,0),pos(1,0),pos(2,0),pos(3,0),pos(4,0),pos(4,1),pos(4,2),pos(3,2),pos(2,2),pos(1,2),pos(0,2),pos(0,3),pos(0,4),pos(1,4),pos(2,4),pos(3,4),pos(4,4)].
