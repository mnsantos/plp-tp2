:- consult(esqueleto).

:- begin_tests(basics).

test(camino2) :- tablero(ej1,T), camino2(pos(0,0), pos(2,2), T, [pos(0,0), pos(0,1), pos(0,2), pos(1,2), pos(2,3)]).

:- end_tests(basics).

/* The following directive runs the tests. You can also give the goal
   run_tests on the command line (without the :- part). */
:- run_tests.
