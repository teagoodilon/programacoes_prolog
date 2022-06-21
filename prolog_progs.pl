menorDeDois(A,B,A) :-
    A =< B.
menorDeDois(_,B,B).

menorDeTres(A,B,C,D) :-
    menorDeDois(A,B,E),
    menorDeDois(E,C,D).

pertence(C, [C|_]).
pertence(E, [_|R]) :-
    pertence(E, R).

nroOcorrencia(C, [C|R], Qtd) :- !,
    nroOcorrencia(C, R, NOCR),
    Qtd is 1 + NOCR.
nroOcorrencia(E, [_|R], Qtd) :-
    nroOcorrencia(E, R, Qtd).
nroOcorrencia(_, [], 0).

unicaOcorrencia(C, [C|R]) :- !,
    not(pertence(C, R)).                             %nroOcorrencia(Item, Lista, 1).
unicaOcorrencia(E, [_|R]) :-
    unicaOcorrencia(E, R).

maioresQue(N, [C|R], [C|Outros]) :-
    C > N, !,                           
    maioresQue(N, R, Outros).
maioresQue(N, [_|R], Outros) :-
    maioresQue(N, R, Outros).
maioresQue(_, [], []).

concatena([C|R], L,[C|Outros]) :-
    concatena(R, L, Outros).
concatena([], L, L).

remove(C, [C|R], R).
remove(E, [C|R], [C|RsemE]) :-
    remove(E, R, RsemE).

removeUltimo([C|R], [C|Outros]) :-
    removeUltimo(R, Outros).
removeUltimo([_], []).

removeTodos(C, [C|R], RsemC) :- !,  % corte porque linha 65 tbm usa [C|R]
    removeTodos(C, R, RsemC).
removeTodos(E, [C|R], [C|RsemE]) :-
    removeTodos(E, R, RsemE).
removeTodos(_, [], []).
    
removeRepetidos([C|R], [C|Outros]) :-
    removeTodos(C, R, RsemC),
    removeRepetidos(RsemC, Outros).
removeRepetidos([_], [_]).

menor([A], A).
menor([A,B|R], Menor) :-
    A < B, !,
    menor([A|R], Menor).
    

removeUltimaOc(E, [E|R], RsemE, sim) :-
    removeUltimaOc(E, R, RsemE, nao). 
removeUltimaOc(E, [E|R], [E|RsemE], sim) :- 
    removeUltimaOc(E, R, RsemE, sim), !.
removeUltimaOc(E, [C|R], [C|RsemE], sim) :-
    removeUltimaOc(E, R, RsemE, sim).
removeUltimaOc(_, [], [], nao).                 % usa corte quando tem casos que vao repetir