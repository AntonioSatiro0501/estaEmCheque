preenche_vazio(0,[]).
preenche_vazio(N,['.'|X]):- N2 is N - 1, preenche_vazio(N2,X).

checaSePeca('r').
checaSePeca('t').
checaSePeca('c').
checaSePeca('b').
checaSePeca('d').
checaSePeca('p').
checaSePeca('R').
checaSePeca('T').
checaSePeca('C').
checaSePeca('B').
checaSePeca('D').
checaSePeca('P').

preenche_linha([],[]).
preenche_linha([C|X],[C|R]):- checaSePeca(C), preenche_linha(X,R).
preenche_linha([C|X],R2):- atom_number(C,N), preenche_vazio(N,B), append(B,R,R2), preenche_linha(X,R).

extraiCaracteres(N, [D]):- integer(N), atom_number(D, N).
extraiCaracteres(S, C):- string_chars(S, C).

preenche_tabuleiro([],[]).
preenche_tabuleiro([L|X],[LC|R]):- extraiCaracteres(L,C), preenche_linha(C,LC), preenche_tabuleiro(X,R).

procuraReiLinha(['R'|_],Nlinha,Ncoluna,[Nlinha,Ncoluna]).
procuraReiLinha([_|X],Nlinha, Ncoluna, R):- NC2 is Ncoluna + 1, procuraReiLinha(X,Nlinha,NC2,R).

procuraReiTabuleiro([L|_],Nlinha,R):- procuraReiLinha(L,Nlinha,1,R).
procuraReiTabuleiro([_|X],Nlinha,R):- N2 is Nlinha + 1, procuraReiTabuleiro(X,N2,R).

devolveLinha([X|_], 1, X).
devolveLinha([X|Y], N, R):- N2 is N - 1, devolveLinha(Y, N2, R).

div(['R'|Y], [], Y).
div([X|Y], [X|W], Z) :- div(Y, W, Z).

inv([], []).
inv([C|X], R) :- inv(X, R1), append(R1, [C], R).

procuraLinhaEsq(N, T, R):- devolveLinha(T, N, L), div(L, AntesRei, _), inv(AntesRei, LinhaInvertida), auxProcuraLinha(LinhaInvertida, R).
procuraLinhaDir(N, T, R):- devolveLinha(T,N,L), div(L, _, DepoisRei), auxProcuraLinha(DepoisRei, R).
auxProcuraLinha(['.'|X], R):- auxProcuraLinha(X,R).
auxProcuraLinha(['d'|_], 'd').
auxProcuraLinha(['t'|_], 't').

procuraColunaCima(LR, CR, T, R):- L2 is LR - 1, devolveLinha(T, L2, L), auxProcuraColunaCima(L, 1, CR, T, L2, R).
auxProcuraColunaCima(['d'|_], C, C, _, _, 'd').
auxProcuraColunaCima(['t'|_], C, C, _, _, 't').
auxProcuraColunaCima(['.'|_], C, C, T, LN, R):- procuraColunaCima(LN, C, T, R).
auxProcuraColunaCima([_|Y], C, Cr, T, LN, R):- C < Cr, C2 is C + 1, auxProcuraColunaCima(Y, C2, Cr, T, LN, R).

procuraColunaBaixo(LR, CR, T, R):- L2 is LR + 1, devolveLinha(T, L2, L), auxProcuraColunaBaixo(L, 1, CR, T, L2, R).
auxProcuraColunaBaixo(['d'|_], C, C, _, _, 'd').
auxProcuraColunaBaixo(['t'|_], C, C, _, _, 't').
auxProcuraColunaBaixo(['.'|_], C, C, T, LN, R):- procuraColunaBaixo(LN, C, T, R).
auxProcuraColunaBaixo([_|Y], C, Cr, T, LN, R):- C < Cr, C2 is C + 1, auxProcuraColunaBaixo(Y, C2, Cr, T, LN, R).

procuraDiagonalEsqSup(LR, CR, T, R):- L2 is LR - 1, C2 is CR - 1, devolveLinha(T, L2, L), auxProcuraDiagonalEsqSup(L, 1, C2, T, L2, R).
auxProcuraDiagonalEsqSup(['d'|_], C, C, _, _, 'd').
auxProcuraDiagonalEsqSup(['b'|_], C, C, _, _, 'b').
auxProcuraDiagonalEsqSup(['.'|_], C, C, T, LN, R):- procuraDiagonalEsqSup(LN, C, T, R).
auxProcuraDiagonalEsqSup([_|Y], C, Cr, T,LN, R):- C < Cr, C2 is C + 1, auxProcuraDiagonalEsqSup(Y, C2, Cr, T, LN, R).

procuraDiagonalEsqInf(LR, CR, T, R):- L2 is LR + 1, C2 is CR - 1, devolveLinha(T, L2, L), auxProcuraDiagonalEsqInf(L, 1, C2, T, L2, R).
auxProcuraDiagonalEsqInf(['d'|_], C, C, _, _, 'd').
auxProcuraDiagonalEsqInf(['b'|_], C, C, _, _, 'b').
auxProcuraDiagonalEsqInf(['.'|_], C, C, T, LN, R):- procuraDiagonalEsqInf(LN, C, T, R).
auxProcuraDiagonalEsqInf([_|Y], C, Cr, T,LN, R):- C < Cr, C2 is C + 1, auxProcuraDiagonalEsqInf(Y, C2, Cr, T, LN, R).

procuraDiagonalDirSup(LR, CR, T, R):- L2 is LR - 1, C2 is CR + 1, devolveLinha(T, L2, L), auxProcuraDiagonalDirSup(L, 1, C2, T, L2, R).
auxProcuraDiagonalDirSup(['d'|_], C, C, _, _, 'd').
auxProcuraDiagonalDirSup(['b'|_], C, C, _, _, 'b').
auxProcuraDiagonalDirSup(['.'|_], C, C, T, LN, R):- procuraDiagonalDirSup(LN, C, T, R).
auxProcuraDiagonalDirSup([_|Y], C, Cr, T,LN, R):- C < Cr, C2 is C + 1, auxProcuraDiagonalDirSup(Y, C2, Cr, T, LN, R).

procuraDiagonalDirInf(LR, CR, T, R):- L2 is LR + 1, C2 is CR + 1, devolveLinha(T, L2, L), auxProcuraDiagonalDirInf(L, 1, C2, T, L2, R).
auxProcuraDiagonalDirInf(['d'|_], C, C, _, _, 'd').
auxProcuraDiagonalDirInf(['b'|_], C, C, _, _, 'b').
auxProcuraDiagonalDirInf(['.'|_], C, C, T, LN, R):- procuraDiagonalDirInf(LN, C, T, R).
auxProcuraDiagonalDirInf([_|Y], C, Cr, T,LN, R):- C < Cr, C2 is C + 1, auxProcuraDiagonalDirInf(Y, C2, Cr, T, LN, R).

teste(T,R):- preenche_tabuleiro(T,T2), procuraReiTabuleiro(T2,1,R).
