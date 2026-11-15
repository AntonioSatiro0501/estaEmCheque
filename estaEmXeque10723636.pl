/*
  EP2 - Paradigmas de Programação - Projeto 2: Está em xeque?
  Antônio Costa Satiro de Souza

  Como executar via SWI-Prolog:
  
    swipl -s estaEmXeque10723636.pl
    ?- chess(["tcbdrbct","pppppppp",8,8,8,8,"PPPPPPPP","TCBDRBCT"]).
    
    Exemplos de Entrada:
    
    ---------- Em Xeque ----------
    
    chess(["r7",8,8,8,"t3R3",8,8,8]). 
    chess(["r7",8,8,8,"3R3t",8,8,8]). 
    chess([8,"4R3",8,8,8,8,8,"r3d3"]). 
    chess([8,"1b6",8,8,"4R3",8,8,"7r"]). 
    chess([8,8,"3c4",8,"4R3",8,8,"r7"]). 
    chess(["r7",8,8,"5p2","4R3",8,8,8]). 
    chess([8,8,8,8,"3rR3",8,8,8]). 
    
    ---------- Não em Xeque ----------
    
    chess(["r7",8,8,"4p3","4R3",8,8,8]). 
    chess(["tcbdrbct","pppppppp",8,8,8,8,"PPPPPPPP","TCBDRBCT"]).
    
*/

% ---------- Preencher espaços sem peças ----------
preenche_vazio(0, []).
preenche_vazio(N, ['.'|X]) :- N > 0, N2 is N - 1, preenche_vazio(N2, X).

% ---------- Checa se char é peça ou int ----------
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

% ---------- Preenche linha do tabuleiro ----------
preenche_linha([], []).
preenche_linha([C|X], [C|R]) :- checaSePeca(C), preenche_linha(X, R).
preenche_linha([C|X], R2) :- atom_number(C, N), preenche_vazio(N, B), append(B, R, R2), preenche_linha(X, R).

% ---------- Transforma String em chars ou int ----------
extraiCaracteres(N, [D]) :- integer(N), !, atom_number(D, N).
extraiCaracteres(S, C) :- string_chars(S, C).

% ---------- Preenche tabuleiro ----------
preenche_tabuleiro([], []).
preenche_tabuleiro([L|X], [LC|R]) :- extraiCaracteres(L, C), preenche_linha(C, LC), preenche_tabuleiro(X, R).

% ---------- Procura rei branco na linha ----------
procuraReiLinha(['R'|_], Nlinha, Ncoluna, [Nlinha, Ncoluna]).
procuraReiLinha([_|X], Nlinha, Ncoluna, R) :- NC2 is Ncoluna + 1, procuraReiLinha(X, Nlinha, NC2, R).

% ---------- Procura rei branco no tabuleiro ----------
procuraReiTabuleiro([L|_], Nlinha, R) :- procuraReiLinha(L, Nlinha, 1, R).
procuraReiTabuleiro([_|X], Nlinha, R) :- N2 is Nlinha + 1, procuraReiTabuleiro(X, N2, R).

% ---------- Devolve linha de indice solicitado ----------
devolveLinha([X|_], 1, X).
devolveLinha([_|Y], N, R) :- N > 1, N2 is N - 1, devolveLinha(Y, N2, R).

% ---------- Divide Linha de acordo com o rei branco ----------
div(['R'|Y], [], Y).
div([X|Y], [X|W], Z) :- div(Y, W, Z).

% ---------- Inverte linha ----------
inv([], []).
inv([C|X], R) :- inv(X, R1), append(R1, [C], R).

% ---------- Linhas (torre / dama) ----------
procuraLinhaEsq(LR, T, R) :- devolveLinha(T, LR, L), div(L, AR, _), inv(AR, LI), auxProcuraLinha(LI, R). % Inverte linha para garantir leitura da casa mais proxima do rei até o fim do tabuleiro
procuraLinhaDir(LR, T, R) :- devolveLinha(T, LR, L), div(L, _, DR), auxProcuraLinha(DR, R).
auxProcuraLinha(['.'|X], R) :- auxProcuraLinha(X, R). % Avanca para proxima coluna
auxProcuraLinha(['d'|_], 'd').
auxProcuraLinha(['t'|_], 't').

% ---------- Colunas (torre / dama) ----------
procuraColunaCima(LR, CR, T, R) :- L2 is LR - 1, devolveLinha(T, L2, L), auxProcuraColunaCima(L, 1, CR, T, L2, R).
auxProcuraColunaCima(['d'|_], C, C, _, _, 'd').
auxProcuraColunaCima(['t'|_], C, C, _, _, 't').
auxProcuraColunaCima(['.'|_], C, C, T, LN, R) :- procuraColunaCima(LN, C, T, R). % Avanca para proxima linha
auxProcuraColunaCima([_|Y], C, Cr, T, LN, R) :- C < Cr, C2 is C + 1, auxProcuraColunaCima(Y, C2, Cr, T, LN, R). % Avanca para proxima coluna

procuraColunaBaixo(LR, CR, T, R) :- L2 is LR + 1, devolveLinha(T, L2, L), auxProcuraColunaBaixo(L, 1, CR, T, L2, R).
auxProcuraColunaBaixo(['d'|_], C, C, _, _, 'd').
auxProcuraColunaBaixo(['t'|_], C, C, _, _, 't').
auxProcuraColunaBaixo(['.'|_], C, C, T, LN, R) :- procuraColunaBaixo(LN, C, T, R). % Avanca para proxima linha
auxProcuraColunaBaixo([_|Y], C, Cr, T, LN, R) :- C < Cr, C2 is C + 1, auxProcuraColunaBaixo(Y, C2, Cr, T, LN, R). % Avanca para proxima coluna

% ---------- Diagonais (bispo / dama) ----------
% Verificacao separando tabuleiro em 4 direcoes onde o rei seria o centro delas
procuraDiagonalEsqSup(LR, CR, T, R) :- L2 is LR - 1, C2 is CR - 1, devolveLinha(T, L2, L), auxProcuraDiagonalEsqSup(L, 1, C2, T, L2, R).
auxProcuraDiagonalEsqSup(['d'|_], C, C, _, _, 'd').
auxProcuraDiagonalEsqSup(['b'|_], C, C, _, _, 'b').
auxProcuraDiagonalEsqSup(['.'|_], C, C, T, LN, R) :- procuraDiagonalEsqSup(LN, C, T, R). % Avanca para proxima linha
auxProcuraDiagonalEsqSup([_|Y], C, Cr, T, LN, R) :- C < Cr, C2 is C + 1, auxProcuraDiagonalEsqSup(Y, C2, Cr, T, LN, R). % Avanca para proxima coluna

procuraDiagonalEsqInf(LR, CR, T, R) :- L2 is LR + 1, C2 is CR - 1, devolveLinha(T, L2, L), auxProcuraDiagonalEsqInf(L, 1, C2, T, L2, R). 
auxProcuraDiagonalEsqInf(['d'|_], C, C, _, _, 'd').
auxProcuraDiagonalEsqInf(['b'|_], C, C, _, _, 'b').
auxProcuraDiagonalEsqInf(['.'|_], C, C, T, LN, R) :- procuraDiagonalEsqInf(LN, C, T, R). % Avanca para proxima linha
auxProcuraDiagonalEsqInf([_|Y], C, Cr, T, LN, R) :- C < Cr, C2 is C + 1, auxProcuraDiagonalEsqInf(Y, C2, Cr, T, LN, R).  % Avanca para proxima coluna

procuraDiagonalDirSup(LR, CR, T, R) :- L2 is LR - 1, C2 is CR + 1, devolveLinha(T, L2, L), auxProcuraDiagonalDirSup(L, 1, C2, T, L2, R).
auxProcuraDiagonalDirSup(['d'|_], C, C, _, _, 'd').
auxProcuraDiagonalDirSup(['b'|_], C, C, _, _, 'b').
auxProcuraDiagonalDirSup(['.'|_], C, C, T, LN, R) :- procuraDiagonalDirSup(LN, C, T, R). % Avanca para proxima linha
auxProcuraDiagonalDirSup([_|Y], C, Cr, T, LN, R) :- C < Cr, C2 is C + 1, auxProcuraDiagonalDirSup(Y, C2, Cr, T, LN, R). % Avanca para proxima coluna

procuraDiagonalDirInf(LR, CR, T, R) :- L2 is LR + 1, C2 is CR + 1, devolveLinha(T, L2, L), auxProcuraDiagonalDirInf(L, 1, C2, T, L2, R).
auxProcuraDiagonalDirInf(['d'|_], C, C, _, _, 'd').
auxProcuraDiagonalDirInf(['b'|_], C, C, _, _, 'b').
auxProcuraDiagonalDirInf(['.'|_], C, C, T, LN, R) :- procuraDiagonalDirInf(LN, C, T, R). % Avanca para proxima linha
auxProcuraDiagonalDirInf([_|Y], C, Cr, T, LN, R) :- C < Cr, C2 is C + 1, auxProcuraDiagonalDirInf(Y, C2, Cr, T, LN, R). % Avanca para proxima coluna

% ---------- Cavalo ----------
% Varre todo o tabuleiro em busca de Cavalos nas posições capazes de serem Xeque
procuraCavalo(L, C, LR, CR, T, R) :- L =:= LR - 2, C =:= CR - 1, devolveLinha(T, L, LT), auxBuscaCavalo(LT, C, R).
procuraCavalo(L, C, LR, CR, T, R) :- L =:= LR + 2, C =:= CR - 1, devolveLinha(T, L, LT), auxBuscaCavalo(LT, C, R).
procuraCavalo(L, C, LR, CR, T, R) :- L =:= LR - 2, C =:= CR + 1, devolveLinha(T, L, LT), auxBuscaCavalo(LT, C, R).
procuraCavalo(L, C, LR, CR, T, R) :- L =:= LR + 2, C =:= CR + 1, devolveLinha(T, L, LT), auxBuscaCavalo(LT, C, R).
procuraCavalo(L, C, LR, CR, T, R) :- L =:= LR - 1, C =:= CR - 2, devolveLinha(T, L, LT), auxBuscaCavalo(LT, C, R).
procuraCavalo(L, C, LR, CR, T, R) :- L =:= LR + 1, C =:= CR - 2, devolveLinha(T, L, LT), auxBuscaCavalo(LT, C, R).
procuraCavalo(L, C, LR, CR, T, R) :- L =:= LR - 1, C =:= CR + 2, devolveLinha(T, L, LT), auxBuscaCavalo(LT, C, R).
procuraCavalo(L, C, LR, CR, T, R) :- L =:= LR + 1, C =:= CR + 2, devolveLinha(T, L, LT), auxBuscaCavalo(LT, C, R).
procuraCavalo(L, 8, LR, CR, T, R) :- L < 8, L2 is L + 1, procuraCavalo(L2, 1, LR, CR, T, R).
procuraCavalo(L, C, LR, CR, T, R) :- C < 8, C2 is C + 1, procuraCavalo(L, C2, LR, CR, T, R).
auxBuscaCavalo(['c'|_], 1, 'c').
auxBuscaCavalo([_|Y], N, R) :- N > 1, N2 is N - 1, auxBuscaCavalo(Y, N2, R).

% ---------- Peão ----------
% Verifica as duas possiveis posicoes de xeque via peao
procuraPeao(LR, CR, T, R) :- L2 is LR - 1, CP1 is CR - 1, CP2 is CR + 1, devolveLinha(T, L2, LT), auxPeao(LT, CP1, CP2, R).
auxPeao(['p'|_], 1, _, 'p').
auxPeao(['p'|_], _, 1, 'p').
auxPeao([_|Y], P1, P2, R) :- P2 > 0, P11 is P1 - 1, P22 is P2 - 1, auxPeao(Y, P11, P22, R). % Avanca para proxima coluna

% ---------- Rei preto ----------
% Verifica as 3 linhas (2 tangentes ao rei branco e a linha a qual ele pertence) separadamente
procuraReiPreto(LR, CR, T, R) :- L2 is LR - 1, CP1 is CR - 1, CP2 is CR + 1, devolveLinha(T, L2, LT), auxReiPreto(LT, CP1, CR, CP2, R).
procuraReiPreto(LR, CR, T, R) :- L2 is LR + 1, CP1 is CR - 1, CP2 is CR + 1, devolveLinha(T, L2, LT), auxReiPreto(LT, CP1, CR, CP2, R).
procuraReiPreto(LR, CR, T, R) :- CP1 is CR - 1, CP2 is CR + 1, devolveLinha(T, LR, LT), auxReiPreto(LT, CP1, CR, CP2, R).
auxReiPreto(['r'|_], 1, _, _, 'r').
auxReiPreto(['r'|_], _, 1, _, 'r').
auxReiPreto(['r'|_], _, _, 1, 'r').
auxReiPreto([_|Y], P1, P2, P3, R) :- P3 > 0, P11 is P1 - 1, P22 is P2 - 1, P33 is P3 - 1, auxReiPreto(Y, P11, P22, P33, R). % Avanca para proxima coluna

% ---------- Xeque ----------
emXeque(T, LR, _)  :- procuraLinhaEsq(LR, T, _).
emXeque(T, LR, _)  :- procuraLinhaDir(LR, T, _).
emXeque(T, LR, CR) :- procuraColunaCima(LR, CR, T, _).
emXeque(T, LR, CR) :- procuraColunaBaixo(LR, CR, T, _).
emXeque(T, LR, CR) :- procuraDiagonalEsqSup(LR, CR, T, _).
emXeque(T, LR, CR) :- procuraDiagonalEsqInf(LR, CR, T, _).
emXeque(T, LR, CR) :- procuraDiagonalDirSup(LR, CR, T, _).
emXeque(T, LR, CR) :- procuraDiagonalDirInf(LR, CR, T, _).
emXeque(T, LR, CR) :- procuraCavalo(1, 1, LR, CR, T, _).
emXeque(T, LR, CR) :- procuraPeao(LR, CR, T, _).
emXeque(T, LR, CR) :- procuraReiPreto(LR, CR, T, _).

chess(TB) :- preenche_tabuleiro(TB, T), procuraReiTabuleiro(T, 1, [LR, CR]), emXeque(T, LR, CR).
