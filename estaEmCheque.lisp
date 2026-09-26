; Antônio Costa Satiro de Souza 10723636

(defun verificaSePeca (caracter) ; verifica se o caracter é uma peça 
    (find caracter "tTcCbBdDrRpP"))

(defun defineCasaVazia (i) ; Constroi uma casa vazia no tabuleiro
    (cond ((<= i 0) nil)
    (t (cons nil (defineCasaVazia (- i 1))))))

(defun defineLinha (i posicaoLeitura stringLinha) ; Controi as linhas do tabuleiro a partir do input
    (cond ((>= i 8) nil)
    ((verificaSePeca (char stringLinha posicaoLeitura))
    (cons (char stringLinha posicaoLeitura) (defineLinha (+ i 1) (+ posicaoLeitura 1) stringLinha)))     
    (t(let ((n (digit-char-p (char stringLinha posicaoLeitura))))
        (append (defineCasaVazia n) (defineLinha (+ i n) (+ posicaoLeitura 1) stringLinha))))))

(defun defineMatriz (i linhas) ; Constroi a matriz do tabuleiro
    (cond ((>= i 8) nil)
    (t (let ((stringLinha (nth i linhas)))
        (cons (defineLinha 0 0 stringLinha) (defineMatriz (+ i 1) linhas))))))

(defun buscaColunaRei (i linha) ; Busca coluna do rei branco
    (cond ((>= i 8) nil)
    ((eql (nth i linha) #\r) i)
    (t (buscaColunaRei (+ i 1) linha ))))

(defun buscaReiBranco (i matriz) ; Busca posição do rei branco
    (cond ((>= i 8) nil)
    (t (let ((c (buscaColunaRei 0 (nth i matriz))))
        (cond (c (list i c))
        (t (buscaReiBranco (+ i 1) matriz )))))))

(defun verificaPecaBuscada (i linha peca) ; Verifica se a posicao definida possui a peça buscada
    (cond((eql (nth i linha) peca) peca)
    (t nil)))

(defun buscaDiagonalEsquerdaSuperior (linha coluna matriz) ; Auxiliar de buscaBispoRainha
    (cond ((< linha 0) nil)
    ((< coluna 0) nil)
    ((null (nth coluna (nth linha matriz)))(buscaDiagonalEsquerdaSuperior (- linha 1) (- coluna 1) matriz))
    ((verificaPecaBuscada coluna (nth linha matriz) #\D) #\D)
    ((verificaPecaBuscada coluna (nth linha matriz) #\B) #\B)
    (t nil)))

(defun buscaDiagonalEsquerdaInferior (linha coluna matriz) ; Auxiliar de buscaBispoRainha
    (cond ((> linha 7) nil)
    ((< coluna 0) nil)
    ((null (nth coluna (nth linha matriz)))(buscaDiagonalEsquerdaInferior (+ linha 1) (- coluna 1) matriz))
    ((verificaPecaBuscada coluna (nth linha matriz) #\D) #\D)
    ((verificaPecaBuscada coluna (nth linha matriz) #\B) #\B)
    (t nil)))

(defun buscaDiagonalDireitaSuperior (linha coluna matriz) ; Auxiliar de buscaBispoRainha
    (cond ((< linha 0) nil)
    ((> coluna 7) nil)
    ((null (nth coluna (nth linha matriz)))(buscaDiagonalDireitaSuperior (- linha 1) (+ coluna 1) matriz))
    ((verificaPecaBuscada coluna (nth linha matriz) #\D) #\D)
    ((verificaPecaBuscada coluna (nth linha matriz) #\B) #\B)
    (t nil)))

(defun buscaDiagonalDireitaInferior (linha coluna matriz) ; Auxiliar de buscaBispoRainha
    (cond ((> linha 7) nil)
    ((> coluna 7) nil)
    ((null (nth coluna (nth linha matriz)))(buscaDiagonalDireitaInferior (+ linha 1) (+ coluna 1) matriz))
    ((verificaPecaBuscada coluna (nth linha matriz) #\D) #\D)
    ((verificaPecaBuscada coluna (nth linha matriz) #\B) #\B)
    (t nil)))

(defun buscaBispoRainha (posicaoRei matriz) ; Busca por possíveis peças nas diagonais do rei
    (cond((buscaDiagonalEsquerdaSuperior (- (car posicaoRei) 1) (- (cadr posicaoRei) 1) matriz))
    ((buscaDiagonalEsquerdaInferior (+ (car posicaoRei) 1) (- (cadr posicaoRei) 1) matriz))
    ((buscaDiagonalDireitaSuperior (- (car posicaoRei) 1) (+ (cadr posicaoRei) 1) matriz))
    ((buscaDiagonalDireitaInferior (+ (car posicaoRei) 1) (+ (cadr posicaoRei) 1) matriz))
    (t nil)))

(defun buscaDireita (linha coluna matriz) ; Auxiliar de buscaTorreRaina
    (cond ((> coluna 7) nil)
    ((null (nth coluna (nth linha matriz)))(buscaDireita linha (+ coluna 1) matriz))
    ((verificaPecaBuscada coluna (nth linha matriz) #\D) #\D)
    ((verificaPecaBuscada coluna (nth linha matriz) #\T) #\T)
    (t nil)))

(defun buscaEsquerda (linha coluna matriz) ; Auxiliar de buscaTorreRaina
    (cond ((< coluna 0) nil)
    ((null (nth coluna (nth linha matriz)))(buscaEsquerda linha (- coluna 1) matriz))
    ((verificaPecaBuscada coluna (nth linha matriz) #\D) #\D)
    ((verificaPecaBuscada coluna (nth linha matriz) #\T) #\T)
    (t nil)))

(defun buscaCima (linha coluna matriz) ; Auxiliar de buscaTorreRaina
    (cond ((< linha 0) nil)
    ((null (nth coluna (nth linha matriz)))(buscaCima (- linha 1) coluna matriz))
    ((verificaPecaBuscada coluna (nth linha matriz) #\D) #\D)
    ((verificaPecaBuscada coluna (nth linha matriz) #\T) #\T)
    (t nil)))

(defun buscaBaixo (linha coluna matriz) ; Auxiliar de buscaTorreRaina
    (cond ((> linha 7) nil)
    ((null (nth coluna (nth linha matriz)))(buscaBaixo (+ linha 1) coluna matriz))
    ((verificaPecaBuscada coluna (nth linha matriz) #\D) #\D)
    ((verificaPecaBuscada coluna (nth linha matriz) #\T) #\T)
    (t nil)))

(defun buscaTorreRaina (posicaoRei matriz) ; Busca por possíveis peças nas retas do rei
    (cond ((buscaDireita (car posicaoRei) (+ (cadr posicaoRei) 1) matriz))
    ((buscaCima (- (car posicaoRei) 1) (cadr posicaoRei) matriz))
    ((buscaEsquerda (car posicaoRei) (- (cadr posicaoRei) 1) matriz))
    ((buscaBaixo (+ (car posicaoRei) 1) (cadr posicaoRei) matriz))
    (t nil)))

(defun buscaPeaoLado (linha coluna matriz) ; Auxílio para checar as duas possíveis casas do peão
    (cond ((< coluna 0) nil)
    ((> coluna 7) nil)
    (t (verificaPecaBuscada coluna (nth linha matriz) #\P))))

(defun buscaPeao (posicaoRei matriz) ; Busca todas as posições possíveis de peão
    (let ((linha (- (car posicaoRei) 1))
    (colunaEsq (- (cadr posicaoRei) 1))
    (colunaDir (+ (cadr posicaoRei) 1)))
    (cond ((< linha 0) nil)
    (t (cond ((buscaPeaoLado linha colunaEsq matriz) #\P)
        (t (buscaPeaoLado linha colunaDir matriz)))))))

(defun buscaCasaCavalo (linha coluna matriz) ; Verifica se posicao é legal para busca de cavalos
    (cond ((< linha 0) nil)
    ((> linha 7) nil)
    ((< coluna 0) nil)
    ((> coluna 7) nil)
    (t (verificaPecaBuscada coluna (nth linha matriz) #\C))))

(defun buscaCavalo (posicaoRei matriz) ; Verifica todas as posições do cavalo
    (let ((linha (car posicaoRei)) (coluna (cadr posicaoRei)))
    (cond ((buscaCasaCavalo (- linha 2) (- coluna 1) matriz) #\C)
    ((buscaCasaCavalo (- linha 2) (+ coluna 1) matriz) #\C)
    ((buscaCasaCavalo (+ linha 2) (- coluna 1) matriz) #\C)
    ((buscaCasaCavalo (+ linha 2) (+ coluna 1) matriz) #\C)
    ((buscaCasaCavalo (- linha 1) (- coluna 2) matriz) #\C)
    ((buscaCasaCavalo (- linha 1) (+ coluna 2) matriz) #\C)
    ((buscaCasaCavalo (+ linha 1) (- coluna 2) matriz) #\C)
    ((buscaCasaCavalo (+ linha 1) (+ coluna 2) matriz) #\C)
    (t nil))))

(defun estaEmCheque (posicaoRei matriz) ; Verifica se o rei esta em cheque
    (cond ((buscaCavalo posicaoRei matriz) "Esta em cheque")
    ((buscaPeao posicaoRei matriz) "Esta em cheque")
    ((buscaTorreRaina posicaoRei matriz) "Esta em cheque")
    ((buscaBispoRainha posicaoRei matriz) "Esta em cheque")
    (t nil)))

(defun chess (linhas) ; Recebe a lista de linhas do tabuleiro e retorna t ou nil se o rei branco esta em xeque
    (let* ((matriz (defineMatriz 0 linhas))
    (posicaoRei (buscaReiBranco 0 matriz)))
    (cond ((estaEmCheque posicaoRei matriz) t)
    (t nil))))

; Funcionamento:
;
;
; Pecas:    t -> torre branca
;           T -> torre preta
;           c -> cavalo branco
;           C -> cavalo preto
;           b -> bispo branco
;           B -> bispo preto
;           d -> dama branca
;           D -> dama preta
;           r -> rei branco
;           R -> rei preto (não considerado durante análise do em cheque)
;           p -> peao branco
;           P -> peao preto
;
; Tabuleiro deve ser interpretado com as peças pretas acima e as brancas na parte inferior
; Exemplo entrada:
;
; (chess (list "TCBDRBCT" "PPPPPPPP" "8" "8" "8" "8" "pppppppp" "tcbdrbct"))
;
; TCBDRBCT representa a linha 0, topo do tabuleiro
; tcbdrbct representa a linha 7, fim do tabuleiro
; 
; *ATENCAO* colocar aspas idependentemente da linha ter entrada composta apenas de valores numéricos 
; Escrever list para que a lista das linhas não sejam interpretadas como uma função
;
; Outros exemplo de entrada:
;
;(chess (list "8" "8" "8" "2P5" "3r4" "8" "8" "8")) -> está em cheque por peão
;(chess (list "8" "8" "8" "2p5" "3r4" "8" "8" "8")) -> não está em cheque
;(chess (list "8" "8" "8" "26" "3r4" "1C6" "8" "8")) -> está em cheque por cavalo
;
; Lista de comandos para iniciar código via codeSpace Github:   sbcl
;                                                               (load "estaEmCheque.lisp")
;                                                               (chess (list "TCBDRBCT" "PPPPPPPP" "8" "8" "8" "8" "pppppppp" "tcbdrbct"))
;
; Fontes:
;
; https://www.tutorialspoint.com/lisp/lisp_variables.htm\
; https://www.tutorialspoint.com/lisp/lisp_input_output.htm
; https://www.tutorialspoint.com/lisp/lisp_string_access_char.htm
; https://stackoverflow.com/questions/52241906/check-if-character-is-in-string
