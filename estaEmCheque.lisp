(defun verificaSePeca (caracter)
    (find caracter "tTcCbBdDrRpP"))

(defun defineCasaVazia (i)
    (cond ((<= i 0) nil)
    (t (cons nil (defineCasaVazia (- i 1))))))

(defun loopExtrairLinhas (i nlinhas)
    (cond ((>= i nlinhas) nil)
        (t (format t "Digite a linha ~a: " i)
            (finish-output)
            (let ((linha (read-line)))
                (write linha)
                (terpri)
                (append (list linha) (loopExtrairLinhas (+ i 1) nlinhas))))))

(defun defineLinha (i posicaoLeitura stringLinha)
    (cond ((>= i 8) nil)
        ((verificaSePeca (char stringLinha posicaoLeitura))
        (cons (char stringLinha posicaoLeitura) (defineLinha (+ i 1) (+ posicaoLeitura 1) stringLinha)))     
            (t(let ((n (digit-char-p (char stringLinha posicaoLeitura))))
                (append (defineCasaVazia n) (defineLinha (+ i n) (+ posicaoLeitura 1) stringLinha))))))

(defun defineMatriz (i linhas)
    (cond ((>= i 8) nil)
    (t (let ((stringLinha (nth i linhas)))
        (cons (defineLinha 0 0 stringLinha) (defineMatriz (+ i 1) linhas))))))

(defun buscaColunaRei (i linha)
  (cond ((>= i 8) nil)
        ((eql (nth i linha) #\r) i)
        (t (buscaColunaRei (+ i 1) linha ))))

(defun buscaReiBranco (i matriz)
    (cond ((>= i 8) nil)
    (t (let ((c (buscaColunaRei 0 (nth i matriz))))
            (cond (c (list i c))
            (t (buscaReiBranco (+ i 1) matriz )))))))

(defun verificaPecaBuscada (i linha peca)
    (cond((eql (nth i linha) peca) peca)
    (t nil)))

(defun buscaDiagonalEsquerdaSuperior (linha coluna matriz)
    (cond ((< linha 0) nil)
    ((< coluna 0) nil)
    ((null (nth coluna (nth linha matriz)))(buscaDiagonalEsquerdaSuperior (- linha 1) (- coluna 1) matriz))
    ((verificaPecaBuscada coluna (nth linha matriz) #\D) #\D)
    ((verificaPecaBuscada coluna (nth linha matriz) #\B) #\B)
    (t nil)))

(defun buscaDiagonalEsquerdaInferior (linha coluna matriz)
    (cond ((> linha 7) nil)
    ((< coluna 0) nil)
    ((null (nth coluna (nth linha matriz)))(buscaDiagonalEsquerdaInferior (+ linha 1) (- coluna 1) matriz))
    ((verificaPecaBuscada coluna (nth linha matriz) #\D) #\D)
    ((verificaPecaBuscada coluna (nth linha matriz) #\B) #\B)
    (t nil)))

(defun buscaDiagonalDiereitaSuperior (linha coluna matriz)
    (cond ((< linha 0) nil)
    ((> coluna 7) nil)
    ((null (nth coluna (nth linha matriz)))(buscaDiagonalDireitaSuperior (- linha 1) (+ coluna 1) matriz))
    ((verificaPecaBuscada coluna (nth linha matriz) #\D) #\D)
    ((verificaPecaBuscada coluna (nth linha matriz) #\B) #\B)
    (t nil)))

(defun buscaDiagonalDiereitaSInferior (linha coluna matriz)
    (cond ((> linha 7) nil)
    ((> coluna 7) nil)
    ((null (nth coluna (nth linha matriz)))(buscaDiagonalDireitaInferior (+ linha 1) (+ coluna 1) matriz))
    ((verificaPecaBuscada coluna (nth linha matriz) #\D) #\D)
    ((verificaPecaBuscada coluna (nth linha matriz) #\B) #\B)
    (t nil)))

(defun buscaBispoRainha (posicaoRei matriz)
    (cond   ((buscaDiagonalEsquerdaSuperior (- (nth 0 posicaoRei) 1) (- (nth 1 posicaoRei) 1) matriz))
            ((buscaDiagonalEsquerdaInferior (+ (nth 0 posicaoRei) 1) (- (nth 1 posicaoRei) 1) matriz))
            ((buscaDiagonalDireitaSuperior (- (nth 0 posicaoRei) 1) (+ (nth 1 posicaoRei) 1) matriz))
            ((buscaDiagonalDireitaInferior (+ (nth 0 posicaoRei) 1) (+ (nth 1 posicaoRei) 1) matriz))
            (t nil)))

(defun buscaDireita (linha coluna matriz)
    (cond ((> coluna 7) nil)
    ((null (nth coluna (nth linha matriz)))(buscaDireita linha (+ coluna 1) matriz))
    ((verificaPecaBuscada coluna (nth linha matriz) #\D) #\D)
    ((verificaPecaBuscada coluna (nth linha matriz) #\T) #\T)
    (t nil)))

(defun buscaEsquerda (linha coluna matriz)
    (cond ((< coluna 0) nil)
    ((null (nth coluna (nth linha matriz)))(buscaEsquerda linha (- coluna 1) matriz))
    ((verificaPecaBuscada coluna (nth linha matriz) #\D) #\D)
    ((verificaPecaBuscada coluna (nth linha matriz) #\T) #\T)
    (t nil)))

(defun buscaCima (linha coluna matriz)
    (cond ((< linha 0) nil)
    ((null (nth coluna (nth linha matriz)))(buscaCima (- linha 1) coluna matriz))
    ((verificaPecaBuscada coluna (nth linha matriz) #\D) #\D)
    ((verificaPecaBuscada coluna (nth linha matriz) #\T) #\T)
    (t nil)))

(defun buscaBaixo (linha coluna matriz)
    (cond ((> linha 7) nil)
    ((null (nth coluna (nth linha matriz)))(buscaBaixo (+ linha 1) coluna matriz))
    ((verificaPecaBuscada coluna (nth linha matriz) #\D) #\D)
    ((verificaPecaBuscada coluna (nth linha matriz) #\T) #\T)
    (t nil)))

(defun buscaTorreRaina (posicaoRei matriz)
    (cond   ((buscaDireita (nth 0 posicaoRei) (+ (nth 1 posicaoRei) 1) matriz))
            ((buscaCima (- (nth 0 posicaoRei) 1) (nth 1 posicaoRei) matriz))
            ((buscaEsquerda (nth 0 posicaoRei) (- (nth 1 posicaoRei) 1) matriz))
            ((buscaBaixo (+ (nth 0 posicaoRei) 1) (nth 1 posicaoRei) matriz))
            (t nil)))

(defun buscaPeaoLado (linha coluna matriz)
    (cond ((< coluna 0) nil)
    ((> coluna 7) nil)
    (t (verificaPecaBuscada coluna (nth linha matriz) #\P))))

(defun buscaPeao (posicaoRei matriz)
    (let ((linha (- (nth 0 posicaoRei) 1))
    (colunaEsq (- (nth 1 posicaoRei) 1))
    (colunaDir (+ (nth 1 posicaoRei) 1)))
    (cond ((< linha 0) nil)
    (t (cond ((buscaPeaoLado linha colunaEsq matriz) #\P)
        (t (buscaPeaoLado linha colunaDir matriz)))))))

;(defun buscaCavalo)




(defvar *linhas* (loopExtrairLinhas 0 8))
(defvar *matriz* (defineMatriz 0 *linhas*))
(defvar *posicaoRei* (buscaReiBranco 0 *matriz*))


; https://www.tutorialspoint.com/lisp/lisp_variables.htm\
; https://www.tutorialspoint.com/lisp/lisp_input_output.htm
; https://www.tutorialspoint.com/lisp/lisp_string_access_char.htm
; https://stackoverflow.com/questions/52241906/check-if-character-is-in-string
