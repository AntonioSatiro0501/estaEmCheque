(defun verificaSePeca (caracter)
    (find caracter "tTcCbBdDrRpP"))

(defun defineCasaVazia (n)
    (cond ((<= n 0) nil)
        (t (cons nil (defineCasaVazia (- n 1))))))

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

(defvar *linhas* (loopExtrairLinhas 0 8))
(defvar *matriz* (defineMatriz 0 *linhas*))

; https://www.tutorialspoint.com/lisp/lisp_variables.htm\
; https://www.tutorialspoint.com/lisp/lisp_input_output.htm
; https://www.tutorialspoint.com/lisp/lisp_string_access_char.htm
; https://stackoverflow.com/questions/52241906/check-if-character-is-in-string