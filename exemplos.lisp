;;; neste ficheiro podem encontrar 10 exemplos de tabuleiros com diferentes graus de complexidade
;;; atencao que nestes tabuleiros, o jogador 1 tem normalmente uma grande vantagem

;;; Tabuleiro t1
(defparameter t1 (cria-tabuleiro 2 3))
(tabuleiro-adiciona-moeda-posicao! t1 (cria-posicao 0 0) 5)
(tabuleiro-adiciona-moeda-posicao! t1 (cria-posicao 0 1) 3)
(tabuleiro-adiciona-moeda-posicao! t1 (cria-posicao 1 1) 1)
(tabuleiro-adiciona-moeda-posicao! t1 (cria-posicao 1 2) 6)
(tabuleiro-adiciona-moeda-posicao! t1 (cria-posicao 1 0) 1)
(tabuleiro-adiciona-fio! t1 (cria-posicao 0 1) (cria-posicao 0 0))
(tabuleiro-adiciona-fio! t1 (cria-posicao 1 0) (cria-posicao 0 0))
(tabuleiro-adiciona-fio! t1 (cria-posicao 1 1) (cria-posicao 0 0))
(tabuleiro-adiciona-fio! t1 (cria-posicao 1 0) (cria-posicao 0 1))
(tabuleiro-adiciona-fio! t1 (cria-posicao 0 1) (cria-posicao 1 1))
(tabuleiro-adiciona-fio! t1 (cria-posicao 1 0) (cria-posicao 1 1))
(tabuleiro-adiciona-fio! t1 (cria-posicao 1 2) (cria-posicao 0 1))
(tabuleiro-adiciona-fio! t1 (cria-posicao 1 1) (cria-posicao 1 2))


;;; Tabuleiro t2
(defparameter t2 (cria-tabuleiro 3 3))
(tabuleiro-adiciona-moeda-posicao! t2 (cria-posicao 0 0) 5)
(tabuleiro-adiciona-moeda-posicao! t2 (cria-posicao 0 1) 3)
(tabuleiro-adiciona-moeda-posicao! t2 (cria-posicao 0 2) 8)
(tabuleiro-adiciona-moeda-posicao! t2 (cria-posicao 1 0) 1)
(tabuleiro-adiciona-moeda-posicao! t2 (cria-posicao 1 1) 1)
(tabuleiro-adiciona-moeda-posicao! t2 (cria-posicao 1 2) 6)
(tabuleiro-adiciona-moeda-posicao! t2 (cria-posicao 2 0) 2)
(tabuleiro-adiciona-moeda-posicao! t2 (cria-posicao 2 1) 4)
(tabuleiro-adiciona-moeda-posicao! t2 (cria-posicao 2 2) 7)

(tabuleiro-adiciona-fio! t2 (cria-posicao 0 0) (cria-posicao 0 1))
(tabuleiro-adiciona-fio! t2 (cria-posicao 0 0) (cria-posicao 1 0))
(tabuleiro-adiciona-fio! t2 (cria-posicao 1 0) (cria-posicao 1 1))
(tabuleiro-adiciona-fio! t2 (cria-posicao 1 1) (cria-posicao 0 1))
(tabuleiro-adiciona-fio! t2 (cria-posicao 0 1) (cria-posicao 0 2))
(tabuleiro-adiciona-fio! t2 (cria-posicao 1 1) (cria-posicao 1 2))
(tabuleiro-adiciona-fio! t2 (cria-posicao 0 2) (cria-posicao 1 2))
(tabuleiro-adiciona-fio! t2 (cria-posicao 1 0) (cria-posicao 2 0))
(tabuleiro-adiciona-fio! t2 (cria-posicao 1 1) (cria-posicao 2 1))
(tabuleiro-adiciona-fio! t2 (cria-posicao 1 2) (cria-posicao 2 2))
(tabuleiro-adiciona-fio! t2 (cria-posicao 2 0) (cria-posicao 2 1))
(tabuleiro-adiciona-fio! t2 (cria-posicao 2 1) (cria-posicao 2 2))

;;; Tabuleiro t3
(defparameter t3 (copia-tabuleiro t1))
(tabuleiro-adiciona-moeda-posicao! t3 (cria-posicao 0 2) 2)
(tabuleiro-adiciona-fio! t3 (cria-posicao 0 1) (cria-posicao 0 2))
(tabuleiro-adiciona-fio! t3 (cria-posicao 0 2) (cria-posicao 1 2))
(tabuleiro-adiciona-fio! t3 (cria-posicao 1 1) (cria-posicao 0 2))

;;; Tabuleiro t4
(defparameter t4 (copia-tabuleiro t2))
(tabuleiro-adiciona-fio! t4 (cria-posicao 0 0) (cria-posicao 1 1))
(tabuleiro-adiciona-fio! t4 (cria-posicao 1 0) (cria-posicao 0 1))
(tabuleiro-adiciona-fio! t4 (cria-posicao 0 1) (cria-posicao 1 2))
(tabuleiro-adiciona-fio! t4 (cria-posicao 1 1) (cria-posicao 0 2))
(tabuleiro-adiciona-fio! t4 (cria-posicao 1 0) (cria-posicao 2 1))
(tabuleiro-adiciona-fio! t4 (cria-posicao 1 1) (cria-posicao 2 0))
(tabuleiro-adiciona-fio! t4 (cria-posicao 1 1) (cria-posicao 2 2))
(tabuleiro-adiciona-fio! t4 (cria-posicao 1 2) (cria-posicao 2 1))

;;; Tabuleiro t5
(defparameter t5 (cria-tabuleiro 3 5))
(tabuleiro-adiciona-moeda-posicao! t5 (cria-posicao 0 0) 1)
(tabuleiro-adiciona-moeda-posicao! t5 (cria-posicao 0 1) 1)
(tabuleiro-adiciona-moeda-posicao! t5 (cria-posicao 0 2) 1)
(tabuleiro-adiciona-moeda-posicao! t5 (cria-posicao 0 3) 6)
(tabuleiro-adiciona-moeda-posicao! t5 (cria-posicao 1 1) 1)
(tabuleiro-adiciona-moeda-posicao! t5 (cria-posicao 1 2) 5)
(tabuleiro-adiciona-moeda-posicao! t5 (cria-posicao 1 3) 5)
(tabuleiro-adiciona-moeda-posicao! t5 (cria-posicao 1 4) 5)
(tabuleiro-adiciona-moeda-posicao! t5 (cria-posicao 2 0) 1)
(tabuleiro-adiciona-moeda-posicao! t5 (cria-posicao 2 1) 7)
(tabuleiro-adiciona-moeda-posicao! t5 (cria-posicao 2 2) 7)
(tabuleiro-adiciona-moeda-posicao! t5 (cria-posicao 2 4) 1)
(tabuleiro-adiciona-fio! t5 (cria-posicao 0 0) (cria-posicao 0 1))
(tabuleiro-adiciona-fio! t5 (cria-posicao 0 1) (cria-posicao 0 2))
(tabuleiro-adiciona-fio! t5 (cria-posicao 0 1) (cria-posicao 1 1))
(tabuleiro-adiciona-fio! t5 (cria-posicao 1 1) (cria-posicao 2 1))
(tabuleiro-adiciona-fio! t5 (cria-posicao 2 1) (cria-posicao 2 0))
(tabuleiro-adiciona-fio! t5 (cria-posicao 2 1) (cria-posicao 2 2))
(tabuleiro-adiciona-fio! t5 (cria-posicao 2 2) (cria-posicao 1 2))
(tabuleiro-adiciona-fio! t5 (cria-posicao 1 2) (cria-posicao 0 3))
(tabuleiro-adiciona-fio! t5 (cria-posicao 1 4) (cria-posicao 0 3))
(tabuleiro-adiciona-fio! t5 (cria-posicao 1 4) (cria-posicao 1 3))
(tabuleiro-adiciona-fio! t5 (cria-posicao 1 2) (cria-posicao 1 3))
(tabuleiro-adiciona-fio! t5 (cria-posicao 1 4) (cria-posicao 2 4))



;;; Tabuleiro t6
(defparameter t6 (cria-tabuleiro 5 5))
(tabuleiro-adiciona-moeda-posicao! t6 (cria-posicao 0 0) 1)
(tabuleiro-adiciona-moeda-posicao! t6 (cria-posicao 0 1) 1)
(tabuleiro-adiciona-moeda-posicao! t6 (cria-posicao 0 2) 1)
(tabuleiro-adiciona-moeda-posicao! t6 (cria-posicao 0 3) 6)
(tabuleiro-adiciona-moeda-posicao! t6 (cria-posicao 1 1) 1)
(tabuleiro-adiciona-moeda-posicao! t6 (cria-posicao 1 2) 5)
(tabuleiro-adiciona-moeda-posicao! t6 (cria-posicao 1 3) 5)
(tabuleiro-adiciona-moeda-posicao! t6 (cria-posicao 1 4) 5)
(tabuleiro-adiciona-moeda-posicao! t6 (cria-posicao 2 0) 1)
(tabuleiro-adiciona-moeda-posicao! t6 (cria-posicao 2 1) 7)
(tabuleiro-adiciona-moeda-posicao! t6 (cria-posicao 2 2) 7)
(tabuleiro-adiciona-moeda-posicao! t6 (cria-posicao 2 4) 1)
(tabuleiro-adiciona-moeda-posicao! t6 (cria-posicao 3 0) 3)
(tabuleiro-adiciona-moeda-posicao! t6 (cria-posicao 3 1) 2)
(tabuleiro-adiciona-moeda-posicao! t6 (cria-posicao 3 2) 5)
(tabuleiro-adiciona-moeda-posicao! t6 (cria-posicao 3 3) 2)
(tabuleiro-adiciona-moeda-posicao! t6 (cria-posicao 3 4) 2)
(tabuleiro-adiciona-moeda-posicao! t6 (cria-posicao 4 0) 3)
(tabuleiro-adiciona-moeda-posicao! t6 (cria-posicao 4 1) 2)
(tabuleiro-adiciona-moeda-posicao! t6 (cria-posicao 4 2) 5)
(tabuleiro-adiciona-moeda-posicao! t6 (cria-posicao 4 3) 2)
(tabuleiro-adiciona-moeda-posicao! t6 (cria-posicao 4 4) 2)

(tabuleiro-adiciona-fio! t6 (cria-posicao 0 0) (cria-posicao 0 1))
(tabuleiro-adiciona-fio! t6 (cria-posicao 0 1) (cria-posicao 0 2))
(tabuleiro-adiciona-fio! t6 (cria-posicao 0 1) (cria-posicao 1 1))
(tabuleiro-adiciona-fio! t6 (cria-posicao 1 1) (cria-posicao 2 1))
(tabuleiro-adiciona-fio! t6 (cria-posicao 2 1) (cria-posicao 2 0))
(tabuleiro-adiciona-fio! t6 (cria-posicao 2 1) (cria-posicao 2 2))
(tabuleiro-adiciona-fio! t6 (cria-posicao 2 2) (cria-posicao 1 2))
(tabuleiro-adiciona-fio! t6 (cria-posicao 1 2) (cria-posicao 0 3))
(tabuleiro-adiciona-fio! t6 (cria-posicao 1 4) (cria-posicao 0 3))
(tabuleiro-adiciona-fio! t6 (cria-posicao 1 4) (cria-posicao 1 3))
(tabuleiro-adiciona-fio! t6 (cria-posicao 1 2) (cria-posicao 1 3))
(tabuleiro-adiciona-fio! t6 (cria-posicao 1 4) (cria-posicao 2 4))
(tabuleiro-adiciona-fio! t6 (cria-posicao 3 0) (cria-posicao 3 1))
(tabuleiro-adiciona-fio! t6 (cria-posicao 3 1) (cria-posicao 4 1))
(tabuleiro-adiciona-fio! t6 (cria-posicao 4 0) (cria-posicao 4 1))
(tabuleiro-adiciona-fio! t6 (cria-posicao 4 0) (cria-posicao 3 0))
(tabuleiro-adiciona-fio! t6 (cria-posicao 3 3) (cria-posicao 3 4))
(tabuleiro-adiciona-fio! t6 (cria-posicao 4 4) (cria-posicao 3 4))
(tabuleiro-adiciona-fio! t6 (cria-posicao 4 3) (cria-posicao 4 4))
(tabuleiro-adiciona-fio! t6 (cria-posicao 4 3) (cria-posicao 3 3))
(tabuleiro-adiciona-fio! t6 (cria-posicao 4 1) (cria-posicao 3 2))
(tabuleiro-adiciona-fio! t6 (cria-posicao 4 3) (cria-posicao 3 2))
(tabuleiro-adiciona-fio! t6 (cria-posicao 4 1) (cria-posicao 4 2))
(tabuleiro-adiciona-fio! t6 (cria-posicao 4 3) (cria-posicao 4 2))


;;; tabuleiro t7
;;; este e um dos poucos tabuleiro onde o jogador 1 nao tem vantagem
;;; aqui o melhor que cada jogador consegue fazer (contra um jogador
;;; optimo) e empatar o jogo
(defparameter t7 (cria-tabuleiro 2 6))
(tabuleiro-adiciona-moeda-posicao! t7 (cria-posicao 0 0) 1)
(tabuleiro-adiciona-moeda-posicao! t7 (cria-posicao 0 1) 1)
(tabuleiro-adiciona-moeda-posicao! t7 (cria-posicao 1 0) 1)
(tabuleiro-adiciona-moeda-posicao! t7 (cria-posicao 1 1) 1)
(tabuleiro-adiciona-moeda-posicao! t7 (cria-posicao 0 2) 2)
(tabuleiro-adiciona-moeda-posicao! t7 (cria-posicao 0 3) 3)
(tabuleiro-adiciona-moeda-posicao! t7 (cria-posicao 1 2) 3)
(tabuleiro-adiciona-moeda-posicao! t7 (cria-posicao 0 4) 3)
(tabuleiro-adiciona-moeda-posicao! t7 (cria-posicao 0 5) 3)
(tabuleiro-adiciona-moeda-posicao! t7 (cria-posicao 1 4) 3)
(tabuleiro-adiciona-moeda-posicao! t7 (cria-posicao 1 5) 3)

(tabuleiro-adiciona-fio! t7 (cria-posicao 0 0) (cria-posicao 0 1))
(tabuleiro-adiciona-fio! t7 (cria-posicao 0 0) (cria-posicao 1 0))
(tabuleiro-adiciona-fio! t7 (cria-posicao 0 1) (cria-posicao 1 1))
(tabuleiro-adiciona-fio! t7 (cria-posicao 1 1) (cria-posicao 1 0))
(tabuleiro-adiciona-fio! t7 (cria-posicao 0 2) (cria-posicao 0 3))
(tabuleiro-adiciona-fio! t7 (cria-posicao 0 3) (cria-posicao 1 2))
(tabuleiro-adiciona-fio! t7 (cria-posicao 1 2) (cria-posicao 0 2))
(tabuleiro-adiciona-fio! t7 (cria-posicao 0 4) (cria-posicao 0 5))
(tabuleiro-adiciona-fio! t7 (cria-posicao 0 4) (cria-posicao 1 4))
(tabuleiro-adiciona-fio! t7 (cria-posicao 0 5) (cria-posicao 1 5))
(tabuleiro-adiciona-fio! t7 (cria-posicao 1 5) (cria-posicao 1 4))


;;; Tabuleiro t8
(defparameter t8 (cria-tabuleiro 5 5))
(tabuleiro-adiciona-moeda-posicao! t8 (cria-posicao 0 0) 1)
(tabuleiro-adiciona-moeda-posicao! t8 (cria-posicao 0 1) 1)
(tabuleiro-adiciona-moeda-posicao! t8 (cria-posicao 0 2) 1)
(tabuleiro-adiciona-moeda-posicao! t8 (cria-posicao 0 3) 1)
(tabuleiro-adiciona-moeda-posicao! t8 (cria-posicao 0 4) 1)
(tabuleiro-adiciona-moeda-posicao! t8 (cria-posicao 1 0) 1)
(tabuleiro-adiciona-moeda-posicao! t8 (cria-posicao 1 1) 1)
(tabuleiro-adiciona-moeda-posicao! t8 (cria-posicao 1 2) 1)
(tabuleiro-adiciona-moeda-posicao! t8 (cria-posicao 1 3) 1)
(tabuleiro-adiciona-moeda-posicao! t8 (cria-posicao 1 4) 1)
(tabuleiro-adiciona-moeda-posicao! t8 (cria-posicao 2 0) 1)
(tabuleiro-adiciona-moeda-posicao! t8 (cria-posicao 2 1) 1)
(tabuleiro-adiciona-moeda-posicao! t8 (cria-posicao 2 2) 1)
(tabuleiro-adiciona-moeda-posicao! t8 (cria-posicao 2 3) 1)
(tabuleiro-adiciona-moeda-posicao! t8 (cria-posicao 2 4) 1)
(tabuleiro-adiciona-moeda-posicao! t8 (cria-posicao 3 0) 1)
(tabuleiro-adiciona-moeda-posicao! t8 (cria-posicao 3 1) 1)
(tabuleiro-adiciona-moeda-posicao! t8 (cria-posicao 3 2) 1)
(tabuleiro-adiciona-moeda-posicao! t8 (cria-posicao 3 3) 1)
(tabuleiro-adiciona-moeda-posicao! t8 (cria-posicao 3 4) 1)
(tabuleiro-adiciona-moeda-posicao! t8 (cria-posicao 4 0) 1)
(tabuleiro-adiciona-moeda-posicao! t8 (cria-posicao 4 1) 1)
(tabuleiro-adiciona-moeda-posicao! t8 (cria-posicao 4 2) 1)
(tabuleiro-adiciona-moeda-posicao! t8 (cria-posicao 4 3) 1)
(tabuleiro-adiciona-moeda-posicao! t8 (cria-posicao 4 4) 1)

(tabuleiro-adiciona-fio! t8 (cria-posicao 0 0) (cria-posicao 0 1))
(tabuleiro-adiciona-fio! t8 (cria-posicao 0 0) (cria-posicao 1 0))
(tabuleiro-adiciona-fio! t8 (cria-posicao 1 0) (cria-posicao 1 1))
(tabuleiro-adiciona-fio! t8 (cria-posicao 1 1) (cria-posicao 0 1))
(tabuleiro-adiciona-fio! t8 (cria-posicao 0 1) (cria-posicao 0 2))
(tabuleiro-adiciona-fio! t8 (cria-posicao 1 1) (cria-posicao 1 2))
(tabuleiro-adiciona-fio! t8 (cria-posicao 0 2) (cria-posicao 1 2))
(tabuleiro-adiciona-fio! t8 (cria-posicao 1 0) (cria-posicao 2 0))
(tabuleiro-adiciona-fio! t8 (cria-posicao 1 1) (cria-posicao 2 1))
(tabuleiro-adiciona-fio! t8 (cria-posicao 1 2) (cria-posicao 2 2))
(tabuleiro-adiciona-fio! t8 (cria-posicao 2 0) (cria-posicao 2 1))
(tabuleiro-adiciona-fio! t8 (cria-posicao 2 1) (cria-posicao 2 2))
(tabuleiro-adiciona-fio! t8 (cria-posicao 0 2) (cria-posicao 0 3))
(tabuleiro-adiciona-fio! t8 (cria-posicao 0 3) (cria-posicao 0 4))
(tabuleiro-adiciona-fio! t8 (cria-posicao 1 2) (cria-posicao 1 3))
(tabuleiro-adiciona-fio! t8 (cria-posicao 1 3) (cria-posicao 1 4))
(tabuleiro-adiciona-fio! t8 (cria-posicao 2 2) (cria-posicao 2 3))
(tabuleiro-adiciona-fio! t8 (cria-posicao 2 3) (cria-posicao 2 4))
(tabuleiro-adiciona-fio! t8 (cria-posicao 3 2) (cria-posicao 3 3))
(tabuleiro-adiciona-fio! t8 (cria-posicao 3 3) (cria-posicao 3 4))
(tabuleiro-adiciona-fio! t8 (cria-posicao 4 2) (cria-posicao 4 3))
(tabuleiro-adiciona-fio! t8 (cria-posicao 4 3) (cria-posicao 4 4))
(tabuleiro-adiciona-fio! t8 (cria-posicao 3 0) (cria-posicao 3 1))
(tabuleiro-adiciona-fio! t8 (cria-posicao 3 1) (cria-posicao 3 2))
(tabuleiro-adiciona-fio! t8 (cria-posicao 4 0) (cria-posicao 4 1))
(tabuleiro-adiciona-fio! t8 (cria-posicao 4 1) (cria-posicao 4 2))
(tabuleiro-adiciona-fio! t8 (cria-posicao 2 0) (cria-posicao 3 0))
(tabuleiro-adiciona-fio! t8 (cria-posicao 3 0) (cria-posicao 4 0))
(tabuleiro-adiciona-fio! t8 (cria-posicao 2 1) (cria-posicao 3 1))
(tabuleiro-adiciona-fio! t8 (cria-posicao 3 1) (cria-posicao 4 1))
(tabuleiro-adiciona-fio! t8 (cria-posicao 2 2) (cria-posicao 3 2))
(tabuleiro-adiciona-fio! t8 (cria-posicao 3 2) (cria-posicao 4 2))
(tabuleiro-adiciona-fio! t8 (cria-posicao 0 3) (cria-posicao 1 3))
(tabuleiro-adiciona-fio! t8 (cria-posicao 0 4) (cria-posicao 1 4))
(tabuleiro-adiciona-fio! t8 (cria-posicao 1 3) (cria-posicao 2 3))
(tabuleiro-adiciona-fio! t8 (cria-posicao 1 4) (cria-posicao 2 4))
(tabuleiro-adiciona-fio! t8 (cria-posicao 2 3) (cria-posicao 3 3))
(tabuleiro-adiciona-fio! t8 (cria-posicao 2 4) (cria-posicao 3 4))
(tabuleiro-adiciona-fio! t8 (cria-posicao 3 3) (cria-posicao 4 3))
(tabuleiro-adiciona-fio! t8 (cria-posicao 3 4) (cria-posicao 4 4))

;;; Tabuleiro t9
(defparameter t9 (cria-tabuleiro 6 6))
(tabuleiro-adiciona-moeda-posicao! t9 (cria-posicao 0 0) 2)
(tabuleiro-adiciona-moeda-posicao! t9 (cria-posicao 0 1) 2)
(tabuleiro-adiciona-moeda-posicao! t9 (cria-posicao 0 2) 2)
(tabuleiro-adiciona-moeda-posicao! t9 (cria-posicao 0 3) 2)
(tabuleiro-adiciona-moeda-posicao! t9 (cria-posicao 0 4) 2)
(tabuleiro-adiciona-moeda-posicao! t9 (cria-posicao 0 5) 2)
(tabuleiro-adiciona-moeda-posicao! t9 (cria-posicao 1 0) 2)
(tabuleiro-adiciona-moeda-posicao! t9 (cria-posicao 1 1) 3)
(tabuleiro-adiciona-moeda-posicao! t9 (cria-posicao 1 2) 3)
(tabuleiro-adiciona-moeda-posicao! t9 (cria-posicao 1 3) 3)
(tabuleiro-adiciona-moeda-posicao! t9 (cria-posicao 1 4) 3)
(tabuleiro-adiciona-moeda-posicao! t9 (cria-posicao 1 5) 2)
(tabuleiro-adiciona-moeda-posicao! t9 (cria-posicao 2 0) 2)
(tabuleiro-adiciona-moeda-posicao! t9 (cria-posicao 2 1) 3)
(tabuleiro-adiciona-moeda-posicao! t9 (cria-posicao 2 2) 5)
(tabuleiro-adiciona-moeda-posicao! t9 (cria-posicao 2 3) 5)
(tabuleiro-adiciona-moeda-posicao! t9 (cria-posicao 2 4) 3)
(tabuleiro-adiciona-moeda-posicao! t9 (cria-posicao 2 5) 2)
(tabuleiro-adiciona-moeda-posicao! t9 (cria-posicao 3 0) 2)
(tabuleiro-adiciona-moeda-posicao! t9 (cria-posicao 3 1) 3)
(tabuleiro-adiciona-moeda-posicao! t9 (cria-posicao 3 2) 5)
(tabuleiro-adiciona-moeda-posicao! t9 (cria-posicao 3 3) 5)
(tabuleiro-adiciona-moeda-posicao! t9 (cria-posicao 3 4) 3)
(tabuleiro-adiciona-moeda-posicao! t9 (cria-posicao 3 5) 2)  
(tabuleiro-adiciona-moeda-posicao! t9 (cria-posicao 4 0) 2)
(tabuleiro-adiciona-moeda-posicao! t9 (cria-posicao 4 1) 3)
(tabuleiro-adiciona-moeda-posicao! t9 (cria-posicao 4 2) 3)
(tabuleiro-adiciona-moeda-posicao! t9 (cria-posicao 4 3) 3)
(tabuleiro-adiciona-moeda-posicao! t9 (cria-posicao 4 4) 3)
(tabuleiro-adiciona-moeda-posicao! t9 (cria-posicao 4 5) 2)
(tabuleiro-adiciona-moeda-posicao! t9 (cria-posicao 5 0) 2)
(tabuleiro-adiciona-moeda-posicao! t9 (cria-posicao 5 1) 2)
(tabuleiro-adiciona-moeda-posicao! t9 (cria-posicao 5 2) 2)
(tabuleiro-adiciona-moeda-posicao! t9 (cria-posicao 5 3) 2)
(tabuleiro-adiciona-moeda-posicao! t9 (cria-posicao 5 4) 2)
(tabuleiro-adiciona-moeda-posicao! t9 (cria-posicao 5 5) 2)


(tabuleiro-adiciona-fio! t9 (cria-posicao 0 0) (cria-posicao 1 1))
(tabuleiro-adiciona-fio! t9 (cria-posicao 5 5) (cria-posicao 4 4))
(tabuleiro-adiciona-fio! t9 (cria-posicao 0 5) (cria-posicao 1 4))
(tabuleiro-adiciona-fio! t9 (cria-posicao 5 0) (cria-posicao 4 1))
(tabuleiro-adiciona-fio! t9 (cria-posicao 0 0) (cria-posicao 0 1))
(tabuleiro-adiciona-fio! t9 (cria-posicao 0 1) (cria-posicao 0 2))
(tabuleiro-adiciona-fio! t9 (cria-posicao 0 2) (cria-posicao 0 3))
(tabuleiro-adiciona-fio! t9 (cria-posicao 0 3) (cria-posicao 0 4))
(tabuleiro-adiciona-fio! t9 (cria-posicao 0 4) (cria-posicao 0 5))
(tabuleiro-adiciona-fio! t9 (cria-posicao 0 5) (cria-posicao 1 5))
(tabuleiro-adiciona-fio! t9 (cria-posicao 1 5) (cria-posicao 2 5))
(tabuleiro-adiciona-fio! t9 (cria-posicao 2 5) (cria-posicao 3 5))
(tabuleiro-adiciona-fio! t9 (cria-posicao 3 5) (cria-posicao 4 5))
(tabuleiro-adiciona-fio! t9 (cria-posicao 4 5) (cria-posicao 5 5))
(tabuleiro-adiciona-fio! t9 (cria-posicao 5 5) (cria-posicao 5 4))
(tabuleiro-adiciona-fio! t9 (cria-posicao 5 4) (cria-posicao 5 3))
(tabuleiro-adiciona-fio! t9 (cria-posicao 5 3) (cria-posicao 5 2))
(tabuleiro-adiciona-fio! t9 (cria-posicao 5 2) (cria-posicao 5 1))
(tabuleiro-adiciona-fio! t9 (cria-posicao 5 1) (cria-posicao 5 0))
(tabuleiro-adiciona-fio! t9 (cria-posicao 5 0) (cria-posicao 4 0))
(tabuleiro-adiciona-fio! t9 (cria-posicao 4 0) (cria-posicao 3 0))
(tabuleiro-adiciona-fio! t9 (cria-posicao 3 0) (cria-posicao 2 0))
(tabuleiro-adiciona-fio! t9 (cria-posicao 2 0) (cria-posicao 1 0))
(tabuleiro-adiciona-fio! t9 (cria-posicao 1 0) (cria-posicao 0 0))
(tabuleiro-adiciona-fio! t9 (cria-posicao 1 1) (cria-posicao 1 2))
(tabuleiro-adiciona-fio! t9 (cria-posicao 1 2) (cria-posicao 1 3))
(tabuleiro-adiciona-fio! t9 (cria-posicao 1 3) (cria-posicao 1 4))
(tabuleiro-adiciona-fio! t9 (cria-posicao 1 4) (cria-posicao 2 4))
(tabuleiro-adiciona-fio! t9 (cria-posicao 2 4) (cria-posicao 3 4))
(tabuleiro-adiciona-fio! t9 (cria-posicao 3 4) (cria-posicao 4 4))
(tabuleiro-adiciona-fio! t9 (cria-posicao 4 4) (cria-posicao 4 3))
(tabuleiro-adiciona-fio! t9 (cria-posicao 4 3) (cria-posicao 4 2))
(tabuleiro-adiciona-fio! t9 (cria-posicao 4 2) (cria-posicao 4 1))
(tabuleiro-adiciona-fio! t9 (cria-posicao 4 1) (cria-posicao 3 1))
(tabuleiro-adiciona-fio! t9 (cria-posicao 3 1) (cria-posicao 2 1))
(tabuleiro-adiciona-fio! t9 (cria-posicao 2 1) (cria-posicao 1 1))
(tabuleiro-adiciona-fio! t9 (cria-posicao 2 2) (cria-posicao 2 3))
(tabuleiro-adiciona-fio! t9 (cria-posicao 2 3) (cria-posicao 3 3))
(tabuleiro-adiciona-fio! t9 (cria-posicao 3 3) (cria-posicao 3 2))
(tabuleiro-adiciona-fio! t9 (cria-posicao 3 2) (cria-posicao 2 2))

;;; Tabuleiro t10
(defparameter t10 (cria-tabuleiro 6 6))
(tabuleiro-adiciona-moeda-posicao! t10 (cria-posicao 0 0) 3)
(tabuleiro-adiciona-moeda-posicao! t10 (cria-posicao 0 1) 7)
(tabuleiro-adiciona-moeda-posicao! t10 (cria-posicao 0 2) 7)
(tabuleiro-adiciona-moeda-posicao! t10 (cria-posicao 0 3) 7)
(tabuleiro-adiciona-moeda-posicao! t10 (cria-posicao 0 4) 9)
(tabuleiro-adiciona-moeda-posicao! t10 (cria-posicao 1 0) 3)
(tabuleiro-adiciona-moeda-posicao! t10 (cria-posicao 1 1) 3)
(tabuleiro-adiciona-moeda-posicao! t10 (cria-posicao 1 2) 7)
(tabuleiro-adiciona-moeda-posicao! t10 (cria-posicao 1 3) 9)
(tabuleiro-adiciona-moeda-posicao! t10 (cria-posicao 2 0) 2)
(tabuleiro-adiciona-moeda-posicao! t10 (cria-posicao 2 1) 3)
(tabuleiro-adiciona-moeda-posicao! t10 (cria-posicao 2 2) 5)
(tabuleiro-adiciona-moeda-posicao! t10 (cria-posicao 2 3) 5)
(tabuleiro-adiciona-moeda-posicao! t10 (cria-posicao 2 4) 3)
(tabuleiro-adiciona-moeda-posicao! t10 (cria-posicao 2 5) 2)
(tabuleiro-adiciona-moeda-posicao! t10 (cria-posicao 3 0) 2)
(tabuleiro-adiciona-moeda-posicao! t10 (cria-posicao 3 1) 3)
(tabuleiro-adiciona-moeda-posicao! t10 (cria-posicao 3 2) 5)
(tabuleiro-adiciona-moeda-posicao! t10 (cria-posicao 3 3) 5)
(tabuleiro-adiciona-moeda-posicao! t10 (cria-posicao 3 4) 3)
(tabuleiro-adiciona-moeda-posicao! t10 (cria-posicao 3 5) 2)  
(tabuleiro-adiciona-moeda-posicao! t10 (cria-posicao 4 0) 1)
(tabuleiro-adiciona-moeda-posicao! t10 (cria-posicao 4 1) 1)
(tabuleiro-adiciona-moeda-posicao! t10 (cria-posicao 4 2) 1)
(tabuleiro-adiciona-moeda-posicao! t10 (cria-posicao 4 3) 3)
(tabuleiro-adiciona-moeda-posicao! t10 (cria-posicao 4 4) 3)
(tabuleiro-adiciona-moeda-posicao! t10 (cria-posicao 4 5) 2)
(tabuleiro-adiciona-moeda-posicao! t10 (cria-posicao 5 0) 1)
(tabuleiro-adiciona-moeda-posicao! t10 (cria-posicao 5 1) 1)
(tabuleiro-adiciona-moeda-posicao! t10 (cria-posicao 5 2) 1)
(tabuleiro-adiciona-moeda-posicao! t10 (cria-posicao 5 3) 3)
(tabuleiro-adiciona-moeda-posicao! t10 (cria-posicao 5 4) 3)
(tabuleiro-adiciona-moeda-posicao! t10 (cria-posicao 5 5) 2)

(tabuleiro-adiciona-fio! t10 (cria-posicao 0 0) (cria-posicao 1 0))
(tabuleiro-adiciona-fio! t10 (cria-posicao 1 0) (cria-posicao 1 1))
(tabuleiro-adiciona-fio! t10 (cria-posicao 1 1) (cria-posicao 0 0))
(tabuleiro-adiciona-fio! t10 (cria-posicao 0 1) (cria-posicao 0 2))
(tabuleiro-adiciona-fio! t10 (cria-posicao 0 2) (cria-posicao 1 2))
(tabuleiro-adiciona-fio! t10 (cria-posicao 1 2) (cria-posicao 0 1))
(tabuleiro-adiciona-fio! t10 (cria-posicao 0 3) (cria-posicao 0 4))
(tabuleiro-adiciona-fio! t10 (cria-posicao 0 4) (cria-posicao 1 3))
(tabuleiro-adiciona-fio! t10 (cria-posicao 1 3) (cria-posicao 0 3))
(tabuleiro-adiciona-fio! t10 (cria-posicao 2 0) (cria-posicao 3 1))
(tabuleiro-adiciona-fio! t10 (cria-posicao 3 1) (cria-posicao 2 2))
(tabuleiro-adiciona-fio! t10 (cria-posicao 2 2) (cria-posicao 3 3))
(tabuleiro-adiciona-fio! t10 (cria-posicao 2 3) (cria-posicao 3 2))
(tabuleiro-adiciona-fio! t10 (cria-posicao 3 3) (cria-posicao 2 4))
(tabuleiro-adiciona-fio! t10 (cria-posicao 2 4) (cria-posicao 3 5))
(tabuleiro-adiciona-fio! t10 (cria-posicao 3 5) (cria-posicao 2 5))
(tabuleiro-adiciona-fio! t10 (cria-posicao 2 5) (cria-posicao 3 4))
(tabuleiro-adiciona-fio! t10 (cria-posicao 3 4) (cria-posicao 2 3))
(tabuleiro-adiciona-fio! t10 (cria-posicao 3 2) (cria-posicao 2 1))
(tabuleiro-adiciona-fio! t10 (cria-posicao 2 1) (cria-posicao 3 0))
(tabuleiro-adiciona-fio! t10 (cria-posicao 3 0) (cria-posicao 2 0))
(tabuleiro-adiciona-fio! t10 (cria-posicao 4 0) (cria-posicao 5 1))
(tabuleiro-adiciona-fio! t10 (cria-posicao 5 1) (cria-posicao 4 2))
(tabuleiro-adiciona-fio! t10 (cria-posicao 4 2) (cria-posicao 5 2))
(tabuleiro-adiciona-fio! t10 (cria-posicao 5 3) (cria-posicao 4 4))
(tabuleiro-adiciona-fio! t10 (cria-posicao 4 4) (cria-posicao 5 5))
(tabuleiro-adiciona-fio! t10 (cria-posicao 5 5) (cria-posicao 4 5))
(tabuleiro-adiciona-fio! t10 (cria-posicao 4 5) (cria-posicao 5 4))
(tabuleiro-adiciona-fio! t10 (cria-posicao 5 4) (cria-posicao 4 3))
(tabuleiro-adiciona-fio! t10 (cria-posicao 5 2) (cria-posicao 4 1))
(tabuleiro-adiciona-fio! t10 (cria-posicao 4 1) (cria-posicao 5 0))
(tabuleiro-adiciona-fio! t10 (cria-posicao 5 0) (cria-posicao 4 0))
(tabuleiro-adiciona-fio! t10 (cria-posicao 4 3) (cria-posicao 5 3))
