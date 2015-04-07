;;;; Grupo al003: 72913 - Bruno Henriques 73559 - Tiago Diogo 72960 - Tiago Santos

;;;; =========================================================
;;;; =============== FUNCOES AUXILIARES GENERICAS ============
;;;; =========================================================

; copia-lista - Copia lista dado como argumento
(defun copia-lista (l)
	(if (null l)
		(list)
		(cons (first l) (copia-lista (rest l)))))


;;;; ********************************************************************
;;;; *************************** ESTRUTURAS DE DADOS ********************
;;;; ********************************************************************

;;;; =========================================================
;;;; ====================     POSICAO    =====================
;;;; =========================================================

(defstruct posicao linha coluna)

(defun cria-posicao (x y)
	(make-posicao :linha x :coluna y))

(defun posicoes-iguais-p(p1 p2)
	(and(= (posicao-linha p1) (posicao-linha p2))
		(= (posicao-coluna p1) (posicao-coluna p2))))

;;;; =========================================================
;;;; ====================       FIO      =====================
;;;; =========================================================

(defstruct fio id origem destino)

(defun cria-fio(i pos-orig pos-dest)
	(make-fio :id i :origem pos-orig :destino pos-dest))

;;;; =========================================================
;;;; ====================    TABULEIRO   =====================
;;;; =========================================================

(defstruct tabuleiro fios quant-moedas total-fios matriz)



(defun cria-tabuleiro(x y)
	(make-tabuleiro :fios (list) :quant-moedas 0 :total-fios 0 :matriz (make-array (list x y))))

(defun tabuleiro-linhas (tab)
	(array-dimension (tabuleiro-matriz tab) 0))

(defun tabuleiro-colunas (tab)
	(array-dimension (tabuleiro-matriz tab) 1))


; copia-tabuleiro - e feita copia das listas e da matriz
(defun copia-tabuleiro(tab)
	(labels ((copia-array (array)
				(let* ((x (array-dimension array 0))
					   (y (array-dimension array 1))
					   (copy (make-array (list x y))))
					(dotimes (i x)
						(dotimes (j y)
							(setf (aref copy i j) (aref array i j))))
					copy)))
	  (make-tabuleiro 	:fios (copia-lista (tabuleiro-fios tab)) 
						:quant-moedas (tabuleiro-quant-moedas tab) 
						:total-fios (tabuleiro-total-fios tab) 
						:matriz (copia-array (tabuleiro-matriz tab)))))

; tabuleiro-fio-com-id - percorre a lista de fios ate encontrar o id desejado
(defun tabuleiro-fio-com-id(tab id)
	(labels ((procura-id (lst id)
		(cond ( (null lst) nil)
			  ( (= (fio-id (first lst)) id) (first lst))
			  ( t (procura-id (rest lst) id)))))
	(procura-id (tabuleiro-fios tab) id)))
		

; tabuleiro-fios-posicao - percorre a lista de fios e constroi uma lista em que o fio esta ligado, ou pela origem do fio, ou pelo destino do fio a posicao pos dado como argumento
(defun tabuleiro-fios-posicao(tab pos)
	(labels ((tabuleiro-fios-posicao-aux (fios p r)
 			(cond( (null fios) r)
 				 ( (or 	(posicoes-iguais-p (fio-origem(first fios)) p) 
						(posicoes-iguais-p (fio-destino(first fios)) p)) 
					(tabuleiro-fios-posicao-aux (rest fios) p (append r (list (first fios)))))
 				 ( t (tabuleiro-fios-posicao-aux (rest fios) p r)))))
		(tabuleiro-fios-posicao-aux (tabuleiro-fios tab) pos (list))))


(defun tabuleiro-moeda-posicao(tab pos)
	(aref (tabuleiro-matriz tab) (posicao-linha pos) (posicao-coluna pos)))


(defun tabuleiro-total-moedas(tab)
	(tabuleiro-quant-moedas tab))


(defun tabuleiro-adiciona-fio!(tab pos1 pos2)
	(let ((id (incf (tabuleiro-total-fios tab))))
		(setf (tabuleiro-fios tab) (append (tabuleiro-fios tab) (list (cria-fio id pos1 pos2))))))


; tabuleiro-adiciona-moeda-posicao, actualiza a matriz na posicao dada como argumento e actualiza a variavel que controla a quantidade
; de moedas no tabuleiro
(defun tabuleiro-adiciona-moeda-posicao!(tab pos val)
	(let ((moeda (tabuleiro-moeda-posicao tab pos)))
			(when (not (null moeda))
				(setf (tabuleiro-quant-moedas tab) (- (tabuleiro-total-moedas tab) moeda)))
		(setf (aref (tabuleiro-matriz tab) (posicao-linha pos) (posicao-coluna pos)) val) 
		(setf (tabuleiro-quant-moedas tab) (+ (tabuleiro-total-moedas tab) val))))

; tabuleiro-remove-fio-com-id! - Devolve uma nova lista de fios sem o fio com o id passado como argumento.
; E iterada a lista ate encontrar o fio pretendido, quando encontrado, devolvemos o resto da lista
(defun tabuleiro-remove-fio-com-id!(tab id)
	(labels ((remove-id (lst id)
		(cond ( (null lst) nil)
			  ( (= (fio-id (first lst)) id) (rest lst))
			  ( t (cons (first lst) (remove-id (rest lst) id))))))
	 (setf (tabuleiro-fios tab) (remove-id (tabuleiro-fios tab) id))))
		
; tabuleiro-remove-moeda-posicao - remove moeda na posicao dada como argumento e actualiza a variavel que controla a quantidade de moedas
; no tabuleiro
(defun tabuleiro-remove-moeda-posicao!(tab pos)
	(let* ((mat (tabuleiro-matriz tab))
		  (x (posicao-linha pos))
		  (y (posicao-coluna pos))) 
		(setf (tabuleiro-quant-moedas tab) (- (tabuleiro-total-moedas tab) (aref mat x y)))
		(setf (aref mat x y) nil)))
		
;;;; =========================================================
;;;; ====================       JOGO      ====================
;;;; =========================================================

(defstruct jogo jogador pontos-jogador1 pontos-jogador2 historico-jogadas tabuleiro)

(defun cria-jogo (tab)
	(make-jogo :jogador 1 
			   :pontos-jogador1 0 
			   :pontos-jogador2 0 
			   :historico-jogadas (list) 
			   :tabuleiro tab))
	
(defun copia-jogo (jogo)
	(make-jogo :jogador (jogo-jogador jogo) 
			   :pontos-jogador1 (jogo-pontos-jogador1 jogo) 
			   :pontos-jogador2 (jogo-pontos-jogador2 jogo) 
			   :historico-jogadas (copia-lista (jogo-historico-jogadas jogo))
			   :tabuleiro (copia-tabuleiro(jogo-tabuleiro jogo))))
			   
;;;; =========================================================
;;;; ====================    AUXILIARES   ====================
;;;; =========================================================

(defun alterna-jogador (jogo)
	(if (= 1 (jogo-jogador jogo))
		(incf (jogo-jogador jogo))
		(decf (jogo-jogador jogo)))) 
			   
(defun adiciona-pontos-jogador (jogo pontos)
	(if (= (jogo-jogador jogo) 1)
		(setf (jogo-pontos-jogador1 jogo) (+ (jogo-pontos-jogador1 jogo) pontos))
		(setf (jogo-pontos-jogador2 jogo) (+ (jogo-pontos-jogador2 jogo) pontos))))
		
;;;; =========================================================
;;;; =========================================================

; jogo-aplica-jogada! - Altera o jogo dado como argumento
(defun jogo-aplica-jogada! (jogo id)
	;;actualiza a posicao, eliminando a moeda ou nao, e devolve os pontos
	(labels ((actualiza-posicao (tab pos)
				(let ((moeda (tabuleiro-moeda-posicao tab pos)))
					(cond ((null (tabuleiro-fios-posicao tab pos)) 
							(tabuleiro-remove-moeda-posicao! tab pos) moeda)
						  (t 0)))))
		(let* ((tab (jogo-tabuleiro jogo))
			   (fio (tabuleiro-fio-com-id tab id))
			   (pontos-ganhos 0))
			   (when (not (null fio))
				   (progn
					   (tabuleiro-remove-fio-com-id! tab id)
					   (setf pontos-ganhos (+ (actualiza-posicao tab (fio-origem fio)) (actualiza-posicao tab (fio-destino fio))))
					   (setf (jogo-historico-jogadas jogo) (append (jogo-historico-jogadas jogo) (list id)))
					   (if (zerop pontos-ganhos)
						   (alterna-jogador jogo)
						   (adiciona-pontos-jogador jogo pontos-ganhos)))))))

; jogo-terminado-p - Um jogo so esta terminado se nao houver fios para remover 
(defun jogo-terminado-p (jogo)
	(null (tabuleiro-fios (jogo-tabuleiro jogo))))
	
;;;; =========================================================
;;;; ====================  TIPO PROBLEMA  ====================
;;;; =========================================================

(defstruct problema estado-inicial jogador accoes resultado teste-corte-p funcao-avaliacao historico-accoes chave-equivalencia)

; accoes- Devolve uma lista de fios, iterando a lista todas de fios retirando apenas o id
(defun accoes (jogo)
	(labels ((iterativo (lst res)
				(cond ((null lst) res)
					  (t (setf res (cons (fio-id (first lst)) res)) (iterativo (rest lst) res)))))
		(iterativo (tabuleiro-fios (jogo-tabuleiro jogo)) (list))))
		
(defun resultado (jogo id)
	(let ((copia (copia-jogo jogo)))
		(jogo-aplica-jogada! copia id)
		copia))

(defun teste-terminal-p (jogo profundidade)
	(declare (ignore profundidade))
	(jogo-terminado-p jogo ))


(defun utilidade (jogo jogador)
	(if (= jogador 1)
		(- (jogo-pontos-jogador1 jogo) (jogo-pontos-jogador2 jogo))
		(- (jogo-pontos-jogador2 jogo) (jogo-pontos-jogador1 jogo))))



;;;; ********************************************************************
;;;; ********************** ALGORITMOS MINIMAX GENERICOS ****************
;;;; ********************************************************************

;;;; ===========================================================
;;;; =================  MINIMAX ALGORITHM  =====================
;;;; ===========================================================




;;;; get-pair: 
;;;; Ordena a lista dado um criterio de ordenacao g
;;;; Devolve a lista ordenada
(defun get-pair (lista g) 
	(let ((pair (first lista)))
		(labels((aux (lista)
					(cond((null lista) pair)
						 ((funcall g (cdr (first lista)) (cdr pair)) (setf pair (first lista)) (aux (rest lista)))
						 (T (aux (rest lista))))))
				(aux (rest lista)))))
	
; algoritmo minimax normal
(defun minimax (problema jogadormax)
	(let ((estadoi (problema-estado-inicial problema))
		 (visitados 0))
		(labels ((calculaSucessores (lista estado &optional (res (list)))
					(cond ( (null lista) res)
						  (t (setf res (cons (cons (first lista) (funcall (problema-resultado problema) estado (first lista))) res))
							 (calculaSucessores (rest lista) estado res)))) ;inverte a lista
				(DameValor (estado profundidade &optional (inicial nil))
					(let ((jogador (funcall (problema-jogador problema) estado))
						 (sucessores nil)
						 (sucessoresaux (list))
						 (parEscolhido (cons nil nil)))
					(if (funcall (problema-teste-corte-p problema) estado profundidade) 
							(progn (incf visitados)
									(funcall (problema-funcao-avaliacao problema) estado jogadormax))  
							(progn
								;; constroi lista de pares com (id , estado-aplica-jogada-id)
								(setf sucessores (calculaSucessores (funcall (problema-accoes problema) estado) estado (list)))
								(dolist (x sucessores) (setf sucessoresaux (cons (cons (car x) (DameValor (cdr x) (1+ profundidade))) sucessoresaux)))			;inverte a lista, beware
								(if (= jogador jogadormax)
									(setf parEscolhido (get-pair sucessoresaux #'>))
									(setf parEscolhido (get-pair sucessoresaux #'<)))
								(if inicial ;devolver o par se for o ultimo no a propagar, devolver o valor caso nao seja
									(list (car parEscolhido) (cdr parEscolhido) visitados)
									(cdr parEscolhido)))))))
				(values-list (DameValor estadoi 0 t)))))

(defun jogador-minimax-simples (jogo actjogador tempoParaJogar)
	(declare (ignore tempoParaJogar))
	(values (minimax-alfa-beta (make-problema :estado-inicial jogo 
							:jogador #'jogo-jogador 
							:accoes #'accoes 
							:resultado #'resultado 
							:teste-corte-p #'teste-terminal-p 
							:funcao-avaliacao #'utilidade)
				actjogador)))

;;;; =========================================================
;;;; ============  MINIMAX ALFA-BETA ALGORITHM  ==============
;;;; =========================================================


; algoritmo minimax com cortes alfa-beta
(defun minimax-alfa-beta (problema jogadormax)
	(let ((estadoi (problema-estado-inicial problema))
		 (visitados 0))
		(labels ((calculaSucessores (lista estado)
					(if (null lista)
						(list)
						(cons (cons (first lista) (funcall (problema-resultado problema) estado (first lista))) 
							  (calculaSucessores (rest lista) estado))))
				(alfa-beta-aux (estado profundidade alfa beta &optional (inicial nil))
					(let ((jogador (funcall (problema-jogador problema) estado))
						  (sucessores nil)
						  (parEscolhido (cons nil nil))
						  (alfabeta nil)
						  (iminimax nil)
						  (op-max-min nil)
						  (op-teste-corte nil)
						  (comp nil))
					(if (funcall (problema-teste-corte-p problema) estado profundidade) 
							(progn 
									(incf visitados)
									(funcall (problema-funcao-avaliacao problema) estado jogadormax))
							(progn
									;; constroi lista de pares com (id , estado-aplica-jogada-id)
									(setf sucessores (calculaSucessores (funcall (problema-accoes problema) estado) estado))
									(if (= jogador jogadormax)
										(progn 
											(setf alfabeta alfa)
											(setf comp beta)
											(setf op-max-min #'>)
											(setf op-teste-corte #'>=)
											(setf (cdr parEscolhido) -99999))
										(progn
											(setf alfabeta beta)
											(setf comp alfa)
											(setf op-max-min #'<)
											(setf op-teste-corte #'<=)
											(setf (cdr parEscolhido) 99999)))
									(dolist (x sucessores)
										(if (= jogador jogadormax)
											(setf iminimax (alfa-beta-aux (cdr x) (1+ profundidade) alfabeta beta))
											(setf iminimax (alfa-beta-aux (cdr x) (1+ profundidade) alfa alfabeta)))
										(when (funcall op-max-min iminimax alfabeta) ; alfa = max( alfa, minimax-alfa-beta) ou beta= min(beta, minimax-alfa-beta)
											(setf alfabeta iminimax))
										(when (funcall op-max-min iminimax (cdr parEscolhido)) ;escolha do melhor valor
											(setf parEscolhido (cons (car x) iminimax)))
										(when (funcall op-teste-corte alfabeta comp);corte alfa-beta
											(return)))
									(if inicial ;devolver o par se for o ultimo no a propagar, devolver o valor caso nao seja. E uma verificacao feia
										(list (car parEscolhido) (cdr parEscolhido) visitados)
										(cdr parEscolhido)))))))
				(values-list (alfa-beta-aux estadoi 0 -999999 999999 t)))));t indica que e necessario devolver o fio e os nos visitados 


;;;; ********************************************************************
;;;; ******************** ALGORITMOS MINIMAX OPTIMIZADOS ****************
;;;; ********************************************************************

;;;; =========================================================
;;;; =====================  HEURISTICAS  =====================
;;;; =========================================================


;;;; push-sem-repetir-lista: 
;;;; Insere um elemento sem o repetir na lista
;;;; Devolve a lista alterada
(defun push-sem-repetir-lista (pos lst)
	(if (find-if #'(lambda (x) (equalp x pos))  lst)
		lst
		(cons pos lst)))

;;;; converte-lista-fios-lista-moedas: 
;;;; Dado uma lista de fios cria a lista de moedas associada
;;;; Devolve a lista de moedas
(defun converte-lista-fios-lista-moedas (lista-fios)
	(let ((lista-moedas (list)))
		(dolist (fio lista-fios)
			(setf lista-moedas (push-sem-repetir-lista (fio-destino fio) lista-moedas))
			(setf lista-moedas (push-sem-repetir-lista (fio-origem fio) lista-moedas)))
		lista-moedas))
		
;;;; insere-ordenado: 
;;;; Insere a variavel valor no sitio ordenado na lista
;;;; Devolve a lista ordenada
(defun insere-ordenado (valor lista)
	(cond	((null lista) (list valor))
			((< valor (first lista)) (cons valor lista))
			(t (cons (first lista) (insere-ordenado valor (rest lista))))))



;;;; cadeia-p: 
;;;; Dado uma posicao e um tabuleiro identifica se a moeda pertence a uma cadeia fechada ou aberta
;;;; Retorna 3 elementos
;;;; O primeiro: se t quer dizer que e uma cadeia aberta i.e. sao varias moedas ligadas com 2 fios 
;;;; em que uma das pontas tem pelo menos um fio ligado. 
;;;; Devolve nil caso for uma cadeia fechada
;;;; O segundo: O total de moedas das moedas analisada 
;;;; O terceiro: A lista de moedas analisadas
(defun cadeia-p (tabuleiro pos-moeda-original)
	(let* ((primeiraCadeia nil)
		(segundaCadeia (list nil 0))
		(fiosligados1 (tabuleiro-fios-posicao tabuleiro pos-moeda-original))
		(nfiosligados1 (length fiosligados1))
		(valor-moeda1 (tabuleiro-moeda-posicao tabuleiro pos-moeda-original))
		(listaMoedas (list pos-moeda-original)))
		(labels ((cadeia-p-aux (pos-moeda-em-analise ultimo-id-fio-analisado total)
			(let* ((fiosligados (tabuleiro-fios-posicao tabuleiro pos-moeda-em-analise))
				   (nfiosligados (length fiosligados))
				   (valor-moeda (tabuleiro-moeda-posicao tabuleiro pos-moeda-em-analise)))
				   (setf listaMoedas (cons pos-moeda-em-analise listaMoedas))
					(cond ((= nfiosligados 1)
								(list t (+ valor-moeda total) listaMoedas))					
							; caso terminal 2: deu o loop ate a primeira moeda analsiada e logo e uma cadeia fechada
						  ((posicoes-iguais-p pos-moeda-original pos-moeda-em-analise)
								(setf listaMoedas (rest listaMoedas))
								(list nil total listaMoedas))
						  ((= nfiosligados 2)
							(if (= ultimo-id-fio-analisado (fio-id (first fiosligados)))
								(if (posicoes-iguais-p (fio-destino (second fiosligados)) pos-moeda-em-analise)
									(cadeia-p-aux (fio-origem (second fiosligados)) (fio-id (second fiosligados)) (+ total valor-moeda))
									(cadeia-p-aux (fio-destino (second fiosligados)) (fio-id (second fiosligados)) (+ total valor-moeda)))
								(if (posicoes-iguais-p (fio-destino (first fiosligados)) pos-moeda-em-analise)
									(cadeia-p-aux (fio-origem (first fiosligados)) (fio-id (first fiosligados)) (+ total valor-moeda))
									(cadeia-p-aux (fio-destino (first fiosligados)) (fio-id (first fiosligados)) (+ total valor-moeda)))))
						  (t (list nil total listaMoedas))))))
				
	(cond ((= 1 nfiosligados1) (if (posicoes-iguais-p (fio-destino (first fiosligados1)) pos-moeda-original)
								   (setf primeiraCadeia (cadeia-p-aux (fio-origem (first fiosligados1)) (fio-id (first fiosligados1)) valor-moeda1))
								   (setf primeiraCadeia (cadeia-p-aux (fio-destino (first fiosligados1)) (fio-id (first fiosligados1)) valor-moeda1)))
								(list t (second primeiraCadeia) (third primeiraCadeia)))
							
		   ((= 2 nfiosligados1) (if (posicoes-iguais-p (fio-destino (first fiosligados1)) pos-moeda-original)
									(setf primeiraCadeia (cadeia-p-aux (fio-origem (first fiosligados1)) (fio-id (first fiosligados1)) valor-moeda1))
									(setf primeiraCadeia (cadeia-p-aux (fio-destino (first fiosligados1)) (fio-id (first fiosligados1)) valor-moeda1))) 
							(when (first primeiraCadeia) 
								(if (posicoes-iguais-p (fio-destino (second fiosligados1)) pos-moeda-original)
									(setf segundaCadeia (cadeia-p-aux (fio-origem (second fiosligados1)) (fio-id (second fiosligados1)) 0))
									(setf segundaCadeia (cadeia-p-aux (fio-destino (second fiosligados1)) (fio-id (second fiosligados1)) 0))))
							(list (or (first primeiraCadeia)(first segundaCadeia)) (+ (second primeiraCadeia) (second segundaCadeia)) listaMoedas))
		   (t (list nil 0 listaMoedas))))))



; heuristica v1
; Retorna a soma de todos os pontos no ecra tendo em conta o jogadormax
(defun avaliacao-v1 (jogo jogadormax)
	(if (= jogadormax 1)
	    (- (+ (jogo-pontos-jogador1 jogo) (tabuleiro-total-moedas (jogo-tabuleiro jogo))) (jogo-pontos-jogador2 jogo))
	    (- (+ (jogo-pontos-jogador2 jogo) (tabuleiro-total-moedas (jogo-tabuleiro jogo))) (jogo-pontos-jogador1 jogo))))


; heuristica v2
; Ao jogador actual soma-se as cadeias abertas, e ao outro as cadeias fechadas
(defun avaliacao-v2 (jogo jogadormax)
	(let* ((tabuleiro (jogo-tabuleiro jogo))
		   (valor1 (jogo-pontos-jogador1 jogo))
		   (valor2 (jogo-pontos-jogador2 jogo))
		   (lista-pos-moedas (converte-lista-fios-lista-moedas (tabuleiro-fios tabuleiro)))
		   (retorno-cadeia-p nil)
		   (pontos-aberta 0)
		   (pontos-fechada 0))
		   
		
		(loop for while t do
			(when (null lista-pos-moedas)
				  (return))
			(setf retorno-cadeia-p (cadeia-p tabuleiro (first lista-pos-moedas)))
			(if (first retorno-cadeia-p) 
				(setf pontos-aberta (+ pontos-aberta (second retorno-cadeia-p)))
				(setf pontos-fechada (+ pontos-fechada (second retorno-cadeia-p))))
			
			(dolist (pos-moeda-analisada (third retorno-cadeia-p))
				(setf lista-pos-moedas (remove-if (lambda (x) (equalp x pos-moeda-analisada)) lista-pos-moedas))))
		
			(if (= (jogo-jogador jogo) 1)
				(progn (setf valor1 (+ valor1 pontos-aberta))
					   (setf valor2 (+ valor2 pontos-fechada)))
				(progn (setf valor1 (+ valor1 pontos-fechada))
					   (setf valor2 (+ valor2 pontos-aberta))))
				
		(if (= jogadormax 1)
			(- valor1 valor2)
			(- valor2 valor1))))


; heuristica v3
; Soma das moedas isoladas com as cadeias abertas
; Nao e admissivel! Se uma moeda tem 2 fios ligados, pode ser benefico para o jogador
(defun avaliacao-v3 (jogo jogadormax)
	(let ((tabuleiro (jogo-tabuleiro jogo))
			(valor1 (jogo-pontos-jogador1 jogo))
			(valor2 (jogo-pontos-jogador2 jogo))) 
		(dolist (x (converte-lista-fios-lista-moedas (tabuleiro-fios tabuleiro)))
			(if (oddp (length (tabuleiro-fios-posicao tabuleiro x)))
				(setf valor1 (+ valor1 (tabuleiro-moeda-posicao tabuleiro x)))
				(setf valor2 (+ valor2 (tabuleiro-moeda-posicao tabuleiro x)))))
		(if (= jogadormax 1)
			(- valor1 valor2)
			(- valor2 valor1))))


;heuristicav4
;JOgador actual soma-se as cadeias abertas e ao outro a cadeia fechada de menor valor
(defun avaliacao-v4 (jogo jogadormax)
	(let* ((tabuleiro (jogo-tabuleiro jogo))
		   (valor1 (jogo-pontos-jogador1 jogo))
		   (valor2 (jogo-pontos-jogador2 jogo))
		   (lista-pos-moedas (converte-lista-fios-lista-moedas (tabuleiro-fios tabuleiro)))
		   (retorno-cadeia-p nil)
		   (pontos-aberta 0)
		   (valorCadeiaFechadaMenor 99999)
		   (valorCadeiaFechadaMaior -99999))
		
		(loop for while t do
			(when (null lista-pos-moedas)
				  (return))
			(setf retorno-cadeia-p (cadeia-p tabuleiro (first lista-pos-moedas)))
			(if (first retorno-cadeia-p) 
				(setf pontos-aberta (+ pontos-aberta (second retorno-cadeia-p)))
					(progn (when (< (second retorno-cadeia-p) valorCadeiaFechadaMenor)
						(setf valorCadeiaFechadaMenor (second retorno-cadeia-p)))
					(when (> (second retorno-cadeia-p) valorCadeiaFechadaMaior)
						(setf valorCadeiaFechadaMenor (second retorno-cadeia-p)))))
			
			(dolist (pos-moeda-analisada (third retorno-cadeia-p))
				(setf lista-pos-moedas (remove-if (lambda (x) (equalp x pos-moeda-analisada)) lista-pos-moedas))))
		
			(if (= (jogo-jogador jogo) 1)
				(progn (setf valor1 (+ valor1 valorCadeiaFechadaMenor))
					   (setf valor2 (+ valor2 pontos-aberta valorCadeiaFechadaMaior)))
				(progn (setf valor1 (+ valor1 pontos-aberta valorCadeiaFechadaMaior))
					   (setf valor2 (+ valor2 valorCadeiaFechadaMenor))))
				
		(if (= jogadormax 1)
			(- valor1 valor2)
			(- valor2 valor1))))


; heuristica v5
; guarda cadeias fechadas numa lista e atribui os pontos das cadeias impares ao adversario
; e as pares ao jogador actual
(defun avaliacao-v5 (jogo jogadormax)
	(let* ((tabuleiro (jogo-tabuleiro jogo))
		   (valor1 (jogo-pontos-jogador1 jogo))
		   (valor2 (jogo-pontos-jogador2 jogo))
		   (lista-pos-moedas (converte-lista-fios-lista-moedas (tabuleiro-fios tabuleiro)))
		   (retorno-cadeia-p nil)
		   (pontos-aberta 0)
		   (valoresFechadas (list))
		   (valorImpar 0)
		   (valorPar 0)
		   (controleLista 1))
		   	
			(loop for while t do
				(when (null lista-pos-moedas)
					  (return))
				(setf retorno-cadeia-p (cadeia-p tabuleiro (first lista-pos-moedas)))
				(if (first retorno-cadeia-p) 
					(setf pontos-aberta (+ pontos-aberta (second retorno-cadeia-p)))
					(when (> (second retorno-cadeia-p) 0) (setf valoresFechadas  (insere-ordenado (second retorno-cadeia-p) valoresFechadas))))
				(dolist (pos-moeda-analisada (third retorno-cadeia-p))
					(setf lista-pos-moedas (remove-if (lambda (x) (equalp x pos-moeda-analisada)) lista-pos-moedas))))
					
			(dolist (valorF valoresFechadas) 
				(if (evenp controleLista) 
					(progn (setf valorPar (+ valorPar valorF)) (incf controleLista))
					(progn (setf valorImpar (+ valorImpar valorF)) (decf controleLista))))
			
			(if (= (jogo-jogador jogo) 1)
				(progn (setf valor1 (+ valor1 pontos-aberta valorPar))
					   (setf valor2 (+ valor2 valorImpar)))
				(progn (setf valor1 (+ valor1 valorImpar))
					   (setf valor2 (+ valor2 pontos-aberta valorPar))))
					
			(if (= jogadormax 1)
				(- valor1 valor2)
				(- valor2 valor1)))) 
				
				
(defun insere-ordenado2 (valor lista)
	(cond	((null lista) (list valor))
			((> valor (first lista)) (cons valor lista))
			(t (cons (first lista) (insere-ordenado valor (rest lista))))))
				
(defun avaliacao-v7 (jogo jogadormax)
	(let* ((tabuleiro (jogo-tabuleiro jogo))
		   (valor1 (jogo-pontos-jogador1 jogo))
		   (valor2 (jogo-pontos-jogador2 jogo))
		   (lista-pos-moedas (converte-lista-fios-lista-moedas (tabuleiro-fios tabuleiro)))
		   (retorno-cadeia-p nil)
		   (valoresFechadas (list))
		   (valorImpar 0)
		   (valorPar 0)
		   (controleLista 1))
		   	
			(loop for while t do
				(when (null lista-pos-moedas)
					  (return))
				(setf retorno-cadeia-p (cadeia-p tabuleiro (first lista-pos-moedas)))
				(when (and (not(first retorno-cadeia-p)) (> (second retorno-cadeia-p) 0)) 
					(setf valoresFechadas  (insere-ordenado (second retorno-cadeia-p) valoresFechadas)))
				(dolist (pos-moeda-analisada (third retorno-cadeia-p))
					(setf lista-pos-moedas (remove-if (lambda (x) (equalp x pos-moeda-analisada)) lista-pos-moedas))))
					
			(dolist (valorF valoresFechadas) 
				(if (evenp controleLista) 
					(progn (setf valorPar (+ valorPar valorF)) (incf controleLista))
					(progn (setf valorImpar (+ valorImpar valorF)) (decf controleLista))))
			
			(if (= (jogo-jogador jogo) 1)
				(progn (setf valor1 (+ valor1 valorPar))
					   (setf valor2 (+ valor2 valorImpar)))
				(progn (setf valor1 (+ valor1 valorImpar))
					   (setf valor2 (+ valor2 valorPar))))
					
			(if (= jogadormax 1)
				(- valor1 valor2)
				(- valor2 valor1)))) 
				
(defun avaliacao-v8 (jogo jogadormax)
	(let* ((tabuleiro (jogo-tabuleiro jogo))
		   (valor1 (jogo-pontos-jogador1 jogo))
		   (valor2 (jogo-pontos-jogador2 jogo))
		   (lista-pos-moedas (converte-lista-fios-lista-moedas (tabuleiro-fios tabuleiro)))
		   (retorno-cadeia-p nil)
		   (pontos-aberta 0)
		   (valoresFechadas (list))
		   (valorImpar 0)
		   (valorPar 0)
		   (controleLista 1))
		   	
			(loop for while t do
				(when (null lista-pos-moedas)
					  (return))
				(setf retorno-cadeia-p (cadeia-p tabuleiro (first lista-pos-moedas)))
				(if (first retorno-cadeia-p) 
					(setf pontos-aberta (+ pontos-aberta (second retorno-cadeia-p)))
					(when (> (second retorno-cadeia-p) 0) (setf valoresFechadas  (insere-ordenado2 (second retorno-cadeia-p) valoresFechadas))))
				(dolist (pos-moeda-analisada (third retorno-cadeia-p))
					(setf lista-pos-moedas (remove-if (lambda (x) (equalp x pos-moeda-analisada)) lista-pos-moedas))))
					
			(dolist (valorF valoresFechadas) 
				(if (evenp controleLista) 
					(progn (setf valorPar (+ valorPar valorF)) (incf controleLista))
					(progn (setf valorImpar (+ valorImpar valorF)) (decf controleLista))))
			
			(if (= (jogo-jogador jogo) 1)
				(progn (setf valor1 (+ valor1 pontos-aberta valorPar))
					   (setf valor2 (+ valor2 valorImpar)))
				(progn (setf valor1 (+ valor1 valorImpar))
					   (setf valor2 (+ valor2 pontos-aberta valorPar))))
					
			(if (= jogadormax 1)
				(- valor1 valor2)
				(- valor2 valor1)))) 
;;;; =========================================================
;;;; ============  MINIMAX-TABELA DE TRANSPOSICAO  ===========
;;;; =========================================================

(defstruct jogo-equivalente jogador-act pontos-j1 pontos-j2 lista-fios)

(defun cria-jogo-equivalente (jogo)
	(make-jogo-equivalente
		   :jogador-act (jogo-jogador jogo) 
		   :pontos-j1 (jogo-pontos-jogador1 jogo) 
		   :pontos-j2 (jogo-pontos-jogador2 jogo)
		   :lista-fios (tabuleiro-fios (jogo-tabuleiro jogo)))) ; acho que aqui nao ha necessidade de fazer copia

(defun cria-tabela-transposicao ()
	(make-hash-table :test #'equalp))

(defun procura-estado-minimax (jogo hash)
	(gethash (cria-jogo-equivalente jogo) hash))

(defun adiciona-estado-minimax! (jogo minimax hash)
	(setf (gethash (cria-jogo-equivalente jogo) hash) minimax))

;;;; =========================================================
;;;; =================  AUXILIARES MINIMAX  ==================
;;;; =========================================================

(defparameter *visitados* 1)
(defparameter *total_visitados* 1)
(defparameter *terminais* 1)

(defun tempo-passado (tempo-inicio)
	(/ (- (get-internal-real-time) tempo-inicio) internal-time-units-per-second))


;;;; =========================================================
;;;; ====================  MINIMAX-V1  =======================
;;;; =========================================================
;;;; CORTES alfa e beta e calcula sucessores para cada estado
;;;; =========================================================

(defun minimax-alfa-beta-v1 (jogo jogadormax profundidade-max tempo-limite tempo-inicio)
	(labels ((calculaSucessores (lista estado)
				(if (null lista)
					(list)
					(cons (cons (first lista) (resultado  estado (first lista))) 
						  (calculaSucessores (rest lista) estado))))
			(minimax-alfa-beta-v1-aux (estado profundidade alfa beta &optional (inicial nil))
				(let ((jogador (jogo-jogador estado))
					  (parEscolhido (cons nil nil))
					  (alfabeta nil)
					  (iminimax nil)
					  (op-max-min nil)
					  (op-teste-corte nil)
					  (comp nil))
					  	(when (< tempo-limite (tempo-passado tempo-inicio)) (return-from minimax-alfa-beta-v1 nil))
						(cond  ((jogo-terminado-p jogo) (utilidade estado jogadormax))
								((= 0 profundidade) (avaliacao-v1 estado jogadormax))
								(t
									(if (= jogador jogadormax)
										(progn 
											(setf alfabeta alfa)
											(setf comp beta)
											(setf op-max-min #'>)
											(setf op-teste-corte #'>=)
											(setf (cdr parEscolhido) -99999))
										(progn
											(setf alfabeta beta)
											(setf comp alfa)
											(setf op-max-min #'<)
											(setf op-teste-corte #'<=)
											(setf (cdr parEscolhido) 99999)))

									;; constroi lista de pares com (id , estado-aplica-jogada-id)
									(dolist (x (calculaSucessores (accoes estado) estado)) 
										(if (= jogador jogadormax)
											(setf iminimax (minimax-alfa-beta-v1-aux (cdr x) (1- profundidade) alfabeta beta))
											(setf iminimax (minimax-alfa-beta-v1-aux (cdr x) (1- profundidade) alfa alfabeta)))
										(when (funcall op-max-min iminimax alfabeta) ; alfa = max( alfa, minimax-alfa-beta) ou beta= min(beta, minimax-alfa-beta)
											(setf alfabeta iminimax))
										(when (funcall op-max-min iminimax (cdr parEscolhido)) ;Actualizacao dos intervalos [alfa, beta] : minimax > alfa ou minimax < beta
											(setf parEscolhido (cons (car x) iminimax)))
										(when (funcall op-teste-corte alfabeta comp); teste de corte
											(return)))
									(if inicial
										(car parEscolhido)
										(cdr parEscolhido)))))))
			(minimax-alfa-beta-v1-aux jogo profundidade-max -999999 999999 t)))


;;;; =========================================================
;;;; ====================  MINIMAX-V2  =======================
;;;; =========================================================
;;;; CORTES alfa e beta
;;;; Apenas gera os sucessores que nao sao cortados
;;;; =========================================================

(defun minimax-alfa-beta-v2 (jogo jogadormax profundidade-max tempo-limite tempo-inicio)
	(labels ((minimax-alfa-beta-v2-aux (estado profundidade alfa beta &optional (inicial nil))
				(let ((jogador (jogo-jogador estado))
					  (parEscolhido (cons nil nil))
					  (alfabeta nil)
					  (iminimax nil)
					  (op-max-min nil)
					  (op-teste-corte nil)
					  (sucessores nil)
					  (comp nil))
					  	(when (< tempo-limite (tempo-passado tempo-inicio)) (return-from minimax-alfa-beta-v2 nil))
						(cond  ((jogo-terminado-p jogo) (utilidade estado jogadormax))
								((= 0 profundidade) (avaliacao-v4 estado jogadormax))  ;devolve a utilidade para o jogador nesse estado
								(t
									(if (= jogador jogadormax)
										(progn 
											(setf alfabeta alfa)
											(setf comp beta)
											(setf op-max-min #'>)
											(setf op-teste-corte #'>=)
											(setf (cdr parEscolhido) -99999))
										(progn
											(setf alfabeta beta)
											(setf comp alfa)
											(setf op-max-min #'<)
											(setf op-teste-corte #'<=)
											(setf (cdr parEscolhido) 99999)))
									(dolist (x (accoes estado)) ;; constroi lista de pares com (id , estado-aplica-jogada-id)
										(setf sucessores (cons x (resultado estado x)))
										(if (= jogador jogadormax)
											(setf iminimax (minimax-alfa-beta-v2-aux (cdr sucessores) (1- profundidade) alfabeta beta))
											(setf iminimax (minimax-alfa-beta-v2-aux (cdr sucessores) (1- profundidade) alfa alfabeta)))
										(when (funcall op-max-min iminimax alfabeta) ; alfa = max( alfa, minimax-alfa-beta) ou beta= min(beta, minimax-alfa-beta)
											(setf alfabeta iminimax))
										(when (funcall op-max-min iminimax (cdr parEscolhido)) ;Actualizacao dos intervalos [alfa, beta] : minimax > alfa ou minimax < beta
											(setf parEscolhido (cons x iminimax)))
										(when (funcall op-teste-corte alfabeta comp); teste de corte
											(return)))
									(if inicial
										(car parEscolhido)
										(cdr parEscolhido)))))))
			(minimax-alfa-beta-v2-aux jogo profundidade-max -999999 999999 t)))


;;;; =========================================================
;;;; ====================  MINIMAX-V3  =======================
;;;; =========================================================
;;;; CORTES alfa e beta
;;;; Apenas gera os sucessores que nao sao cortados
;;;; Usa tabela de transposicao
;;;; Simplificacao da logica do minimax-alfa-beta
;;;; =======================		
(defun avaliacao-v6 (jogo jogadormax)
	(let* ((tabuleiro (jogo-tabuleiro jogo))
		   (valor1 (jogo-pontos-jogador1 jogo))
		   (valor2 (jogo-pontos-jogador2 jogo))
		   (lista-pos-moedas (converte-lista-fios-lista-moedas (tabuleiro-fios tabuleiro)))
		   (retorno-cadeia-p nil)
		   (pontos-aberta 0))
		   
		
		(loop for while t do
			(when (null lista-pos-moedas)
				  (return))
			(setf retorno-cadeia-p (cadeia-p tabuleiro (first lista-pos-moedas)))
			(when (first retorno-cadeia-p) 
				(setf pontos-aberta (+ pontos-aberta (second retorno-cadeia-p))))
			
			(dolist (pos-moeda-analisada (third retorno-cadeia-p))
				(setf lista-pos-moedas (remove-if (lambda (x) (equalp x pos-moeda-analisada)) lista-pos-moedas))))
		
			(if (= (jogo-jogador jogo) 1)
				 (setf valor1 (+ valor1 pontos-aberta))
				 (setf valor2 (+ valor2 pontos-aberta)))
				
			(if (= jogadormax 1)
				(- valor1 valor2)
				(- valor2 valor1))))

(defun minimax-alfa-beta-v3 (jogo jogadormax profundidade-max tempo-limite tempo-inicio)
	(let ((tabela-transposicao (cria-tabela-transposicao)))
			(labels ((minimax-alfa-beta-v3-aux (estado profundidade alfa beta &optional (inicial nil))
				(let ((jogador (jogo-jogador estado))
					  (parEscolhido (cons nil nil))
					  (iminimax nil))
					  	(when (< tempo-limite (tempo-passado tempo-inicio)) (return-from minimax-alfa-beta-v3 nil))

					  	(setf iminimax (procura-estado-minimax estado tabela-transposicao))

						(cond   ((not (null iminimax)) iminimax)
								((teste-terminal-p estado profundidade) (utilidade estado jogadormax))
								((= 0 profundidade) (utilidade estado jogadormax))
								(t
									(if (= jogador jogadormax)
										(setf (cdr parEscolhido) -99999)
										(setf (cdr parEscolhido)  99999))

									;; constroi lista de pares com (id , estado-aplica-jogada-id)
									(dolist (x (accoes estado)) 
										(incf *visitados*)
										(setf iminimax (minimax-alfa-beta-v3-aux (resultado estado x) (1- profundidade) alfa beta))

										(if (= jogador jogadormax)
											(progn 
												(when (> iminimax alfa) ; alfa = max( alfa, minimax-alfa-beta) ou beta= min(beta, minimax-alfa-beta)
													(setf alfa iminimax))
												(when (> iminimax (cdr parEscolhido)) ;Escolha da jogada melhor ate ao momento
													(setf parEscolhido (cons x iminimax))))
											(progn
												(when (< iminimax beta)
													(setf beta iminimax))
												(when (< iminimax (cdr parEscolhido)) ;Actualizacao dos intervalos [alfa, beta] : minimax > alfa ou minimax < beta
													(setf parEscolhido (cons x iminimax)))))
										(when (>= alfa beta); teste de corte
											  ;(setf jogador 0)
											  (return)))
									
									;(when (= jogador 0)
										  (adiciona-estado-minimax! estado iminimax tabela-transposicao)
									(if inicial
										(car parEscolhido)
										(cdr parEscolhido)))))))
			(minimax-alfa-beta-v3-aux jogo profundidade-max -999999 999999 t)))) 

			
(defun minimax-alfa-beta-vcopia (jogo jogadormax profundidade-max tempo-limite tempo-inicio)
	(labels ((minimax-alfa-beta-vcopia-aux (estado profundidade alfa beta &optional (inicial nil))
				(let ((jogador (jogo-jogador estado))
					  (parEscolhido (cons nil nil))
					  (iminimax nil))
					  	(when (< tempo-limite (tempo-passado tempo-inicio)) (return-from minimax-alfa-beta-vcopia nil))
						(cond  ((teste-terminal-p estado profundidade) (utilidade estado jogadormax))
								((= 0 profundidade) (utilidade estado jogadormax))  ;devolve a utilidade para o jogador nesse estado
								(t
									(if (= jogador jogadormax)
										(setf (cdr parEscolhido) -99999)
										(setf (cdr parEscolhido)  99999))
									
									(dolist (x (accoes estado)) ;; constroi lista de pares com (id , estado-aplica-jogada-id)
										;(incf *visitados*)
										(setf iminimax (minimax-alfa-beta-vcopia-aux (resultado estado x) (1- profundidade) alfa beta))

										(if (= jogador jogadormax)
											(progn 
												(when (> iminimax alfa) ; alfa = max( alfa, minimax-alfa-beta) ou beta= min(beta, minimax-alfa-beta)
													(setf alfa iminimax))
												(when (> iminimax (cdr parEscolhido)) ;Escolha da jogada melhor ate ao momento
													(setf parEscolhido (cons x iminimax))))
											(progn
												(when (< iminimax beta)
													(setf beta iminimax))
												(when (< iminimax (cdr parEscolhido)) ;Actualizacao dos intervalos [alfa, beta] : minimax > alfa ou minimax < beta
													(setf parEscolhido (cons x iminimax)))))
										(when (>= alfa beta); teste de corte
											  (return)))
									
									;(print 'par-escolhido)
									;(print parEscolhido)
									(if inicial
										(car parEscolhido)
										(cdr parEscolhido)))))))
			(minimax-alfa-beta-vcopia-aux jogo profundidade-max -999999 999999 t)))
			
(defun minimax-alfa-beta-vcopia2 (jogo jogadormax profundidade-max tempo-limite tempo-inicio)
	(labels ((minimax-alfa-beta-vcopia-aux (estado profundidade alfa beta &optional (inicial nil))
				(let ((jogador (jogo-jogador estado))
					  (parEscolhido (cons nil nil))
					  (iminimax nil))
					  	(when (< tempo-limite (tempo-passado tempo-inicio)) (return-from minimax-alfa-beta-vcopia2 nil))
						(cond  ((teste-terminal-p estado profundidade) (utilidade estado jogadormax))
								((= 0 profundidade) (avaliacao-v7 estado jogadormax))  ;devolve a utilidade para o jogador nesse estado
								(t
									(if (= jogador jogadormax)
										(setf (cdr parEscolhido) -99999)
										(setf (cdr parEscolhido)  99999))
									
									(dolist (x (accoes estado)) ;; constroi lista de pares com (id , estado-aplica-jogada-id)
										(incf *visitados*)
										(setf iminimax (minimax-alfa-beta-vcopia-aux (resultado estado x) (1- profundidade) alfa beta))

										(if (= jogador jogadormax)
											(progn 
												(when (> iminimax alfa) ; alfa = max( alfa, minimax-alfa-beta) ou beta= min(beta, minimax-alfa-beta)
													(setf alfa iminimax))
												(when (> iminimax (cdr parEscolhido)) ;Escolha da jogada melhor ate ao momento
													(setf parEscolhido (cons x iminimax))))
											(progn
												(when (< iminimax beta)
													(setf beta iminimax))
												(when (< iminimax (cdr parEscolhido)) ;Actualizacao dos intervalos [alfa, beta] : minimax > alfa ou minimax < beta
													(setf parEscolhido (cons x iminimax)))))
										(when (>= alfa beta); teste de corte
											  (return)))
									
									;(print 'par-escolhido)
									;(print parEscolhido)
									(if inicial
										(car parEscolhido)
										(cdr parEscolhido)))))))
			(minimax-alfa-beta-vcopia-aux jogo profundidade-max -999999 999999 t)))
;;;; =========================================================
;;;; ===================  MINIMAX-VBEST  ====================
;;;; =========================================================

;factorial-limitado
;factorial ate um limite passado como argumento
(defun factorial-limitado (fios limite)
	(if (= fios limite)
		limite
		(* fios (factorial-limitado (1- fios) limite))))


; jogador-minimax-vbest
; E estimada uma profundidade minima analisando o tempo dispensado numa iteracao de baixa profundidade
; Utiliza a versao 3 do algoritmo minimax alfa beta com tabelas de transposicao e cortes alfa e beta
; e com uma logica de minimax alfa e beta simplificada
;
; Utiliza a heuristica v5 que e a mais pesada: O jogador fica com as cadeias abertas e as cadeias fechadas
; alternadas dado uma lista de valores de cadeias fechadas em ordem crescente
(defun jogador-minimax-vbest (jogo jogadormax tempo-limite)
	(let ((tempo-inicio (get-internal-real-time))
		  (profundidade-max 1)
		  (tempo-prof nil)
		  (jogada nil)
		  (tempo-max (* tempo-limite 0.95))
		  (temp nil)
		  (node-time nil)
		  (nfios (length (tabuleiro-fios (jogo-tabuleiro jogo)))))
			;CALCULO DA PROFUNDIDADE MINIMA
			;funcionamento: corre uma vez o minimax para profundidade 1 e divide o tempo que demorou pelo numero de nos ate essa profundidade.
			;				enquanto o tempo maximo for maior que o tempo para explorar todos os nos ate ao nivel corrente, increvementa a profundidade maxima.
			;				esta e a profundidade maxima a ser passada para a primeira iteracao do minimax.
			(when (> nfios 2)
				(setf tempo-prof (get-internal-real-time))
				(setf jogada (minimax-alfa-beta-v3 jogo jogadormax 2 tempo-max tempo-inicio))
				(setf node-time (* (/ (tempo-passado tempo-prof) (factorial-limitado nfios (- nfios 2))) 0.7))
				;(print 'node_time)
				;(print node-time)
				;(print 'nfios)
				;(print nfios)
				(loop while (and (> tempo-max (* node-time (factorial-limitado nfios (- nfios profundidade-max)))) (< profundidade-max nfios)) do
					(incf profundidade-max)))
			(decf profundidade-max)
			(loop while (and (> tempo-max (tempo-passado tempo-inicio)) (<= profundidade-max nfios))  do
				(print '***********_BOSS_****************)
				(print 'profundidade_actual)
				(print profundidade-max)
				(setf temp (minimax-alfa-beta-v3 jogo jogadormax profundidade-max tempo-max tempo-inicio))
				
				(when (not (null temp)) (setf jogada temp))
				
				(incf profundidade-max)
				(print 'visitados_nivel)
				(print *visitados*)
				(print 'jogada_escolhida)
				(print jogada)
				(print '---------------------------)
				(setf *total_visitados* (+ *total_visitados* *visitados*))
				(setf *visitados* 1))
			(if (> profundidade-max nfios)
				(print 'stopped_because_reached_end_of_tree)
				(print 'stopped_because_reached_end_of_time))
			
			(print 'total_visitados)
			(print *total_visitados*)
			(print 'total_terminais)
			(print *terminais*)
			
			(setf *total_visitados* 1)
			jogada))

(defun jogador-minimax-vbestc (jogo jogadormax tempo-limite)
	(let ((tempo-inicio (get-internal-real-time))
		  (profundidade-max 1)
		  (tempo-prof nil)
		  (jogada nil)
		  (tempo-max (* tempo-limite 0.95))
		  (temp nil)
		  (node-time nil)
		  (nfios (length (tabuleiro-fios (jogo-tabuleiro jogo)))))
			;CALCULO DA PROFUNDIDADE MINIMA
			;funcionamento: corre uma vez o minimax para profundidade 1 e divide o tempo que demorou pelo numero de nos ate essa profundidade.
			;				enquanto o tempo maximo for maior que o tempo para explorar todos os nos ate ao nivel corrente, increvementa a profundidade maxima.
			;				esta e a profundidade maxima a ser passada para a primeira iteracao do minimax.
			(when (> nfios 2)
				(setf tempo-prof (get-internal-real-time))
				(setf jogada (minimax-alfa-beta-vcopia2 jogo jogadormax 2 tempo-max tempo-inicio))
				(setf node-time (* (/ (tempo-passado tempo-prof) (factorial-limitado nfios (- nfios 2))) 0.7))
				;(print 'node_time)
				;(print node-time)
				;(print 'nfios)
				;(print nfios)
				(loop while (and (> tempo-max (* node-time (factorial-limitado nfios (- nfios profundidade-max)))) (< profundidade-max nfios)) do
					(incf profundidade-max)))
			(decf profundidade-max)
			(loop while (and (> tempo-max (tempo-passado tempo-inicio)) (<= profundidade-max nfios))  do
				(print '***********_FRACO_****************)
				(print 'profundidade_actual)
				(print profundidade-max)
				(setf temp (minimax-alfa-beta-vcopia2 jogo jogadormax profundidade-max tempo-max tempo-inicio))
				
				(when (not (null temp)) (setf jogada temp))
				
				(incf profundidade-max)
				(print 'visitados_nivel)
				(print *visitados*)
				(print 'jogada_escolhida)
				(print jogada)
				(print '---------------------------)
				(setf *total_visitados* (+ *total_visitados* *visitados*))
				(setf *visitados* 1))
			(if (> profundidade-max nfios)
				(print 'stopped_because_reached_end_of_tree)
				(print 'stopped_because_reached_end_of_time))
			
			(print 'total_visitados)
			(print *total_visitados*)
			(print 'total_terminais)
			(print *terminais*)
			
			(setf *total_visitados* 1)
			jogada))
			
(defun jogador-compara (tab tempo)
 (desenha-tabuleiro tab)
 (print '-------------VBEST----------------)
 (print (jogador-minimax-vbest (cria-jogo tab) 1 tempo))
 (print '-------------VCOPIA----------------)
 (print (jogador-minimax-vbestc (cria-jogo tab) 1 tempo))

 (print '-------------IGNORETHIS----------------))

;;;;=================================================
(load "interface-moedas.fas")
(load "exemplos.fas")
;;;;=================================================
