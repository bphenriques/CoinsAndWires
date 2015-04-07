Interface-moedas-fios

I am waiting for the professor response in order to make the code available publicly in his name. Until then, I the interface is available (no implementation).

;; acrescentei algumas funcoes auxiliares que vao dar jeito para testar automaticamente o codigo dos alunos
(defun ignore-value (x)
	
;; dois fios sao iguais se tem o mesmo id, a mesma origem e o mesmo destino
(defun fios-iguais-p (f1 f2)
		 
;; muito dificil fazer esta funcao... :D
(defun conjuntos-iguais-p (l1 l2 &key (test #'equal))

;; funcao compara - compara dois jogadores num tabuleiro, fazendo dois jogos, um em que o jogador1 joga em primeiro lugar,
;; e outro em que o jogador2 em primeiro lugar. A pontuacao final e obtida pela diferenca de pontos nos dois jogos.
(defun compara (tabuleiro jogador1 jogador2 &optional (tempo-limite 30) &key (silencioso NIL))
	   
;;funcao tempo-decorrido - da-nos o tempo (em segundos) decorrido desde um instante de tempo recebido como
;; argumento. O instante de tempo recebido deve estar em internal-time-units.
(defun tempo-decorrido (tempo-inicio)
		
;;; funcao joga com implementacao restrita de tempo-limite. Se algum dos jogadores ultrapassar o tempo limite permitido
;;; esse jogador perde automaticamente o jogo pelo maior numero de pontos possivel (ou seja como se ele tivesse 0 moedas
;;; e o oponente todas as moedas). Isto e para desencorajar ultrapassar o tempo limite por 0.1 segundos que sejam.
(defun joga-tempo-limite (tabuleiro jogador1 jogador2 &optional (tempo-limite 30) &key (silencioso NIL))
		
;;; joga: tabuleiro x funcao x funcao x inteiro --> inteiro
;;; funcao que recebe um tabuleiro; duas funcoes correspondentes a jogadores, i.e. funcoes que recebem um jogo e um id de jogador e devolvem 
;;; uma jogada; e um inteiro opcional que representa o tempo limite (em segundos) que cada jogador tem para tomar uma decisao (se nao for especificado
;;; e usado o tempo limite por omissao de 30 segundos). Esta funcao executa um jogo entre os dois jogadores recebidos, criando um jogo a partir
;:; do tabuleiro recebido, e pedindo a cada jogador (alternadamente ou nao) uma jogada. A funcao acaba quando o jogo e considerado terminado
;;; e retorna a diferenca de pontos entre o jogador 1 e 2. Ou seja, se o jogador 1 ganhou, retorna um valor positivo, se o jogador 2 ganhou
;;; retorna um valor negativo, e retorna 0 em caso de empate. 
;;; Importante: esta funcao nao altera o tabuleiro recebido, pois faz uma copia dele
(defun joga (tabuleiro jogador1 jogador2 &optional (tempo-limite 30))

;;; jogador-humano: jogo x inteiro x inteiro --> inteiro 
;;; funcao que recebe um jogo, um inteiro correspondente ao ID de um jogador, e um inteiro opcional que indica o tempo
;;; limite para tomar uma decisao. Retorna um inteiro correspondente ao numero do fio que o jogador pretende cortar.
;;; Esta funcao serve de interface com o utilizador, pedindo-lhe para escrever o codigo do fio desejado, e verificando
;;; se este e valido. Se o numero introduzido nao for valido, esta funcao escreve uma mensagem de erro no ecra e
;;; volta a pedir um novo numero.
;;; Apesar de receber o limite de tempo, nao sera tido em conta nesta funcao. Assim o jogador pode levar o tempo que
;;; entender para tomar sua decisao. Esta funcao recebe o limite de tempo apenas para ser chamado da mesma maneira que
;;; um jogador automatico.
(defun jogador-humano (jogo jogador &optional tempo-limite)
		
	
;;; desenha-jogo: jogo --> {}
;;; funcao que recebe um jogo, e desenha o estado do jogo no ecra, incluindo informacao acerca do numero de pontos por jogador
;;; nao retorna nada
(defun desenha-jogo (jogo)
	
;;; desenha-tabuleiro: tabuleiro --> {}
;;; funcao que recebe um tabuleiro e desenha o tabuleiro no ecra, com todas as moedas e fios
;;; nao retorna nada 
(defun desenha-tabuleiro (tabuleiro)	
	
;;; funcoes auxiliares para desenhar o tabuleiro, nao as defini com flet/labels porque a indentacao ia ficar muito dificil
;;; de perceber

;;; desenha-barra-horizontal: tabuleiro --> {}
;;; funcao que recebe um tabuleiro e desenha uma linha horizontal com o caracter "="
;;; a linha tem o tamanho correspondente ao tamanho do tabuleiro recebido
;;; esta funcao nao retorna nada
(defun desenha-barra-horizontal (tabuleiro)

;;; desenha-linha-tabuleiro: tabuleiro x inteiro positivo --> {}
;;; funcao que recebe um tabuleiro e um inteiro >= 0 que indica uma linha do tabuleiro, e desenha no ecra as moedas e fios 
;;; correspondente a linha do tabuleiro
;;; Uma linha do tabuleiro corresponde a 6 linhas impressas (chamadas de sublinhas) no ecra. Podemos ver um exemplo das 6 sublinhas
;;; impressas para a linha 0 e coluna 0, com uma moeda na posicao 0,0 e varios fios ligando as moedas adjacentes a posicao 0,0
;;; (3)---04-- 
;;;  | \     /  
;;;  | 03      
;;;  02   x    
;;;  |  04     
;;;  | /     \ 
(defun desenha-linha-tabuleiro (tabuleiro linha)
			
;;; obtem-vector-fio: inteiro positivo x inteiro positivo x inteiro positivo --> posicao
;;; recebe um inteiro correspondente a uma linha do tabuleiro, um inteiro correspondente
;;; a uma coluna, e um inteiro correspondente a um fio, e verifica se o fio esta ligado
;;; a linha e coluna do tabuleiro. Se o fio nao estiver ligado a funcao retorna NIL, se o fio estiver
;;; ligado retorna uma posicao que representa o vector do fio. Por exemplo, um fio horizontal
;;; e representado por (1,0) ou (-1,0). Existe um caso especial usado para detectar um fio que embora
;;; nao esteja ligado a linha/coluna, corresponde a um fio diagonal inverso (este fio tem que ser desenhado
;;; juntamente com os fios ligados). Neste caso e retornado o vector (-2,2).  			
(defun obtem-vector-fio (linha coluna fio)
				  
;;; obtem-vector: posicao x posicao --> posicao
;;; recebe uma posicao pOrigem, e outra posicao pDestino e devolve o vector
;;; que corresponde a diferenca entre o destino e a origem. O vector retornado
;;; e representado tambem atraves de uma posicao
(defun obtem-vector (pOrigem pDestino)
 
;;;	calcula-id-fios-a-imprimir: inteiro positivo x inteiro positivo x lista --> lista			  
;;;	recebe um inteiro correspondente a uma linha, um inteiro correspondente a uma coluna,
;;;	uma lista com todos os id's de fios do tabuleiro, e determina qual o id do fio horizontal/vertical/diagonal que deve
;;; ser desenhado juntamente com a linha e coluna recebidas. Retorna uma lista com 4 elementos.
;;; O 1 elemento corresponde ao id do fio horizontal (ou NIL se nao existir),
;;; o 2 ao id do fio vertical (ou NIL se nao existir), o 3 ao id do fio diagonal (ou NIL se nao existir),
;;; o 4 ao id do fio diagonal inverso (ou NIL se nao existir).
(defun calcula-id-fios-a-imprimir (linha coluna fios)

(defun vectores-iguais-p (v1 v2)

;;; desenha-sublinha-tabuleiro: tabuleiro x inteiro positivo x inteiro positivo x vector --> {}
;;; funcao que recebe um tabuleiro, um inteiro >= 0 correspondente a uma linha,
;;; um inteiro >= 0 correspondente a sublinha, e um array unidimensional com tamanho igual ao numero
;;; de colunas do tabuleiro, que contem em cada posicao uma lista com os ids dos fios (horizontais, verticais e 
;;; diagonais que estao ligados a coluna correspondente
;;; a funcao vai desenhar a sublinha recebida no ecra, tendo em conta os fios que estao ligados as moedas da linha 
;;; nao retorna nada
(defun desenha-sublinha-tabuleiro (tabuleiro linha sublinha array-ids-fios-a-imprimir)
		

;;; desenha-sublinha-celula: inteiro positivo x inteiro positivo x lista --> {}
;;; recebe um inteiro correspondente a uma sublinha de uma celula, um inteiro correspondente a uma moeda ou NIL se
;;; nao existir moeda na celula, e uma lista de 4 elementos com os ids dos fios horizontal,vertical,diagonal e diagonal inverso
;;; a desenhar na celula. Esta funcao desenha uma sublinha da celula em questao.
;;; Uma celula corresponde ao conjunto de todas as sublinhas e subcolunas correspondentes a uma linha e coluna do tabuleiro
;;; Por exemplo, esta celula corresponde a linha 0 e coluna 0 do tabuleiro, e tem uma moeda de valor - 3, um fio horizontal - 4,
;;; um fio vertical - 2, um fio diagonal - 3, e um fio diagonal inversa 5.
;;; (3)---04-- 
;;;  | \     /  
;;;  | 03      
;;;  02   x    
;;;  |  05     
;;;  | /     \
(defun desenha-sublinha-celula (sublinha moeda ids-fios-a-imprimir)

;;; desenha-sublinha-celula-ultima-coluna: inteiro positivo x inteiro positivo x lista --> {}
;;; ver definicao da funcao anterior. Esta funcao e um caso particular da funcao anterior,
;;; porque as celulas correspondentes a ultima coluna do tabuleiro so tem uma unica subcoluna.
;;; Isto acontece porque nao existe nenhum fio que ligue moedas da ultima coluna a uma coluna mais a 
;;; direita.				
(defun desenha-sublinha-celula-ultima-coluna (sublinha moeda ids-fios-a-imprimir)
			
;;; desenha-celula-sl0-sc0: inteiro positivo x lista --> {}
;;; recebe um inteiro correspondente a uma moeda (ou NIL caso nao exista), e uma lista 4 elementos com os
;;; ids dos fios horizontal,vertical,diagonal e diagonal inversa a desenhar na celula. 
;;; Esta funcao desenha a subcoluna 0 da sublinha 0 da celula. Nao retorna nada.
(defun desenha-celula-sl0-sc0 (moeda ids-fios-a-imprimir)
	
;;; desenha-celula-sl0: inteiro positivo x lista --> {}
;;; recebe um inteiro correspondente a uma moeda (ou NIL caso nao exista), e uma lista 4 elementos com os
;;; ids dos fios horizontal,vertical,diagonal e diagonal inversa a desenhar na celula. 
;;; Esta funcao desenha a sublinha 0 da celula. Nao retorna nada.
(defun desenha-celula-sl0 (moeda ids-fios-a-imprimir)

;;; desenha-celula-sl1-sc0: inteiro positivo x lista --> {}
;;; recebe um inteiro correspondente a uma moeda (ou NIL caso nao exista), e uma lista 4 elementos com os
;;; ids dos fios horizontal,vertical,diagonal e diagonal inversa a desenhar na celula. 
;;; Esta funcao desenha a subcoluna 0 da sublinha 1 da celula. Nao retorna nada.		
(defun desenha-celula-sl1-sc0 (moeda ids-fios-a-imprimir)

;;; desenha-celula-sl1: inteiro positivo x lista --> {}
;;; recebe um inteiro correspondente a uma moeda (ou NIL caso nao exista), e uma lista 4 elementos com os
;;; ids dos fios horizontal,vertical,diagonal e diagonal inversa a desenhar na celula. 
;;; Esta funcao desenha a sublinha 1 da celula. Nao retorna nada.	
(defun desenha-celula-sl1 (moeda ids-fios-a-imprimir)

;;; desenha-celula-sl2: inteiro positivo x lista --> {}
;;; recebe um inteiro correspondente a uma moeda (ou NIL caso nao exista), e uma lista 4 elementos com os
;;; ids dos fios horizontal,vertical,diagonal e diagonal inversa a desenhar na celula. 
;;; Esta funcao desenha a sublinha 2 da celula. Nao retorna nada.	
(defun desenha-celula-sl2 (moeda ids-fios-a-imprimir)
	
;;; desenha-celula-sl3-sc0: inteiro positivo x lista --> {}
;;; recebe um inteiro correspondente a uma moeda (ou NIL caso nao exista), e uma lista 4 elementos com os
;;; ids dos fios horizontal,vertical,diagonal e diagonal inversa a desenhar na celula. 
;;; Esta funcao desenha a subcoluna 0 da sublinha 3 da celula. Nao retorna nada.
(defun desenha-celula-sl3-sc0 (moeda ids-fios-a-imprimir)

;;; desenha-celula-sl3: inteiro positivo x lista --> {}
;;; recebe um inteiro correspondente a uma moeda (ou NIL caso nao exista), e uma lista 4 elementos com os
;;; ids dos fios horizontal,vertical,diagonal e diagonal inversa a desenhar na celula. 
;;; Esta funcao desenha a sublinha 3 da celula. Nao retorna nada.		
(defun desenha-celula-sl3 (moeda ids-fios-a-imprimir)
		  
;;; desenha-celula-sl4: inteiro positivo x lista --> {}
;;; recebe um inteiro correspondente a uma moeda (ou NIL caso nao exista), e uma lista 4 elementos com os
;;; ids dos fios horizontal,vertical,diagonal e diagonal inversa a desenhar na celula. 
;;; Esta funcao desenha a sublinha 4 da celula. Nao retorna nada.			  
(defun desenha-celula-sl4 (moeda ids-fios-a-imprimir)

;;; desenha-celula-sl5: inteiro positivo x lista --> {}
;;; recebe um inteiro correspondente a uma moeda (ou NIL caso nao exista), e uma lista 4 elementos com os
;;; ids dos fios horizontal,vertical,diagonal e diagonal inversa a desenhar na celula. 
;;; Esta funcao desenha a sublinha 5 da celula. Nao retorna nada.	
(defun desenha-celula-sl5 (moeda ids-fios-a-imprimir)