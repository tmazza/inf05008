;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lab-05-extra) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp")) #f)))
;; LISTA EXTRA-CLASSE LAB 5
;; OLIVER HUNG BUO TSO, 231519
;; 2016/06/14

(define-struct triangle (v1 v2 v3))
;; Um elemento triangle do conjunto TRIANGLE é uma estrutura:
;;   (make-triangle v1 v2 v3),
;; onde:
;;   v1: Posn, primeiro vértice do triângulo,
;;   v2: Posn, segundo vértice do triângulo,
;;   v3: Posn, terceiro vértice do triângulo.

(define BOUND-X 500)
(define BOUND-Y 600)

(define CORES (list 'Red 'Orange 'Yellow 'Green 'Blue 'Purple))

(define A (make-posn 200 27))
(define B (make-posn 27 300))
(define C (make-posn 373 300))
(define D (make-posn 50 20))
(define E (make-posn 350 250))
(define F (make-posn 90 380))
(define G (make-posn 350 650))

(define tri1 (make-triangle A B C))
(define tri2 (make-triangle D E F))
(define tri3 (make-triangle F D E))
(define tri4 (make-triangle G F D))

;; CONTRATO:
;;   draw-triangle: Posn Posn Posn Symbol -> Boolean
;; OBJETIVO:
;;   Recebendo 3 Posn e uma cor, desenha um triângulo nesta cor na tela.
(define (draw-triangle v1 v2 v3 color)
  (and
   (draw-solid-line v1 v2 color)
   (draw-solid-line v1 v3 color)
   (draw-solid-line v2 v3 color)))

;; Testes
(start BOUND-X BOUND-Y)
(draw-triangle A B C 'red)

;; CONTRATO:
;;   too-small?: Posn Posn Posn -> Boolean
;; OBJETIVO:
;;   Verifica se um triângulo é pequeno demais (um lado menor do que 10).
;; Exemplos:
;;   (too-small? (make-posn 5 0)
;;               (make-posn 0 10)
;;               (make-posn 10 10)) -> #false
;;
;;   (too-small? (make-posn 4 0)
;;               (make-posn 0 8)
;;               (make-posn 8 8)) -> #true
(define (too-small? v1 v2 v3)
  (local
    [(define min 10)
     (define (distance v1 v2)
       (sqrt (+
              (sqr (- (posn-x v1) (posn-x v2)))
              (sqr (- (posn-y v1) (posn-y v2))))))]
    (or
     (< (distance v1 v2) min)
     (< (distance v1 v3) min)
     (< (distance v2 v3) min))))

;; Testes
(check-expect (too-small?
               (make-posn 5 0)
               (make-posn 0 10)
               (make-posn 10 10))
              #false)

(check-expect (too-small?
               (make-posn 4 0)
               (make-posn 0 8)
               (make-posn 8 8))
              #true)

;; CONTRATO:
;;   sierpinski: Triangle Symbol -> Boolean
;; OBJETIVO:
;;   Desenha um triângulo de Sierpinski com vértices nos pontos passados
;;   como argumentos, da cor passada como argumento, e devolve true. Se o
;;   triângulo passado como argumento tiver dimensões muito pequenas, nada
;;   é desenhado.
(define (sierpinski v1 v2 v3 color)
  (local
    [(define (mid v1 v2)
       (/ (+ v1 v2) 2))
     (define (mid-point v1 v2)
       (make-posn
        (mid (posn-x v1) (posn-x v2))
        (mid (posn-y v1) (posn-y v2))))
     (define new-v1 (mid-point v1 v2))
     (define new-v2 (mid-point v2 v3))
     (define new-v3 (mid-point v3 v1))]
    (cond
      [(too-small? v1 v2 v3) #false]
      [(draw-triangle v1 v2 v3 color)
       (or
        (sierpinski v1 new-v1 new-v3 color)
        (sierpinski v2 new-v2 new-v1 color)
        (sierpinski v3 new-v3 new-v2 color))])))

;; Testes
(start BOUND-X BOUND-Y)
(sierpinski A B C 'red)

(start BOUND-X BOUND-Y)
(sierpinski D E F 'blue)

;; 1)

;; CONTRATO:
;;   out-of-bound? Triangle -> Boolean
;; OBJETIVO:
;;   Recebe um triângulo, retorna #true se o primeiro vértice dele estiver
;;   fora da janela, caso contrário, retorna #false.
;; EXEMPLOS:
;;   (out-of-bound? tri1) -> #false
;;   (out-of-bound? tri2) -> #false
;;   (out-of-bound? tri3) -> #true
(define (out-of-bound? tri)
  (local
    [(define v1 (triangle-v1 tri))
     (define x1 (posn-x v1))
     (define y1 (posn-y v1))]
    (cond
      [(and (<= x1 BOUND-X) (<= y1 BOUND-Y)) #false]
      [else #true])))

;; Testes
(check-expect (out-of-bound? tri1) #false)
(check-expect (out-of-bound? tri2) #false)
(check-expect (out-of-bound? tri4) #true)

;; CONTRATO:
;;   create-new-triangle: Triangle Number Number Boolean -> Triangle
;; OBJETIVO:
;;   Dados um triângulo, os deslocamentos nos eixos x e y, retorna um novo
;;   triângulo deslocado. Se tris2 for verdadeiro, o deslocamento no eixo y
;;   do terceiro vértice será negativo.
(define (create-new-triangle tri dx dy tris2)
  (local
    [(define (create-new-vertex v dx dy)
       (make-posn
        (+ (posn-x v) dx)
        (+ (posn-y v) dy)))]
    (make-triangle
     (create-new-vertex (triangle-v1 tri) dx dy)
     (create-new-vertex (triangle-v2 tri) dx dy)
     (cond
       [tris2 (create-new-vertex (triangle-v3 tri) dx (- dy))]
       [else (create-new-vertex (triangle-v3 tri) dx dy)]))))

;; Testes
(check-expect (create-new-triangle tri1 10 15 #false)
              (make-triangle
               (make-posn 210 42)
               (make-posn 37 315)
               (make-posn 383 315)))

(check-expect (create-new-triangle tri1 10 15 #true)
              (make-triangle
               (make-posn 210 42)
               (make-posn 37 315)
               (make-posn 383 285)))

;; CONTRATO:
;;   triangles1: Triangle Number Number Number -> Boolean
;; OBJETIVO:
;;   Dados um triângulo, um número que representa a cor do primeiro
;;   triângulo a ser desenhado, os deslocamentos nos eixos x e y, desenha
;;   vários triângulos de Sierpinski, mudando a cor, até que o primeiro
;;   vértice do triângulo saia da janela.
(define (triangles1 tri color dx dy)
  (cond
    [(out-of-bound? tri) #false]
    [else
     (or
      (sierpinski
       (triangle-v1 tri)
       (triangle-v2 tri)
       (triangle-v3 tri)
       (list-ref
        CORES
        (modulo color (length CORES))))
      (triangles1
       (create-new-triangle tri dx dy #false) (add1 color) dx dy))]))

;; Testes
(start BOUND-X BOUND-Y)
(triangles1 tri1 0 30 40)

;; 2)

;; a)

;; b)

;; c)

;; d)

;; e)
;;  O programa sempre termina, pois os valores para as posições nos eixos x
;;  e y do primeiro vértice sempre são incrementados, assim, em algum passo
;;  de iteração o valor vai ultrapassar a borda.

;; 3)

;; CONTRATO:
;;   triangles2: Triangle Number Number Number -> Boolean
;; OBJETIVO:
;;   Versão da triangles1 modificada, de modo que a cada nova iteração, o
;;   deslocamento diminui 10%, e o deslocamento da coordenada y do terceiro
;;   vértice do triângulo é negativo. A execução termina quando o valor
;;   absoluto do deslocamento no eixo y for menor ou igual a 15.
(define (triangles2 tri color dx dy)
  (cond
    [(<= (abs dy) 15) #false]
    [else
     (or
      (sierpinski
       (triangle-v1 tri)
       (triangle-v2 tri)
       (triangle-v3 tri)
       (list-ref
        CORES
        (modulo color (length CORES))))
      (local
        [(define new-dx (* dx 0.9))
         (define new-dy (* dy 0.9))]
        (triangles2
         (create-new-triangle tri new-dx new-dy #true)
         (add1 color)
         new-dx
         new-dy)))]))

;; Testes
(start BOUND-X BOUND-Y)
(triangles2 tri1 0 30 40)

(start BOUND-X BOUND-Y)
(triangles2 tri3 0 40 -30)

;; 4)

(define-struct seleção (nome advs))
;; Um elemento seleção do conjunto SELEÇÃO é uma estrutura:
;;   (make-seleção nome advs),
;; onde:
;;   nome: Symbol, nome da seleção;
;;   advs: Lista-de-seleções, uma lista com os nomes dos adversários.

;; Uma Lista-de-símbolos é
;; 1. empty; ou
;; 2. (cons s lds), onde
;;    s: Symbol, é o nome de uma seleção ou 'Fim;
;;    lds: Lista-de-símbolos.

;; Uma Lista-de-seleções é
;; 1. empty; ou
;; 2. (cons s lds), onde
;;    s: Seleção;
;;    lds: Lista-de-seleções.

(define LISTA (list 'Brasil 'Argentina 'Chile 'Uruguai 'Fim
                    'Argentina 'Chile 'Brasil 'Fim
                    'Chile 'Paraguai 'Brasil 'Brasil 'Argentina 'Fim))

;; CONTRATO:
;;   transforma: Lista-de-símbolos -> Lista-de-seleções
;; OBJETIVO:
;;   Dada uma lista de símbolos contendo nomes de seleções e a palavra
;;   'Fim, retorna uma lista de seleções.
;; EXEMPLOS:
;;   (transforma empty) -> empty
;;   (transforma LISTA) ->
;;     (list
;;      (make-seleção 'Brasil (list 'Argentina 'Chile 'Uruguai))
;;      (make-seleção 'Argentina (list 'Chile 'Brasil))
;;      (make-seleção 'Chile (list 'Paraguai 'Brasil 'Brasil 'Argentina)))
(define (transforma sims)
  (cond
    [(empty? sims) empty]
    [else
     (local
       [(define (pega-seleções-até-fim l)
          (cond
            [(symbol=? (first l) 'Fim) empty]
            [else (cons (first l) (pega-seleções-até-fim (rest l)))]))
        (define (seleções-depois-de-fim n l)
          (cond
            [(= n 0) l]
            [else (rest (seleções-depois-de-fim (sub1 n) l))]))
        (define sels
          (pega-seleções-até-fim sims))
        (define resto-sims
          (seleções-depois-de-fim (add1 (length sels)) sims))
        (define seleção
          (make-seleção (first sels) (rest sels)))]
       (cons seleção (transforma resto-sims)))]))

;; Testes
(check-expect (transforma empty) empty)
(check-expect (transforma LISTA)
              (list (make-seleção 'Brasil
                                  (list 'Argentina 'Chile 'Uruguai))
                    (make-seleção 'Argentina
                                  (list 'Chile 'Brasil))
                    (make-seleção 'Chile
                                  (list 'Paraguai 'Brasil 'Brasil 'Argentina))))

;; 5)

;; A função pega-seleções-até-fim usa recursão generativa, termina quando
;; encontra a palavra 'Fim.
;; A função seleções-depois-de-fim também usa recursão generativa, termina
;; quando o argumento n for zero, que é sempre decrementado a cada passo de
;; iteração.
;; A função transforma usa recursão estrutural, a cada iteração é retirada
;; uma porção da lista sims, termina quando a lista for vazia. Logo, lista
;; sem a palavra 'Fim nunca é passada para a função pega-seleções-até-fim.

;; 6)

(define-struct confronto (time1 time2))
;; Um elemento confronto do conjunto CONFRONTO é uma estrutura:
;;   (make-confronto time1 time2),
;; onde:
;;   time1: Symbol, nome do time;
;;   time2: Symbol, nome do outro time.

;; Uma Lista-de-confrontos é
;; 1. empty; ou
;; 2. (cons c ldc), onde
;;    c: Confronto;
;;    ldc: Lista-de-confrontos.

(define lds (list 'Brasil 'Argentina 'Holanda 'Uruguai))

;; CONTRATO:
;;   gera-confronts: Lista-de-símbolos -> Lista-de-confrontos
;; OBJETIVO:
;;   Dada uma lista de símbolos contendo nomes de seleções, retorna uma
;;   lista de confrontos.
;; EXEMPLOS:
;;   (gera-confrontos lds) -> empty
;;   (gera-confrontos lds) -> (list
;;                             (make-confronto 'Brasil 'Argentina)
;;                             (make-confronto 'Brasil 'Holanda)
;;                             (make-confronto 'Brasil 'Uruguai)
;;                             (make-confronto 'Argentina 'Holanda)
;;                             (make-confronto 'Argentina 'Uruguai)
;;                             (make-confronto 'Holanda 'Uruguai))
(define (gera-confrontos lds)
  (cond
    [(empty? lds) empty]
    [else
     (local
       [(define (combinações n lds)
          (cond
            [(= n 0) (list empty)]
            [(empty? lds) empty]
            [else
             (local
               [(define (f l) (cons (first lds) l))]
               (append
                (map f (combinações (sub1 n) (rest lds)))
                (combinações n (rest lds))))]))
        (define (lista->confronto lst)
          (make-confronto (first lst) (second lst)))]
       (map lista->confronto
            (combinações 2 lds)))]))

;; Testes
(check-expect (gera-confrontos empty) empty)
(check-expect (gera-confrontos lds)
              (list
               (make-confronto 'Brasil 'Argentina)
               (make-confronto 'Brasil 'Holanda)
               (make-confronto 'Brasil 'Uruguai)
               (make-confronto 'Argentina 'Holanda)
               (make-confronto 'Argentina 'Uruguai)
               (make-confronto 'Holanda 'Uruguai)))