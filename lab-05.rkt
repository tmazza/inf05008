;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lab-05) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp")) #f)))
;; LISTA LAB 5
;; OLIVER HUNG BUO TSO, 231519
;; 2016/06/07

(define A (make-posn 200 27))
(define B (make-posn 27 300))
(define C (make-posn 373 300))
(define D (make-posn 50 20))
(define E (make-posn 350 250))
(define F (make-posn 90 380))
(define G (make-posn 9 38))

(define ldp (list A B C D E F G))

;; 1)

;; CONTRATO:
;;   draw-triangle: Posn Posn Posn Symbol -> Boolean
;; OBJETIVO:
;;   Recebendo 3 Posn e uma cor, desenha um triângulo nesta cor na tela.
(define (draw-triangle posn1 posn2 posn3 color)
  (and
   (draw-solid-line posn1 posn2 color)
   (draw-solid-line posn1 posn3 color)
   (draw-solid-line posn2 posn3 color)))

;; Testes
(start 400 400)
(draw-triangle A B C 'red)

;; 2)

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
(define (too-small? posn1 posn2 posn3)
  (local
    [(define min 10)
     (define (distance posn1 posn2)
       (sqrt (+ (sqr (- (posn-x posn1) (posn-x posn2)))
                (sqr (- (posn-y posn1) (posn-y posn2))))))]
    (or
     (< (distance posn1 posn2) min)
     (< (distance posn1 posn3) min)
     (< (distance posn2 posn3) min))))

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

;; 3)

;; CONTRATO:
;;   sierpinski: Posn Posn Posn Symbol -> Boolean
;; OBJETIVO:
;;   Desenha um triângulo de Sierpinski com vértices nos pontos passados
;;   como argumentos, da cor passada como argumento, e devolve true. Se o
;;   triângulo passado como argumento tiver dimensões muito pequenas, nada
;;   é desenhado.
(define (sierpinski posn1 posn2 posn3 color)
  (local
    [(define (mid posn1 posn2)
       (/ (+ posn1 posn2) 2))
     (define (mid-point posn1 posn2)
       (make-posn
        (mid (posn-x posn1) (posn-x posn2))
        (mid (posn-y posn1) (posn-y posn2))))
     (define new-posn1 (mid-point posn1 posn2))
     (define new-posn2 (mid-point posn2 posn3))
     (define new-posn3 (mid-point posn3 posn1))]
    (cond
      [(too-small? posn1 posn2 posn3) #false]
      [(draw-triangle posn1 posn2 posn3 color)
       (or
        (sierpinski posn1 new-posn1 new-posn3 color)
        (sierpinski posn2 new-posn2 new-posn1 color)
        (sierpinski posn3 new-posn3 new-posn2 color))])))

;; Testes
(start 400 400)
(sierpinski A B C 'red)

(start 400 400)
(sierpinski D E F 'blue)

;; 5)

;; CONTRATO:
;;   draw-sierpinskis: Lista-de-pontos Symbol -> Boolean
;; OBJETIVO:
;;   Dada uma lista de pontos, desenha um triângulo de Sierpinski para cada
;;   3 pontos consecutivos da lista. Se sobrarem pontos, estes devem ser
;;   ignorados.
(define (draw-sierpinskis ldp color)
  (cond
    [(< (length ldp) 3) #false]
    [else
     (local
       [(define posn1 (first ldp))
        (define posn2 (second ldp))
        (define posn3 (third ldp))
        (define rest-ldp (rest (rest (rest ldp))))]
       (or
        (sierpinski posn1 posn2 posn3 color)
        (draw-sierpinskis rest-ldp color)))]))

;; Testes
(start 400 400)
(draw-sierpinskis ldp 'red)