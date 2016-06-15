;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab-01) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; LISTA LAB 1
;; OLIVER HUNG BUO TSO, 231519
;; 2016/03/17

;; calcular-imposto number -> number
;; exemplos:
;;   (calcular-imposto 240) -> 0
;;   (calcular-imposto 480) -> 72
;;   (calcular-imposto 500) -> 140

(define (calcular-imposto salario-bruto)
  (cond
    [(<= salario-bruto 240) (* salario-bruto .0)]
    [(<= salario-bruto 480) (* salario-bruto .15)]
    [else (* salario-bruto .28)]))

(check-expect (calcular-imposto 240) 0)
(check-expect (calcular-imposto 480) 72)
(check-expect (calcular-imposto 500) 140)

;; salario-liquido number -> number
;; exemplos:
;;   (salario-liquido 16) -> 240
;;   (salario-liquido 32) -> 408
;;   (salario-liquido 34) -> 367.2
(define (salario-liquido tempo)
  (- (salario-bruto tempo) (calcular-imposto (salario-bruto tempo))))

(define (salario-bruto tempo)
  (* 15 tempo))

(check-expect (salario-liquido 16) 240)   ;; salario bruto: 240
(check-expect (salario-liquido 32) 408)   ;; salario bruto: 480
(check-expect (salario-liquido 34) 367.2) ;; salario bruto: 510

;; testa-cores symbol symbol symbol symbol -> symbol
;; exemplos:
;;   (testa-cores verde azul verde azul)       -> 'Exato!
;;   (testa-cores verde azul vermelho azul)    -> 'UmaPosicaoCorreta
;;   (testa-cores verde azul verde vermelho)   -> 'UmaPosicaoCorreta
;;   (testa-cores verde azul azul amarelo)     -> 'UmaCorCorreta
;;   (testa-cores verde azul amarelo verde)    -> 'UmaCorCorreta
;;   (testa-cores verde azul vermelho amarelo) -> 'TudoErrado!
(define (testa-cores p1 p2 c1 c2)
  (cond
    [(and (symbol=? p1 c1) (symbol=? p2 c2)) 'Exato!]
    [(or (symbol=? p1 c1) (symbol=? p2 c2)) 'UmaPosicaoCorreta]
    [(or (symbol=? p1 c2) (symbol=? p2 c1)) 'UmaCorCorreta]
    [else 'TudoErrado!]))

(check-expect (testa-cores 'verde 'azul 'verde 'azul) 'Exato!)
(check-expect (testa-cores 'verde 'azul 'vermelho 'azul) 'UmaPosicaoCorreta)
(check-expect (testa-cores 'verde 'azul 'verde 'vermelho) 'UmaPosicaoCorreta)
(check-expect (testa-cores 'verde 'azul 'azul 'amarelo) 'UmaCorCorreta)
(check-expect (testa-cores 'verde 'azul 'amarelo 'verde) 'UmaCorCorreta)
(check-expect (testa-cores 'verde 'azul 'vermelho 'amarelo) 'TudoErrado!)

;; mover-robo number char number -> number
;; exemplos:
;;   (mover-robo 2 #\D 5)   -> 7
;;   (mover-robo 23 #\E 10) -> 13
;;   (mover-robo 2 #\E 5)   -> 1
;;   (mover-robo 48 #\D 5)  -> 50

(define (mover-robo pos-atual sentido passos)
  (cond
    [(char=? #\D sentido) (andar-para-direita pos-atual passos)]
    [(char=? #\E sentido) (andar-para-esquerda pos-atual passos)]))

(define (andar-para-direita pos-atual passos)
  (cond
    [(> (+ pos-atual passos) 50) 50]
    [else (+ pos-atual passos)]))

(define (andar-para-esquerda pos-atual passos)
  (cond
    [(< (- pos-atual passos) 1) 1]
    [else (- pos-atual passos)]))

(check-expect (mover-robo 2 #\D 5) 7)
(check-expect (mover-robo 23 #\E 10) 13)
(check-expect (mover-robo 2 #\E 5) 1)
(check-expect (mover-robo 48 #\D 5) 50)