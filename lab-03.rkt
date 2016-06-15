;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab-03) (read-case-sensitive #t) (teachpacks ((lib "convert.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "convert.rkt" "teachpack" "htdp")) #f)))
;; LISTA EXTRA-CLASSE LAB 2
;; OLIVER HUNG BUO TSO, 231519
;; 2016/04/19

;; EXER-01 ----------------------------------------------------------------

(define-struct time (nome jogos vit emp der))
;; Um elemento time do conjunto TIME é uma estrutura:
;;   (make-time nome jogos vit emp der),
;; onde:
;;   nome:  String, nome do time,
;;   jogos: Number, número de jogos totais,
;;   vit:   Number, número de vitórias,
;;   emp:   Number, número de empates,
;;   der:   Number, número de derrotas.

(define-struct jogo (time-local time-visitante gols-local gols-visitante))
;; Um elemento jogo do conjunto JOGO é uma estrutura:
;;   (make-jogo time-local time-visitante gols-local gols-visitante)
;; onde:
;;   time-local:     Time,   time local,
;;   time-visitante: Time,   time visitante,
;;   gols-local:     Number, número de gols marcados pelo time local,
;;   gols-visitante: Number, número de gols marcados pelo time visitante.

;; Uma Lista-de-times é
;;   - empty, ou
;;   - (cons t ldt), onde
;;      t:   Time
;;      ldt: Lista-de-times.

;; Uma Lista-de-jogos é
;;   - empty, ou
;;   - (cons j ldj), onde
;;      j:   Jogo
;;      ldj: Lista-de-jogos.

;; EXER-02 ----------------------------------------------------------------

(define gsw (make-time "Warriors" 3 3 0 0))
(define lac (make-time "Clippers" 1 0 0 1))
(define okc (make-time "Thunder"  5 0 0 5))
(define sas (make-time "Spurs"    4 3 1 0))

(define gsw-lac (make-jogo gsw lac 5 2))
(define gsw-okc (make-jogo gsw okc 9 2))
(define gsw-sas (make-jogo gsw sas 2 1))
(define lac-okc (make-jogo lac okc 5 1))
(define sas-gsw (make-jogo sas gsw 0 0))
(define sas-okc (make-jogo sas okc 3 0))

(define lista-de-times (list gsw
                             lac
                             okc
                             sas))

(define lista-de-jogos (list gsw-lac
                             gsw-okc
                             gsw-sas
                             lac-okc ;
                             lac-sas ;
                             okc-gsw ;;
                             okc-sas
                             sas-okc))

;; EXER-03 ----------------------------------------------------------------

;; CONTRATO:
;;   conta-times-com-vit: List Number -> Number
;; OBJETIVO:
;;   Dada uma Lista-de-times(campeonato) e um número, retornar o número de
;;   times com número de vitórias maior do que esse número.
;; EXEMPLOS:
;;   (conta-times-com-vit lista-de-times 19) -> 0
;;   (conta-times-com-vit lista-de-times 15) -> 2
;;   (conta-times-com-vit lista-de-times 10) -> 3
(define (conta-times-com-vit lista-de-times n)
  (cond
    [(empty? lista-de-times) 0]
    [(> (time-vit (first lista-de-times)) n)
     (add1 (conta-times-com-vit (rest lista-de-times) n))]
    [else (conta-times-com-vit (rest lista-de-times) n)]))

(check-expect (conta-times-com-vit lista-de-times 19) 0)
(check-expect (conta-times-com-vit lista-de-times 15) 2)
(check-expect (conta-times-com-vit lista-de-times 10) 3)

;; EXER-04 ----------------------------------------------------------------

;; CONTRATO:
;;   times-com-jogos-inferior: List Number -> List
;; OBJETIVO:
;;   Dada uma Lista-de-times(campeonato) e um número, retornar uma lista
;;   com os nomes dos times com número de jogos inferior ao número.
;; EXEMPLOS:
;;   (times-com-jogos-inferior lista-de-times 25) ->
;;     (list "Warriors" "Clippers" "Thunder" "Spurs")
;;   (times-com-jogos-inferior lista-de-times 21) ->
;;     (list "Warriors" "Thunder" "Spurs")
;;   (times-com-jogos-inferior lista-de-times 15) ->
;;     (list)
(define (times-com-jogos-inferior lista-de-times n)
  (cond
    [(empty? lista-de-times) empty]
    [(< (time-jogos (first lista-de-times)) n)
     (cons (time-nome (first lista-de-times))
           (times-com-jogos-inferior (rest lista-de-times) n))]
    [else (times-com-jogos-inferior (rest lista-de-times) n)]))

(check-expect (times-com-jogos-inferior lista-de-times 25)
              (list "Warriors" "Clippers" "Thunder" "Spurs"))
(check-expect (times-com-jogos-inferior lista-de-times 21)
              (list "Warriors" "Thunder" "Spurs"))
(check-expect (times-com-jogos-inferior lista-de-times 15)
              (list))

;; EXER-05 ----------------------------------------------------------------

;; CONTRATO:
;;   
;; OBJETIVO:
;;   
;; EXEMPLOS:

;; EXER-06 ----------------------------------------------------------------

;; CONTRATO:
;;   
;; OBJETIVO:
;;   
;; EXEMPLOS: