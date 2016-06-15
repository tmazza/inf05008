;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab-02-extra) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; LISTA EXTRA-CLASSE LAB 2
;; OLIVER HUNG BUO TSO, 231519
;; 2016/04/05

(define-struct linha (numero titular mensalidade habilitada? cod-empresa))
;; Um elemento linha do conjunto LINHA é uma estrutura:
;;   (make-linha numero titular mensalidade habilitada? empresa),
;; onde:
;;   numero:      String,   número da linha,
;;   titular:     String,   nome do titular,
;;   mensalidade: Number,   valor da mensalidade,
;;   habilitada?: Booleano, true se habilitada, false se desabilitada.
;;   cod-empresa: String,   código da empresa pertencente.

(define-struct empresa (codigo total-hab total-desab))
;; Um elemento empresa do conjunto EMPRESA é uma estrutura:
;;   (make-empresa codigo total-hab total-desab),
;; onde:
;;   codigo:      String, código de identificação da empresa,
;;   total-hab:   Number, total de linhas habilitadas,
;;   total-desab: Number, total de linhas desabilitadas.

(define-struct titular-gasto (titular gasto))
;; Um elemento titular-gasto do conjunto TITULAR-GASTO é uma estrutura:
;;   (make-titular-gasto titular gasto),
;; onde:
;;   titular: String, nome do titular,
;;   gasto:   Number, total gasto com as linhas habilitadas do titular.

(define empresa1 (make-empresa "01" 400 100))
(define linha1 (make-linha "001" "nome-1" 50 true "01"))
(define linha2 (make-linha "002" "nome-2" 60 true "01"))
(define linha3 (make-linha "003" "nome-1" 70 false "01"))
(define linha4 (make-linha "004" "nome-1" 50 true "01"))
(define linha5 (make-linha "005" "nome-1" 30 true "01"))

;; EXER-01 ---------------------------------------------------------------------

;; CONTRATO:
;;   percentual-desab: Empresa -> Number
;; OBJETIVO:
;;   Dada uma empresa, retornar o percentual das suas linhas desabilitadas.
;; EXEMPLOS:
;;   (percentual-desab empresa1) -> 0.2
(define (percentual-desab empresa)
  (/ (empresa-total-desab empresa) (total-linhas empresa)))

(check-expect (percentual-desab empresa1) 0.2)

;; CONTRATO:
;;   total-linhas: Empresa -> Number
;; OBJETIVO:
;;   Dada uma empresa, retornar a quantidade total de linhas.
;; EXEMPLOS:
;;   (total-linhas empresa1) -> 500
(define (total-linhas empresa)
  (+ (empresa-total-hab empresa) (empresa-total-desab empresa)))

(check-expect (total-linhas empresa1) 500)

;; EXER-02 ---------------------------------------------------------------------

;; CONTRATO:
;;   total-gasto-hab-mesmo-titular: Linha Linha Linha -> Titular-gasto
;; OBJETIVO:
;;   Dadas 3 linhas, se pertencerem ao mesmo titular, retornar um elemento
;;   do tipo titular-gasto com o nome do titular e o total gasto com as
;;   linhas habilitadas. Caso não pertençam ao mesmo titular, retornar com
;;   o titular igual a " " e gasto igual a 0.
;; EXEMPLOS:
;;   (total-gasto-hab-mesmo-titular linha1 linha2 linha3)
;;     -> (make-titular-gasto " " 0)
;;   (total-gasto-hab-mesmo-titular linha1 linha4 linha5)
;;     -> (make-titular-gasto "nome-1" 130)
;;   (total-gasto-hab-mesmo-titular linha1 linha3 linha4)
;;     -> (make-titular-gasto "nome-1" 100)
(define (total-gasto-hab-mesmo-titular linha1 linha2 linha3)
  (cond
    [(mesmo-titular? linha1 linha2 linha3)
     (make-titular-gasto (linha-titular linha1)
                         (total-gasto-hab linha1 linha2 linha3))]
    [else (make-titular-gasto " " 0)]))

(check-expect (total-gasto-hab-mesmo-titular linha1 linha2 linha3)
              (make-titular-gasto " " 0))
(check-expect (total-gasto-hab-mesmo-titular linha1 linha4 linha5)
              (make-titular-gasto "nome-1" 130))
(check-expect (total-gasto-hab-mesmo-titular linha1 linha3 linha4)
              (make-titular-gasto "nome-1" 100))

;; CONTRATO:
;;   mesmo-titular?: Linha Linha Linha -> Booleano
;; OBJETIVO:
;;   Dadas 3 linhas, retornar true se pertencerem ao mesmo titular, caso
;;   contrário, retornar false.
;; EXEMPLOS:
;;   (mesmo-titular? linha1 linha2 linha3) -> false
;;   (mesmo-titular? linha1 linha3 linha4) -> true
(define (mesmo-titular? linha1 linha2 linha3)
  (string=? (linha-titular linha1)
            (linha-titular linha2)
            (linha-titular linha3)))

(check-expect (mesmo-titular? linha1 linha2 linha3) false)
(check-expect (mesmo-titular? linha1 linha3 linha4) true)

;; CONTRATO:
;;   total-gasto-hab: Linha Linha Linha -> Number
;; OBJETIVO:
;;   Dadas 3 linhas, retornar o total gasto das linhas habilitadas.
;; EXEMPLOS:
;;   (total-gasto-hab linha1 linha4 linha5) -> 130
;;   (total-gasto-hab linha1 linha3 linha4) -> 100
(define (total-gasto-hab linha1 linha2 linha3)
  (+ (gasto-se-habilitada linha1)
     (gasto-se-habilitada linha2)
     (gasto-se-habilitada linha3)))

(check-expect (total-gasto-hab linha1 linha4 linha5) 130)
(check-expect (total-gasto-hab linha1 linha3 linha4) 100)

;; CONTRATO:
;;   gasto-se-habilitada: Linha -> Number
;; OBJETIVO:
;;   Dada 1 linha, retornar seu gasto se for habilitada, caso contrário,
;;   retornar 0.
;; EXEMPLOS:
;;   (gasto-se-habilitada linha1) -> 50
;;   (gasto-se-habilitada linha3) -> 0
(define (gasto-se-habilitada linha)
  (cond
    [(linha-habilitada? linha) (linha-mensalidade linha)]
    [else 0]))

(check-expect (gasto-se-habilitada linha1) 50)
(check-expect (gasto-se-habilitada linha3) 0)

;; EXER-03 ---------------------------------------------------------------------

;; CONTRATO:
;;   atualizar-empresa: Empresa Linha Linha Linha -> Empresa
;; OBJETIVO:
;;   Adicionar 3 novas linhas a uma empresa.
;; EXEMPLOS:
;;   (atualizar-empresa empresa1 linha1 linha2 linha3) ->
;;     (make-empresa (empresa-codigo empresa1) 402 101)
(define (atualizar-empresa empresa linha1 linha2 linha3)
  (make-empresa (empresa-codigo empresa)
                (+ (empresa-total-hab empresa)
                   (novas-linhas-hab linha1 linha2 linha3))
                (+ (empresa-total-desab empresa)
                   (novas-linhas-desab linha1 linha2 linha3))))

(check-expect (atualizar-empresa empresa1 linha1 linha2 linha3)
              (make-empresa (empresa-codigo empresa1) 402 101))

;; CONTRATO:
;;   novas-linhas-hab: Linha Linha Linha -> Number
;; OBJETIVO:
;;   Dadas 3 linhas, retornar a quantidade de linhas habilitadas.
;; EXEMPLOS:
;;   (novas-linhas-hab linha1 linha2 linha3) -> 2
(define (novas-linhas-hab linha1 linha2 linha3)
  (+ (nova-linha-hab? linha1)
     (nova-linha-hab? linha2)
     (nova-linha-hab? linha3)))

(check-expect (novas-linhas-hab linha1 linha2 linha3) 2)

;; CONTRATO:
;;   novas-linhas-desab: Linha Linha Linha -> Number
;; OBJETIVO:
;;   Dadas 3 linhas, retornar a quantidade de linhas desabilitadas.
;; EXEMPLOS:
;;   (novas-linhas-desab linha1 linha2 linha3) -> 1
(define (novas-linhas-desab linha1 linha2 linha3)
  (- 3 (novas-linhas-hab linha1 linha2 linha3)))

(check-expect (novas-linhas-desab linha1 linha2 linha3) 1)

;; CONTRATO:
;;   nova-linha-hab? Linha -> Number
;; OBJETIVO:
;;   Retornar 1 se a linha for habilitada, caso contrário, retornar 0.
;; EXEMPLOS:
;;   (nova-linha-hab? linha1) -> 1
;;   (nova-linha-hab? linha3) -> 0
(define (nova-linha-hab? linha)
  (cond
    [(linha-habilitada? linha) 1]
    [else 0]))

(check-expect (nova-linha-hab? linha1) 1)
(check-expect (nova-linha-hab? linha3) 0)