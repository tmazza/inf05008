;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab-02) (read-case-sensitive #t) (teachpacks ((lib "convert.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "convert.rkt" "teachpack" "htdp")) #f)))
;; LISTA LAB 2
;; OLIVER HUNG BUO TSO, 231519
;; 2016/03/29

;; EXER-01 ---------------------------------------------------------------------

(define-struct linha(numero nome-titular mensalidade habilitada))
;; Um elemento linha do conjunto Linha é uma estrutura:
;;   (make-linha numero nome-titular mensalidade habilitada),
;; onde:
;;   numero:       string, é o número da linha,
;;   nome-titular: string, é o nome do titular,
;;   mensalidade:  number, é o valor da mensalidade,
;;   habilitada:   bool,   é o estado da linha.

(define-struct empresa(codigo-id total-habilitadas total-desabilitadas))
;; Um elemento empresa do conjunto Empresa é uma estrutura:
;;   (make-empresa codigo-id total-habilitadas total-desabilitadas),
;; onde:
;;   codigo-id:           string, é o código de identificação,
;;   total-habilitadas:   number, é o total de linhas habilitadas,
;;   total-desabilitadas: number, é o total de linhas desabilitadas.

;; EXER-02 ---------------------------------------------------------------------

;; Instâncias:
(define empresa1 (make-empresa "01" 4 1))
(define linha1 (make-linha "001" "nome-1" 50 true))
(define linha2 (make-linha "002" "nome-2" 60 true))
(define linha3 (make-linha "003" "nome-1" 70 false))
(define linha4 (make-linha "004" "nome-1" 50 true))
(define linha5 (make-linha "005" "nome-1" 30 true))
(define nova-linha1 (make-linha "011" "nome-3" 30 true))
(define nova-linha2 (make-linha "012" "nome-3" 30 false))

;; EXER-03 ---------------------------------------------------------------------

;; confirmar-linha: linha string string -> string
;; obj: determinar se a linha informada pertence ao titular fornecido
;;      e possui o número indicado.
;; exemplo:
;;   (confirmar-linha linha1 "001" "nome-1") -> "Tudo certo."
;;   (confirmar-linha linha1 "001" "nome-2") -> "Número certo, nome errado."
;;   (confirmar-linha linha1 "002" "nome-1") -> "Número errado, nome certo."
;;   (confirmar-linha linha1 "002" "nome-2") -> "Tudo errado."
(define (confirmar-linha linha nome-titular numero)
  (cond
    [(and (string=? (linha-numero linha) numero)
          (string=? (linha-nome-titular linha) nome-titular))
     "Tudo certo."]
    [(and (string=? (linha-numero linha) numero)
          (not (string=? (linha-nome-titular linha) nome-titular)))
     "Número certo, nome errado."]
    [(and (not (string=? (linha-numero linha) numero))
          (string=? (linha-nome-titular linha) nome-titular))
     "Número errado, nome certo."]
    [else "Tudo errado."]))

(check-expect (confirmar-linha linha1 "nome-1" "001") "Tudo certo.")
(check-expect (confirmar-linha linha1 "nome-2" "001")
              "Número certo, nome errado.")
(check-expect (confirmar-linha linha1 "nome-1" "002")
              "Número errado, nome certo.")
(check-expect (confirmar-linha linha1 "nome-2" "002") "Tudo errado.")

;; EXER-04 ---------------------------------------------------------------------

;; inf-da-empresa: empresa -> symbol
;; obj: dadas informações da empresa, retornar seu código de identificação e o
;;      número total de linhas controladas.
;; exemplo:
;;   (inf-da-empresa empresa1) -> '
(define (total-linhas empresa)
  (+ (empresa-total-habilitadas empresa) (empresa-total-desabilitadas empresa)))

;; EXER-05 ---------------------------------------------------------------------

;; maior-mensalidade-mesmo-titular: linha linha -> string
;; obj: casos ambas linhas pertençam ao mesmo titular, retornar a que tem
;;      valor de mensalidade mais alto. Caso tenham mesmo titular e mesmo
;;      valor de mensalidade, retornar a primeira linha.
;; exemplo:
;;   (maior-mensalidade-mesmo-titular linha1 linha3) -> "003"
;;   (maior-mensalidade-mesmo-titular linha1 linha4) -> "001"
;;   (maior-mensalidade-mesmo-titular linha1 linha2) -> "Linhas de titulares
;;                                                       diferentes."
(define (maior-mensalidade-mesmo-titular linha1 linha2)
  (cond
    [(mesmo-titular linha1 linha2) (maior-mensalidade linha1 linha2)]
    [else "Linhas de titulares diferentes."]))

(define (mesmo-titular linha1 linha2)
  (cond
    [(string=? (linha-nome-titular linha1) (linha-nome-titular linha2)) true]
    [else false]))

(define (maior-mensalidade linha1 linha2)
  (cond
    [(>= (linha-mensalidade linha1) (linha-mensalidade linha2))
     (linha-numero linha1)]
    [else (linha-numero linha2)]))

(check-expect (maior-mensalidade-mesmo-titular linha1 linha3) "003")
(check-expect (maior-mensalidade-mesmo-titular linha1 linha4) "001")
(check-expect (maior-mensalidade-mesmo-titular linha1 linha5) "001")
(check-expect (maior-mensalidade-mesmo-titular linha1 linha2)
              "Linhas de titulares diferentes.")

;; EXER-06 ---------------------------------------------------------------------

;; adicionar-linha: empresa linha -> empresa
;; obj: atualizar os dados da empresa a partir das informações da linha
;;      fornecida para que os totais de controle incluam esta nova linha.
;; exemplo:
;;   (total-linhas (adicionar-linha empresa1 nova-linha1)) -> 6
;;   (total-linhas (adicionar-linha empresa1 nova-linha2)) -> 6
(define (adicionar-linha empresa nova-linha)
  (cond
    [(linha-habilitada nova-linha)
     (make-empresa
       (empresa-codigo-id empresa)
       (+ (empresa-total-habilitadas empresa) 1)
       (empresa-total-desabilitadas empresa))]
    [else
     (make-empresa
       (empresa-codigo-id empresa)
       (empresa-total-habilitadas empresa)
       (+ (empresa-total-desabilitadas empresa) 1))]))

(check-expect (total-linhas (adicionar-linha empresa1 nova-linha1)) 6)
(check-expect (total-linhas (adicionar-linha empresa1 nova-linha2)) 6)
