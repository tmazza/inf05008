;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lab-04-extra) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; LISTA EXTRA-CLASSE LAB 4
;; OLIVER HUNG BUO TSO, 231519
;; 2016/05/27

;; Uma Lista-de-nomes é
;; 1. empty; ou
;; 2. (cons n ldn), onde
;;    n: Symbol;
;;    ldn: Lista-de-nomes.

;; Lista-de-lista-de-nomes é
;; 1. empty; out
;; 2. (cons ldn ldldn), onde
;;    ldn: Lista-de-nomes;
;;    ldldn: Lista-de-lista-de-nomes.

(define lista-de-nomes (list 'C.Paul 'J.Lin 'S.Curry))
(define lista-de-lista-de-nomes (list (list 'C.Paul 'J.Lin 'S.Curry)
                                      (list 'C.Paul 'S.Curry 'J.Lin)
                                      (list 'J.Lin 'C.Paul 'S.Curry)
                                      (list 'J.Lin 'S.Curry 'C.Paul)
                                      (list 'S.Curry 'C.Paul 'J.Lin)
                                      (list 'S.Curry 'J.Lin 'C.Paul)))

;; CONTRATO:
;;   add-nome-em-ldldn: Symbol Lista-de-lista-de-nomes ->
;;                      Lista-de-lista-de-nomes
;; OBJETIVO:
;;   Dado um nome e uma lista de lista de nomes, adiciona este nome no
;;   início de cada lista de nome.
;; EXEMPLOS:
;;   (map-cons 'J.Lin empty) -> empty
;;   (map-cons 'C.Paul
;;             (list (list 'J.Lin 'S.Curry)
;;                   (list 'S.Curry 'Y.Ming))) ->
;;     (list (list 'C.Paul 'J.Lin 'S.Curry)
;;           (list 'C.Paul 'S.Curry 'Y.Ming))
(define (add-nome-em-ldldn nome ldn)
  (cond
    [(empty? ldn) empty]
    [else (cons (cons nome (first ldn))
                (add-nome-em-ldldn nome (rest ldn)))]))

(check-expect (add-nome-em-ldldn 'J.Lin empty) empty)
(check-expect (add-nome-em-ldldn 'C.Paul
                                 (list (list 'J.Lin 'S.Curry)
                                       (list 'S.Curry 'Y.Ming)))
              (list (list 'C.Paul 'J.Lin 'S.Curry)
                    (list 'C.Paul 'S.Curry 'Y.Ming)))

;; CONTRATO:
;;   arranjos: Lista-de-nomes -> Lista-de-lista-de-nomes
;; OBJETIVO:
;;   Dada uma lista de nomes, devolve todos os possíveis arranjos com
;;   os elementos desta lista.
;; EXEMPLOS:
;;   (arranjos empty) -> empty
;;   (arranjos lista-de-nomes) -> lista-de-lista-nomes
(define (arranjos ldn)
  (cond
    [(empty? ldn) empty]
    [(= (length ldn) 1) (list ldn)]
    [else
     (apply append
            (local
              [(define (f nome)
                 (add-nome-em-ldldn nome (arranjos (remove nome ldn))))]
              (map f ldn)))]))

(check-expect (arranjos empty) empty)
(check-expect (arranjos lista-de-nomes) lista-de-lista-de-nomes)

;; CONTRATO:
;;   nomes-diferentes: Lista-de-nomes Lista-de-lista-de-nomes ->
;;                     Lista-de-lista-de-nomes
;; OBJETIVO:
;;   Dada uma lista de nomes e uma lista com os arranjos dela, retorna
;;   a lista de arranjos dela nos quais cada nome não está na mesma
;;   posição na lista de nomes.
;; EXEMPLOS:
;;   (nomes-diferentes lista-de-nomes empty -> empty
;;   (nomes-diferentes empty lista-de-lista-de-nomes ->
;;      lista-de-lista-de-nomes
;;   (nomes-diferentes lista-de-nomes lista-de-lista-de-nomes ->
;;      (list (list 'C.Paul 'S.Curry 'J.Lin)
;;            (list 'J.Lin 'C.Paul 'S.Curry)
;;            (list 'J.Lin 'S.Curry 'C.Paul)
;;            (list 'S.Curry 'C.Paul 'J.Lin)
;;            (list 'S.Curry 'J.Lin 'C.Paul))
(define (nomes-diferentes ldn ldldn)
  (cond
    [(empty? ldldn) empty]
    [else
     (local
       [(define comparado (first ldldn))
        (define resto (rest ldldn))
        (define (nome-mesma-posição? l1 l2)
          (cond
            [(or (empty? l1) (empty? l2)) #false]
            [(equal? (first l1) (first l2)) #true]
            [else (nome-mesma-posição? (rest l1) (rest l2))]))]
       (cond
         [(nome-mesma-posição? ldn comparado) (nomes-diferentes ldn resto)]
         [else (cons comparado (nomes-diferentes ldn resto))]))]))

(check-expect (nomes-diferentes lista-de-lista-de-nomes empty) empty)
(check-expect (nomes-diferentes empty lista-de-lista-de-nomes)
              lista-de-lista-de-nomes)
(check-expect (nomes-diferentes lista-de-nomes lista-de-lista-de-nomes)
              (list (list 'J.Lin 'S.Curry 'C.Paul)
                    (list 'S.Curry 'C.Paul 'J.Lin)))

;; CONTRATO:
;;   seleção-random: Lista-de-lista-de-nomes -> Lista-de-nomes
;; OBJETIVO:
;;   Dada uma lista de lista de nomes, retorna randomicamente um de
;;   seus elementos.
;; EXEMPLOS:
;;   Esta função envolve componente randômico na resposta.
;;   O elemento retornado deve fazer parte da lista de entrada.
(define (seleção-random ldldn)
  (local
    [(define len (length ldldn))
     (define n (random len))]
    (list-ref ldldn n)))

;; CONTRATO:
;;   amigo-secreto: Lista-de-nomes -> Lista-de-nomes
;; OBJETIVO:
;;   Dada uma lista de nomes, gerar randomicamente uma lista de nomes
;;   na qual cada nome da lista de saída está em uma posição diferente
;;   do que na lista de entrada.
;; EXEMPLOS:
;;   Esta função envolve componente randômico na resposta.
;;   A lista retornada deve ser um dos arranjos da lista de entrada,
;;   exceto a lista de entrada.
(define (amigo-secreto ldn)
  (seleção-random (nomes-diferentes ldn (arranjos ldn))))

(amigo-secreto lista-de-nomes)