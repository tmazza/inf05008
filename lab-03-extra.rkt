;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab-03-extra) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; LISTA EXTRA-CLASSE LAB 3
;; OLIVER HUNG BUO TSO, 231519
;; 2016/05/05

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

(define-struct info (nome pont))
;; Um elemento info do conjunto INFO é uma estrutura:
;;   (make-info nome pont),
;; onde:
;;   nome: String, nome do time,
;;   pont: Number, pontuação do time.

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

;; Uma Lista-de-resultados é
;;   - empty, ou
;;   - (cons r ldr), onde
;;      r:   Symbol, Resultado do time em um jogo, 'v, 'e e 'd para
;;                   vitória, empate e derrota, respectivamente.
;;      ldr: Lista-de-resultados.

(define gsw (make-time "Warriors" 0 0 0 0))
(define lac (make-time "Clippers" 0 0 0 0))
(define okc (make-time "Thunder"  0 0 0 0))
(define sas (make-time "Spurs"    0 0 0 0))

(define gsw-lac (make-jogo gsw lac 5 4))
(define gsw-okc (make-jogo gsw okc 9 0))
(define gsw-sas (make-jogo gsw sas 1 1))
(define lac-okc (make-jogo lac okc 5 1))
(define lac-sas (make-jogo lac sas 1 2))
(define okc-gsw (make-jogo okc gsw 0 6))
(define okc-sas (make-jogo okc sas 0 4))
(define sas-gsw (make-jogo sas gsw 0 1))
(define sas-okc (make-jogo sas okc 3 0))

(define lista-de-times (list gsw lac okc sas))

(define lista-de-jogos (list gsw-lac
                             gsw-okc
                             gsw-sas
                             lac-okc
                             lac-sas
                             okc-gsw
                             okc-sas
                             sas-gsw
                             sas-okc))

;; EXER-01 ---------------------------------------------------------------------

;; CONTRATO:
;;   atualiza-time: Time Lista-de-jogos -> Time
;; OBJETIVO:
;;   Dados um time e uma lista de jogos, atualiza as informações do time.
;; EXEMPLOS:
;;   (atualiza-time sas empty) -> (make-time "Spurs" 0 0 0 0)
;;   (atualiza-time sas lista-de-jogos) -> (make-time "Spurs" 5 3 1 1)
(define (atualiza-time time ldj)
  (make-time (time-nome time)
             (conta-jogos 't (resultados time ldj))
             (conta-jogos 'v (resultados time ldj))
             (conta-jogos 'e (resultados time ldj))
             (conta-jogos 'd (resultados time ldj))))

(check-expect (atualiza-time sas empty)
              (make-time "Spurs" 0 0 0 0))
(check-expect (atualiza-time sas lista-de-jogos)
              (make-time "Spurs" 5 3 1 1))

;; CONTRATO:
;;   conta-jogos: Lista-de-resultados -> Number
;; OBJETIVO:
;;   Dados uma Lista-de-resultados, retorna o total de jogos se ved='t,
;;   total de vitórias se ved='v, total de empates se ved='e, e total de
;;   derrotas se ved='d.
;; EXEMPLOS:
;;   (conta-jogos 't (resultados sas empty)) -> 0
;;   (conta-jogos 'v (resultados sas empty)) -> 0
;;   (conta-jogos 'e (resultados sas empty)) -> 0
;;   (conta-jogos 'd (resultados sas empty)) -> 0
;;   (conta-jogos 't (resultados sas lista-de-jogos)) -> 5
;;   (conta-jogos 'v (resultados sas lista-de-jogos)) -> 3
;;   (conta-jogos 'e (resultados sas lista-de-jogos)) -> 1
;;   (conta-jogos 'd (resultados sas lista-de-jogos)) -> 1
(define (conta-jogos ved ldr)
  (cond
    [(empty? ldr) 0]
    [(or (symbol=? ved 't)
         (symbol=? ved (first ldr)))
     (add1 (conta-jogos ved (rest ldr)))]
    [else (conta-jogos ved (rest ldr))]))

(check-expect (conta-jogos 't (resultados sas empty)) 0)
(check-expect (conta-jogos 'v (resultados sas empty)) 0)
(check-expect (conta-jogos 'e (resultados sas empty)) 0)
(check-expect (conta-jogos 'd (resultados sas empty)) 0)
(check-expect (conta-jogos 't (resultados sas lista-de-jogos)) 5)
(check-expect (conta-jogos 'v (resultados sas lista-de-jogos)) 3)
(check-expect (conta-jogos 'e (resultados sas lista-de-jogos)) 1)
(check-expect (conta-jogos 'd (resultados sas lista-de-jogos)) 1)

;; CONTRATO:
;;   resultados: Time Lista-de-jogos -> Lista-de-resultados
;; OBJETIVO:
;;   Dados um time e uma lista de jogos, retorna uma Lista-de-resultados do
;;   time.
;; EXEMPLOS:
;;   (resultados sas empty) -> empty
;;   (resultados sas lista-de-jogos) -> (list 'e 'v 'v 'd 'v)
(define (resultados time ldj)
  (cond
    [(empty? ldj) empty]
    [(eq? time (jogo-time-local (first ldj)))
     (cond
       [(symbol=? 'v (ved-local (first ldj)))
        (cons 'v (resultados time (rest ldj)))]
       [(symbol=? 'e (ved-local (first ldj)))
        (cons 'e (resultados time (rest ldj)))]
       [else (cons 'd (resultados time (rest ldj)))])]
    [(eq? time (jogo-time-visitante (first ldj)))
     (cond
       [(symbol=? 'd (ved-local (first ldj)))
        (cons 'v (resultados time (rest ldj)))]
       [(symbol=? 'e (ved-local (first ldj)))
        (cons 'e (resultados time (rest ldj)))]
       [else (cons 'd (resultados time (rest ldj)))])]
    [else (resultados time (rest ldj))]))

(check-expect (resultados sas empty) empty)
(check-expect (resultados sas lista-de-jogos) (list 'e 'v 'v 'd 'v))

;; CONTRATO:
;;   ved-local: Jogo -> Symbol
;; OBJETIVO:
;;   Dado um jogo, retorna 'v caso vitória, 'e caso empate e 'd caso
;;   derrota do time local.
;; EXEMPLOS:
;;   (ved-local gsw-okc) -> 'v
;;   (ved-local gsw-sas) -> 'e
;;   (ved-local okc-sas) -> 'd
(define (ved-local jogo)
  (cond
    [(> (jogo-gols-local jogo) (jogo-gols-visitante jogo)) 'v]
    [(= (jogo-gols-local jogo) (jogo-gols-visitante jogo)) 'e]
    [(< (jogo-gols-local jogo) (jogo-gols-visitante jogo)) 'd]))

(check-expect (ved-local gsw-okc) 'v)
(check-expect (ved-local gsw-sas) 'e)
(check-expect (ved-local okc-sas) 'd)

;; EXER-02 ---------------------------------------------------------------------

;; CONTRATO:
;;   campeao: Lista-de-times Lista-de-jogos -> Info
;; OBJETIVO:
;;   Dados uma lista de times e uma lista de jogos, retorna o nome e a
;;   pontuação do time campeão numa estrutura Info. Se a lista de times ou
;;   a lista de jogos for empty, retorna nome como " " e pontuação 0.
;; EXEMPLOS:
;;   (campeao lista-de-times empty) -> (make-info " " 0)
;;   (campeao empty lista-de-jogos) -> (make-info " " 0)
;;   (campeao lista-de-times lista-de-jogos) -> (make-info "Warriors" 13)
(define (campeao ldt ldj)
  (cond
    [(empty? ldt) (make-info " " 0)]
    [(> (pontuacao (atualiza-time (first ldt) ldj))
        (info-pont (campeao (rest ldt) ldj)))
     (make-info (time-nome (first ldt))
                (pontuacao (atualiza-time (first ldt) ldj)))]
    [else (campeao (rest ldt) ldj)]))

(check-expect (campeao lista-de-times empty) (make-info " " 0))
(check-expect (campeao empty lista-de-jogos) (make-info " " 0))
(check-expect (campeao lista-de-times lista-de-jogos)
              (make-info "Warriors" 13))

;; CONTRATO:
;;   pontuacao: Time -> Number
;; OBJETIVO:
;;   Dado um time, retorna sua pontuação.
;; EXEMPLOS:
;;   (pontuacao (atualiza-time sas lista-de-jogos)) -> 10
(define (pontuacao time)
  (+ (* 3 (time-vit time)) (time-emp time)))

(check-expect (pontuacao (atualiza-time sas lista-de-jogos)) 10)