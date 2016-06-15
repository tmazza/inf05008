;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab-04) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct pag-web (nome conteudo))
;; Um elemento do conjunto Pag-web é uma estrutura
;; (make-pag-web um-nome um-cont) onde
;; um-nome: Símbolo, é o nome da página e
;; um-cont: Lista-palOUweb, é uma lista de palavras e outras páginas web.

;; Uma Lista-palOUweb é
;; 1. empty; ou
;; 2. (cons p lpw), onde p: Símbolo e lpw: Lista-palOUweb ; ou
;; 3. (cons w lpw), onde w: Pag-web e lpw: Lista-palOUweb.

;; EXER-01 ----------------------------------------------------------------

(define conteudo3 (list 'gsw 'sas 'lac))
(define pag3 (make-pag-web 'times conteudo3))

(define conteudo2 (list 'jogos pag3))
(define pag2 (make-pag-web 'nba conteudo2))

(define conteudo1 (list 'mlb pag2 'nhl 'nfl))
(define pag1 (make-pag-web 'esporte conteudo1))

;; EXER-01 ----------------------------------------------------------------

;; Uma Lista-de-simbolo é
;; 1. empty; ou
;; 2. (cons s lds), onde s: Símbolo e lds: Lista-de-simbolo.

;; CONTRATO:
;;   lista-pal: Pag-web -> Lista-de-simbolo
;; OBJETIVO:
;;   Dada uma página web, retorna uma lista com as palavras do conteúdo
;;   dela. Caso a página contiver uma sub-página, apenas o nome desta
;;   sub-página será colocado na lista, e não seu conteúdo.
;; EXEMPLOS:
;;   (lista-pal pag3) -> (list 'gsw 'sas 'lac)
;;   (lista-pal pag1) -> (list 'mlb 'nba 'nhl 'nfl)
(define (lista-pal pag)
  (cond
    [(symbol=? (pag-web-conteudo pag