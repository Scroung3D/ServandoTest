#lang plai

(require "grammars.rkt")

;; Desendulzador.
;; Regresa un árbol de sintaxis abstracta sin azúcar sintáctica.
;; desugar: FWBAE -> FBAE
(define (desugar expr)
  (match expr
    [(boolS b) (bool b)]
    [(idS i) (id i)]
    [(numS n) (num n)]
    [(opS f args) (op f (map desugar args))]
    [(withS bindings body) (app (fun (list-auxS bindings) (desugar body)) (list-aux2S bindings))]
    [(withS* bindings body) (desugar (list-aux3 bindings body))]
    [(funS params body) (fun  params (desugar body))]
    [(appS fun args) (app (desugar fun) (map desugar args))]))

(define (list-auxS list)
  (match list
    ['() '()]
    [(cons x xs) (cons (binding-name x) (list-auxS xs))]))


(define (list-aux2S list)
  (match list
    ['() '()]
    [(cons x xs) (cons (desugar (binding-value x)) (list-aux2S xs))]))

(define (list-aux3 b-list body)
  (match b-list
    ['() body]
    [(cons x xs)  (withS (list x) (list-aux3 xs body))]))

