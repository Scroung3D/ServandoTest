#lang plai

;; Predicado que permite validar los operadores.
(define (operador-valido? f)
    (or (equal? f +) 
        (equal? f -)
        (equal? f *) 
        (equal? f /) 
        (equal? f mmodulo)
        (equal? f min)
        (equal? f max) 
        (equal? f mexpt)
        (equal? f sqrt)
        (equal? f <)
        (equal? f <=)
        (equal? f mequal?)
        (equal? f not-equal?)
        (equal? f >)
        (equal? f >=)
        (equal? f not)
        (equal? f mand)
        (equal? f mor)))

;; Predicado para restringir el tipo de números.
(define (numero-valido? n)
    (or (integer? n) (real? n)))

;; Función que calcula el módulo de forma multiparamétrica.
;; mmodulo: number number ... -> number
(define (mmodulo . args)
    (letrec (
             [mfoldl (λ (f v lst)
                       (match lst
                         ['() v]
                         [(cons x xs) (mfoldl f (f v x) xs)]))])
      (mfoldl modulo (car args) (cdr args))))

;; Función que calcula la potencia de forma multiparamétrica.
;; mmexpt: number number ... -> number
(define (mexpt . args)
    (letrec (
             [mfoldl (λ (f v lst)
                       (match lst
                         ['() v]
                         [(cons x xs) (mfoldl f (f v x) xs)]))])
      (mfoldl expt (car args) (cdr args))))

;; Función que indica si las expresiones pasadas como parámetro son iguales.
;; mequal?: any any ... -> boolean
(define (mequal? . args)
    (letrec (
             [mfoldl (λ (f v lst)
                       (match lst
                         ['() v]
                         [(cons x xs) (mfoldl f (f v x (car args)) xs)]))])
      (mfoldl equal-m #t (cdr args))))

(define (equal-m v x u)
  (if (equal? v #f) #f (equal? x u)))

;; Función que indica si las expresiones pasadas como parámetro son distintas.
;; not-equal?: any any ... -> boolean
(define (not-equal? . args)
    (letrec (
             [mfoldl (λ (f v lst)
                       (match lst
                         ['() v]
                         [(cons x xs) (mfoldl f (f v x (car args)) xs)]))])
      (mfoldl equal-m2 (not (empty? (cdr args))) (cdr args))))

(define (equal-m2 v x u)
  (if (equal? v #f) #f (not (equal? x u))))



;; Función que calcula la conjunción de forma multiparamétrica.
;; mand: boolean boolean ... -> boolean
(define (mand . args)
    (if (empty? args)
        #t
    (letrec (
             [mfoldl (λ (f v lst)
                       (match lst
                         ['() v]
                         [(cons x xs) (mfoldl f (f v x (car args)) xs)]))])
      (mfoldl and-m #t  args))))

(define (and-m v x u)
  (if (equal? v #f) #f (equal? #t (and x u))))

;; mor: boolean boolean ... -> boolean
(define (mor . args)
  (if (empty? args)
        #f
    (letrec (
             [mfoldl (λ (f v lst)
                       (match lst
                         ['() v]
                         [(cons x xs) (mfoldl f (f v x) xs)]))])
      (mfoldl or-m #f args))))

(define (or-m v x)
  (if (equal? v #t) #t (equal? x #t)))

;; TDA para representar el árbol de sintaxis abstracto del lenguaje FWBAE.
(define-type FWBAE
    [idS    (i symbol?)]
    [numS   (n numero-valido?)]
    [boolS  (b boolean?)]
    [opS    (f operador-valido?) (args (listof FWBAE?))]
    [withS  (bindings (listof binding?)) (body FWBAE?)]
    [withS* (bindings (listof binding?)) (body FWBAE?)]
    [funS   (params (listof symbol?)) (body FWBAE?)]
    [appS   (fun-expr FWBAE?) (args (listof FWBAE?))])

;; TDA para asociar identificadores con valores.
(define-type Binding
    [binding (name symbol?) (value FWBAE?)])

;; TDA que es una versión sin azúcar sintáctica del TDA FWBAE.
(define-type FBAE
    [id   (i symbol?)]
    [num  (n numero-valido?)]
    [bool (b boolean?)]
    [op   (f operador-valido?) (args (listof FBAE?))]
    [fun  (params (listof symbol?)) (body FBAE?)]
    [app  (fun-expr FBAE?) (args (listof FBAE?))])

;; TDA para representar el ambiente de evaluación.
(define-type Env
    [mtSub]
    [aSub (name symbol?) (value FBAE-Value?) (env Env?)])

;; TDA para representar los resultados devueltos por el intérprete.
(define-type FBAE-Value
    [numV     (n number?)]
    [boolV    (b boolean?)]
    [closureV (params (listof symbol?)) (body FBAE?) (env Env?)])