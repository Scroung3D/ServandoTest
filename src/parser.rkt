#lang plai

(require "grammars.rkt")

;; Analizador sintáctico.
;; Regresa el árbol de sintaxis abstracto asociado a la sintaxis concreta.
;; parse: s-expression -> WAE.
(define (parse sexp)
   (match sexp
    [(? symbol?) (if (equal? sexp 'false) (boolS #f) (if (equal? sexp 'true) (boolS #t) (idS sexp)))]
    [(? number?) (numS sexp)]
    [(list 'with nvalue body) (withS (list-aux nvalue) (parse body))]
    [(list 'with* nvalue body) (withS* (list-aux nvalue) (parse body))]
    [(list 'fun params body) (funS params (parse body))]
    [(cons x xs) (if (or (list? x) (equal? 'foo x)) (appS (parse (car sexp)) (map parse (cdr sexp)))
                 (opS (elige (car sexp)) (list-aux2 (cdr sexp))))]))

     
(define (list-aux list)
  (match list
    ['() '()]
    [(cons x xs) (cons  (binding (car x) (parse (cadr x))) (list-aux xs))])) 

(define (list-aux2 list)
  (match list
    ['() '()]
    [(cons x xs) (cons (parse x) (list-aux2 xs))]))


;; Realiza un mapeo entre las operaciones del lenguaje anfitrión y el lenguaje objetivo.
;; elige: symbol -> procedure
(define (elige s)
   (match s
      ['+    +]
      ['-    -]
      ['*    *]
      ['/    /]
      ['%    mmodulo]
      ['min  min]
      ['max  max]
      ['pow  mexpt]
      ['sqrt sqrt]
      ['<    <]
      ['<=   <=]
      ['=    mequal?]
      ['/=   not-equal?]
      ['>    >]
      ['>=   >=]
      ['not  not]
      ['and  mand]
      ['or   mor]))
