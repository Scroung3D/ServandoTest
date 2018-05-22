#lang plai

(require "grammars.rkt")

;; Analizador semántico.
;; Interpreta el árbol de sintaxis abstracta generado por el desendulzador.
;; interp: FBAE -> FBAE-Value
(define (interp expr env)
   (match expr
     [(id i) (lookup i env)]
     [(num n) (numV n)]
     [(op opA izq der)
        (opA (numV-n (strict(interp izq env)))
            (numV-n (strict(interp izq env))))]
     [(if0 test-expr then-expr else-expr)
      (if (zero? (numV-n(strict(interp test-expr))))
          (interp then-expr env)
          (interp else-expr env))]
     [(fun param body)
      (closureV param body env)]
     [(app fun-expr arg)
      (let ([fun-val (strict (interp fun-expr arg))])
        (interp (clousureV-body fun-val)
               (aSub (closureV-param fun-val)
               (exprV arg env)
               (closureV-env fun-val)
                )))]))


;; Busca el valor de un identificador en el ambiente.
;; Si el identificador no se encuentra, se genera el error "Identificador libre".
;; lookup: symbol Env -> FBAE-Value
(define (lookup id env)
    (match env
        [(mtSub) (error 'interp "Identificador libre")]
        [(aSub name value rest-env) (if (symbol=? name id) value (lookup id rest-env))]))


(define (strict expr)
  (match expr
    [(exprV expr env) (strict(interp expr env))]
    [_ expr]))