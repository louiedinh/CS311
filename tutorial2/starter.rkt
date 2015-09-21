#lang plai

(define-type WAE
  [num (n number?)]
  [add (lhs WAE?) (rhs WAE?)]
  [sub (lhs WAE?) (rhs WAE?)]
  [with (name symbol?) (named-expr WAE?) (body WAE?)]
  [id (name symbol?)]
  )

; Takes an sexp and returns a WAE construct
(define (parse-WAE sexp)
  (cond ((number? sexp) (num sexp))
        ((symbol? sexp) (id sexp))
        ((list? sexp)
         (let [(op (first sexp))
               (body (rest sexp))]
           (cond ((eq? op '+) (add (num (first body)
                                        (num (second body)))))
                 ((eq? op '-) '-)
                 (else "Don't know what this is!"))))))
  