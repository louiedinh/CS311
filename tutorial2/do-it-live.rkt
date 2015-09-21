#lang plai

(define-type WAE
  [num (n number?)]
  [add (lhs WAE?) (rhs WAE?)]
  [sub (lhs WAE?) (rhs WAE?)]
  [with (name symbol?) (named-expr WAE?) (body WAE?)]
  [id (name symbol?)]
  )

(define (parse-WAE sexp)
  (cond ((number? sexp) (num sexp))
        ((symbol? sexp) (id sexp))
        ((list? sexp)
         (let [(op (first sexp))
               (body (rest sexp))]
           (cond ((eq? op '+) (add (parse-WAE (first body)) (parse-WAE (second body))))
                 ((eq? op '-) (sub (parse-WAE (first body)) (parse-WAE (second body))))
                 ((eq? op 'with) (with (first (first body))
                                       (parse-WAE (second (first body)))
                                       (parse-WAE (second body))))
                  (else (error "Unsupported op")))))          
        (else (error "Help!"))))