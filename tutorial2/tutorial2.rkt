#lang plai
;; Questions : Lecture Content + Assignment.
;; - Details about parsing: Go through writing a parser for WAE (live coding)
;; - Substitution
;; Tutorial itself:
;; - Show read / print(ln)
;; - Implement a REPL
;; - Implement list-append and list-revert from scratch
;; - Show string->list and list->string
;; - Implement split-list and split-words
;; - Add "if0" to the WAE language.
(define-type WAE
  [num (n number?)]
  [add (lhs WAE?) (rhs WAE?)]
  [sub (lhs WAE?) (rhs WAE?)]
  [with (name symbol?) (named-expr WAE?) (body WAE?)]
  [id (name symbol?)]
  )

; Parser for WAEs (arithmetic expressions with "with"):
(define (parse-WAE sexp)
  (cond
    
    [(number? sexp) (num sexp)]
    
    [(symbol? sexp) (id sexp)]
    
    [(list? sexp)
     (let*
         ([head      (first sexp)]
          [args      (rest sexp)]
          [arg-count (length args)])
       
       (case head
         [(+) (if (= arg-count 2)
                  (add (parse-WAE (first args)) (parse-WAE (second args)))
                  (error "parse-WAE: + needs exactly 2 arguments"))]
         
         [(-) (if (= arg-count 2)
                  (sub (parse-WAE (first args)) (parse-WAE (second args)))
                  (error "parse-WAE: - needs exactly 2 arguments"))]
       
         ; new in parse-WAE:
         ;   for with, {<id> <WAE>} is first arg (itself a list)
         ;
         [(with) (if (= arg-count 2)
                   (let ([inner-sexp (first args)]
                         [body-sexp  (second args)])
                     (if (and (list? inner-sexp)
                              (= (length inner-sexp) 2))
                         
                         ; extract from the inner list {<id> <WAE>}
                         (let ([name       (first inner-sexp)]
                               [named-sexp (second inner-sexp)]) 
                           (if (symbol? name)
                               (with name
                                     (parse-WAE named-sexp)
                                     (parse-WAE body-sexp))
                               
                               (error "parse-WAE: malformed `with'")))
                         
                         (error "parse-WAE: malformed `with'")))
                   (error "parse-WAE: malormed `with'"))]
         
         [else (error "parse-WAE: I only understand +, -, and `with'")]))]
    [else (error "parse-WAE: syntax error")]))



; subst : WAE? symbol? number? -> WAE?
;
(define (subst e x v)
  (type-case WAE e
    [num (n) (num n)]
    [add (eL eR) (add (subst eL x v) (subst eR x v))]
    [sub (eL eR) (sub (subst eL x v) (subst eR x v))]
    
    [with (y e eB)           ; see note below
          (if (symbol=? x y)
              (with x (subst e x v) eB) 
              (with y (subst e x v) (subst eB x v)))]
    [id (y)
          (if (symbol=? x y)
              v
              (id y))]
  )
)


; Interpreter for WAEs:
;
; interp : WAE → number
;
; Given a WAE e, return the number n such that
;
;                e ⇓ n
;
; according to the rules in the lecture notes "04-operational"
; (Eval-num, Eval-add, Eval-sub, Eval-with).

; When following rules, don't try to be clever!  The more clever
; you are, the more likely you'll make a mistake and end up with
; an interpreter that doesn't match the rules.  (There will be 
; times when you might want to be a little clever.  But not yet.)


(define (interp e)
  (type-case WAE e
    
    [num (n) n]
    
    [add (e1 e2)
         (let ([n1 (interp e1)]
               [n2 (interp e2)])
           (+ n1 n2))
         ]
    
    [sub (e1 e2)
         (let ([n1 (interp e1)]
               [n2 (interp e2)])
           (- n1 n2))
         ]
    
    [with (x e1 e2)
          (let* ([v1 (interp e1)]
                 [v2 (interp (subst e2 x (num v1)))])
            v2)]
    
    [id (x)
        (error "free-variable")]
    ))

(define (repl)
    (begin
      (println (interp (parse-WAE (read))))
      (repl)))

(define (list-append lst1 lst2)
    (cond
      ((empty? lst1) lst2)
      (else (cons (first lst1) (list-append (rest lst1) lst2)))))

(define (list-reverse lst)
    (cond
      ((empty? lst) lst)
      (else (list-append (list-reverse (rest lst)) (list (first lst))))))

;;list-split: any? (listof any?) -> (listof (listof any?))
;; (list-split 5 '(1 3 2 5 3 5 9 5 8)) -> '((1 3 2) (3) (9) (8))

(define (list-split separator lst)
    (if (empty? lst) (list empty)
    (let ((rest-split (list-split separator (rest lst))))
      (cond
        ((equal? separator (first lst)) (cons empty rest-split))
        (else (cons (cons (first lst) (first rest-split))
                    (rest rest-split)))))))

;;split-words: string? -> (listof string?)
(define (split-words str)
    (map list->string (list-split #\space (string->list str))))