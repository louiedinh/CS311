(define (insertR new old lat)
  (cond ((null? lat) '())
        ((eq? (car lat) old) (cons old (cons new (cdr lat))))
        (else (cons (car lat) (insertR new old (cdr lat))))))

(define (insertL new old lat)
  (cond ((null? lat) '())
        ((eq? (car lat) old) (cons new lat))
        (else (cons (car lat) (insertL new old (cdr lat))))))

(define (subst new old lat)
  (cond ((null? lat) '())
        ((eq? (car lat) old) (cons new (cdr lat)))
        (else (cons (car lat) (subst new old (cdr lat))))))

(define (multirember a lat)
  (cond ((null? lat) '())
        ((eq? a (car lat)) (multirember a (cdr lat)))
        (else (cons (car lat) (multirember a (cdr lat))))))

(define (multiinsertR new old lat)
  (cond ((null? lat) '())
        ((eq? (car lat) old) (cons (car lat) (cons new (multiinsertR new old (cdr lat)))))
        (else (cons (car lat) (multiinsertR new old (cdr lat))))))

(define (multiinsertL new old lat)
  (cond ((null? lat) '())
        ((eq? old (car lat)) (cons new (cons old (multiinsertL new old (cdr lat)))))
        (else (cons (car lat) (multiinsertL new old (cdr lat))))))

(define (multisubst new old lat)
  (cond ((null? lat) '())
        ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
        (else (cons (car lat) (multisubst new old (cdr lat))))))
