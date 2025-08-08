#lang racket

(provide dig-pow)

(define (split-dig n)
  (map (lambda (c) (string->number (string c))) (string->list (number->string n))))

(define (sum lst p)
  (for/sum ([i lst]
            [j (in-range (length lst))])
    (expt i (+ p j))))

(define (dig-pow n p)
  (let ([res (/ (sum (split-dig n) p) n)])
    (if (integer? res)
        res
        -1)))


#| (define (dig-pow1 n p)
     (let ([s (for/sum ([i (in-naturals p)]
                        [digit (in-string (number->string n))])
                (expt (string->number (string digit)) i))])
    (if (zero? (modulo s n)) (/ s n) -1))) |#()