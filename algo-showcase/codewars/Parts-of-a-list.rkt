#lang racket

(provide part-list)

(define (part-list ls)
  (let ([n (length ls)])
    (for/list ([i (in-range 1 n)])
      (list (string-join (reverse (list-tail (reverse ls) (- n i)))  " ") (string-join (list-tail ls i) " ")))))


#| (define (part-list ls)
     (for/list ([i (in-range 1 (length ls))])
       (list (string-join (take ls i)  " " )
          (string-join (drop ls i) " " )))) |#