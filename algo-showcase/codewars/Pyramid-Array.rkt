#lang racket

(provide pyramid)


(define (pyramid n)
  (let ([v (make-vector n)])
    (do ([i 0 (+ i 1)])
      ((>= i n) (vector->list v))
      (vector-set! v i (make-list (add1 i) 1)))))

#| (define (pyramid1 n)
  (map (lambda (x) (make-list x 1)) (range 1 (+ n 1)))) |#

#| (define (pyramid n)
     (for/list ([i (in-range 1 (add1 n))])
    (make-list i 1))) |#