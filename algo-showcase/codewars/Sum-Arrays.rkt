#lang racket

(provide sum)

(define (sum lst)
  (if (null? lst) 0
      (for/sum ([i lst]) i)))


#| (define (sum lst)
  (apply + lst)) |#

#| (define (sum lst)
  (foldl + 0 lst)) |#