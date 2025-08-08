#lang racket

(provide sum-mix)

(define (sum-mix lst)
  (for/sum ([i lst]) (if (integer? i) i (string->number i))))


#| (define (sum-mix lst)
  (apply + (map (lambda (x) (if (string? x) (string->number x) x)) lst))) |#
