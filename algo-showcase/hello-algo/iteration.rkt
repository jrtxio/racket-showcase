#lang racket

(define (for-loop1 n)
  (define res 0)
  (for ([i (in-range 1 (+ n 1))])
    (set! res (+ res i)))
  res)

(define (for-loop2 n)
  (for/sum ([i (in-range 1 (+ n 1))])
    i))