#lang racket

;; declarative programming
(define/contract (move-zeroes nums)
  (-> (vectorof exact-integer?) void?)
  (define left 0)
  
  (for ([right (in-range (vector-length nums))])
    (when (not (= (vector-ref nums right) 0))
      (let ([temp (vector-ref nums right)])
        (vector-set! nums right (vector-ref nums left))
        (vector-set! nums left temp))
      (set! left (+ left 1)))
    ))

;(define nums (vector 0 1 0 3 12))
;(define nums (vector 0))
;(define nums (vector 1 0 0))
(define nums (vector 0 1 1))

(move-zeroes nums)
(printf "~a\n" nums)
