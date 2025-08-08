#lang racket

;; imperative programming
(define/contract (two-sum nums target)
  (-> (listof exact-integer?) exact-integer? (listof exact-integer?))
  (define table (make-hash))
  (for/last ([i (in-naturals)]
             [num (in-list nums)]
             #:final (hash-has-key? table (- target num)))
    (when (not (hash-has-key? table num))
      (hash-set! table num i))
    (list (hash-ref table (- target num) #f) i))
  )

;; declarative programming
(define/contract (two-sum nums target)
  (-> (listof exact-integer?) exact-integer? (listof exact-integer?))
  (let loop ([table (hash)] [i 0] [num nums])
    (match num
      ['() #f]
      [(cons head rest)
       (define maybe-pos (hash-ref table (- target head) #f))
       (if maybe-pos
           (list maybe-pos i)
           (loop (hash-set table head i) (add1 i) rest))])))

(define/contract (two-sum nums target)
  (-> (listof exact-integer?) exact-integer? (listof exact-integer?))
  (let iter ([l nums] [h (hash)] [i 0])
    (if (hash-has-key? h (car l))
        (list (hash-ref h (car l)) i)
        (iter (cdr l) (hash-set h (- target (car l)) i) (add1 i)))))

(two-sum (list 2 7 11 15) 9)
(two-sum (list 3 2 4) 6)
(two-sum (list 3 3) 6)