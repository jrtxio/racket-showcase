#lang racket

(provide find-odd)

(define (find-odd lst)
     (let ([h (make-hash)]
           [res void])
       (for ([i lst])
         (hash-update! h i add1 0))
    (hash-for-each h (lambda (k v) (when (odd? v) (set! res k)))) res))

#| (define (find-odd lst)
     (let ([h (hash)]
           [res void])
       (for ([i lst])
         (set! h (hash-update h i add1 0)))
    (hash-for-each h (lambda (k v) (when (odd? v) (set! res k)))) res)) |#


#| (define (find-odd lst)
  (foldl bitwise-xor 0 lst)) |#