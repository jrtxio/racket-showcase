#lang racket


#| (define (f x)
  (* x (add1 x))) |#

#| (define/contract (f x)
     (-> number? number?)
  (* x (add1 x))) |#

(define/contract (f x)
  (-> integer? integer?)
  (* x (add1 x)))

#| (define/contract (cool x y)
     (-> number? number? number?)
     (f (max (ceiling x)
          (ceiling y)))) |#

#| (define/contract (cool x y)
     (number? number? . -> . exact-integer?)
     (f (inexact->exact
         (max (ceiling x)
           (ceiling y))))) |#

#| (define/contract (cool x y)
     (real? real? . -> . exact-integer?)
     (f (inexact->exact
         (max (ceiling x)
           (ceiling y))))) |#

(define/contract (cool x y)
  (real? real? . -> . (or/c exact-integer?
                            (=/c +inf.0)))
  (define m (max (ceiling x)
                 (ceiling y)))

  (if (= m +inf.0)
      +inf.0
      (f (inexact->exact m))))
