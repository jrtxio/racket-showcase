#lang racket


(require (file "bin.rkt"))

(module+ test
  (require rackunit)

  
  (check-true (location? super-shoes-loc))
  (check-= 5
         (location-aisle super-shoes-loc)
         0)
  
  (define b (bin (location "A" 5 1)
                 "Fancy Pants"
                 1
                 #t))
  (define picked-b (pick b))
  (check-= 1
           (bin-qty b)
           0)
  (check-= 0
           (bin-qty picked-b)
           0)
  (check-exn exn:fail?
             (lambda ()
               (pick picked-b))))

