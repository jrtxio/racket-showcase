#lang racket

(provide evaporator)


(define (evaporator content evap-per-day threshold)
  (do
      ([i 0 (add1 i)])
    ((<= (* 100.0 (expt (- 1 (/ evap-per-day 100.0)) i)) threshold) i)
    (void)))

#| (define (evaporator content evap-per-day threshold)
     (let evaporate ([days 0]
                     [remaining 100.0])
       (if (< remaining threshold)
           days
        (evaporate (add1 days) (* remaining (- 1 (/ evap-per-day 100.0))))))) |#