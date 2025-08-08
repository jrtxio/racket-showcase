#lang racket


(define location%
  (class object%
    (init aisle
          section
          shelf)
    (define $aisle aisle)
    (define $section section)
    (define $shelf shelf)
    (super-new)
    (define/public (as-string)
      (format "~a-~a-~a"
              $aisle
              $section
              $shelf))
    (define/public (get-aisle)
      $aisle)))

(new location%
     [aisle "13"]
     [section 5]
     [shelf 2])

(define super-shoes-loc
  (new location%
       [aisle 13]
       [section "B"]
       [shelf 5]))

(send super-shoes-loc as-string)

(define bin%
  (class object%
    (init location
          product
          qty
          pickable?)
    (define $location location)
    (define $product product)
    (define $qty qty)
    (define $pickable? pickable?)
    (super-new)
    (define/public (is-available?)
      (and $pickable?
           (> $qty 0)))
    (define/public (pick!)
      (unless (> $qty 0)
        (error
         (format "Cannot pick non-postive qty ~a from bin ~a."
                 $qty
                 (send $location as-string))))
      (set! $qty (sub1 $qty)))
    (define/public (get-aisle)
      (send $location get-aisle))
    (define/public (make-unpickable!)
      (set! $pickable? #f))
    ))