#lang racket

(provide (struct-out bin))

(struct bin
  (location
   product
   qty
   pickable?) #:mutable)

(bin "13-A-2"
     "Super Shoes"
     4
     #t)

(struct location
  (aisle
   section
   shelf))

(location 13 "A" 2)


(bin (location 13 "A" 2)
     "Super Shoes"
     20
     #t)

(define super-shoes-loc
  (location 13 "A" 2))

(bin super-shoes-loc
     "Super Shoes"
     20
     #t)

(location-aisle super-shoes-loc)

(define super-shoes-bin
  (bin super-shoes-loc
       "Super Shoes"
       20
       #t))

(bin-qty super-shoes-bin)

(define (bin-aisle bin)
  (location-aisle (bin-location bin)))

(define (location->string loc)
  (format "~a-~a-~a"
          (location-aisle loc)
          (location-section loc)
          (location-shelf loc)))

#| (define (location->string loc)
     (define aisle (location-aisle loc))
     (define section (location-section loc))
     (define shelf (location-shelf loc))
     (format "~a-~a-~a"
             aisle
             section
          shelf)) |#

(struct-copy bin
             super-shoes-bin
             [pickable? #f])

(define unpickable-super-shoes-bin
  (struct-copy bin
               super-shoes-bin
               [pickable? #f]))

(bin-pickable? unpickable-super-shoes-bin)

(bin-qty unpickable-super-shoes-bin)

(define unpickable-&-out-of-stock-super-shoes-bin
  (struct-copy bin
               super-shoes-bin
               [pickable? #f]
               [qty 0]))

(define (make-out-of-stock b)
  (struct-copy bin
               b
               [pickable? #f]
               [qty 0]))

(define (pick b)
  (define qty (bin-qty b))
  (define loc (bin-location b))
  (define product (bin-product b))
  (unless (> qty 0)
    (error
     (format
      "Cannot pick from bin ~a (~a): qty is non-positive."
      (location->string loc)
      product)))
  (struct-copy bin
              b
              [qty (sub1 qty)]))

#| (location 3 "A" 15) |#

(struct location1
  (aisle
   section
      lf)
  #:transparent)

#| (location1 "5" 3 "B") |#













