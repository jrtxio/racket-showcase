#lang racket


(list "a" 4 #t)

(define a (list "a" 4 #t))

(list-ref a 0) ; "a"
(list-ref a 1) ; 4
(list-ref a 2) ; #t

(first a) ; "a
(second a) ; #4
(third a) ; #t

(length a)

(rest a)

(define/contract (cube x)
  (number? . -> . number?)
  (* x x x))

(define/contract l
  (listof number?)
  (list 4 -9 45.132 45-2i))

(map cube l)

(define/contract hamlet
  string?
  "To be or not to be, that is the question.")

(define/contract (uppercase str)
  (string? . -> . string?)
  (cond ((string=? str "")
         "")
        (else
         (define c (string-ref str 0))
         (format "~a~a"
                 (char-upcase c)
                 (substring str 1)))))

(map uppercase (string-split hamlet))

(define/contract (square-sum x y)
  (number? number? . -> . number?)
  (expt (+ x y) 2))

(define nums-1 (list -5 2 4.6))
(define nums-2 (list 4 01.3 1980))

(map square-sum nums-1 nums-2)

(foldl +
       0
       (list 1 2 3 4 5))

(andmap even?
        (list 313 55 94 4551 5+5i))

(define (imaginary? x)
  (and (number? x)
       (not (real? x))))

(ormap imaginary?
       (list 313 5.6 94 4.551 5+5i))

(define/contract (talk-about-cubes nums)
  ((listof number?) . -> . void?)
  (for ([n nums])
    (displayln (format "The cube of ~a is ~a."
                       n
                       (cube n)))))

(define nums (list 4 1.9 -5 3+2i))

(talk-about-cubes nums)

(for ([a (list "a" "b" "c")]
      [b (list 1 2 3)])
  (displayln (format "a = ~a and b = ~a"
                     a
                     b)))

(require (file "types.rkt"))

(define (set-qtys-for-bins! bins qtys)
  (for ([b bins]
        [q qtys])
    (set-bin-qty! b q)))

(match "143"
  [(? integer?)
   (error "Will never happen.")]
  ["134"
   (error "Close but no cigar.")]
  [(pregexp "1[0-9]3")
   "yes"]
  [else
   "you shouldn't see me"])

(define louis
  (list "l" "ou" "i" "s"))

(match louis
  [(? string?)
      "no way Jose"]
  [(list "l")
   "llist contains more than that"]
  [(list-rest "l" "o" more)
   "close but not quire"]
  [(list-rest "l" (pregexp "o[a-z]") more)
   (displayln
    (format "the second element of the list is: ~a"
            (second louis)))
   more]
  [else
   "ouch!"])

#| (match louis
     [(? string?)
      "no way Jose"]
     [(list "l")
      "list contains more than that"]
     [(list-rest "l" "o" more)
   "close, but not quire"]) |#























