#lang racket


(define welcome-msg "Welcome to Racket Weekend!")

(define cheers "Cheers!")

(define course-num-days 2)

(define (explanation num-days)
  (format "Racket Weekend is a ~a-day course on Racket."
          num-days))

(define (welcome)
  (displayln welcome-msg))

(define (cheer)
  (displayln cheers))

(define (explain num-days)
  (displayln (explanation num-days)))

(define (add a b)
  (+ a b))

(module+ main
  (welcome)
  (explain course-num-days)
  (cheer))