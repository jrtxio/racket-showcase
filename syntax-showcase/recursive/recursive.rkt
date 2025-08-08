#lang racket

(define fact
  (lambda (n)
    (if (= n 0) 1
        (* n (fact (- n 1))))))

(define fib
  (lambda (n)
    (cond
      [(= n 0) 0] ; base case
      [(= n 1) 1] ; base case
      [else (+ (fib (- n 1)) (fib (- n 2)))]))) ; recursive case