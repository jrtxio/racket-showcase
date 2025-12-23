#lang racket

(define (make-counter)
  (let ([count 0])
    (lambda (msg . args)
      (cond
        [(eq? msg 'inc)
         (set! count (add1 count))
         count]
        [(eq? msg 'get)
         count]
        [(eq? msg 'reset)
         (set! count 0)
         count]
        [else
         (error "Unkown method" msg)]))))

(define (make-person name age)
  (let ([name (string->symbol name)]
        [age age])
    (letrec ([greet
              (lambda ()
                (format "Hi, I'm ~a and I'm ~a years old." (symbol->string name)
                        age))]
             [self
              (lambda (msg . args)
                (cond
                  [(eq? msg 'get-name) (symbol->string name)]
                  [(eq? msg 'set-name) (set! name (string->symbol (car args))) 'ok]
                  [(eq? msg 'get-age) age]
                  [(eq? msg 'have-birthday) (set! age (add1 age)) age]
                  [(eq? msg 'greet) (greet)]
                  [else (error "Unkown method" msg)]))])
      self)))

(define (make-student name age school)
  (let ([person (make-person name age)]
        [school school])
    (letrec ([self
              (lambda (msg . args)
                (cond
                  [(eq? msg 'get-school) school]
                  [(eq? msg 'set-school) (set! school (car args)) 'ok]
                  [(eq? msg 'study) (format "~a studies at ~a" (send person 'get-name) school)]
                  [else (apply person msg args)]))])
      self)))

(define (make-object method-table)
  (lambda (msg . args)
      (let ([pair (assoc msg method-table)])
        (if pair
            (apply (cdr pair) args)
            (error "Unkown method msg")))))

(define (make-counter-2)
  (let ([count 0])
    (make-object
        (list
         (cons 'inc (lambda ()
                      (set! count (add1 count))
                      count))
         (cons 'get (lambda ()
                      count))))))

(define (send obj msg . args)
  (apply obj msg args))