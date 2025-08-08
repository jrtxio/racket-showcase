#lang racket

;; (define welcome-msg "Welcome to Racket Weekeend!")

(list 'define 'welcome-msg "Welcome to Racket Weekend")

(define (define-greeting greet)
  (list 'define 'welcome-msg greet))

(module+ test
  (require rackunit)
  (define-syntax (let-test stx)
    (syntax-case stx()
      [(_ formals tests ...)
       #'(let formals
           (test-begin
            tests ...))])))

(module+ test
  (let-test ([a 5]
             [b 3])
            (check-true (exact-integer? a))
            (check-false (even? b))))

(require (for-syntax racket/match))

#| (define-syntax (foo stx)
     ; using match somewhere here is ok
     ; thanks to the (require (for-syntax ...))
     ; line above
) |#

