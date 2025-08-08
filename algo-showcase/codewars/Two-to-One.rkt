#lang racket

(provide longest)

(define (longest s1 s2)
  (let ([v1 (list->set (string->list s1))]
        [v2 (list->set (string->list s2))])
    (list->string (sort (set->list (set-union v1 v2)) char<?))))


#| (define (longest s1 s2)
  (list->string (sort (remove-duplicates (string->list (string-append s1 s2))) char<=?))) |#