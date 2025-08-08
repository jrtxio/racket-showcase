#lang racket

(provide stock-list)

(define (stock-list list-of-books list-of-cat)
  (if (or (null? list-of-books) (null? list-of-cat))
      '()
      (let ([h (make-hash)])
        (for ([cat list-of-cat])
          (hash-set! h cat 0))
        (for ([book list-of-books])
          (when (hash-ref h (string (string-ref (list-ref (string-split book) 0) 0)) #f)
            (hash-update! h
                          (string (string-ref (list-ref (string-split book) 0) 0))
                          (lambda (val) (+ val (string->number (list-ref (string-split book) 1)))))))
        (for/list ([cat list-of-cat])
          (cons cat (hash-ref h cat))))))


#| (define (stock-list list-of-books list-of-cat)
     (if (null? list-of-books)
         null
         (map
          (lambda (cat)
            (cons cat
                  (apply +
                         (map (lambda (book)
                                (match-define (list name count) (string-split book))
                                (if (string-prefix? name cat)
                                    (string->number count)
                                    0))
                              list-of-books))))
       list-of-cat))) |#
