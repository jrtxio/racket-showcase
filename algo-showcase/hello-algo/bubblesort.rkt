#lang racket

;; 交换列表中两个相邻元素的位置
(define (swap lst i j)
  (cons (list-ref lst i)
        (cons (list-ref lst j)
              (append (take lst i)
                      (drop lst (add1 j))))))

;; 冒泡排序函数
(define (bubble-sort lst)
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (let loop ((lst lst) (swapped? #f))
        (if (null? (cdr lst))
            (if swapped?
                (bubble-sort lst)
                lst)
            (let ((next-pair (cons (car lst) (cadr lst))))
              (if (> (car next-pair) (cdr next-pair))
                  (loop (swap lst 0 1) #t)
                  (loop (cdr lst) swapped?)))))))
