#lang racket

; Definition for singly-linked list:
#|

; val : integer?
; next : (or/c list-node? #f)
(struct list-node
  (val next) #:mutable #:transparent)

; constructor
(define (make-list-node [val 0])
  (list-node val #f))

|#

; 定义一个链表节点结构体
(struct list-node
  (val next) #:mutable #:transparent)

; 定义一个构造函数来创建链表节点
(define (make-list-node [val 0] [next #f])
  (list-node val next))

; 创建链表
(define node1 (make-list-node 1))
(define node2 (make-list-node 2))
(define node3 (make-list-node 3))

; 连接链表节点
(set-list-node-next! node1 node2)
(set-list-node-next! node2 node3)

; 遍历链表并打印值
(define (print-list node)
  (cond
    [(list-node? node)
     (display (list-node-val node))
     (newline)
     (print-list (list-node-next node))]
    [else '()]))

; 打印链表
(print-list node1)


(define/contract (reverse-list head)
  (-> (or/c list-node? #f) (or/c list-node? #f))
  )