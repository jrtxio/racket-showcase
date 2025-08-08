;success.scrbl
;判断是否成功完成拼图。

#lang scribble/manual

@title[#:tag "model/success"]{判断是否成功完成拼图}

先看代码，再作解释。

@codeblock|{
;判断是否成功完成拼图：
(define (success?)
  (define s #t);成功完成标志
  ;检查复原情况的函数：
  (define (id=old? key value)
    (unless (= key (cell-id value))
      (set! s #f)))
  ;逐个检查是否复原：
  (hash-for-each cells id=old?)
  s)
  }|

对单元格散列表逐个检查，如果每个键值都和初始图片的位置一致（判断函数通过闭包提供），则表示完成了拼图。@racket[id=old?]，难以置信而又能直观表达的一个函数名！