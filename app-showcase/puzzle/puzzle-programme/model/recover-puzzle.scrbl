;recover-puzzle.scrbl
;恢复拼图初始状态。

#lang scribble/manual

@title[#:tag "model/recover-puzzle"]{恢复拼图初始状态}

需要做两方面工作：将单元格顺序恢复成初始状态；以及恢复拼图图片。

@itemlist[
@item{将单元格顺序恢复成初始状态：
      
@codeblock|{
;恢复单元格初始列表：
(define (recover-cells-hash)
  ;恢复单元格：
  (define (recover-cell id c)
    (let* ([id/old (list-ref id-list/blend id)]
           [pic/old (split-picture-by-id id/old)])
      (hash-set! cells id (cell id/old pic/old))))
  (hash-for-each cells recover-cell))
}|

这里使用了闭包，对单元格散列表进行操作。}

 @item{恢复拼图图片：

@codeblock|{
;恢复拼图图片：
(define (recover-puzzle-picture)
  (recover-cells-hash)
  (set-puzzle-picture!))
  }|

这个函数应该很清楚了，不在解释。}
 ]

至此，所有拼图数据建模操作全部完成。汇总一下模块内所使用的全局标识。

@codeblock|{
;定义主要数据：
(define source void);原图
(define picture void);图片对象
(define height/picture 0);图片高度
(define width/picture 0);图片宽度
(define height/cell 0);单元高度
(define width/cell 0);单元宽度
(define rows 3);行数
(define cols 3);列数
(define cells (make-hash));单元格散列表
(define gap 1);单元格显示间隙
(define puzzle-picture void);混合的图片
(define id/current 0);当前单元格id号
(define id-list/blend void);混合后的id号列表

;定义单元格数据结构：
(struct cell (id bitmap)
  #:transparent
  #:mutable
  )
}|

接下来选择需要提供出去的函数用@racket[provide]进行设置。同时，为了外部调用这些函数时不能违背设计意图，进行@bold{合约}申明。