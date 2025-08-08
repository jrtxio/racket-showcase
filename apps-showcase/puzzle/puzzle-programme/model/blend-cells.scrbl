;blend-cells.scrbl
;混合单元格。

#lang scribble/manual

@title[#:tag "blend-cells"]{混合单元格}

@section[#:tag "blend-cells/swap-cell"]{调换单元格}

为了混合单元格，需要能够将表中的任意两个单元格进行调换。如下：

@codeblock|{
;交换列表中的两个单元格：
(define (swap-cell id/target id/source)
  (let* ([cell/t (hash-ref cells id/target)]
         [cell/s (hash-ref cells id/source)]
         [swap/t (cell (cell-id cell/s)
                       (cell-bitmap cell/s))]
         [swap/s (cell (cell-id cell/t)
                       (cell-bitmap cell/t))])
    (hash-set! cells id/target swap/t)
    (hash-set! cells id/source swap/s)))
}|

这里用散列表的优势就体现出来了，可以直接操作表中的两个值进行对调，整个表不用动。

@section[#:tag "blend-cells/swap-all-cells"]{交换所有单元格}

接下来就可以进行整个散列表的单元格互换了。但是，如何确定谁和谁互换呢？这需要一个策略。

这里采取的策略是：

@itemlist[#:style 'ordered
@item{将散列表里随机的两个单元格id号所对应的单元格互换；}
@item{将散列表里未互换的单元格执行上边的操作直到全部完成。}
]

找到两个未进行互换的且随机的单元格id号是关键。

这里采取的策略是：

@itemlist[#:style 'ordered
@item{将散列表中的键值抽取出来生成一个列表，把它叫做未使用表；}
@item{以未使用表的长度为随机数种子生成一个随机数，该随机数对应的列表成员值作为源id号；}
@item{将未使用表中源id号列表成员剔除，产生新的未使用表；}
@item{用选取源id号的方法产生一个目标id号；}
@item{将未使用表中目标id号列表成员剔除，产生新的未使用表；}
@item{将散列表中源id号键对应成员与目标id号对应成员交换；}
@item{重复以上操作，直到未使用表低于两个成员为止。}
 ]

从以上描述就可以看出，这是可以用递归实现的。

同时需要注意的是，这个过程不涉及连带求值计算，仅涉及操作。请问，它是尾递归还是普通递归？（答案是尾递归）

实现代码如下：

@codeblock|{
;打乱列表中单元格表顺序：
(define (blend-cells)
  ;从剩余成员中随机抽取一个未调换过的：
  (define (swap-two-cells ids/unused)
    (if (< (length ids/unused) 2)
        ;保存混合后的id列表（记录混合后的状态）：
        (set! id-list/blend
              (hash-map cells
                        (lambda (k v)
                                (cell-id v))))
        (let* ([id/i (list-ref
                      ids/unused
                      (random 0 (length ids/unused)))]
               [ids/unused (remove id/i ids/unused)]
               [id/j (list-ref
                      ids/unused
                      (random 0 (length ids/unused)))]
               [ids/unused (remove id/j ids/unused)])
          (swap-cell id/i id/j)
          (swap-two-cells ids/unused))))
  (swap-two-cells (hash-keys cells)))
}|

这里用到了散列表遍历函数@racket{hash-map}，它返回遍历求值生成的列表。还有一个随机数函数@racket{random}来生成随机数。

需要注意的是，程序代码中在“未使用表低于两个成员”时并没有“为止”，而是采用@racket{if}分支了一个操作：保存混合后的id列表（@var{id-list/blend}），这个操作是为了记录混合后的状态，以便后边用户可以使用“再来一次”功能重复同样的混合表来进行和之前完全一样的拼图过程（如果采用重新混合，由于混合是随机的，不能实现和之前完全一样的拼图）。

因此，还需定义一个全局绑定：

@codeblock|{
(define id-list/blend void);混合后的id号列表
}|

@section[#:tag "blend-cells/recover-cells-hash"]{恢复单元格混合表}

@codeblock|{
;恢复单元格混合表：
(define (recover-cells-hash)
  ;恢复单元格：
  (define (recover-cell id c)
    (let* ([id/old (list-ref id-list/blend id)]
           [pic/old (split-picture-by-id id/old)])
      (hash-set! cells id (cell id/old pic/old))))
  (hash-for-each cells recover-cell))
}|

这个函数根据保存的混合后的id号列表@var{id-list/blend}重新切割单元格图片以恢复单元格散列表。

这里使用了@racket{hash-for-each}而不是@racket{hash-map}，因为只需要执行遍历操作，不需要返回列表。
