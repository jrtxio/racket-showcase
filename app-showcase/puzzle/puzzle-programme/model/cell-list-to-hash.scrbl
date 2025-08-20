;cell-list-to-hash.scrbl
;将单元格列表转换为散列表。

#lang scribble/manual

@title[#:tag "cell-list-to-hash"]{将单元格列表转换为散列表}

考虑到后续可能出现单元格混合、内容修改等，为了方便，实际使用的时候采用散列表来存储分割的单元格数据，这样就可以分别修改它的键或值来改变单元格属性（如当前位置）了。

@section[#:tag "cell-list-to-hash/hash-table"]{散列表}

散列表实现了从键到值的映射，其中键和值都可以是任意的Racket值，并且对散列表的访问和更新通常是常量时间操作。

散列表的创建有三种方式：@racket{make-hash}、@racket{make-hasheqv} 或 @racket{make-hasheq}，它们创建空的不可变散列表，差别在于创建出的散列表在进行比较时采用的对比函数不同，分别为： @racket{equal?}、@racket{eqv?} 或 @racket{eq?}。

如果有初始值，则可以用@racket{hash}、@racket{hasheqv} 和 @racket{hasheq}函数创建具有初始值的三列表，其各自取表同上。

散列表除上边介绍的函数外，常用的还有：@racket{hash?}判断、@racket{hash-set!}改变值或扩展散列表、@racket{hash-ref}返回值、@racket{hash-eq?}比较、@racket{hash-has-key?}判断键值是否存在、@racket{hash-remove!}移除值、@racket{hash-clear}清空散列表、@racket{hash-map}遍历散列表并返回列表、@racket{hash-keys}返回键值列表、@racket{hash-values}返回值列表、@racket{hash->list}返回键值序对列表、@racket{hash-for-each}遍历散列表、@racket{hash-count}返回值数量、@racket{hash-empty?}判断空表等。

@section[#:tag "cell-list-to-hash/list-to-hash"]{转换单元格列表为散列表}

通过前边的阐述可以知道可以通过遍历列表，通过将每个列表元素来构建散列表键、值即可实现。如下：

@codeblock|{
;分割单元格到单元格散列表：
(define (split-picture-to-cells)
  (for-each
   (lambda (cell)
     (hash-set! cells
                (cell-id cell)
                cell))
   (split-picture-to-list)))
}|

这里@var{cells}之前是没有定义的，把它绑定为全局值：

@codeblock|{
(define cells (make-hash))
}|

另外，@racket{for-each}对列表进行遍历，并对列表每个元素用所给函数求值。这里考虑到该函数仅使用一次，就用@racket{lambda}实现了。

其实Racket的所有函数本质上都可以直接用@racket{lambda}定义，如下：

@codeblock|{
;到单元散列表：
(define to-cells
  (lambda (cell)
    (hash-set! cells
               (cell-id cell)
               cell)))
}|

这个和

@codeblock|{
;到单元散列表：
(define (to-cells cell)
  (hash-set! cells
             (cell-id cell)
             cell))
}|

等价。

只是简化了表达方式。从这里可以看出，在Racket里，函数是作为值来定义和传递的。我们甚至可以这样理解：Racket的唯一语法是值和表组成S表达式，然后形成程序。就这么简单！是不是闻到了“道生一，一生二，二生三，三生万物”的香味。
