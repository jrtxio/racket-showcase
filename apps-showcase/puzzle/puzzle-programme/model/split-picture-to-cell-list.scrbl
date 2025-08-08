;split-picture-to-cell-list.scrbl
;分割图片到单元格列表

#lang scribble/manual

@title[#:tag "split-picture-to-cell-list"]{分割图片到单元格列表}

接下来我们把整个图片切割成单元格图片，并组成一个列表。

@section[#:tag "pair-and-list"]{序对与列表}

一个列表是一个序对的组合，它创建一个链表。更确切地说，一个列表要么是空列表@var{null}，要么是个序对。是序对时，其第一个值是一个列表元素，第二个值是一个列表。

一个序对是两个任意值的有序组合。

它用@racket{cons}构建， @racket{car} 和 @racket{cdr}分别提取序对的第一和第二个值。@racket{pair?} 判断识别序对。如以下用例：

@codeblock|{
>(cons 1 2)
 '(1 . 2)
>(car (cons 1 2))
 1
>(cdr (cons 1 2))
 2
>(pair? (cons 1 2))
 #t
>(cons 1 (cons 2 null))
 '(1 2)
 }|

请注意，最后一行的显示形式和第一行的不一样（没有中间的点），因为这个序对已经是一个列表了。一个列表通常打印为一个@racket{'}后跟一对括号包裹列表元素。

之前的单元格字段也是可以表示为一个序对的。想想怎么实现？

列表的相关函数有：@racket{null}空列表、@racket{list}返回一个分配好的列表、@racket{list?}列表判断、@racket{build-list}创建列表、@racket{length}返回列表长度、@racket{list-ref}返回列表指定元素、@racket{append}组合列表、@racket{reverse}将列表反序等等。当然，既然列表也是序对，所有序对能用的函数列表也可以用。另外列表还有一些迭代函数：@racket{map}、@racket{andmap}、@racket{ormap}、@racket{for-each}等，列表过滤函数：@racket{filter}、@racket{remove}、@racket{sort}，列表搜索函数：@racket{member}、@racket{findf}等等。

@section[#:tag "split-cell-to-list"]{分割图片到列表}

思路是首先定义一个空列表，然后按行列顺序依次切割单元格图片并创建单元格结构值，再把创建的结构值添加到已有列表中（注意列表内顺序要保持不变），最终完成一个包含单元格结构值的列表。整个过程用递归来实现。如下：

@codeblock|{
;分割图片到列表：
(define (split-picture-to-list)
  (define (split/in l r c)
    (if (>= r rows)
        l
        (if (>= c cols)
            (split/in l (+ r 1) 0)
            (split/in
             (append l
                     (list
                      (make-cell
                       (r&c->id r c))))
             r (+ c 1)))))
  (split/in null 0 0))
           }|

上边为了确保列表内元素顺序不变而先把新的单元结构值构建成列表，再用@racket{append}函数把它添加到已有列表中。

这里用到了尾递归，而且进行了嵌套。

而递归函数又是嵌套在主函数内部的局部函数，这就是传说中的闭包。这里的闭包函数也是可以拆分出来的，像这样：

@codeblock|{
;分割图片到列表：
(define (split-picture-to-list)
  (split/in null 0 0))
  
;分割图片到列表的递归函数：
(define (split/in l r c)
    (if (>= r rows)
        l
        (if (>= c cols)
            (split/in l (+ r 1) 0)
            (split/in
             (append l
                     (list
                      (make-cell
                       (r&c->id r c))))
             r (+ c 1)))))
}|

拆分之后就可以看出来了，分割图片到列表函数只是对这个递归函数做了一个初始化并求值，重点内容在递归函数里。

而这个递归函数还可以拆分成两个递归函数，分別求值行和列，如下：

@codeblock|{
;分割图片到列表的递归函数：
(define (split/in l r c)
  (split/in-row l r c))
;递归求值行：
(define (split/in-row l r c)
  (if (>= r rows)
      l
      (split/in-col l r c)))
;递归求值列：
(define (split/in-col l r c)
  (if (>= c cols)
      (split/in-row l (+ r 1) 0)
      (split/in-col
       (append l
               (list
                (make-cell
                 (r&c->id r c))))
       r (+ c 1))))
}|

@section[#:tag "recursion-and-tail-recursion"]{递归和尾递归}

在Lisp语言里，递归的概念无处不在，这也是Lisp语言的特色了。Lisp采用递归来完成的事情其它语言用迭代大多能完成，不过采用递归更灵活，可以完成的事情更多更复杂。因为，迭代只是递归的一个特例。

之前就有读者在后台问什么是尾递归？这得从递归的特点讲起。

递归就是函数在满足条件的时候改变参数重复对自身求值，直到不满足条件时返回不满足条件的结果值。

普通递归函数在达到不满足条件返回结果值后，如果前一级函数求值需要这个返回结果（之前求值过程处于等待状态），得到结果后前一级继续完成剩下的求值，然后把求值结果返回上一级，……，直到各级函数求值完成，返回满足条件的结果值。这样导致的结果就是在不满足条件的情况出现之前，每一级的求值都没有完成，内存随着函数调用级数累积越多耗费就越大（甚至可能耗尽）。而大多数语言的递归是用堆栈实现的，就会因累积导致堆栈溢出，产生风险。

而尾递归采用的方式则是每一级将求值完成，将结果值作为参数传递到下一级函数，没有等待求值的内容占据空间，这样在遇到不满足条件时，返回的结果就是最终结果值。既不需要返回各级去继续计算，也不用担心累积使空间占用的多少，程序始终在恒定的空间占用下进行，取得求值速度和空间占用的双优。

之前有人说用递归求值比迭代慢，采用尾递归是不存在的。

把下边这个这个普通递归函数改为尾递归函数，看一下有什么不同，如下：

@codeblock|{
;普通递归：
(define (my-length lst)
  (cond
    [(empty? lst) 0]
    [else (+ 1 (my-length (rest lst)))]))
   
;尾递归：(函数调用时result初始化给值：0）
(define (my-length result lst)
  (cond
    [(empty? lst) result]
    [else (my-length (+ 1 result) (rest lst))]))
}|

看到区别了吧，普通递归在满足条件返回值后求值才开始；而尾递归当时就把求值完成了，在满足条件时直接返回结果。

那怎么写尾递归函数呢？这样：

@itemlist[#:style 'ordered
@item{在尾递归函数中增加结果值参数标识（普通递归函数不需要这个参数）；}
@item{在满足条件返回的判断分支返回结果值（普通递归函数在这里是初始化结果值）；}
@item{在递归计算分支将计算值作为结果值参数传递给后一级递归求值（普通递归函数将下一级递归函数作为计算分支求值的一个未知值对待）。}
]

尾递归的核心思想是避免所有因为等待下一级递归调用的返回值而不能及时完成的计算。反之，实现了这个思想的递归即是尾递归。

由于尾递归函数没有初始化，需要对其进行一次额外调用，以便给结果值参数传递一个初始值（普通递归函数不需要这一步）。因此一般把尾递归定义函数放在求值函数内部作为闭包，同时在求值函数内对递归函数做一次求值（同时对尾递归结果值参数进行初始化）。
