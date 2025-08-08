;split-picture-by-row-col.scrbl
;按行列分割图片

#lang scribble/manual

@title[#:tag "split-picture-by-row-col"]{按行列分割图片}

这一节，我们把调整好尺寸的图片分割成指定的rows行，columns列，每个行列单元格都存储一张分割后的小图片，需要的时候直接取出来使用即可。

行数和列数很好理解。但如果仅仅考虑分割图片，只需给出行列数去分割图片就可以。但是考虑到行列数需要被记住，后边便于使用。因此这里设置两个值来指向行数值和列数值。把值设置为全局绑定，如下：

@codeblock|{
(define rows 3)
(define cols 3)
}|

这里设置了行列的初始值，为了能够让用户设定行列数，这里需要定义两个函数来实现该功能。如下：

@codeblock|{
;设置单元格行数：
(define (set-rows! num)
  (set! rows num))

;设置单元格列数：
(define (set-cols! num)
  (set! cols num))
           }|

这里函数名@racket{set-rows!}后面有一个”!“，表明它是有副作用的函数。函数里边有对函数@racket{set!}求值，它也是有副作用的。

接下来分割图片。

分割图片用到的函数还是之前用过的@racket{draw-bitmap-section},需要给它传递被分割图片对象，分割的左上角起点，分割的右下角止点。函数定义如下：

@codeblock|{
;根据指定行、列分割图片：
(define (split-picture-by-row&col pic row col)
  (let* ([pic/cell (make-bitmap width/cell height/cell)]
         [dc/pic (send pic/cell make-dc)]
         [xl (* col width/cell)]
         [yt (* row height/cell)]
         [xr (+ xl width/cell)]
         [yb (+ yt height/cell)])
    (send dc/pic draw-bitmap-section
          pic
          0 0
          xl yt xr yb)
    pic/cell))
           }|

如果看过了前面的章节，应该已经了解@racket{define}、@racket{let*}、@racket{send}的使用，同时也知道@racket{make-bitmap}函数和@racket{draw-bitmap-section}的用法。

这里有一个新的函数：@racket{make-dc}，是创建一个绘图环境对象，供画图用。具体怎么来的，留到后边的章节再细讲。

请注意这个函数名@racket{split-picture-by-row&col}，其中用了一个字符“&”，如果你用过其它程序语言，有想这样写函数名却没这样写的举一下手。因为，其它语言不准许呀！能自由表达多好！

但这个自定义函数有一个问题，@var{width/cell}和@var{height/cell}是未定义的。如果这样对这个函数求值就会报错。

由于我们会多次求值以取得单元图片，我们把这两个值作为全局绑定值处理。把如下代码放到模块顶层：

@codeblock|{
(define width/cell 0)
(define height/cell 0)
           }|

这里设置的单元图片宽、高均为0，这里再写一个函数来设置他们的值，在每次重新设置行列数的时候对其求值就可以了。如下：

@codeblock|{
(define (set-cell-size!)
  (set! height/cell
        (floor (/ height/picture rows)))
  (set! width/cell
        (floor (/ width/picture cols))))
           }|

这里还有一个之前没见过的函数：@racket{floor}，它是一个数值函数，用来对@racket{\}这个函数（的确没错，@racket{\}就是一个函数，用来做除法，和数学的除号意义是一样的）求值，返回不超过所给参数值的最大整数（就是数学里的对数值取整）。

数值函数还有很多，如常用的：四则运算函数@racket{+}、@racket{-}、@racket{×}、@racket{\}，大小比较函数@racket{>}、@racket{<}、@racket{<=}、@racket{>=}、@racket{=}，@racket{expt}平方、@racket{sqrt}开方，以及@racket{abs}取绝对值、@racket{max}取最大值、@racket{min}取最小值……等等。
