;use-id.scrbl
;操作id。

#lang scribble/manual

@title[#:tag "model/use-id"]{操作id}

谈到互动操作，往往都绕不过一个话题，就是对象，而每个对象要进行区分，必然需要一个标识，对于有规律的对象，往往采用编号来做标识，这就是id（identification，身份，身份象征等）。

为了更好地识别单元格图片对象，也给每个单元格图片给定一个id，这个id与前面定义单元结构时的@var[id]一致。所有对拼图单元图片的操作均以其id为基础，因此准备一些函数来对其进行操作。

涉及id的操作有：
@itemlist[
 @item{之前已经介绍过的：根据id分割单元格图片@racket[(split-picture-by-id id)]；根据id创建单元格@racket[(make-cell id)]；根据id绘制单元格@racket[(draw-cell id dc)]；根据id交换列表中的两个单元格@racket[(swap-cell id/target id/source)]；将id转换为行列号@racket[(id->r&c id)]；将行列号转换为id值@racket[(r&c->id row col)]。}
  @item{接下来要介绍的：根据坐标位置获取id，根据拖动的坐标位置获取id，在指定坐标位置绘制指定id单元格、在指定id单元格位置绘制指定id单元格内容、绘制id单元格外框、取得当前单元格id、设置当前单元格id、判断指定的id是否有效、绘制焦点单元格、绘制失焦单元格。}
  ]

@section[#:tag "use-id/x-y-to-id"]{根据坐标位置获取id}

之前介绍过，可以通过行列号转换为id。而通过坐标位置求值行列号是比较容易的，只要判断出坐标值在哪一个行列单元格的x、y最大最小值之间就可以了。如下：

@codeblock|{
;根据坐标位置判断行列号(返回row、col)：
(define (x&y->r&c x y)
  (let* ([gh (+ height/cell gap)]
         [gw (+ width/cell gap)]
         [row (quotient y gh)]
         [col (quotient x gw)]
         [max-x/gap (- (* (+ col 1) gw) gap)]
         [min-x/gap (- max-x/gap width/cell)]
         [max-y/gap (- (* (+ row 1) gh) gap)]
         [min-y/gap (- max-y/gap height/cell)])
    (if (and (and (>= x min-x/gap)
                  (< x max-x/gap))
             (and (>= y min-y/gap)
                  (< y max-y/gap)))
        (values row col)
        (values -1 -1))))

;根据坐标位置获取id：
(define (x&y->id x y)
  (let-values ([(r c) (x&y->r&c x y)])
    (r&c->id r c)))
}|

有了这个函数就比较方便了，比如用鼠标将一个拼图单元格的图片拖到另一个单元格里，需要判断拖入的目标单元格的id，这样来实现：

@codeblock|{
;根据拖动的坐标位置获取单元格id:
(define (drag-x&y->id x y)
  (x&y->id x y))
}|

@section[#:tag "use-id/draw-cell-to-xy"]{在指定坐标位置绘制指定id单元格}

首先，按一般操作习惯，这个”指定坐标“应该是单元格中心位置。比如拖动一个单元格图片，鼠标光标指针停放在单元格图片中心，看起来才比较协调。

那么，在绘制该指定单元格时，需要从其左上角作为绘制的起始坐标才可以。由此，代码如下：

@codeblock|{
;在指定坐标位置绘制指定单元格：
(define (draw-cell-to-x&y x y id dc)
  (let ([x0 (- x (/ width/cell 2))]
        [y0 (- y (/ height/cell 2))]
        [cell (hash-ref cells id)])
    (send dc draw-bitmap
          (cell-bitmap cell)
          x0 y0)))
}|

这里使用了@racket[draw-bitmap]方法来绘制单元格图片。

@section[#:tag "use-id/draw-cell-to-target"]{在指定单元格位置绘制指定单元格内容}

这个比较好理解，就是将指定id单元格内容绘制到指定id的目标单元格位置上。需要根据目标单元格id取得其位置信息（左上角坐标）。如下：

@codeblock|{
;在指定单元格位置绘制指定单元格内容：
(define (draw-cell-to-id/target id/t id dc)
  (let-values ([(r/t c/t) (id->r&c id/t)])
    (let ([x0/t (* c/t (+ width/cell gap))]
          [y0/t (* r/t (+ height/cell gap))]
          [cell (hash-ref cells id)])
      (send dc draw-bitmap
            (cell-bitmap cell)
            x0/t y0/t))))
}|

@section[#:tag "use-id/draw-cell-outline"]{绘制单元格外框}

先看代码，再作解释：

@codeblock|{
;绘制单元格外框：
(define (draw-cell-outline id color dc)
  (let-values ([(r c) (id->r&c id)])
    (let ([x (* c (+ width/cell gap))]
          [y (* r (+ height/cell gap))]
          [pen/old (send dc get-pen)]
          [brush/old (send dc get-brush)])
      (send dc set-pen color 2 'solid)
      (send dc set-brush "white" 'transparent)
      (send dc draw-rectangle x y width/cell height/cell)
      (send dc set-pen pen/old)
      (send dc set-brush brush/old))))
}|

这个函数用来在单元格周边画个外框，以使整个拼图各单元格彼此独立，产生马赛克的视觉效果。用到了笔、画刷、矩形绘图方法。整个过程就是@racket[racket/draw]的基本绘图步骤：

@itemlist[#:style 'ordered
@item{保存原有画刷和笔；}
@item{设置笔颜色、宽度及类型；}
@item{设置画刷颜色及透明度;}
@item{调用绘图方法（这里是@racket[draw-rectangle]）绘制图形；}
@item{恢复原有画刷和笔。}
 ]

@subsection[#:tag "draw-cell-outline/pen"]{笔}

@racket[set-pen]用颜色、画笔宽度、类型设置笔对象。用@racket[get-pen]取得笔对象。

@bold{笔}（@racket[pen%]）是一种具有颜色、宽度和样式的绘图工具，可以画图形、写文本以及位图。在单色绘图状态中，所有非白色的笔都被画成黑色。其中

@itemlist[
 @item{颜色是一个代表红-绿-蓝（RGB）三原色组合的对象（color%），加上一个不透明的 "alpha"。也可以直接给预定义的颜色名称。预定义颜色名称由@racket[ color-database<%> ]描述，其全局的对象实例是@var[the-color-database]。}
 @item{笔的宽度范围在0～255之间。}
 @item{笔的类型列表类@racket[pen-list%] 对象维护一个 @racket[pen%] 对象的列表，以避免重复创建@racket[pen%]对象。 笔列表中的一个 @racket[pen%] 对象不能被改变，而一个全局笔列表 @var[the-pen-list] 是自动创建的。}
 @item{除了颜色、宽度和样式之外，笔还可以使用@bold{笔刷}位图。}
]

笔的创建方式为：

@codeblock|{
(new pen%
   	 [[color color]
   	  [width width]
   	  [style style]
   	  [cap cap]
   	  [join join]	 	 	 
   	  [stipple stipple]])
}|

@subsection[#:tag "draw-cell-outline/brush"]{画刷}

@bold{画刷}（@racket[brush%]）是一种具有颜色和样式的绘图工具，用于填充区域。在单色绘图状态中，所有非白色的笔刷都被绘制成黑色。一般用@racket[set-brush]、@racket[get-brush]设置或取得画刷对象。

画刷对象的创建方式为：

@codeblock|{
(new brush%
   	 [[color color]
   	  [style style]
   	  [stipple stipple]
   	  [gradient gradient]
   	  [transformation transformation]])
}|

除了颜色和样式，画刷还可以有@bold{画刷}位图；也可以有 @bold{渐变}， 即 @racket[linear-gradient%]（线型渐变） 或 @racket[radial-gradient%]（射线渐变）；还可以设置变换（transformation）。

这里为了绘制正方形，将画笔设置成了透明样式（'transparent）。

@section[#:tag "use-id/set-get-current-id"]{设置/取得当前单元格id}

先看代码，再作解释：

@codeblock|{
;设置当前单元格id：
(define (set-current-id! id)
  (set! id/current id))

;取得当前单元格id：
(define (get-current-id)
  id/current)
}|

@bold{当前单元格}是指用户当前在操作的单元格。比如用鼠标拖动一个单元格移动，这个被拖动的单元格就是当前单元格。当前单元格是唯一的，而且需要被记住。因此在模块开始的时候定义一个标识用来记录当前单元格的id值：

@codeblock|{
(define id/current 0);当前单元格id
}|

由于模块全局值如果不进行提供设置（provide），在模块外是不可见的，就无法直接使用。因此，定义@racket[set-current-id!]和@racket[get-current-id]来访问它。这是不是感觉有点像其它语言的类访问机制。

@section[#:tag "use-id/has-id"]{判断指定的id是否存在}

由于所有单元格是使用散列表进行存储的，id值就是散列表的键值，因此，判断id是否做存在，就是检查键值是否存在。

@codeblock|{
;指定的id有效：
(define (has-id? id)
  (hash-has-key? cells id))
  }|

从这里可以看出，采用合适的方式组织数据，对简化算法是非常有利的。毕竟，”程序 = 数据 + 算法“。

@section[#:tag "use-id/draw-focus-cell"]{绘制焦点单元格}

把”焦点单元格“理解成”当前单元格“；”失焦单元格“理解成即将失去焦点的”焦点单元格“，一切就顺理成章了——都是操作@var[id/current]单元格。如下：

@codeblock|{
;绘制焦点单元格：
(define (draw-focus-cell dc)
  (draw-cell-outline id/current "red" dc))

;绘制失焦单元格：
(define (draw-lose-focus-cell dc)
  (draw-cell id/current dc))
}|

对焦点单元格，绘制一个红色边框进行标记；对失去焦点的单元格，按通常的状态恢复单元格图片内容。

@section[#:tag "use-id/draw-blank-cell"]{绘制空白单元格}

先看代码，再作解释：

@codeblock|{
;绘制空白单元格：
(define (draw-blank-cell id dc)
  (let-values ([(row col) (id->r&c id)])
    (let ([x (* col (+ width/cell gap))]
          [y (* row (+ height/cell gap))]
          [pen/old (send dc get-pen)])
      (send dc set-pen "white" 0 'solid)
      (send dc draw-rectangle
            x y
            width/cell height/cell)
      (send dc set-pen pen/old))))
}|

这个函数就是之前的绘制单元格图片和绘制焦点单元格的结合，完成了绘制一个白色的正方形（之前画焦点是采用透明色）。
