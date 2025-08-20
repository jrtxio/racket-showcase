;draw-cells-to-picture.scrbl
;绘制单元格表为图片。

#lang scribble/manual

@title[#:tag "model/draw-cells-to-picture"]{绘制单元格表为图片}

先看代码，再做解释：

@codeblock|{
;绘制单元格：
(define (draw-cell id dc)
  (let-values ([(row col) (id->r&c id)])
    (let ([bitmap (cell-bitmap
                   (hash-ref cells id))]
          [dest-x (* col (+ width/cell gap))]
          [dest-y (* row (+ height/cell gap))])
      (send dc draw-bitmap bitmap dest-x dest-y))))
}|

为了绘图环境的一致性，所有涉及绘图的函数均将绘图环境对象@racket{dc}作为参数传递。

函数最后一行将单元格表中的取得的单元格图片绘制到绘图环境中。

@section[#:tag "draw-cells-to-picture/draw-context"]{绘图环境}

@racket[dc<%>] 对象是一个用于绘制图形和文本的绘图环境。

它以一种通用的方式表示输出设备，如图片、屏幕、打印机等等。

其成员函数主要有：@racket{clear}清除绘图区域、@racket{copy}复制、@racket{draw-arc}画圆弧、@racket{draw-bitmap}画图片、@racket{draw-bitmap-section}画局部图片、@racket{draw-ellipse}画椭圆、@racket{draw-line}划线段、@racket{draw-lines}画多线、@racket{draw-path}画路径、@racket{draw-polygon}、@racket{draw-rectangle}画矩形、@racket{draw-text}绘制文字、@racket{get-background}取得背景色、@racket{get-brush}取得画笔、@racket{get-pen}取得笔等等，其中带“get”（取得）的函数与换成“set”（设置）的函数对应。

注意，这里dc后边跟的是<%>而不是%，是因为@racket[dc<%>] 是一个接口。接口对于检查一个对象或一个类实现一组具有特定（隐含）行为的方法非常有用。类和接口的概念留待后边讲。

@section[#:tag "draw-cells-to-picture/draw-cells"]{绘制所有单元格}

由于有了上边的@racket{draw-cell}，只需要遍历单元格表就可以完成绘制了。如下：

@codeblock|{
;绘制所有单元格：
(define (draw-cells dc)
  (hash-for-each cells
                 (lambda (id cell)
                   ( draw-cell id dc))))
}|

@section[#:tag "draw-cells-to-picture/set-puzzle-picture!"]{设置拼图图片}

把上边的绘图函数的绘图环境设置成图片绘图环境，就可以绘制出图片了（如果将绘图环境设置成屏幕画布呢？），如下：

@codeblock|{
;设置拼图图片：
(define (set-puzzle-picture!)
  (let* ([pic (make-bitmap width/picture
                           height/picture)]
         [dc (send pic make-dc)])
    (draw-cells dc)
    (set! puzzle-picture pic)))
}|

@section[#:tag "draw-cells-to-picture/recover-puzzle-picture"]{恢复拼图图片}

当需要“重来一次”的时候，只需要恢复单元格表并重新设置拼图图片就可以达到恢复拼图图片的功能了。非常简单！如下：

@codeblock|{
;恢复拼图图片：
(define (recover-puzzle-picture)
  (recover-cells-hash)
  (set-puzzle-picture!))
}|

@section[#:tag "draw-cells-to-picture/reset-puzzle-picture"]{重置拼图图片}

同样的，重置拼图图片也只需要将前面写好的函数拿来组合就可以了。如下：

@codeblock|{
;重置拼图图片：
(define (reset-puzzle-picture)
  ;设置单元格尺寸：
  (set-cell-size!)
  ;分割图片到表：
  (hash-clear! cells);清空单元格列表
  (set! id/current 0);重置当前单元格id号
  (split-picture-to-cells)
  ;混合图片：
  (blend-cells)
  ;初始化拼图图片：
  (set-puzzle-picture!))
}|

不用解释，应该都能明白意思了吧。

@section[#:tag "draw-cells-to-picture/init-puzzle-data"]{初始化拼图数据}

@codeblock|{
;初始化拼图数据
(define (init-puzzle-data)
  ;初始化源图片：
  (let ([in (build-path
             (current-directory)
             "test"
             "test.jpg")])
    (get-source-picture in)
    (adjust-picture-size))
  ;重置拼图：
  (reset-puzzle-picture))
  }|

这里使用了@racket{path}数据类型，并使用@racket{build-path}将各级目录（文件夹）路径组合起来，构建一个@racket[path]值。

另外@racket{current-directory}是系统函数，返回当前的目录路径字串。

以上的内容，主要集中于图片自身的操作，接下来的内容主要用于与用户的互动，这样才能做到在图形界面下让用户操作图形数据。