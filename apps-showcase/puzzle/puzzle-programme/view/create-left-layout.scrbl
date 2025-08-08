;create-left-layout.scrbl
;创建左侧布局

#lang scribble/manual

@title[#:tag "gui/create-left-layout"]{创建左侧布局}

先看代码，再作分析。

@codeblock|{
;创建左侧布局：
(define (create-left-layout p)
  (let ([vp (new vertical-panel%
                 [parent p]
                 [alignment '(center top)]
                 [min-width 300]
                 [stretchable-width #f])])
    ;创建目标图画布：
    (set! canvas/target
          (new canvas%
               [parent vp] 
               [horiz-margin 3]
               [paint-callback
                  canvas/target-callback]))
    ;创建命令功能区：
    (create-command-area vp)
    ;创建状态显示区：
    (create-status-area vp)))
}|

由以上代码可以看出，左侧布局创建了一个竖向面板@racket[vertical-panel%]的实例，并以此为父级创建了三个对象：一个画布实例@var[canvas/target]、一个命令区、一个状态显示区，它们三者是并列关系，形成竖向排列。

咦！上边不是说框架窗口不是默认自带有状态条的嘛，为什么还要单独创建一个状态区呢？这是因为框架窗口自带的状态条比较简单，只能显示信息，如果要实现更复杂的状态栏，然它包含控件这些，则可以如上代码那样单独创建状态栏以实现更多功能。

@section[#:tag "create-left-layout/define-canvas"]{画布构件}

@racket[canvas%]对象是一个通用的窗口，用于绘图和处理事件。

@racket[canvas%]的属性有：parent、style、paint-callback、label、gl-config、enabled、vert-margin、horiz-margin、min-width、min-height、stretchable-width、stretchable-height。

@racket[canvas%]类重点是设置@var[style]属性，其值是一个以下符号值的列表：
@itemlist[
 @item{'border：给予画布一个薄的边界。}
 @item{'control-border：给予画布一个边界，就像一个 @racket[text-field%] 控件一样。}
 @item{'combo：给予画布一个组合按钮，就像一个 @racket[combo-field%] 的控件；这个样式旨在与 'control-border 一起使用， 而不是与 'hscroll 或 'vscroll 一起使用。}
 @item{'vscroll 及 'hscroll：启用水平滚动条或垂直滚动条（最初是可见的，但不活动)，这样可以创建一个具有初始非活动滚动条的画布。 滚动条可以通过 @racket[init-manual-scrollbars] 或 @racket[init-auto-scrollbars] 激活, 它们也可以通过 @racket[show-scrollbars] 隐藏并重新显示出来。。}
 @item{'resize-corner：当只有一个滚动条可见时，在画布的右下方为调整大小的控件留下空间。}
 @item{'gl：创建一个用于 OpenGL 绘图的画布，而不是普通的 @racket[dc<%>] 绘图； 在 @racket[get-dc] 的结果上调用 @racket[get-gl-context] 方法； 这种风格通常与 'no-autoclear 结合使用。}
 @item{'no-autoclear：防止窗口系统自动擦除画布。}
 @item{'transparent：窗口系统通过让其父级显示出来来 “擦除” 画布；如果该标志与 'no-autoclear 相结合，结果将无法确定。}
 @item{'no-focus：防止画布在被点击或调用 @racket[focus] 方法时接受键盘焦点。}
 @item{'deleted：创建的画布最初是隐藏的，并且不影响 @var[parent] 的几何形状； 画布可以在以后通过调用 @var[parent] 的 @racket[add-child] 方法使其激活。}
]

这里还定义了一个回调函数——@var[canvas/target-callback]。

@section[#:tag "create-left-layout/callback"]{回调函数}

@bold{回调函数}的定义是：一个被作为参数传递的函数。从其定义中可以理解到，回调函数的使用可以大大提升编程的效率，因此在现代编程中使用非常多。同时，有一些需求必须要使用回调函数来实现，比如上边@racket[canvas%]的定义；比如我之前写过桶排序的文章，如果将排序函数（如：快速排序）作为回调过程传递，则可以在调用桶排序时自由指定桶内数据的排序方式。

前面讲过，Racket的一个特点是函数是用@racket[define]定义的，其标识实际上是一个值（函数的一个引用值），因此它是可以作为参数进行传递的。在Racket使用回调函数非常自然。

但在C语言中，回调函数只能使用函数指针实现，在C++、Python、ECMAScript等更现代的编程语言中可以使用仿函数或匿名函数实现（这也是其它语言借鉴Lisp语言的一个小小的特性的例子）。

对于Racket的GUI程序来说，回调函数的意义在于，程序员从不直接实现 GUI 的事件循环。窗口系统会自动从一个内部队列中提取每个事件，并将该事件派发到一个适当的窗口。派遣调用窗口的回调函数或调用窗口的一个方法。如果一个窗口接收多种事件，这些事件会被派发到窗口类的方法中，而不是派发到回调过程中。

@var[canvas/target-callback]回调函数单纯是为了绘图，即把拼图完成的图片显示在上边，以便用户在拼图的过程中得到参照提示的帮助。它有一个很重要的参数——@racket[paint-callback]，由默认的 @racket[on-paint] 方法调用并使用画布和 @racket[get-dc] 返回的DC作为参数。这样就可以通过这个回调函数实现将内容绘制到画布上。为了不饶乱当前工作的思路，这个回调函数在后边来实现，但这样IDE会报错，先来个缓兵之计。

先定义一个@bold{空函数}——即”什么也不做的函数“，这样既可以满足程序调用，使视图界面的调试工作顺利进行，也不会把精力花在编写这个函数而耽误了工作主线。如下这样定义这个”空函数“：

@codeblock|{
(define (canvas/target-callback control dc)
  void)
}|

后边把主线工作完成后，回过头来编写好回调函数本该有的内容并替换掉@racket[void]就可以了。
