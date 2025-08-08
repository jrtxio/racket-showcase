;create-frame.scrbl
;创建框架窗口：

#lang scribble/manual

@title[#:tag "gui/create-frame"]{定义主视图框架}

之前基本上完成了拼图的数据准备工作，已经可以通过程序对拼图切块并自由拼合。接下来做一些人机交互的界面，以方便客户使用。

Racket提供了专供人机交互编程的@racket[racket/gui]语言工具箱来完成图形界面程序。

@section[#:tag "create-frame/racket-gui"]{racket/gui是一门语言}

且慢，別一看到"语言"就发慌！虽然@racket[racket/gui]的确是一门用于编写图形界面程序的语言，但是不需要像学习一门其它计算机语言那样去学习。其实与其说@racket[racket/gui]是一门语言，不如说它是Racket的一个用于图形编程的代码库。事实上，如果是其它类型的的语言，如C、Java、Python等，像这种情况就是库。但因为Racket是一门可以创造语言的编程语言，它在语言扩展方面有得天独厚的优势，在扩展库的时候，很多时候会使用其创造语言的优势直接创造一门语言来解决问题，这就是所谓领域语言。Racket的语言扩展能力很强，即可使扩展后语法和Racket一致或基本一致，比如接下来要讲到的GUI 库（@racket[racket/gui]语言）；有的甚至可以几乎完全不同，比如现在正在用来编辑排版这个内容的Scribble语言，其语法和著名的排版语言Tex就很像（事实上，它可以编译成Tex的DVI文件，也可编译成PDF文件、HTML文件）。所以，在使用Racket编程的时候不用看到它的一个扩展语言就害怕，自己一不小心就成为语言片段的创造者都是常有的事。

“在编程的时候你可能会想‘Lisp要是有这样或者那样的操作符就好了。’，那你就可以直接去实现它。之后，你会意识到使用新的操作符也可以简化程序中另一部分的设计，如此种种。语言和程序一同演进。就像交战两国的边界一样，语言和程序的界限不断地移动，直到最终沿着山脉和河流确定下来，这也就是你要解决的问题本身的自然边界。最后你的程序看起来就好像语言就是为解决它而设计的。并且当语言和程序彼此都配合得非常完美时，你得到的将是清晰、简短和高效的代码。“

——Paul Graham 《On Lisp》

@section[#:tag "create-frame/racket-gui-toolbox"]{racket/gui工具箱}

@racket{racket/gui} 工具箱大致上分为两部分：

@itemlist[
@item{windowing toolbox， 用于实现窗口、按钮、菜单、文本字段和其他控件。}
@item{editor toolbox， 用于开发传统的文本编辑器，混合文本和图形的编辑器， 或自由形式的布局编辑器（如文字处理器，HTML 编辑器，或基于图标的文件浏览器）。}
]

所有Racket的GUI构件（控件属于构件的一种）组合成了Racket的GUI界面（窗口本身也应理解为构件）。Racket的构件类继承树如下：

@image[#:scale 1]{view/window_class.png}

工具箱的这两部分都广泛地依赖于 @racket{racket/draw} 绘图库。关于@racket{racket/draw} 绘图库，前面的内容已经用到过，后边也会大量使用。

对于@racket{racket/gui} ，在拼图游戏中，仅用到 windowing toolbox 部分的内容，以下正式进入该部分的介绍。

@section[#:tag "create-frame/window"]{窗口}

窗口是GUI程序的一个图形块，它用来容纳其它GUI的部件。窗口工具箱提供了 GUI 程序的基本构件，包括框架（顶级窗口）、模态对话框、菜单、按钮、复选框、文本字段和单选按钮等，所有这些构件都是用类来表达的。

Racket的窗口构件之间是通过并列、嵌套进行组合生成的。而Racket语言的表结构（S表达式）本身就是并列、嵌套的方式组成程序，因此Racket的窗口界面描述与语言本身完全浑然天成。不需要像其它很多语言那样要单独写一个界面描述文件（用XML标记语言等），既不方便也增加了调试难度。

@section[#:tag "create-frame/create-frame"]{创建框架窗口}

Racket描述窗口的类是@racket[frame%]类。一个框架是一个顶级的容器窗口。它有一个标题栏（显示框架的标签），一个可选的菜单栏，以及一个可选的状态栏。

要创建一个新的顶层窗口，只需定义一个@racket[frame%]类的实例即可：

@codeblock|{
;创建界面主框架：
(define main-frame
  (new frame%
       [label "拼图"]
       [width 1000]
       [height 600]))
  }|

可以看到，创建实例的方法很简单，只需要@racket[new]一个类，同时选择需要改变默认初始化值的属性进行初始化即可完成。这里只需要给框架窗口给三个初始化属性：一个标题（标签）、一个宽度值、一个高度值就可以了。当然，创建不同的GUI构件类实例会根据需要给不同属性值。

@section[#:tag "create-frame/properties"]{构件的常用属性}

以下是GUI构件类的一些常用属性：
@itemlist[
 @item{@bold{parent}：指定父级（窗口可以选择#f，则为该程序顶级窗口）；}
 @item{@bold{label}：指定标签显示内容；}
 @item{@bold{x 或 y}：指定了一个框架的初始位置，一般是窗口类才有这个属性；}
 @item{@bold{width 或 height}：指定初始尺寸（像素），假设它大于最小尺寸，否则就使用最小尺寸（min-width、min-height）；}
 @item{@bold{min-width 或 min-height}：指定初始最小尺寸（像素）；}
 @item{@bold{stretchable-width 或 stretchable-height}：设置水平或竖直伸展性，用于几何图形管理；}
 @item{@bold{style}：指定类型，不同构件拥有不同的类型（一般用符号值表示），比如设置类型为'deleted则默认构件定义好后一开始不显示；}
 @item{@bold{enabled}：指定活动状态，@var[#t]或@var[#f]；}
 @item{@bold{border}：指定边框宽度，为整数；}
 @item{@bold{alignment}：指定放置停靠方式；}
 @item{@bold{callback}：指定回调函数；}
 @item{@bold{font}：指定显示标签用的字体；}
 @item{@bold{vert-margin 或 horiz-margin}：指定边距，水平边距会同时加到右边和左边，竖向边距会同时加到上边和下边，用于几何图形管理。}
]

这里的@racket[frame%]属性有：label、parent、width、height、x、y、style、enabled、border、spacing、alignment、min-width、min-height、stretchable-width、stretchable-height。

@section[#:tag "create-frame/control-window"]{操纵GUI构件}

定义好窗口的实例后就可以给它发送消息来操纵它，比如让它显示：

@codeblock|{
(send main-frame show #t)
}|

可以很直观地看到，如果上边的代码的@var[#t]改为@var[#f]就是让它“消失”（hide。不显示，但对象实例值保持）。

Racket内置的类提供了各种机制来处理 GUI 事件。 例如，在实例化 button% 类时，提供一个事件回调程序，当用户点击按钮时，就会被调用。如下：

@codeblock|{
;定义退出按钮：
(define button/exit
  (new button%
       [parent main-frame]
       [label "退出"]
       ;按钮点击的回调函数：
       [callback
        (lambda (button event)
          (send main-frame on-exit))]))
}|
