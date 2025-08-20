;create-main-frame.scrbl
;创建主视图框架

#lang scribble/manual

@title[#:tag "gui/create-main-frame"]{创建主窗口}

根据上边介绍，把之前定义的@var[main-frame]代码进行扩展，如下：

@codeblock|{
;创建界面主框架：
(define (create-main-frame)
  (set! main-frame
        (new frame%
             [label "拼图"]
             [width 1000]
             [height 600]
             [style '(no-resize-border
                      no-system-menu)]))
  (create-layout))
}|

可以看出，创建的框架窗口是采用@racket[set!]设置到标识@var[main-frame]的。

这里和之前创建窗口的代码不同的是增加了窗口的类型属性。@racket[frame%]的@var[style]属性标志可以在某些平台上调整框架的外观，为以下符号值的列表：
@itemlist[
 @item{'no-resize-border：省略窗口周围可调整大小的边框（Windows，Unix）， 调整窗口大小的能力（Mac OS），或右下角的增长框（旧的 Mac OS）。}
 @item{'no-caption：省略框架的标题栏（Windows、Mac OS、Unix）。}
 @item{'no-system-menu：省略系统菜单（Windows）。}
 @item{'hide-menu-bar：当框架处于活动状态时隐藏菜单栏和 dock（Mac OS）， 或者要求窗口管理器使框架全屏（Unix）。}
 @item{'toolbar-button：在框架的标题栏上包括一个工具条按钮(Mac OS 10.6 和更早)；点击工具条按钮会触发对@racket[on-toolbar-button-click] 的调用。}
 @item{'float：导致框架停留在所有其他非浮动窗口的前面（Windows、Mac OS、Unix）； 在 Mac OS 上，浮动框架与活动的非浮动框架共享焦点； 当这个样式与 @var['no-caption] 结合时，显示框架不会导致键盘焦点转移到窗口， 在 Unix 上，点击框架不会移动焦点；在 Windows 上，浮动框架没有任务栏按钮。}
 @item{'metal：忽略(以前支持 Mac OS)。}
 @item{'fullscreen-button：在框架的标题栏上有一个按钮，可以将框架置于全屏模式（Mac OS 10.7 及以后版本）。}
 @item{'fullscreen-aux：允许框架伴随另一个全屏模式的框架（Mac OS 10.7 及更高版本）。}
 ]

这里选择了@var['no-resize-border]、@var['no-system-menu]两个类型，是因为拼图窗口不考虑让客户使用的时候调整大小。

创建函数最后做了一次对函数@racket[create-layout]调用，这很关键，是一个向更下一级创建布局的入口。接下来所有的布局创建都是这样进行一级一级嵌套定义出来的。当然也可以放在一个函数里嵌套定义出来，就会形成上边那张图那样的代码结构，只是可读性就非常差了。
