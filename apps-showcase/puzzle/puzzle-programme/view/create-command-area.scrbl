;create-command-area.scrbl
;创建命令功能区

#lang scribble/manual

@title[#:tag "view/create-command-area"]{创建命令功能区}

先看代码。再作解释：

@codeblock|{
;创建命令功能区：
(define (create-command-area p)
  (let ([vp (new vertical-panel%
                 [parent p]
                 [alignment '(center top)])])
    ;创建行列设置组：
    (create-row&col-set-group vp)
    ;创建拼图命令按钮组：
    (create-puzzle-command-button-group vp)
    (set! button/exit
          (new button%
               [label "退出"]
               [parent vp]
               [callback button/exit-callback]))))
}|

这里同样使用了竖向面板。除了利用两个函数建立两个视图区外，最后还定义了一个按钮类@racket[button%]对象。

@section[#:tag "create-command-area/define-button"]{定义按钮构件}

按钮类@racket[button%]对象创建一个带有字符串标签、位图标签或两者的按钮。每当一个按钮被用户点击时，该按钮的回调过程就会被调用。 当每个按钮被创建时，一个回调过程被作为初始化参数提供。

@racket[button%]的属性有：label、parent、callback、style、font、enabled、vert-margin、horiz-margin、min-width、min-height、stretchable-width、stretchable-height。

@var[label]属性既可以是字符串值，也可以是图片类（@racket[bitmap%]）对象实例值，也可以是二者兼有。

@var[style]属性的值是以下符号值的列表：
@itemlist[
 @item{'border：按钮会被画上一个特殊的边框， 向用户表明它是默认的动作按钮。}
 @item{'multi-line：按钮会以一种可以垂直伸展的方式绘制，并适应文本标签中的多行。 目前，这种样式只在 Mac OS 上有区别， 当 @var[label] 是一个包含 @racket[#\newline] 或 @racket[#\return] 的字符串时，它会被自动选择。}
 @item{'deleted：按钮被创建为隐藏的，它不会影响其父级的几何形状；可以通过调用 @var[parent] 的 @racket[add-child]方法，在以后变成活动的。}
 ]

@section[#:tag "create-command-area/exit-callback"]{定义按钮回调函数}

@var[button/exit]按钮实例是为用户在退出游戏时准备的。用户想退出游戏时，点击该按钮，向主框架窗口发送一个退出消息，程序直接退出。这个按钮的回调函数比较简单，这里直接写好，也方便程序调试时可以正常退出程序。如下：

@codeblock|{
;退出按钮响应函数：
(define (button/exit-callback control event)
  (send main-frame on-exit))
}|

对于这种内容很少的回调函数，也可以用匿名回调函数直接将函数体内容放到按钮的定义中去。这样：

@codeblock|{
;创建命令功能区：
(define (create-command-area p)
  (let ([vp (new vertical-panel%
                 [parent p]
                 [alignment '(center top)])])
    ;创建行列设置组：
    (create-row&col-set-group vp)
    ;创建拼图命令按钮组：
    (create-puzzle-command-button-group vp)
    (set! button/exit
          (new button%
               [label "退出"]
               [parent vp]
               [callback
                (lambda (control event)
                  (send main-frame on-exit))]))))
}|

不过考虑到程序功能的扩展，比如以后可能在退出前做一些善后工作，将回调函数单独定义会更好些，可以减少程序重构的麻烦。
