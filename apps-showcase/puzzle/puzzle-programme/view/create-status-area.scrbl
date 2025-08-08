;create-status-area.scrbl
;创建状态显示区。

#lang scribble/manual

@title[#:tag "gui/create-status-area"]{创建状态显示区}

先看代码，再作分析。

@codeblock|{
;创建状态显示区：
(define (create-status-area p)
  (let ([vp (new vertical-panel%
                 [parent p]
                 [alignment '(left top)]
                 [min-height 30]
                 [stretchable-height #f])])
    (set! message/status
          (new message%
               [label "准备就绪。"]
               [parent vp]))))
}|

这里@racket[vertical-panel%]也不是必需的，这里主要是考虑对状态显示区的扩展，预留空间。

信息控件@racket[message%]用来显示基本的状态信息。信息控件是一行静态的文本或一个静态的位图或者一个指定的图标（'app、'caution、'stop）。该文本或位图与信息控件的标签相对应 。它包含的属性有：label、parent、style、font、color、enabled、vert-margin、horiz-margin、min-width、min-height、stretchable-width、stretchable-height、auto-resize。