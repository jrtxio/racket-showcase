;main-frame-controler.rkt
;主框架视图控制器。

#lang scribble/manual

@title[#:tag "controler/main-frame-controler"]{主框架视图控制器}

主框架视图控制器是之前讲过的MVC程序结构中的“C”部分，将模块文件命名为“main-frame-controler.rkt”。

在这一节里，将进入成果享受型编程模式，一方面是因为主要工作就是为了完善视图（V）部分的相应函数，另一方面是因为基本上都是使用之前模型（M）部分编程的成果来搭建。

@section[#:tag "main-frame-controler/show-main-frame"]{显示主界面}

先看代码。再作解释：

@codeblock|{
;显示主界面：
(define (show-main-frame)
  (send main-frame show #t))
}|

这个函数就一行，发消息给主框架窗口通知其显示出来。之前已经讲清楚了，没啥说的。

接下来一个一个完善控件响应回调函数。

@section[#:tag "main-frame-controler/canvas-target-callback"]{目标画布响应函数}

先看代码。再作解释：

@codeblock|{
;目标画布响应函数：
(define (canvas/target-callback control dc)
  (draw-target-picture dc))
}|

这个函数就一行。用模型部分设计的函数来画出拼图目标图片。之前已经讲清楚了，没啥说的。

@section[#:tag "main-frame-controler/button-exit-callback"]{退出按钮响应函数}

先看代码。再作解释：

@codeblock|{
;退出按钮响应函数：
(define (button/exit-callback control event)
  (send main-frame on-exit))
}|

这个函数就一行，给主框架窗口发送消息，以退出程序。之前已经讲清楚了，没啥说的。

@section[#:tag "main-frame-controler/text-row-col-callback"]{text/row控件和text/col控件响应函数}

先看代码。再作解释：

@codeblock|{
;text/row控件响应函数：
(define (text/row-callback control event)
  (text/r&c-callback/in control "行"))

;text/col控件响应函数：
(define (text/col-callback control event)
  (text/r&c-callback/in control "列"))
}|

这里出现了对@racket[text/r&c-callback/in]的调用，它是一个宏，用来生成处理文本字段控件消息的响应函数。如下：

@codeblock|{
;行列控件响应函数宏模板：
(define-syntax-rule (text/r&c-callback/in control r/c)
  (let [(v (string->number (send control get-value)))]
    (cond
      [(not v)
       (send control set-value "")
       (msg-string
        (format "输入错误！~a数应该是数字。" r/c))]
      [(> v 0)
       (msg-string (format "设置为~a~a。" v r/c))
       (unless (= (send choice/r&c get-selection) 0)
         (send choice/r&c set-selection 0))]
      [else
       (send control set-value "")
       (msg-string
        (format "输入错误！~a必须大于0。" r/c))])))
}|

这里通过判断用户输入@var[v]的值的不同情况，进行有效性判断并给出提示。

这里还是用了几个给控件发消息并调用的不同控件方法：@racket[get-value]方法从文本字段控件取得值；@racket[set-value]方法给文本字段控件设置值；@racket[get-selection]取得选择项的当前项目；@racket[set-selection]设置选择项的当前项目。

也用到了几个内置函数：@racket[string->number]将字符串转换为数字；@racket[format]格式化一个字符串，其中@racket[~a]将每个对应的值转换为一个字符串，这个函数在需要进行格式化输出时非常有用。

另外，这里还用到了一个函数@racket[msg-string]，它是用来实现将字符串输出到信息状态条中。代码如下：

@codeblock|{
;显示子串信息：
(define (msg-string str)
  (send message/status set-label str))
}|

这里用到了@racket[set-label]来给@var[message/status]发消息更改其标签内容，以将给定的字符串信息显示出来。

同样，也可以写一个显示鼠标箭头光标位置的函数。如下：

@codeblock|{
;显示鼠标坐标：
(define (msg-mouse-pos x y)
  (send message/status set-label
        (format "当前光标位置：x=~a，y=~a" x y)))
}|

以上响应函数使用的是@racket[cond]来进行条件分支，如果用过其它编程语言，可能会习惯性地使用@racket[if]来实现它，象下面这样：

@codeblock|{
;行列控件响应函数宏模板：
(define-syntax-rule (text/r&c-callback/in control r/c)
  (let [(v (string->number (send control get-value)))]
    (if v
        (if (> v 0)
            (begin
              (msg-string (format "设置为~a~a。" v r/c))
              (unless (= (send choice/r&c get-selection)
                         0)
                (send choice/r&c set-selection 0)))
            (begin
              (send control set-value "")
              (msg-string
               (format "输入错误！~a必须大于0。" r/c))))
        (begin
          (send control set-value "")
          (msg-string
           (format "输入错误！~a数应该是数字。" r/c))))))
}|

对比之后会发现，采用@racket[cond]更简洁。因此要重视@racket[cond]提供的灵活、简洁的条件分支方式。

其实@racket[cond]也是Lisp语言著名的七个原始操作符（七个公理）之一。这七个原始操作符分别为：quote、atom、eq、car、cdr、cons和cond。这也是神奇的Lisp语言的独特之处。

@section[#:tag "main-frame-controler/choice-rc-callback"]{choice/r&c行列选择框响应函数}

先看代码，再作解释。

@codeblock|{
;choice/r&c行列选择框响应函数：
(define (choice/r&c-callback control event)
    (case (send control get-selection)
      [(1) (set-r&c-text-value 3 3)]
      [(2) (set-r&c-text-value 5 5)]
      [(3) (set-r&c-text-value 8 8)]
      [(4) (set-r&c-text-value 10 10)]
      [(5) (set-r&c-text-value 13 13)]
      [(6) (set-r&c-text-value 15 15)]))
}|

这个响应函数就是根据用户对选择项控件的选择情况来设置文本字段控件的值。对值的匹配是使用@racket[case]来实现的，@racket[case]的完整定义如下：

(case val-expr case-clause ...)

 
case-clause	 	=	[(datum ...) then-body ...+]
 	 	|	 	[else then-body ...+]


对 @var[val-expr]求值并使用其结果来选择一个 @racket[case-clause] 。对于选定的 @racket[case-clause]，最后一个 @racket[then-body] 的结果， 即相对于 @racket[case]表而言处于尾部位置的结果，是整个 @racket[case] 表的结果。以 @racket[else] 开头的 @racket[case-clause] 必须是最后一个 @racket[case-clause]。 

由于需要多次重复操作，为了简单，定义一个函数来完成这个工作：

@codeblock|{
;设置行/列text-field%控件值：
(define (set-r&c-text-value r c)
  ;设置行/列控件值：
  (send text/row set-value (number->string r))
  (send text/col set-value (number->string c))
  (msg-string (format "设置为~a行，~a列。" r c)))
}|

这里边的方法、函数上边已将介绍过，没啥说的。

@section[#:tag "main-frame-controler/button-select-picture-callback"]{button/select-picture选择图片按钮响应函数}

先看代码。后做解释。

@codeblock|{
;button/select-picture选择图片按钮响应函数：
(define (button/select-picture-callback control event)
  ;根据用户输入取得源图片：
  (let* ([extension "*.jp*g;*.png"]
         [filters '(("JPG图片" "*.jpg")
                    ("JPEG图片" "*.jpeg")
                    ("PNG图片" "*.png"))]
         [in (get-file "请选择你需要的图片"
                        main-frame
                        (current-directory)
                        #f
                        extension
                        null
                        filters)])
    (get-source-picture in)
    (adjust-picture-size))
  ;重置拼图：
  (reset-puzzle-picture)
  ;绘制目标图片：
  (draw-target-picture (send canvas/target get-dc))
  ;绘制拼图图片：
  (draw-puzzle-picture (send canvas/puzzle get-dc))
  ;设置拼图画布重玩：
  (send canvas/puzzle replay))
}|

这个响应函数是为了然用户能够自己选择图片来进行拼图。记得之前看过一个故事，说一个男孩为了给女友一个惊喜，制作了一个她的照片的拼图，努力拼呀拼呀拼，终于在在女朋友生日那天拼出了最后一块。这个浪漫通过这个软件就可以自助完成了，拼图自由！

其中最重要的一个函数是@racket[get-file]。它通过特定平台的标准（模式）对话框从用户那里获得一个文件路径名，如果指定了 @var[parent] 作为父窗口， 则使用 @var[message] 作为对话框顶部的信息，如果它不是 #f 。

在函数@racket[get-file]中，@racket[current-directory]给它的@var[directory]参数提供初始文件路径。

接下来就用之前在模型模块里准备好的@racket[get-source-picture]将图片读取到内存中，再用@racket[adjust-picture-size]将其尺寸整理到预订的大小（根据拼图画布来确定尺寸，后边程序初始化的时候会阐述到）。

图片准备好了，就按顺序之行之前在模型模块里写好的函数：@racket[reset-puzzle-picture]、@racket[draw-target-picture]、@racket[draw-puzzle-picture]。

最后，发消息给@var[canvas/puzzle]调用@racket[replay]方法启动游戏。

@section[#:tag "main-frame-controler/button-blend-cells-callback"]{button/blend-cells混合图片按钮响应函数}

先看代码，再作解释。

@codeblock|{
;button/blend-cells混合图片按钮响应函数：
(define (button/blend-cells-callback control event)
  (when (setted-r&c-value?)
    ;重置混合图片：
    (reset-puzzle-picture)
    ;绘制拼图图片：
    (draw-puzzle-picture (send canvas/puzzle get-dc))
    ;设置拼图画布重玩：
    (send canvas/puzzle replay)))
}|

这个回调函数实现将拼图重新打乱混合一次。

首先对行列值是否进行了正确设置进行一个判断，这是前提，函数内容后面讲。

从函数内容来看，就是把上边“选择图片按钮响应函数”后面的工作做一遍。因此，可以把选择图片按钮响应函数在取得图片后，直接向混合图片按钮发送@racket[command]消息模拟点击就可以。如下：

@codeblock|{
;button/select-picture选择图片按钮响应函数：
(define (button/select-picture-callback control event)
  ;根据用户输入取得源图片：
  (let* ([extension "*.jp*g;*.png"]
         [filters '(("JPG图片" "*.jpg")
                    ("JPEG图片" "*.jpeg")
                    ("PNG图片" "*.png"))]
         [in (get-file "请选择你需要的图片"
                        main-frame
                        (current-directory)
                        #f
                        extension
                        null
                        filters)])
    (get-source-picture in)
    (adjust-picture-size))
  (send button/blend-cells
        command
        (new control-event% [even-type 'button])))
}|

当然，也可以将想应代码独立出来作为一个可复用的函数也是可以的，不过没这个方式这样不增加独立函数直接发送消息好，而且更安全。

这里需要为发送的@racket[command]准备一个事件，使用新创键一个@racket[control-event%]类的'button类型事件对象来实现。@racket[control-event%]的事件类型（@var[event-type]）如下：

@itemlist[
@item{'button — 用于 button% 的点击；}
@item{'check-box — 用于 check-box% 的切换；}
@item{'choice — 用于 choice% 项目的选择；}
@item{'list-box — 用于 list-box% 的选择和取消选择；}
@item{'list-box-dclick — 用于 list-box% 的双击；}
@item{'list-box-column — 用于在 column-control-event% 实例中的 list-box% 列的点击；}
@item{'text-field — 用于 text-field% 的变化；}
@item{'text-field-enter — 用于单行 text-field% 的输入事件；}
@item{'menu — 用于 selectable-menu-item<%> 的回调；}
@item{'slider — 用于 slider% 的变化；}
@item{'radio-box — 用于 radio-box% 的选择变化；}
@item{'tab-panel — 用于 tab-panel% 标签的变化；}
@item{'menu-popdown — 用于 popup-menu% 的回调（项目被选中）；}
@item{'menu-popdown-none — 用于 popup-menu% 的回调（没有选择项目）。}
 ]

有了这个，就可以在程序内模拟各种事件了。

如果想设计一个自动测试GUI界面功能的时候，这个@racket[command]方法特别有用。

@section[#:tag "main-frame-controler/setted-rc-value?"]{判断设置行列数是否成功}

虽然行列文本字段控件本身也作了容错检查。但仍可能出现行列文本字段内输入不满足要求而造成错误，因此这里用这个独立函数来做一个检查。实际使用的用户行为是未知的，因此在GUI程序中这种检查行为是非常必要的。

来看代码：

@codeblock|{
;判断设置行列数是否成功，如果不成功返回#f：
(define (setted-r&c-value?)
  (let ([r (string->number (send text/row get-value))]
        [c (string->number (send text/col get-value))])
    (cond
      [(and (and r c) (> r 0) (> c 0))
       (set-rows! r)
       (set-cols! c)
       #t]
      [(and (not r) (not c))
       (msg-string "行数及列数必须是数字。")
       #f]
      [(and (<= r 0) (<= c 0))
       (msg-string "行数及列数必须是大于0的数字。")
       #f]
      [(not r)
       (msg-string "行数必须是数字。")
       #f]
      [(not c)
       (msg-string "列数必须是数字。")
       #f]
      [(<= r 0)
       (msg-string "行数必须是大于0的数字。")
       #f]
      [(<= c 0)
       (msg-string "列数必须是大于0的数字。")
       #f])))
}|

这个判断函数把各种不同的情况都列举了一遍，甚至有些分支永远都不会使用。大家可以分析一下哪个或哪些不是使用。

对于判断函数，一般习惯在标识符末尾带“？”，求值返回#t或#f。

@section[#:tag "main-frame-controler/button-play-again"]{button/play-again再来一次按钮响应函数}

先看代码，再作解释。

@codeblock|{
;button/play-again再来一次按钮响应函数：
(define (button/play-again-callback control event)
  (recover-puzzle-picture)
  ;重新显示混合图片：
  (send canvas/puzzle refresh-now draw-puzzle-picture)
  (send canvas/puzzle replay)
  (msg-string "再来一次！"))
}|

这个响应回调函数把刚才的拼图再来一次。与混合拼图不一样的是，混合拼图要重新混合单元图片，再来一次不用重新混合，直接重新显示出来就可以了。

首先，使用之前编写在模型模块内的@racket[recover-puzzle-picture]函数恢复混合图片，然后重新显示出来。

这里重新显示混合图片采用了向@var[canvas/puzzle]发送@racket[refresh-now]消息完成。

@racket[refresh-now]方法用画布的绘图上下文调用 @racket[draw-puzzle-picture] 来@bold{立即}更新画布（与 @racket[refresh] 相反， @racket[refresh] 只是排队等待更新请求，由窗口系统决定是否处理)。

至此，所有的回调函数定义完善完成。接下来对GUI做初始化等工作。

@section[#:tag "main-frame-controler/init-main-frame"]{初始化主界面}

先看代码，再作解释。

@codeblock|{
;初始化主界面：
(define (init-main-frame)
  ;初始化宽高：
  (set-picture-size! (send canvas/puzzle get-width)
                     (send canvas/puzzle get-height))
  ;初始化行列数：
  (set-rows! (string->number (send text/row get-value)))
  (set-cols! (string->number (send text/col get-value)))
  ;初始化拼图数据：
  (init-puzzle-data)
  ;绘制目标图片：
  (set-target-size);设置目标图片画布尺寸
  (draw-target-picture (send canvas/target get-dc))
  ;绘制拼图图片：
  (draw-puzzle-picture (send canvas/puzzle get-dc))
  ;设置拼图画布重玩：
  (send canvas/puzzle replay))
}|

不用解释了，代码就是解释。

@section[#:tag "main-frame-controler/set-target-size"]{设置目标图片画布尺寸}

直接看代码吧，没什么说的了。

@codeblock|{
;设置目标图片画布尺寸：
(define (set-target-size)
  (let* ([w/p (send canvas/puzzle get-width)]
         [h/p (send canvas/puzzle get-height)]
         [w/t (send canvas/target get-width)]
         [h/t (floor (* h/p (/ w/t w/p)))])
    (send canvas/target min-height h/t)))
}|


好了，为了以上的函数中需要明示的进行@racket[provide]输出，这样就能够让视图模块等其它模块使用了。如下：

@codeblock|{
(provide
 canvas/target-callback
 button/exit-callback
 text/row-callback
 text/col-callback
 choice/r&c-callback
 button/select-picture-callback
 button/blend-cells-callback
 button/play-again-callback
 init-main-frame
 show-main-frame
 msg-mouse-pos
 msg-string
 )
}|

到此，MVC中的三个都介绍完了。运行程序，会发现根本不能移动拼图单元图片，这个，怎么拼图？！

别急，接下来对拼图画布进行扩展，实现交互工作就可以了。