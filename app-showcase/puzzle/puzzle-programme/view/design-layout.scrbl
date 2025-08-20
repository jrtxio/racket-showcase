;design-layout.scrbl
;规划布局

#lang scribble/manual

@title[#:tag "view/design-layout"]{规划布局}

在实际应用中，不一定采用上边的方式创建GUI程序。为了方便程序的控制，可能会将创建和定义分开，即将控件的实例值标识在程序模块开始的时候集中进行定义（值暂时设置为@racket[void]），在后边分别分级创建空间实例并设置给对应值标识。这样就可以只定义需要执行操作的控件标识（如按钮、画布、字段框等），而不进行操作的控件则不用专门定义标识（如窗格、面板、信息框等），程序逻辑性更强，代码更简洁，也方便按控件之间的逻辑关系进行并列、嵌套创建（后边可以看到）。

但是，这种方式如果遇到需要模块之间对GUI构件进行交叉引用的时候，就不能满足，因为模块之间不能循环需求（@racket[require]）。可以将构件标识符写在一个单独的模块内。由于模块内的值使用@racket[provide]可以实现外部可见，但是外部不能直接修改，因此需要在构件标识符的模块内编写函数来改变标识符的值。如下：

@codeblock|{
;main-frame-ids.rkt
;主框架视图控件标识。

#lang racket

(provide set-main-frame
         main-frame
         set-canvas/target
         canvas/target
         set-canvas/puzzle
         canvas/puzzle
         set-button/select-picture
         button/select-picture
         set-button/blend-cells
         button/blend-cells
         set-button/play-again
         button/play-again
         set-button/exit
         button/exit
         set-text/row
         text/row
         set-text/col
         text/col
         set-choice/r&c
         choice/r&c
         set-message/status
         message/status)

;视图控件标识:
(define main-frame void);主框架
(define canvas/target void);目标图片画布
(define canvas/puzzle void);拼图画布
(define button/select-picture void);选择图片按钮
(define button/blend-cells void);混合拼图按钮
(define button/play-again void);再来一次按钮
(define button/exit void);退出按钮
(define text/row void);行数设置按钮
(define text/col void);列数设置按钮
(define choice/r&c void);行列数选择按钮
(define message/status void);信息状态显示条

;设置构件标识值：
(define (set-main-frame val) (set! main-frame val))
(define (set-canvas/target val) (set! canvas/target val))
(define (set-canvas/puzzle val) (set! canvas/puzzle val))
(define (set-button/select-picture val) (set! button/select-picture val))
(define (set-button/blend-cells val) (set! button/blend-cells val))
(define (set-button/play-again val) (set! button/play-again val))
(define (set-button/exit val) (set! button/exit val))
(define (set-text/row val) (set! text/row val))
(define (set-text/col val) (set! text/col val))
(define (set-choice/r&c val) (set! choice/r&c val))
(define (set-message/status val) (set! message/status val))
}|

这里涉及到的GUI类有：frame%（窗口框架）、horizontal-pane%（水平窗格）、vertical-panel%（竖向面板）、canvas%（画布）、group-box-panel%（组面板）、text-field%（文本字段）、choice%（选择项）、button%（按钮）、message%（信息标签）。

上边讲到，Racket的窗口构件之间是通过并列、嵌套进行组合生成的。以上控件的排版关系如下图：

@image[#:scale 1]{view/layout.png}

图中对嵌套的不同层次进行了颜色填充标记，处于相同层次的的并列关系颜色一致。每个块都表示某个类的一个实例，对应的类和实例标识写在左上角（没有标识的用中文写明实例的意义）。对比上面代码中的视图控件标识，在图中都一一规划出来了。

上边实例对象几乎都是由内置类定义的，除了一个——@racket[puzzle-canvas%]。它是通过@racket[canvas%]派生定义的类，后边专门讲如何创建新的类。

根据上面的规划，接下来按嵌套顺序自顶向下用各类构件来组建GUI界面。还是按之前所述的文件划分，后边视图创建的函数写入视图文件内，文件名为“main-frame.rkt”。
