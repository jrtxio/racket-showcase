;puzzle-canvas-class.scrbl
;拼图画布类

#lang scribble/manual

@title[#:tag "controler/puzzle-canvas-class"]{拼图画布类}

拼图画布类（puzzle-canvas%）在之前做过简单介绍，也创建出基本框架，接下来对其进行扩展。

@section[#:tag "puzzle-canvas-class/design-canvas-class"]{进一步规划拼图画布类}

按之前描述的类设计的思路，分析一下拼图画布类要做的工作。

拼图程序的重头戏是给给用户的，所有之前完成的编程工作都是为了让用户能够简单地用一个鼠标，通过拖动图片单元格将图片复原。因此最重要的事情是处理鼠标事件。归纳一下，鼠标事件应该处理以下事情：

@itemlist[
@item{鼠标在拼图上移动，每移动到一个单元格上或移出一个单元格，画布上应该有一个视觉提示；}
 @item{鼠标拖着一个单元格图片到另一个单元格，在过程中应该显示拖出的单元格内的图片没了，图片跟着鼠标拖着走；}
 @item{鼠标拖着单元格图片到了另一个单元格上放下，程序自动完成图片调换的状态。}
 @item{用户不断重复，直到拼图完成为止。}
 ]

这里有两个问题：
@itemlist[#:style 'ordered
 @item{拼图完成了，怎么结束呢？

需要给类一个标志来指明拼图完成了，如果这个标志为#t，标识拼图完成，程序不再响应拖动单元格图片。}

@item{同样是鼠标划过图片，怎么区分是否处于拖动状态呢？

同样设置一个标志来完成，如果这个标志为#t，标识在拖动图片。}
]

根据以上分析，就可以基本确定拼图画布类的内容了：应该有2个字段值，把它们表示为@var[dragging]（表示拖动状态）和@var[over]（表示游戏结束）。把处理鼠标事件的3种行为整理成类方法，这些方法只给鼠标事件用，因此采用@racket[private]类型。

@section[#:tag "puzzle-canvas-class/field"]{字段}

由上所述，定义类字段如下：

@codeblock|{
;类字段：
(field (dragging #f);单元格拖动状态
       (over #f));游戏结束标志
}|

这里定义的字段放在@racket[(super-new)]的前面或后面都是可以的。

@section[#:tag "puzzle-canvas-class/method"]{方法}

同样如上所述，定义鼠标事件处理方法如下：

@codeblock|{
;覆盖定义鼠标事件方法：
(define/override (on-event event)
  ;如果游戏结束，不再处理鼠标事件：
  (unless over
        (let ([dc (get-dc)]
          [x (send event get-x)]
          [y (send event get-y)])
      (msg-mouse-pos x y);显示鼠标位置信息。
      ;根据事件情况进行动态显示：
      (cond
        ;移动但不拖拽：
        [(and (send event moving?)
              (not (send event dragging?)))
         (flag-cell-and-set-current x y dc)]
        ;拖拽并移动：
        [(and (send event moving?)
              (send event dragging?))
         (move-cell-and-simulate-swap x y dc)]
        ;释放鼠标左键：
        [(and (send event button-up? 'left)
              dragging)
         (swap-cell-current-to-target x y dc)]
        [else void]))))
}|

这里顺便用视图控制模块定义的@racket[msg-mouse-pos]把鼠标的即时位置显示在状态信息框里。

鼠标事件是通过@racket[cond]来进行根据判断分支处理的，这也是处理事件的常用操作，在GUI程序里非常常用。分支的判断列举了4中情况：

@itemlist[#:style 'ordered
@item{鼠标移动但不拖动；}
@item{鼠标移动且拖动；}
@item{释放鼠标左键;}
@item{额外情况。}
 ]

以上列举和前面的分析是一一对应的，多出了一个额外情况。

@racket[cond]的额外情况（@racket[else]）是为了补充列举情况之外的所有情况，使得@racket[cond]对问题的描述进行全覆盖。这里用的是@var[void]（什么也不做），因此这个额外情况是可以不要的，只是为了完整性保留了下来。

接下来来看三个事件处理过程。

@section[#:tag "puzzle-canvas-class/mouse-move-not-drag"]{鼠标移动但不拖拽}

@codeblock|{
;鼠标移动但不拖拽，
;对鼠标位置单元格进行标记并记录为当前单元格：
(define/private (flag-cell-and-set-current x y dc)
  (let ([id (x&y->id x y)])
    (unless (= id (get-current-id))
      (draw-puzzle-picture dc)
      (define id/old (get-current-id))
      (draw-lose-focus-cell dc)
      (when (has-id? id)
        (set-current-id! id)
        (draw-focus-cell dc)))))
}|

使用的函数都是拼图模型模块里的。没啥解释的。

@section[#:tag "puzzle-canvas-class/mouse-move-and-drag"]{鼠标移动并拖拽}

@codeblock|{
;拖拽并移动，
;移动当前单元格，在进入其它单元格时模拟调换场景：
(define/private (move-cell-and-simulate-swap x y dc)
  (let ([id/target (drag-x&y->id x y)]
        [id (get-current-id)])
    (cond
      [(has-id? id/target)
       ;绘制两个单元格调换状态：
       (draw-puzzle-picture dc)
       (draw-blank-cell id/target dc)
       (draw-cell-to-id/target id id/target dc)
       (draw-cell-to-x&y x y id dc)]
      [else
       ;绘制当前单元格拖动状态：
       (draw-puzzle-picture dc)
       (draw-blank-cell id dc)
       (draw-cell-to-x&y x y id dc)])
    ;设置单元格拖动状态为#t：
    (unless dragging
      (set! dragging #t))))
}|

使用的函数都是拼图模型模块里的。没啥解释的。

@section[#:tag "puzzle-canvas-class/release-mouse-left-key"]{释放鼠标左键}

@codeblock|{
;释放鼠标左键，
;将当前单元格与目标单元格调换：
(define/private (swap-cell-current-to-target x y dc)
  (let ([id/target (drag-x&y->id x y)])
    (when (has-id? id/target)
      (let ([id (get-current-id)])
        ;将两个单元格调换：
        (swap-cell id/target id)
        (set-puzzle-picture!)
        (draw-puzzle-picture dc)
        ;检查是否成功拼合图片：
        (when (success?)
          (msg-string "恭喜！成功完成拼图！")
          (set! over #t))))
    ;设置单元格拖动状态为#f：
    (when dragging
      (set! dragging #f))))
}|

使用的函数都是拼图模型模块里的，没啥解释的。要注意的是，这里每次完成单元格调换都要检查一次是否已经完成拼图并给出提示及设置结束标志。

通过以上的两个字段及三个处理过程，完成了拼图画布的图形交互工作。

这个类作为独立的模块文件进行编辑。和其它模块同样，为了视图模块能够使用完成的这个类，需要进行提供设置。

@section[#:tag "puzzle-canvas-class/provide-class"]{类提供设置}

完成类编程后，同样需要进行提供设置，以便提供给其它模块需求使用（这里是提供给视图模块使用）。

首先需要了解的是——类是值，它们可以跨越合约边界。使用 @racket[class/c] 表构建类合约，可以用合约保护给定类的一部分。

以下是类合约构建的完整方式：

@codeblock|{
(class/c maybe-opaque member-spec ...)

 
maybe-opaque	 	=	 	
 	 	|	 	#:opaque
 	 	|	 	#:opaque #:ignore-local-member-names
 	 	 	 	 
member-spec	 	=	method-spec
 	 	|	 	(field field-spec ...)
 	 	|	 	(init field-spec ...)
 	 	|	 	(init-field field-spec ...)
 	 	|	 	(inherit method-spec ...)
 	 	|	 	(inherit-field field-spec ...)
 	 	|	 	(super method-spec ...)
 	 	|	 	(inner method-spec ...)
 	 	|	 	(override method-spec ...)
 	 	|	 	(augment method-spec ...)
 	 	|	 	(augride method-spec ...)
 	 	|	 	(absent absent-spec ...)
 	 	 	 	 
method-spec	 	=	method-id
 	 	|	 	(method-id method-contract-expr)
 	 	 	 	 
field-spec	 	=	field-id
 	 	|	 	(field-id contract-expr)
 	 	 	 	 
absent-spec	 	=	method-id
 	 	|	 	(field field-id ...)
}|

在 @racket[class/c] 表中列出的合约有两大类：外部合约和内部合约。
@itemlist[
@item{当一个对象从一个类中实例化时，或者当方法或字段通过该类的一个对象被访问时，外部合约管理行为。}
@item{当方法或字段在类的层次结构中被访问时，内部合约管理行为。}
 ]
这种分离允许为类客户提供更强的合约，为子类提供更弱的合约。

方法合约必须包含一个额外的初始参数，它与方法的隐式 @var[this] 参数相对应。这允许在方法被调用时讨论对象的状态的合约。替代的合约表，（例如 @racket[->m]）， 被提供作为编写方法合约的速记。

在 @racket[absent] 子句中列出的方法和字段必须 @bold{不} 存在于类中。

根据以上描述的方法，为@racket[puzzle-canvas%]加上合约：

@codeblock|{
;拼图画布类：
(define/contract puzzle-canvas%
  ;puzzle-canvas%类的合约：
  (class/c
   (inherit [get-dc
             (->m (or/c (is-a?/c dc<%>) #f))]
            [get-x
             (->m real?)]
            [get-y
             (->m real?)])
   (override [on-event
              (->m (is-a?/c mouse-event%) void?)]))
  (class canvas%
    (inherit get-dc
             get-x
             get-y)
    
    (super-new)
    ;类成员函数，此处省略。
    ))
}|

构造好了合约，直接用@racket[(provide puzzle-canvas%)]设置提供就可以了。当然，不构造合约也是可以这样设置提供的，只是给类提供一个保护更好。

至此，拼图游戏编写完成，希望对大家有所帮助。代码很多地方还有可优化的地方，大家可以评论留言讨论。