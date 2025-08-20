;create-row-col-set-group.scrbl
;创建行列设置组

#lang scribble/manual

@title[#:tag "gui/create-row-col-set-group"]{创建行列设置组}

先看代码，后作解释。

@codeblock|{
;创建行列设置组：
(define (create-row&col-set-group p)
  (let ([gp (new group-box-panel%
                 [label "拼图排列"]
                 [parent p]
                 [min-height 30]
                 [stretchable-height #f])])
    (let ([gp/hp (new horizontal-pane%
                      [parent gp])])
      (set! text/row (new text-field%
                          [label "行数"]
                          [parent gp/hp]
                          [init-value "3"]
                          [callback text/row-callback]))
      (set! text/col (new text-field%
                          [label "列数"]
                          [parent gp/hp]
                          [init-value "3"]
                          [callback text/col-callback])))
    (let ([gp/hp-r&c (new horizontal-pane%
                          [parent gp])])
      (set! choice/r&c (new choice%
                            [label "行列"]
                            [parent gp/hp-r&c]
                            [stretchable-width #t]
                            [choices '("自定义行列数"
                                       "3行3列"
                                       "5行5列"
                                       "8行8列"
                                       "10行10列"
                                       "13行13列"
                                       "15行15列")]
                            [callback choice/r&c-callback])))))
}|

@section[#:tag "create-row-col-set-group/group-box-panel"]{组框面板}

这里使用了一种新的面板类——@racket[group-box-panel%]——@bold{组框面板}，它将其子窗口排在单列中，并且在面板的顶部绘制一个可选的标签，并在面板内容周围绘制一个边框，这样就创建了一个标题为 @var[label] 的组窗格，将指定的多个构件框起来形成一个明确的分组。与大多数面板类不同，组框面板的水平和垂直边距默认为 2 （像素）。

@racket[group-box-panel%]的属性有：label、parent、style、font、enabled、vert-margin、horiz-margin、border、spacing、alignment、min-width、min-height、stretchable-width、stretchable-height。

类似的面板还有标签面板@racket[tab-panel%],它将其子窗口安排在一列，并且在面板顶部有一行水平标签。当一个新的标签被选中时, @racket[tab-panel%] 调用一个回调过程来表示用户改变了标签的选择，可以通过编程实现面板的子构件（组）与标签实现对应关系，从而达到操控界面的目的。一个典型的例子就是用标签面板实现多文档界面，比如现在正在使用的DrRacket编辑器的多文档界面就是用这个标签面板类实现的。

@racket[tab-panel%] 的属性有：choices、parent、callback、style、font、enabled、vert-margin 、horiz-margin 、border、spacing、alignment、min-width、min-height、stretchable-width、stretchable-height。

这里@var[choices]是一个字符串列表，存放面板标签行的字符串值。

@var[style]为符号值列表，如下：
@itemlist[
 @item{'no-border：指定面板内容周围就不要边界。}
 @item{'can-reorder：指定用户可以拖动标签来重新排序，在这种情况下， @racket[on-reorder] 被调用； 如果 'no-border 也包含在 @var[style] 中，重新排序总是被启用。}
 @item{'can-close：用户可以点击一个标签的关闭图标，在这种情况下， @racket[on-close-request] 被调用； 如果 'no-border 也包含在 @var[style] 中，关闭总是被启用。}
 @item{'new-button：}
 @item{'flat-portable：如果样式列表中同时还包括 'no-border， 那么标签控件将使用一个独立于平台的实现； 如果 'can-reorder 或 'can-close 包括在 @var[style] 中，在 Windows 平台上， 'flat-portable 标志实际上总是包含在样式中。}
 @item{'deleted：标签面板被创建为隐藏的，它不会影响其父级的几何形状；以后可以通过调用 父级的 @racket[add-child] 方法变成活动的。}
 ]

在这个组框面板里，定义了两个文本字段（包括在一个水平窗格里），一个选择项（也包括在一个水平窗格里。这里是为了和文本字段窗格对应布局代码结构而增加的，否则完全没有必要增加——”如无必要，勿增实体“）。

@section[#:tag "create-row-col-set-group/text-field"]{文本字段}

@racket[text-field%]对象是一个可编辑的文本字段，前面显示一个可选的标签。

有两种文本字段风格：
@itemlist[
@item{单行文本：当用户按下 Return 或 Enter（当文本字段有焦点时）时，会产生一个特殊的控制事件，该事件不被文本字段的控件或对话框处理。}
@item{多行文本：Enter 不被特别处理。}
]

每当用户改变一个文本字段的内容时，它的回调过程被调用。当每个文本字段被创建时，一个回调过程被作为初始化参数提供。

@racket[text-field%]的属性有：label、parent、callback、init-value、style、font、enabled、vert-margin、horiz-margin、min-width、min-height、stretchable-width、stretchable-height。

其中，@var[style]为如下符号值的列表：
@itemlist[
 @item{'single 或 'multiple：样式必须正好包含 'single 或 'multiple 中的一个。前者指定一个单行字段，后者指定一个多行字段。}
 @item{'hscroll：本样式只适用于多行字段。当 'hscroll 被指定时，该字段有一个水平滚动条，自动换行被禁用；否则，该字段没有水平滚动条，自动换行被启用。 一个多行文本字段总是有一个垂直滚动条。}
 @item{'password：该样式表示该字段应该使用一个通用符号而不是实际的字符来绘制其内容的每个字符。}
 @item{'vertical-label 和 'horizontal-label：指定在控件上方或水平创建标签。 如果不包括 'vertical-label （可选择包括 'horizontal-label），标签会在文本字段左侧创建。}
 @item{'deleted：表示创建为隐藏的，它不会影响其父级的几何形状；可以通过调用父级的 @racket[add-child] 方法，在以后变成活动的。}
 ]

@section[#:tag "create-row-col-set-group/choice"]{选择项}

选择项允许用户从一个弹出的项目列表中选择一个字符串项。

每当用户改变一个选择项的选择时，选择项的回调过程就会被调用。 当每个选择项被创建时，一个回调过程被作为初始化参数提供。

@racket[choice%]的属性有：label、choices、parent、callback、style、selection、font、enabled、vert-margin、horiz-margin、min-width、min-height、stretchable-width、stretchable-height。

@var[choices] 列表指定了该控件的用户可选择项目的初始列表。最初的选择集决定了控件的最小图形宽度。