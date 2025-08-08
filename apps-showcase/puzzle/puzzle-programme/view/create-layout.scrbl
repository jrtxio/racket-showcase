;create-layout.scrbl
;创建窗口布局

#lang scribble/manual

@title[#:tag "layout/create-layout"]{创建窗口布局}

先看代码。再作解释：

@codeblock|{
;创建布局：
(define (create-layout)
  (let ([hp (new horizontal-pane%
                 [parent main-frame])])
    (create-left-layout hp)
    (create-right-layout hp)))
}|

上边@racket[create-left-layout]和@racket[create-right-layout]分别创建左右两块布局。左右两块布局是并列的。

@section[#:tag "create-layout/pane-wedget"]{窗格构件}

@racket[horizontal-pane%]是水平窗格类，它可以将其子窗口格式化排列成水平一排。这样左右布局就呈现为左右并排布置的状态，而不是上下。@racket[horizontal-pane%]是由@racket[pane%]派生出来的。

@racket[horizontal-pane%]的属性有：parent、vert-margin、horiz-margin、border、spacing、alignment、min-width、min-height、stretchable-width、stretchable-height。

@var[alignment]设置容器的对齐规范，这决定了当容器有剩余空间时（当一个子构件在某个特定维度上不能伸展时），如何定位其子构件。其值为如下水平对齐及竖直对齐的符号值的组合列表：
@itemlist[
 @item{'left：所有子构件在容器中左对齐，空白被插入到右边。}
 @item{'center：所有子构件在容器中都水平居中。}
 @item{'right：所有子构件在容器中右对齐，剩余的空白将被插入到左边。}
@item{同样地，容器的垂直对齐方式可以是 'top、'center 或 'bottom 。}
]

即：

@codeblock|{
(list/c (or/c 'left 'center 'right)
        (or/c 'top 'center 'bottom))
=	 	'(left center)

}|

@section[#:tag "create-layout/other-pane"]{其它窗格构件及面板构件}

类似地，@racket[vertical-pane%]是竖直窗格类，可以将其子窗口格式化排列成竖直的一列。

而@racket[horizontal-panel%]与@racket[vertical-panel%]分别是水平面板类和竖直面板类，它们和@racket[horizontal-pane%]与@racket[vertical-pane%]对应。它们相同在于既是一个容器，也是一个被包含的窗口，一般作为一个几何管理设备使用；不同在于@racket[pane%]仅作为一个几何管理设备使用，而@racket[panel%]对象可以被隐藏或禁用并可以设置更多属性（如：滚动条等）。

@racket[panel%]的属性比@racket[pane%]多以下属性：enabled、style。其@var[style]为以下符号值的列表：
@itemlist[
 @item{'border：创建的面板会有一个薄的边框（在这种情况下，面板的客户区尺寸可能小于其总尺寸）。}
 @item{'deleted：面板被创建为隐藏的，它不会影响其父级的几何形状；面板可以通过调用父级的@racket[add-child] 方法，在以后变成活动的。}
 @item{'hscroll 或 'vscroll：面板在相应的方向上包括一个滚动条；并且面板在相应方向上的自身尺寸不受其子区域尺寸的限制。}
 @item{'auto-hscroll 或 'auto-vscroll：意味着具有水平和垂直滚动条，但是当不需要在相应的方向上滚动时，它们会让相应的滚动条消失；该模式假定子区域是使用 @racket[panel%]， 对@racket[vertical-panel%] 或 @racket[horizontal-panel%] 是默认放置的。}
 @item{'hide-hscroll 或'hide-vscroll：意味着具有自动滚动条，但相应的滚动条永远不可见（同时仍然允许面板内容超过其自身大小）。}
 ]
