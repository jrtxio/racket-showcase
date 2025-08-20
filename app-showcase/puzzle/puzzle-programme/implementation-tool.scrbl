;implementation-tool.scrbl
;编程实现

#lang scribble/manual

@title[#:tag "tool"]{实现工具}

思路理清楚了，接下来就一个一个功能实现。在阐述实现功能的编程过程中，会延伸讲解编程思路、相关的Racket函数及相关知识点，力图达到在实践中的学习目的。

在编程实现过程中，首先实现图片操作功能，再通过图形界面组装图片操作功能来完成软件的实现。

使用Racket的一大好处就是可以在交互区输出图片，因此不用担心没有图形界面无法进行图片操作的调试的问题，这样就使调试变得非常方便。

接下来简单了解一下。

@include-section["tool/drracket-editor.scrbl"]
@include-section["tool/racket-introduction.scrbl"]
@include-section["tool/learn-racket.scrbl"]

为了让代码结构清晰，采用MVC模式来组织程序。即分成主要的三类文件来分类存储代码：

@itemlist[#:style 'ordered
@item{模型文件（M）：存放数据及操作数据的代码。把它命名为”puzzle-model.rkt“。}
@item{布局文件（V）：存放视图布局相关代码。涉及两个文件，主框架布局文件及其对应的标识符文件，分别命名为”main-frame.rkt“及”main-frame-ids.rkt“。}
@item{控制文件（C）：存放将模型数据与布局对象结合起来操作的代码。包括”main-frame-controler.rkt“及”puzzle-canvas-class.rkt“两个文件。}
 ]

好。接下来来实现它们～