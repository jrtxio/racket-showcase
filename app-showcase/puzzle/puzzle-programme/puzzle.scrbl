;puzzle.scrbl
;用Racket做一个拼图游戏

#lang scribble/manual

@title[#:tag "puzzle"]{用Racket做一个拼图游戏}
@author{张恒源}

2023-11-26 修订。

@;前言：
@include-section["foreword.scrbl"]
@;设计目标：
@include-section["goal.scrbl"]
@;设计思路：
@include-section["idea.scrbl"]
@;实现工具：
@include-section["implementation-tool.scrbl"]

@;模型板块（操作图片}-----------------------------------------------
@;按每节介绍一个主函数来把整个程序的实现过程展示出来，同时按次顺序由浅入深地介绍Racket语言的知识点，力争在这个小软件的展示中让人学习到Racket 的实战编程知识。

@;取得源图片：
@include-section["model/get-source-picture.scrbl"]
@;设置图片尺寸：
@include-section["model/set-picture-size.scrbl"]
@;将源图片调整到给定的大小：
@include-section["model/adjust-picture-size.scrbl"]
@;按行列分割图片：
@include-section["model/split-picture-by-row-col.scrbl"]
@;生成单元格数据：
@include-section["model/make-cell.scrbl"]
@;分割图片到单元格列表：
@include-section["model/split-picture-to-cell-list.scrbl"]
@;将单元格列表转换为散列表：
@include-section["model/cell-list-to-hash.scrbl"]
@;混合单元格：
@include-section["model/blend-cells.scrbl"]
@;绘制单元格表为图片：
@include-section["model/draw-cells-to-picture.scrbl"]
@;操作id：
@include-section["model/use-id.scrbl"]
@;初始化拼图数据：
@include-section["model/init-puzzle-data.scrbl"]
@;恢复拼图初始状态：
@include-section["model/recover-puzzle.scrbl"]
@;判断是否成功完成拼图：
@include-section["model/success.scrbl"]
@;汇总提供标识：
@include-section["model/provide.scrbl"]

@;视图板块（实现界面布局）--------------------------------------------
@;创建框架窗口：
@include-section["view/create-frame.scrbl"]
@;规划布局：
@include-section["view/design-layout.scrbl"]
@;创建界面主框架：
@include-section["view/create-main-frame.scrbl"]
@;创建布局：
@include-section["view/create-layout.scrbl"]
@;创建左侧布局：
@include-section["view/create-left-layout.scrbl"]
@;创建命令功能区：
@include-section["view/create-command-area.scrbl"]
@;创建行列设置组：
@include-section["view/create-row-col-set-group.scrbl"]
@;创建拼图命令按钮组：
@include-section["view/create-puzzle-command-button-group.scrbl"]
@;创建状态显示区：
@include-section["view/create-status-area.scrbl"]
@;创建右侧布局：
@include-section["view/create-right-layout.scrbl"]

@;控制板块（图形界面交互）--------------------------------------------
@;主视图控制器：
@include-section["controler/main-frame-controler.scrbl"]
@;拼图画布：
@include-section["controler/puzzle-canvas-class.scrbl"]

@;创建可执行程序----------------------------------------------------
@;主程序：
@include-section["puzzle-main.scrbl"]
@;创建可执行程序：
@include-section["create-executable.scrbl"]

@;知识点回顾：-----------------------------------------------------
@include-section["knowledge-points.scrbl"]


