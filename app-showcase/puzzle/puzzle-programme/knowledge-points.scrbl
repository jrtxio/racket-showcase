;knowledge-points.scrbl
;知识点

#lang scribble/manual

@title[#:tag "knowledge-points"]{知识点}

@section[#:tag "kp/forward"]{前言}
@itemlist[
  @item{介绍Racket的来历。}
 ]

@section[#:tag "kp/goal"]{设计目标}

@section[#:tag "kp/idea"]{设计思路}

@subsection[#:tag "kp/picture-control"]{图片操作功能}
@itemlist[
  @item{标记ID的意义。}
  @item{动画实现原理。}
 ]

@subsection[#:tag "kp/interface"]{界面功能设计}
@itemlist[
  @item{界面布局。}
  @item{界面实现原理。}
 ]

@section[#:tag "kp/implementation-tool"]{实现工具}

@subsection[#:tag "kp/drracket-editor"]{DrRacket编辑器}
@itemlist[
  @item{DrRacket编辑器意义。}
  @item{DrRacket编辑器下载及安装。}
  @item{DrRacket编辑器界面及功能介绍。}
 ]

@subsection[#:tag "kp/racket-introduction"]{Racket简单介绍}
@itemlist[
  @item{Racket编程涉及的概念及内容。}
  @item{“Hello World!”代码的实现。}
  @item{深入学习Racket的途径。}
 ]

@subsection[#:tag "kp/learn-racket"]{快速学习Racket}
@itemlist[
  @item{通过最基本的实例代码展示Racket语法。}
 ]

@section[#:tag "kp/modle-control-picture"]{模型板块（操作图片）}

@subsection[#:tag "kp/modle/get-source-picture"]{取得源图片}
@itemlist[
  @item{Racket中标识的概念及意义；}
  @item{介绍函数参数及使用（剩余参数、可选参数、关键字参数）；}
  @item{路径（path）值的概念及使用；}
  @item{介绍模块、子摸快及测试模块及使用；}
  @item{介绍绑定@racket{define}的使用。}
  @item{@racket{read-bitmap}函数。}
  @item{@racket{set!}的使用。}
 ]

@subsection[#:tag "kp/modle/just-source-picture-size"]{调整源图片尺寸}
@itemlist[
  @item{Racket数据类型介绍；}
   @item{Racket源代码文件的组成结构；}
   @item{运行Racket源代码及测试。}
 ]

@subsection[#:tag "kp/modle/just-display-picture-size"]{调整显示图片尺寸}
@itemlist[
  @item{类对象的创建；}
  @item{类对象的消息传递。}
  @item{三种局部绑定：并行绑定@racket{let}、顺序绑定@racket{let*}和递归绑定@racket{letrec}。}

@item{条件分支：@racket{if}、@racket{cond}、@racket{when}和@racket{unless}。}
 @item{类@racket{bitmap%}的使用；}
 @item{函数@racket{make-object}的使用。}
 ]

@subsection[#:tag "kp/modle/split-picture-by-row-col"]{按行列分割图片}
@itemlist[
  @item{介绍数值函数。}
 ]

@subsection[#:tag "kp/modle/make-cell"]{生成单元格数据}
@itemlist[
  @item{详细介绍结构数据类型。}
 ]

@subsection[#:tag "kp/modle/split-picture-to-cell-list"]{分割图片到单元格列表}
@itemlist[
  @item{详细介绍序对与列表。}
  @item{详细介绍递归与尾递归。}
 ]

@subsection[#:tag "kp/modle/cell-list-to-hash"]{将单元格列表转换为散列表}
@itemlist[
  @item{详细介绍散列表。}
  @item{详细介绍lambda及函数定义的本质。}
 ]

@subsection[#:tag "kp/modle/blend-cells"]{混合单元格}
@itemlist[
 @item{介绍散列表遍历@racket[hash-map]和@racket[hash-for-each]。}
 @item{介绍生成随机数@racket[random]。}
 ]

@subsection[#:tag "kp/modle/draw-cells-to-picture"]{绘制单元格表为图片}
@itemlist[
 @item{介绍绘图环境dc<%>。}
 ]

@subsection[#:tag "kp/modle/use-id"]{操作id}
@itemlist[
 @item{基本的绘图步骤。}
 @item{@racket[pen%]和@racket[brush%]的使用。}
 ]

@subsection[#:tag "kp/modle/init-puzzle-data"]{初始化拼图数据}
@subsection[#:tag "kp/modle/recover-puzzle"]{恢复拼图初始状态}
@subsection[#:tag "kp/modle/success"]{判断是否成功完成拼图}
@subsection[#:tag "kp/modle/provide"]{汇总提供标识}
@itemlist[
 @item{介绍合约（值合约、函数合约、结构合约）。}
 ]

@;-----------------------------------------------------
@section[#:tag "kp/view-gui"]{创建GUI界面}

@subsection[#:tag "kp/view/create-frame"]{创建框架窗口}
@itemlist[
 @item{GUI工具箱内容。}
 @item{用@racket[frame%]创建框架窗口。}
 @item{GUI 构件的常用属性。}
 @item{操纵GUI构件。}
 ]

@subsection[#:tag "kp/view/design-layout"]{规划布局}

@itemlist[
 @item{视图布局的层次结构。}
 @item{用Racket进行有效的视图布局组织。}
 ]

@subsection[#:tag "kp/view/create-main-frame"]{创建主窗口}

@itemlist[
 @item{@racket[frame%]的类型@var[style]及意义。}
 ]

@subsection[#:tag "kp/view/create-layout"]{创建布局}
@itemlist[
 @item{窗格构件和面板构件的使用。}
 ]

@subsection[#:tag "kp/view/create-left-layout"]{创建左侧布局}
@itemlist[
 @item{@racket[canvas%]类介绍。}
 @item{介绍回调函数的概念。}
 ]

@subsection[#:tag "kp/view/create-command-area"]{创建命令功能区}
@itemlist[
 @item{@racket[button%]类介绍。}
 @item{定义按钮回调函数。}
 ]

@subsection[#:tag "kp/view/create-row-col-set-group"]{创建行列设置组框}
@itemlist[
 @item{介绍@racket[group-box-panel%]。}
  @item{介绍@racket[tab-panel%]。}
  @item{介绍@racket[text-field%]。}
  @item{介绍@racket[choice%]。}
 ]

@subsection[#:tag "kp/view/create-puzzle-command-button-group"]{创建拼图命令按钮组}
@itemlist[
 @item{介绍宏。}
 ]

@subsection[#:tag "kp/view/create-status-area"]{创建状态显示区}
@itemlist[
 @item{介绍@racket[message%]。}
 ]

@subsection[#:tag "kp/view/main-frame-controler"]{创建右侧布局}
@itemlist[
 @item{介绍类的创建及使用。}
  @item{设计类的思路及方法。}
 ]

@section[#:tag "kp/controler/create-right-layout"]{主视图控制器}

@subsection[#:tag "kp/controler/show-main-frame"]{显示主界面}

@subsection[#:tag "kp/controler/canvas-target-callback"]{目标画布响应函数}

@subsection[#:tag "kp/controler/button-exit-callback"]{退出按钮响应函数}

@subsection[#:tag "kp/controler/text-row-col-callback"]{text/row控件和text/col控件响应函数}
@itemlist[
 @item{用宏生成函数。}
  @item{显示状态信息的方法。}
  @item{Lisp语言著名的七个原始操作符（七个公理）。}
 ]

@subsection[#:tag "kp/controler/choice-rc-callback"]{choice/r&c行列选择框响应函数}

@subsection[#:tag "kp/controler/button-select-picture-callback"]{button/select-picture选择图片按钮响应函数}
@itemlist[
 @item{@racket[get-file]函数的使用。}
 ]

@subsection[#:tag "kp/controler/button-blend-cells-callback"]{button/blend-cells混合图片按钮响应函数}
@itemlist[
 @item{向控件发送@racket[command]消息实现事件控制。}
  @item{@racket[control-event%]的事件类型的使用。}
 ]

@subsection[#:tag "kp/controler/setted-rc-value"]{判断设置行列数是否成功}
@itemlist[
 @item{写判断函数。}
 ]

@subsection[#:tag "kp/controler/button-play-again"]{button/play-again再来一次按钮响应函数}
@itemlist[
 @item{@racket[refresh-now]方法的使用。}
 ]

@subsection[#:tag "kp/controler/init-main-frame"]{初始化主界面}

@subsection[#:tag "kp/controler/set-target-size"]{设置目标图片画布尺寸}
@itemlist[
 @item{@racket[refresh-now]方法的使用。}
 ]

@section[#:tag "kp/controler/puzzle-canvas-class"]{拼图画布类}
@itemlist[
 @item{类合约的使用。}
 ]

@section[#:tag "kp/puzzle-main"]{主程序}
@itemlist[
 @item{主程序的概念及使用。}
 ]

@section[#:tag "kp/create-executable"]{创建可执行程序}
@itemlist[
 @item{Racket运行程序的方法。}
  @item{Racket创建可执行程序的方法。}
 ]

puzzle拼图游戏源代码及本书源代码见https://gitee.com/onroadzy/puzzle。