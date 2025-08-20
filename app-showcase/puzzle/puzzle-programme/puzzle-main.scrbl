;puzzle-main.scrbl
;主程序

#lang scribble/manual

@title[#:tag "puzzle-main"]{创建主程序}

最后来看一下游戏的主程序。

@codeblock|{
;puzzle.rkt
;拼图主程序。

#lang racket

(require "main-frame.rkt"
         "main-frame-controler.rkt")

(create-main-frame)
(show-main-frame)
(init-main-frame)
}|

@section[#:tag "main-programe"]{主程序}

@bold{主程序}是指一个计算机程序中的核心代码，通常是整个程序的最高层次，换句话说就是主函数或入口函数。进程启动后首先会运行主程序，在主程序中做一些初始化操作等，然后再根据需要调用其它模块、函数或子程序。

主程序作为整个程序的核心，承担着对整个程序的掌控和调度任务。主程序的具体功能主要包括：

@itemlist[
@item{进行系统初始化，例如读取配置文件、创建必要的对象或数据结构等；}
@item{处理命令行参数或用户输入，决定程序执行流程；}
@item{调用其它模块或库函数，完成具体的业务逻辑；}
@item{进行错误处理、异常处理等操作，确保程序正确稳定地运行；}
@item{最终将计算结果输出或保存至文件等。}
]

@section[#:tag "puzzle-main-programe"]{拼图主程序}

其实Racket语言并不要求一定有主程序的概念，比如以上代码内容放到视图控制模块里完全是可以的。

这里拼图游戏之所以使用了主程序模块，主要是考虑到这样可以使整个程序文件结构化程度更高，各模块逻辑性更强。

在拼图主程序里，完成了如下工作：
@itemlist[#:style 'ordered
@item{第一部分，文件注释。
  @itemlist[#:style 'ordered
 @item{第一行，注释，指明文件名称。文件名称与程序名称一致，这样在后边进行可执行程序编译的时候就可以自动生成与程序名称一致的可执行行文件名称，而不需要单独给一个参数进行指定。}

 @item{第二行，注释，对文件内容进行说明。}
 ]}

@item{第二部分，语言类型说明。之前有讲Racket是能够创造语言的语言，在模块起始位置指定使用的语言是必需的。}
@item{第三部分，指定需求模块。}
@item{第四部分，模块函数调用。调用了以下三个函数：
 @itemlist[
@item{@racket[create-main-frame]：创建程序主框架窗口，这样就有了一个与客户交互的界面。}
@item{@racket[show-main-frame]：显示上边创建的主框架窗口。}
@item{@racket[init-main-frame]：视图控制模块的函数对程序主界面的视图进行初始化，其实也是对游戏进行初始化。}
 ]}
 ]

这样，程序就可以跑起来了。

用鼠标光标点击DrRacket右上角工具条的Run按钮，程序就会运行起来了。

当然，如果希望游戏有个启动画面，也可以在这个模块里调用。其它的包括程序环境设置等等，都可以在这里完成调用。