;create-executable.scrbl
;创建可执行程序

#lang scribble/manual

@title[#:tag "create-executable"]{创建可执行程序}

主程序完成，接下来就可以感受游戏带来的乐趣了。

@section[#:tag "run-programme"]{运行程序}

最简单的运行程序的方式是在DrRacket中打开“puzzle.rkt”主程序模块并点击“Run”按钮，DrRacket将自动执行程序的编译及运行。不过，每次运行程序都需要先打开DrRacket再打开主程序模块再运行实在是太麻烦了。这种方式适合调试程序，调试完成后就可以脱离DrRacket使用了。

脱离DrRacket环境运行程序的方式是在当前主程序模块文件夹路径下用命令行来实现：

@codeblock|{
 racket puzzle.rkt
}|

@section[#:tag "create-programme"]{创建可执行程序}

一个独立的可执行程序是 "独立的"，因为可以在不启动 racket、 gracket 或 DrRacket 的情况下运行它。

使用以下方式实现：

@itemlist[
@item{在DrRacket里，点击“Racket”菜单，选择“Create Executable…”菜单项，设置好弹出的对话框信息：可执行程序名称（含路径）、可执行程序类型、程序的运行基础、程序图标等，点击对话框的命令按钮“Create”，DrRacket即可自动生成一个可执行程序。需要稍许等带一会儿。}
 @item{如果想在命令行里生成可执行程序，则使用raco exe 命令完成，如下：
 @codeblock|{
raco exe puzzle.rkt
  }|}
 ]

以上 raco exe 命令接受的部分命令行标志如下：

@itemlist[
@item{@bold{-o ‹file›} ：将可执行文件创建为 ‹file›， 根据平台和可执行文件类型在 ‹file› 上添加一个合适的后缀。}
@item{@bold{--gui}：创建一个基于 gracket 而不是 racket 的图形化可执行文件。}
@item{@bold{-l} 或 @bold{--launcher}：创建一个安装专用启动器， 而不是一个独立的可执行文件。 诸如 --config-path、 --collects-path 和 --lib 等标志对启动器没有影响。}
@item{@bold{--embed-dlls}： 在 Windows 下，对于一个独立的可执行文件，将任何需要的 DLL 复制到可执行文件中。 嵌入 DLL 使产生的可执行文件真正成为独立的，如果它不依赖于其他外部文件。 并非所有的 DLL 都可以嵌入,限制主要与线程本地存储和资源有关, 但 Racket 主发行版中的所有 DLL 都可以使用 --embed-dlls 。}
@item{@bold{--config-path ‹path›}：将可执行文件中的 ‹path› 设置为 configuration directory 的路径；如果路径是相对的，它将被视为与可执行文件相对的。 默认路径是 "etc"， 希望在运行时没有这样的目录存在。}
@item{@bold{--collects-dest ‹path›}：将模块与可执行文件一起写入 ‹path› (相对于当前目录)，而不是嵌入可执行文件中。 --collects-dest 标志通常只在与 --collects-path 相结合时才有意义。 这种模式目前不对未引用的子模块进行修剪(并且会拉动子模块的任何依赖关系)。}
@item{@bold{--ico ‹.ico-path›}：在Windows下， 将生成的可执行文件的图标设置为从 ‹.ico-path› 中提取的图标。}
@item{@bold{--icns ‹.icns-path›}：在 Mac OS 上， 将生成的可执行文件的图标设置为 ‹.icns-path› 的内容。}
@item{@bold{--orig-exe}： 在 Unix 上，生成一个基于原始 racket 或 gracket 可执行文件的可执行文件， 而不是一个重定向到原始可执行文件的包装器。 如果原始可执行文件是静态链接到 Racket 运行库的，那么生成的可执行文件同样是独立的。 但要注意的是，如果原始可执行文件以共享库的形式链接到 Racket， 那么 raco distribute 就不能处理以 --orig-exe 创建的可执行文件 (因为当可执行文件被分发到不同机器上时，可执行文件包通常会负责寻找共享库)。}
@item{@bold{--cs}：生成一个基于 Racket 的 CS 实现的可执行文件，这是默认的， 除非运行一个基于 BC 实现的 raco exe 。}
@item{@bold{-v}：报告进展情况，以节略方式进行。}
@item{@bold{--vv}：比 -v 更粗略地报告进度。}
 ]

关于创建可执行程序及其安装方面的内容，可以去看官方的指南和语言参考。会感觉Racket语言真的非常方便，不用再去使用各种工具来完成不同的事情。

当实现了可执行程序的编译和打包后，就可以分发puzzle游戏版本了。