;drracket-editor.scrbl
;DrRacket编辑器

#lang scribble/manual

@title[#:tag "drracket-editor"]{DrRacket编辑器}

为了用Racket进行编程，需要使用Racket自带的DrRacket编辑器（当然任何其它的文本编辑器原则上都可以，看自己喜好）。

Racket可到官方网站（racket-lang.org）下载，安装包里包含有DrRacket编辑器，其界面如下：

@image[#:scale 1]{tool/DrRacket编辑器.png}

界面第一排为菜单，第二排为工具栏，涵盖DrRacket的各项功能。

中间为工作区，分为上下两部分：上部工作区为代码编辑区，用于输入Racket代码；下部工作区为交互，可以实现直接输入Racket代码实现REPL。

界面底部为状态栏，显示当前语言使用状态、当前光标位置、内存收集的相关信息等。

通过在菜单[View]子菜单项内可设置代码编辑区的视图分割、行号显示等，以方便编辑。

Racket支持中文，在Racket中几乎可以任意使用中文，注释、名称、值等等。
