;init-puzzle-data.scrbl
;初始化拼图数据。

#lang scribble/manual

@title[#:tag "model/init-puzzle-data"]{初始化拼图数据}

前面各节将拼图会用到的图形操作的各个方面都设计完了。还需要为用户设置一个默认的初始环境，以避免用户打开软件后面对空白的界面懵圈。

代码如下：

@codeblock|{
;初始化拼图数据
(define (init-puzzle-data)
  ;初始化源图片：
  (let ([in (build-path
             (current-directory)
             "test"
             "test.jpg")])
    (get-source-picture in)
    (adjust-picture-size))
  ;重置拼图：
  (reset-puzzle-picture))
  }|

这里首先用@racket[build-path]来创建一个@racket[path]类型值以供读入图片。接下来的工作就是使用之前编写好的函数做初始化工作了。
