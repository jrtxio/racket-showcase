;racket-introduction.scrbl
;Racket简单介绍

#lang scribble/manual

@title[#:tag "racket-introduction"]{Racket简单介绍}

Racket编程涉及以下概念及内容：值、定义、表达式、标识、绑定、函数、条件分支、关键字、序对、列表、向量、散列表、集合、结构、迭代、递归、模块、合约、类、对象、宏、输入、输出、正则表达式、模式匹配、反射、动态求值、并发、并行等。以上这些概念有的是多数语言的共性，有些是Racket语言的特色。

为了快速对Racket语言有一个基本的了解，可以看《X分钟了解Racket》这一篇微信公众号（Racket社区，Racket_cn）文章。

下面我们来写一段经典的“Hello World!”代码：

@codeblock|{
#lang racket
(display "Hello World!")
}|

更详细的内容可以在微信公众号Racket社区（Racket_cn）去看公众号文章，或者加入Racket社区微信群去参加有关Racket的讨论。

更完整的内容是Racket官方网站https://racket-lang.org/上的https://docs.racket-lang.org/reference/index.html（The Racket Reference（Racket参考））。