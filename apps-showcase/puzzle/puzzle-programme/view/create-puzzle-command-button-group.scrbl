;create-puzzle-command-button-group.scrbl
;创建拼图命令按钮组：

#lang scribble/manual

@title[#:tag "gui/create-puzzle-command-button-group"]{创建拼图命令按钮组}

先看代码，再作解释：

@codeblock|{
;创建拼图命令按钮组：
(define (create-puzzle-command-button-group p)
  (let ([gp (new group-box-panel%
                 [label "拼图命令"]
                 [parent p]
                 [stretchable-height #f])])
    (create-button button/select-picture gp "选择..."
                   button/select-picture-callback)
    (create-button button/blend-cells gp "混合拼图"
                   button/blend-cells-callback)
    (create-button button/play-again gp "再来一次"
                   button/play-again-callback)))
}|

这里在定义组框里的构件时，采用了和之前完全不同的方式。本来画风应该是这样的：

@codeblock|{
(set! button/select-picture
      (new button%
           [label "选择..."]
           [parent gp]
           [stretchable-width #t]
           [callback button/select-picture-callback]))
}|

结果出来的买家秀却是用了一个”函数“调用就搞定了。这很好，更简洁了！

但是，这又不是一个函数。因为如果是函数，函数的定义应该是这样的：

@codeblock|{
;定义创建按钮的函数：
(define (create-button name p l cb)
  (set! name
        (new button%
             [label l]
             [parent p]
             [stretchable-width #t]
             [callback cb])))
}|

但实际采用的定义方式是这样的：

@codeblock|{
;定义创建命令功能区按钮的宏：
(define-syntax-rule (create-button n p l prc)
  (set! n
        (new button%
             [label l]
             [parent p]
             [stretchable-width #t]
             [callback prc])))
}|

从注释可以看出，这里创建一个宏来创建按钮，而不是定义一个函数来创建按钮。

@section[#:tag "create-puzzle-command-button-group/syntax"]{宏}

@bold{宏}（macro） 是一种语法表，它有一个关联的 @bold{转换器}（transformer）， 它将原有的表 扩展（expand） 为现有的表。 换句话说，宏是 Racket 编译器的扩展。 racket/base 和 racket 的大部分句法表实际上是宏（比如：@racket[define]——別惊讶——是的，define也是宏。）， 扩展成一小部分核心结构。

Racket 提供基于模式的宏，使得简单的转换易于实现和可靠使用。 Racket 还支持任意的宏转换器，它在 Racket 中实现，或在 Racket 中的宏扩展变体中实现。

由于Racket拥有强大而灵活的宏，使得它除了具有通用编程语言的能力外，还具有了无与伦比的语言创造能力。于是就出现了这样的画风——当使用其它语言遇到表达困难的时候，会寄希望于语言的维护者推出新的语言特性；当使用Racket语言遇到表达困难的时候，那就自己创造一个满足表达需求的语言就是了，没啥大不了的！

@section[#:tag "create-puzzle-command-button-group/define-syntax-rule"]{创建宏}

@bold{基于模式的宏}（pattern-based macro），是将任何与模式匹配的代码替换为使用与模式部分匹配的原始语法的一部分的扩展。

创建宏的最简单方法是使用@racket[define-syntax-rule]：

@codeblock|{
(define-syntax-rule pattern template)
}|

@racket[define-syntax-rule]表绑定一个与单个模式匹配的宏。@bold{模式}必须总是以一个开放的括号开头，后面跟着一个标识符。 在初始标识符之后，其它标识符是 @bold{宏模式变量}（macro pattern variable），可以匹配宏使用中的任何内容。 在模式之后是 @bold{摸板}（template），模板用于替代与模式匹配的表，模板中的模式变量的每个实例都替换为宏使用模式变量匹配的部分。

其实创建宏和定义函数从表面看起来方式差不多，但必须知道的是，函数传递的是函数参数（值引用），宏传递的是用于匹配并替换的模式变量（替换内容）。

因此，上边的以下代码：

@codeblock|{
(create-button button/select-picture gp "选择..."
                   button/select-picture-callback)
}|

通过宏替换后得到的代码就是：

@codeblock|{
(set! button/select-picture
      (new button%
           [label "选择..."]
           [parent gp]
           [stretchable-width #t]
           [callback button/select-picture-callback]))
}|

这就是熟悉的模样了。但是手工输入太繁琐了，把这种事交给计算机是明智之举。

@section[#:tag "create-puzzle-command-button-group/syntax-rules"]{多个模式匹配的宏}

@racket[define-syntax-rule] 表只能绑定一个与单一模式匹配的宏。Racket 的宏系统还支持从同一标识符开始匹配多个模式的转换器，这就是使用 @racket[define-syntax]表以及 @racket[syntax-rules] 转换器表：

@codeblock|{
(define-syntax id
  (syntax-rules (literal-id ...)
    [pattern template]
    ...))
}|

更进一步，如果要匹配任意数量的标识符，而不是只有两个或三个标识符，则可以使用@bold{宏匹配序列}。是否再次看到了”“道生一，一生二，二生三，三生万物”的影子！

宏还有@bold{标识符宏}（identifier macro）、@bold{赋值转换器}（assignment transformer）等概念来实现更广的扩展，甚至可以采用@bold{宏生成宏}的方式组织代码。可以查阅《Racket指南》做更深入的了解。

有了以上的工具，就不仅仅是可以实现用来创建一个按钮那么简单的工作了，将彻底迈向自由！比如用来排版这个内容的Scribble，几乎不是Racket的样式了，更像Tex。