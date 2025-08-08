;set-picture-size.scrbl
;设置图片尺寸

#lang scribble/manual

@title[#:tag "set-picture-size"]{设置图片尺寸}

前面小节我们把图片读入了计算机内存中。

但是，我们读取的图片并不一定是我们需要的大小。

@racket{set-picture-size!}将源图片调整到给定的大小，这样无论是进行统一规范的处理或者是在屏幕上显示都是定尺寸的、可控的。因此，我们首先要让程序获得图片需要的大小。

我们先看代码,再做解释：

@codeblock|{
(define (set-picture-size! w h)
  (set! width/picture w)
  (set! height/picture h))
  }|

根据上一节的介绍，我们能看出这个函数是一个有副作用的函数，其参数为必须参数@var{w}、@var{h}。这个函数通过给定参数设置对应的两个值：@var{width/picture}、@var{height/picture}。@var{w}、@var{h}为数值，分别为图片的宽度和高度。

这里说@var{width/picture}、@var{height/picture}是两个值，是的没错，@var{width/picture}、@var{height/picture}是值的标识，换句话说，就是两个值的代表。

数值为Racket数据类型的一种。Racket有一些内置的数据类型：布尔值(Boolean)、数值(Number)、字符(Character)、字符串(String)、字节（Byte）、字节字符串(Byte String)、符号(Symbol)、关键字(Keyword)、序对(Pair)和列表(List)、向量(Vector)、散列表(Hash Table)、格子(Box)、无效值(Void)和未定义值(Undefined)。

另外，还有序列（Sequence）和流（Stream）、字典（Dictionarie）、集合（Set）等概念。

@itemlist[
@item{@bold{布尔值(Boolean)}：

有两个值，@racket{#t}表示真，@racket{#f}表示假。大写的@racket{#T}和@racket{#F}解析为同样的值，一般采用小写形式。@racket{boolean?}判断函数识别两个布尔常量。然而，在对@racket{if}、@racket{cond}、@racket{and}、@racket{or}等等的一个测试表达式的结果里，除了@racket{#f}之外，任何值都是记为真。}

@item{@bold{数值(Number)}：

Racket的数值可以是精确数值也可是不精确数值。任意大小的整数、有理数、带精确实部和虚部的复数都属于精确数值；浮点数及实部与虚部为浮点数的复数属于不精确数。数值类别有整数(integer)、有理数(rational)、实数(real)(总是有理数)以及复数(complex)用通常的方法定义，通过@racket{integer?}、@racket{rational?}、@racket{real?}以及@racket{complex?}判断函数来验证。

数值可以用于数值计算，像+、-、*、/这种四则运算，也可以像@racket{sin}、@racket{cos}、@racket{tan}等数值运算函数（其实+-*/也是函数），还可以用=、<、>、<=、>=等对数值进行值的比较，用@racket{eqv?}(乃至@racket{equal?})对数值进行精确性及值的比较。}

@item{@bold{字符(Character)}：

一个可打印字符通常打印为以@racket{#\}后跟着代表字符的形式；一个非打印字符通常打印为以@racket{#\u}后跟着十六进制数值的标量值的形式；另外还有特殊字符，如空格和换行符分别打印为@racket{#\space}和@racket{#\newline}。

@racket{display}函数直接将一个字符写入到当前输出端口。

@racket{char=?}函数比较两个或多个字符，@racket{char-ci=?}比较字符但忽略大写。@racket{eqv?}和@racket{equal?}在字符方面的行为与@racket{char=?}表现一样，当需要更具体地声明正在比较的值是字符时使用@racket{char=?}。}

@item{@bold{字符串(String)}：

是一个固定长度的字符数组，使用双引号打印。在字符串中的双引号和反斜杠字符是用反斜杠转义。其它普通的字符串转义包括：@racket{\n}用于一个换行，@racket{\r}用于一个回车，使用@racket{\}后边跟着多达三个八进制数字实现八进制转义，以及用@racket{\u}(多达四位数)实现十六进制转义。

  字符串有关的操作函数：
  
@itemlist[  @item{@racket{make-string}创建一个给定一个长度和可选填充字符的可变字符串；}
          @item{@racket{string-ref}从一个字符串(用基于0的索引)中访问一个字符；}
          @item{@racket{string-set!}在一个可变字符串中更改一个字符；}
          @item{@racket{string<?}、@racket{string>?}或@racket{string-ci<?}、@racket{string-ci>?}进行在机器和用户之间保持一致的排序；}
@item{string-upcase转换为大写等等。}
]}

@item{@bold{符号(Symbol)}：

一个以@racket{’}开始并带一个标识的表达式产生一个符号值。符号是区分大小写的。调用@racket{string->symbol}产生一个保留符号。保留符号可以用@racket{eq?}(或@racket{eqv?}或@racket{equal?})方便地比较，所以它们作为方便的值用于标签和枚举。

任何字符都可以直接出现在一个标识符里，空白和以下特殊字符除外：

   ( ) [ ]{ }" , ' `; # | \

实际上，@racket{#}仅仅不允许在一个符号开始位置，并且也仅仅不允许@racket{%}在最后位置，除此之外，@racket{#}是被允许的。此外，@racket{.}本身不是一个符号。

空格或特殊字符可以通过用@racket{|}或@racket{\}引用包含进一个标识里。这些引用机制用于包含特殊字符或可能额外看起来像数字的标识的打印表中。}
]

这里先介绍几个基本的数据类型，其它的后续根据使用情况会陆续登台亮相。
上边的@var{width/picture}、@var{height/picture}两个标识是作为全局值来使用的，为数值，我们每次通过用副作用函数@racket{set!}将相应标识进行重新绑定值。

好了，到现在为止，我们的源代码应该是这样的：

@codeblock|{

#lang racket

(define source void)
(define height/picture 0)
(define width/picture 0)

(define (set-source-picture! in)
  (set! source (read-bitmap in)))

(define (set-picture-size! w h)
  (set! width/picture w)
  (set! height/picture h))

(module+ test
  (define in (build-path
              (current-directory)
              "test"
              "test.jpg"))

  (define source (read-bitmap in))
  source

  (set-picture-size! 600 600)
  height/picture
  width/picture)
  }|

这是一个完整的Racket代码文件，我们将它保存为“data-1.rkt”。实际代码中应该还需有注释。

  可以看到，源代码文件可以分为四个部分，分别为：

@itemlist[#:style 'ordered
@item{第一行是"#lang racket"，标识下边采用的是racket语言。Racket是“能够创造编程语言的编程语言”，因此其源代码是可以有其它语言的，比如我现在写这个教程用的就是Scribble语言（可以编写文本内容丰富的程序，无论内容是要排版的文字还是要以编程方式生成的任何其他形式的文本）。为了标明使用的语言，需要在模块的前面一开始就用"#lang"明确指示出所用语言，比如本教程的源文件开头就有"#lang scribble/manual"这样的声明代码。}

@item{接下来是三个标志的定义。这三个变量的生命期在整个@racket{data}模块范围内，我们把它们叫做全局标志。从内容可以看出，它们一开始只是绑定了一个基本值（@var{source}更是绑定了一个@racket{void}（无效）值）。在实际使用的时候，将根据需求重新绑定为其它的有效值。这就是用@racket{set!}的原因。}

@item{再接下来是两个函数，就是我们前面讲的@racket{set-source-picture!}和@racket{set-picture-size!}。}

@item{最后是一个测试模块。我们在其中追加了对新定义的函数@racket{set-picture-size!}的测试。按Ctrl+R运行程序后，根据输出结果我们可以看到，图片尺寸已经设置好了。}
]

但是，设置好了尺寸并不代表我们已经得到了指定尺寸的图片，下一步我们来实现这个功能。