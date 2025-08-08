;get-source-picture.scrbl
;取得源图片

#lang scribble/manual

@title[#:tag "get-source-picture"]{取得源图片}

首先，我们要拼的图片是保存在计算机的存储介质中的（比如硬盘等），要操作图片，首先得把图片读入内存中。

计算机中图片是以二进制格式的文件存储的。为了把图片内容读入内存中，将用到文件的输入输出、Racket输入输出端口、图片读取（@racket{read-bitmap}）等概念。

我们先看代码，再做解释：

@codeblock|{
(define (set-source-picture! in)
  (set! source (read-bitmap in)))
}|

这段代码很简单，它定义了一个函数@racket{set-source-picture!}来实现读取存储器上的图片并保存在内存中的功能；@racket{set!}函数对全局值标识@var{source}设置值；@racket{read-bitmap}读取图片，参数@var{in}为图片文件路径，函数返回图片值。

Racket编程时，一个函数尽量确保实现一个简单功能，这样代码量少，调试方便，比较容易操作。

以下是涉及的几个相关概念：

@itemlist[
@item{@bold{标识：}
       
在Racket里，首先需要理解的是标识的概念，在其它某些资料里也称作标识符。我称它为标识，因为它不仅仅是符号，而是计算机操作的值的代表；它也不是其它语言里所说的变量，因为Racket的标识只是对值的绑定，并不是随时可变的符号（当然，上边用副作用函数set!对其指向的值进行了强制改变。），Racket主张使用无副作用的函数式编程。

函数式编程，要求函数的输入与输出具有确定性，因此一般函数没有任何的副作用。函数式编程语言中的函数能做的唯一一件事情，就是求值并且返回结果。在Racket中，为满足程序员在特定情况下对标识所绑定值的改变，保留了副作用函数，但一般不建议使用。当这个函数会导致改变输出值时，函数加“!”标志，比如上边的@racket{set-source-picture!}。}

@item{@bold{函数定义：}
       
在Racket中，函数的定义采用@racket{lambda}如下：

@code{
(define _id (lambda (_arg ...) _body ...+))
}

如，以下函数定义并绑定到@racket{set-source-picture!}标识：

@codeblock|{
(define set-source-picture!
  (lambda (in)
    (set! source (read-bitmap in))))
}|

这个写法和之前的@racket{get-source-picture}等价，只是之前的函数为简写形式，这样更方便表达。函数的简写形式为：@racket{define}表还支持函数定义的一个简写：

@codeblock|{
(define (id arg ...) body ...+)
}|

可以看出，上边的@racket{get-source-picture}函数即为简写形式。
如果不把@racket{lambda}定义的函数绑定到指定的标识，则为匿名函数。}

@item{@bold{函数的参数：}
       
Racket函数的参数有四种形式：必须参数、剩余参数、可选参数及关键字参数。对一个函数来说，对参数的求值顺序为自左至右进行求值。函数参数@var{arg}是标识，从上边的描述可以看出，函数也是采用标识的（非匿名函数），因此函数是可以作为函数的参数进行传递的。

必须参数：就像上边的一样，显式表示出来，在函数调用的时候必须为参数指定值。}

@item{@bold{剩余参数：}
  其数量是可变的，在实际调用时，给定的参数作为一个列表传递给函数。当剩余参数与必须参数组合时，剩余参数前加一个“.”来表示。

@codeblock|{
 (define (id arg ... . rest-id) body ...+)
 }|
}

 @item{@bold{可选参数：}

  可选参数必须在函数定义时给定一个默认值（如：@racket{[num 3]}），在函数调用时如果给定值则使用当前值，如果未给定值则使用默认值。
}
 
@item{@bold{关键字参数：}
  关键字参数和函数的其它类别参数采用位置传递不同，它根据给定的关键字识别参数（如：@racket{(go 7 #:mode 1)}）。关键字参数可以做必须参数，也可以做可选参数。
}

@item{@bold{路径：}
       
路径用来标识数据文件在存储设备上的位置。如果用字符串表示文件位置，不同的操作系统表达方式是不一样的，比如在Windows中可表示为：

  c:\Documents\puzzle\test\test.jpg

  而在Unix类操作系统中则表示为：

  ~/Documents/puzzle/test/test.jpg

  Racket用Path实现跨平台的文件路径处理。

与路径相关的函数有：@racket{string->path}将字符串转换为路径；@racket{path->string}将路径转换为字符串；@racket{build-path}构建一个路径；@racket{current-directory}取得当前路径（下面将用到这个函数）……等等。
}
 ]

函数写好之后，首先需要验证这个函数是否可以正确运行。

有两种方式完成这个工作，一是在DrRacket编辑器的交互区输入函数调用来做测试，另一种是采用测试模块来实现。

@itemlist[#:style 'ordered
@item{@bold{通过交互输入测试函数：}

@codeblock|{
> (define in (build-path
              (current-directory)
              "test"
              "test.jpg"))
> (define source (read-bitmap in))
> source
}|

可以看到，DrRacket直接将图片值进行了输出，表明函数已经将图片数据从存储介质（这里是硬盘）中读入了计算机内存中。}

@item{@bold{用测试模块测试函数：}

Racket的模块是用来组织程序的一种方式。一般情况下，每个Racket程序文件默认都是一个模块（模块名称默认为文件名称）。

模块声明的一般表示法为：

@code{
(module name-id initial-module-path decl ...)
}

我们更多使用简写方式：

@code{
#lang racket
decl ...
}

它等同于：

@code{
(module name racket
  decl ...)
}

如果在当前模块中要使用其它模块中的函数，首先需要对存有对应函数的模块提出需求：

@code{
(require module_path)
}

除此之外，还有子模块的概念。即@racket{module}表可以嵌套在一个模块内，这个嵌套的@racket{module}就表声明了一个子模块。子模块可以引用外围模块的标识，而外围模块要引用子模块定义的标识则需要用@racket{require}提出需求。

为完成函数测试，我们定义一个测试子模块：

@codeblock|{
(module+ test
  (define in (build-path
              (current-directory)
              "test"
              "test.jpg"))
  (define source (read-bitmap in))
  source)
}|

完成代码后，选择运行代码（快捷键：Ctrl+R），就可以看到和以上交互区输入后得到同样的结果。

采用测试模块这种方式的好处在于可以实现重复测试而不用每次在交互区输入内容。这对于需要准备测试环境（如：上边提前定义图片文件路径）的情况意义更大，后边我们可以看到。}
]

如上一节所述，从本节开始，后边写的有关操作图片的函数都属于模型的内容，把它们都写道一个文件里，文件名命名为“puzzle-model.rkt”。