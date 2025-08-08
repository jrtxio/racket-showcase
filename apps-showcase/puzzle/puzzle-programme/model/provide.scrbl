;provide.scrbl
;汇总提供标识。

#lang scribble/manual

@title[#:tag "model/provide"]{汇总提供标识}

如果只是提供标识，采用如下方式就可以了：

@codeblock|{
(provide get-source-picture;取得源图片
         ;……此处省略n条标识
         reset-puzzle-picture;重置混合图片
         )
}|

@racket[provide]既可以进行模块内的函数标识提供设置，也可以对模块内的全局值、类、结构等进行提供设置。

有一个问题是，模块外部最好是直接调用所提供标识的模块内的提供值，而不管其实现方式，否则每次就要将模块内代码阅读明白，显然这是不现实的。因此，Racket提供了@bold{合约}对提供设置进行限制说明。

@section[#:tag "provide/contract"]{合约（Contract）}

如同两个商业伙伴之间的一个合约，一个软件合约是双方之间的一个协议。这个协议规定了从一方传给另一方的每一”产品“（或值）的义务和保证。一个合约确定了双方之间的一个边界。每当一个值跨越这个边界，这个合约监督系统执行合约检查，确保合作伙伴遵守既定合约。Racket主要在模块边界支持合约。 具体来说，程序员可以附加合约到 @racket[provide]从句从而对输出值的使用施加约束和承诺。

@bold{合约系统}保护程序的一部分不受另一部分的影响。 通过

@codeblock|{
(provide (contract-out ....))
}|

指定一个模块的出口行为，而合约系统则强制执行这些约束。

一个数学函数有一个 @bold{定义域}（domain） 和一个 @bold{值域}（range）。定义域表示这个函数可以作为参数接受的值的类型，值域表示它生成的值的类型。编程语言中的函数也有定义域和值域，而一个合约可以确保一个函数在其定义域中只接收值并且在其值域中只产生值。@racket[ ->] 为一个函数创建这样的一个合约。@racket[ -> ]之后的表为定义域并最后为值域指定一个合约。一个@racket[ ->] 本身不是一个合约；它是一种 @racket{合约组合}（contract combinator），它结合其它合约以构成一个合约，也叫合约构造器。

合约系统允许程序员定义他们自己的合约作为函数。接受一个参数的每一个函数可以当作一个判断从而被用作一个合约。合约连接符像 @racket[and/c] 或 @racket[or/c] 用来连接多个判断。也可以使用一个正则表达式作为一个合约。

合约有两种形式，一种是各种操作构建的合约；另一种是各种普通的 Racket 值，它们可以作为合约，包括：

@itemlist[
@item{符号（symbol）、布尔值（boolean）、关键字（keyword） 和 空值（null）， 它们被视为可以识别自己的合约，使用 @racket[eq?]。}
@item{字符串（string）、字节字符串（byte string）、 字符（character）、 +nan.0 和 +nan.f，它们被视为使用 @racket[equal?] 识别自己的合约。}
@item{数字（number （除了 +nan.0 和 +nan.f））， 它们被视为使用 @racket[=] 识别自己的合约。}
@item{正则表达式（regular expression）， 它们被视为能够识别 字节字符串（byte string）和与正则表达式相匹配的 字符串（string） 的合约。}
@item{判断（predicate）：任何算数为1的程序都被视为判断。 在合约检查过程中，它被应用于出现的值，返回 @var[#f] 表示合约失败，而其它的则表示通过。}
]

除了@racket[->]外，还有@racket[->*]及@racket[->i]这两种合约构造器。@racket[->*]合约构造器为接受可选的参数（无论是关键字还是位置）和（或）任意多的参数的函数构造合约。@racket[->*]合约的第一个子句描述了必须的参数，与@racket[->]合约的参数描述相似；第二个子句描述了可选参数，描述的范围可以是 @var[any]，也可以是一连串的合约，表示函数必须返回多个值。@racket[->i]与@racket[->*]不同之处在于， 每个参数和结果都被命名，这些名称可以在子合约和前后条件条款中使用。 换句话说，@racket[->i]表达了参数和结果之间的依赖关系。

在表达剩余参数的合约时需要加上#:rest关键字；在表达关键字参数的合约时需要指定关键字。

另外，@racket[case->]合约构造器用来匹配 @racket[case-lambda]，其每个参数都是一个合约，以管理@racket[case-lambda]中的一个子句。

结构合约有两种情况：一种是提供结构值时，采用@racket[struct/c]构造合约指定结构的形态；另一种是提供整个结构定义，使用@racket[struct]构造合约。

类在设置提供时也可以构造合约，具体在后边详细讲。

@section[#:tag "provide/contract-provide"]{构造合约并申明提供标识}

根据上边介绍的合约构造方式，在模型模块的最前面（需求（require）申明之后）进行带合约的标识提供申明。如下：

@codeblock|{
(provide
 (contract-out
  [get-source-picture ;取得源图片
   (-> path-string? (is-a?/c bitmap%))]
  [adjust-picture-size ;将源图片调整到给定的大小
   (-> any)]
  [set-puzzle-picture!;设置混合图片
   (-> any)]
  [swap-cell;调换单元格
   (-> number? number? void?)]
  [set-rows!;设置单元格行数
   (-> number? void?)]
  [set-cols!;设置单元格列数
   (-> number? void?)]
  [set-picture-size!;设置拼图图片尺寸
   (-> integer? integer? void?)]
  [draw-puzzle-picture;绘制混合图片
   (-> (is-a?/c dc<%>) boolean?)]
  [draw-target-picture;绘制目标图片
   (-> (is-a?/c dc<%>) boolean?)]
  [has-id?;指定的id有效
   (-> integer? boolean?)]
  [x&y->id;根据坐标位置转id
   (-> integer? integer? integer? )]
  [get-current-id;取得当前单元格id
   (-> integer?)]
  [set-current-id!;设置当前单元格id
   (-> integer? void?)]
  [drag-x&y->id;根据拖动的坐标位置判断单元格id
   (-> integer? integer? integer? )]
  [draw-cell-to-id/target;在指定单元格位置绘制指定单元格内容
   (-> integer? integer? (is-a?/c dc<%>) boolean?)]
  [draw-cell-to-x&y;在指定坐标位置绘制指定单元格
   (-> integer? integer? integer? (is-a?/c dc<%>) boolean?)]
  [draw-focus-cell;绘制焦点单元格
   (-> (is-a?/c dc<%>) any)]
  [draw-lose-focus-cell;绘制失焦单元格
   (-> (is-a?/c dc<%>) any)]
  [draw-blank-cell;绘制空白单元格
   (-> integer? (is-a?/c dc<%>) any)]
  [success?;判断是否成功完成拼图
   (-> boolean?)]
  [init-puzzle-data;初始化拼图数据
   (-> any)]
 [recover-puzzle-picture;恢复混合图片
  (-> any)]
 [reset-puzzle-picture;重置混合图片
  (-> any)]
 ))
}|

可以看出，每个设置提供的函数，都是使用的@racket[->]来构造的合约。其中使用了以下几种合约：

@itemlist[
 @item{@bold{any}：代表一个总是被满足的合约。 特别是，它可以接受多个值。它只能在像 @racket[->] 这样的合约的结果位置使用。 在其它地方使用 @racket[any] 是一个语法错误。类似地，@racket[any/c]也接受任何值。但当使用这个合约作为函数合约的结果（值域）时，考虑使用 @racket[any] 代替。使用 @racket[any] 会导致更好的内存性能，但也允许多个结果。}
 @item{@bold{void?}：这是直接用判断函数作为合约。如果值是常数 #<void>，返回 #t ，否则返回 #f 。}
 @item{@bold{number?}：这是直接用判断函数作为合约。如果值是一个数字，返回 #t ， 否则返回 #f。}
 @item{@bold{integer?}：这是直接用判断函数作为合约。如果值是一个整数，返回 #t，否则返回 #f。}
 @item{@bold{boolean?}：这是直接用判断函数作为合约。如果值 是 #t 或 #f ，则返回 #t ，否则返回 #f 。
}
 @item{@bold{path-string?}：要么是当前平台的路径，要么是一个没有空字符的非空字符串。否则返回 #f 。}
 @item{@bold{is-a?/c}：接受一个类或接口，并返回一个平面合约，该合约可以识别该类/接口的实例化对象。这里分别有@racket[bitmap%]类实例及@racket[dc<%>]接口实例。类似地，@racket[is-a?]如果值是一个类的实例或一个实现了接口的类， 返回 #t ，否则返回 #f 。@racket[is-a?/c]是一个合约，而@racket[is-a?]是一个判断。}
 ]

至此，拼图模型的所有功能准备到位，当其它模块需要其中的功能时，只需用@racket[require]进行需求申明即可，可在不用理解其中细节的状态下按需调用，合约系统会在调用中检查给的参数及返回值是否符合约定要求。

接下来，将进入图形界面及其功能的设计。