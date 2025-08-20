;make-cell.scrbl
;生成单元格数据

#lang scribble/manual

@title[#:tag "make-cell"]{生成单元格数据}

现在可以把分割出的图片作为数据进行整理，以便后边使用。

考虑到拼图存在混合单元格的操作，需要记录初始时的单元格位置，同时也需要记录混合后的单元格位置，另外还要考虑记录当前操作的单元格位置。

像这种需要记录一个对象的多个参数值的，我们一般会考虑用结构来保存对象。有些熟悉编程的朋友会说也可以用类来实现。这当然可以，其实Racket中的类也是用结构来实现的。可以想想如何用结构实现类？这也是Racket语言作为Lisp语言的强大之处。

@section[#:tag "make-cell/use-struct"]{结构的使用}

Racket的结构是一个由若干字段组成的记录数据类型。新的数据类型通常用@racket{struct} 表来创造。如下：

@codeblock|{
(struct struct-id (field-id ...))
}|

这里，构造结构完成后，将得到如下成果：

@itemlist[
@item{@bold{@var{struct-id}：}一个构造器函数，带有和 @var{acket[_field-id]} 的数值一样多的参数， 并返回这个结构类型的一个实例。}

@item{@bold{@racket{struct-id?}：}一个 判断函数， 它带一个单个参数，同时如果它是这个结构类型的一个实例则返回 @var{#t}，否则返回 @var{#f}。}
 
@item{@bold{@var{struct-id-field-id}：}对于每个 @var{field-id}， 一个访问器从这个结构类型的一个实例中解析相应字段的值。}

@item{@bold{@var{struct:struct-id}：}一个结构类型描述符， 它是一个值，体现结构类型作为一个第一类值。}
 ]

 一个 @racket{struct}表对值的种类不设置约束条件，它可表现为这个结构类型的一个实例中的字段。

结构数据类型为Racket提供了强大的数据扩展能力，可以通过@racket{struct-copy}表克隆一个结构并可选地更新克隆中的指定字段；可以通过扩展表来定义一个 结构子类型， 以扩展一个现有结构类型的结构类型，如下：

@codeblock|{
(struct struct-id super-id (field-id ...))
}|

这个 @var{super-id} 必须是一个由 @racket{struct}绑定的结构类型名称（即名称不能被作为一个表达式直接使用）。

也可以通过@racket{equal?}对结构进行比较。

这里先试着定义一个结构：

@codeblock|{
(struck cell ;单元格结构名称
        (row ;行
         col ;列
         pic ;单元格图片
         focu)) ;焦点有无标志
           }|

定义了这个结构后，我们就可以用它定义结构值了，如下：

@codeblock|{
(define c (cell 0 0 source #t))
           }|

这里我只是做个示例，将源图片作为单元格图片使用了。

如果现在我们把@var{c}放到交互区去求值，会发现不会显示结构值的属性值。

这是因为结构定义的时候如果未指定参数@var{#:transparent}，则在交互区求值时不会显示结构值的字段值。类似的的参数还有：@var{#:prefab}获取预制结构类型，@var{#:mutable}实现可变字段值结构，@var{#:auto}指定自动字段并用@var{#:auto-value}指定自动值。

根据上边的描述，结构值字段数据的设置和读取是这样的：

@codeblock|{
(cell-row c) ;读取行值
(set-cell-focu #t) ;设置焦点值为#t
           }|

同样的，上边示例第二行代码运行会出错，因为结构定义时没有指定@var{#:mutable}关键字。

由于结构定义实在太过强大，它的完整定义是这样的：

@codeblock|{

(struct id maybe-super (field ...)
        struct-option ...)
 
maybe-super	 	=	 	
 	 	|	 	super-id
 	 	 	 	 
field	        	=	field-id
 	 	|	 	[field-id field-option ...]
 	 	 	 	 
struct-option	 	=	#:mutable
 	 	|	 	#:super super-expr
 	 	|	 	#:inspector inspector-expr
 	 	|	 	#:auto-value auto-expr
 	 	|	 	#:guard guard-expr
 	 	|	 	#:property prop-expr val-expr
 	 	|	 	#:transparent
 	 	|	 	#:prefab
 	 	|	 	#:sealed
 	 	|	 	#:authentic
 	 	|	 	#:name name-id
 	 	|	 	#:extra-name name-id
 	 	|	 	#:constructor-name constructor-id
 	 	|	 	#:extra-constructor-name constructor-id
 	 	|	 	#:reflection-name symbol-expr
 	 	|	 	#:methods gen:name-id method-defs
 	 	|	 	#:omit-define-syntaxes
 	 	|	 	#:omit-define-values
 	 	 	 	 
field-option	 	=	#:mutable
 	 	|	 	#:auto
 	 	 	 	 
method-defs	 	=	(definition ...)
}|

@section[#:tag "make-cell/construct-cell-struct"]{构造单元格结构}

实际在设计拼图的时候不需要这么复杂的结构，连上边定义的样子都需要简化，“如无必要，勿增实体”，简单是一种美。

@itemlist[
@item{@bold{简化@var{row}及@var{col}：}
       
把@var{row}和@var{col}值用一个@var{id}值代替，只要使它们一一对应就可以了，需要的时候进行相互转换。

如何对应呢？把@var{id}按每行从左到右编号（进入下一行后，编号递增），按这个编号规则进行分析可以发现，

第一行，每个@var{col}完全对应编号；

第二行，@var{id}值为@var{row}乘以总列数加上@var{col}值。

……依次类推，即可完成编号。

因此，可以写一个函数完成根据行列值转换成编号值，这个编号值也是单元格标识id 号。如下：

@codeblock|{
;行列号转id号：
(define (r&c->id row col)
  (if (and (>= row 0)
           (>= col 0))
      (+ (* row cols) col)
      -1))
           }|

这里的函数名称也是采用简单易懂的写法：@racket{r&c->id}。

注意：函数里增加了一个判断，如果@var{row}及@var{col}均为@var{0}时（还没有对图片进行分割的状态），不进行转换，直接输出@var{-1}值，后续程序可以根据这个值判断标志号是否可用。

反之，也可以将@var{id}转换成@var{col}及@var{row}。如下：

@codeblock|{
;id号转行列号(返回row、col)：
(define (id->r&c id)
  (if (>= id 0)
      (quotient/remainder id cols)
      (values -1 -1)))
           }|

这个函数如前面所述那样首先判断@var{id}是否有效，如果有效则用@racket{quotient/remainder}函数将@var{id}除以@racket{cols}，否则返回无效的行列值。

这里函数返回了两个值（多值）。这也是Racket的特色——函数可以返回多值。可以这样接收函数返回得多值（假设有一个3列分割图片）：

@codeblock|{
(define (values row col) (id->r&c 5))
           }|

则@var{row}得到1,@var{col}得到2,及@var{id}为5时，在1行2列位置（行列均从0起算）。

@racket{quotient/remainder}函数是一个数字函数，为数字函数@racket{quotient}（求整数商）与@racket{remainder}（求整余数）的拼合以方便为了想同时求得整商和整余的情况。其实返回多值的函数基本都是为了一次计算能取得多个希望的值的情况，就不用为了取得多个值而重复进行多次计算了，这是非常贴心了，而且符合日常习惯。
}

 @item{@bold{简化@var{focu}（焦点标志）：}
        
如果在每个单元格里都有一个@var{focu}，会造成数据冗余，因为最多只会有一个单元格会有焦点，定义一个全局@var{id}来标记就可以了，简单明了。因此，做一个全局绑定，如下：

@codeblock|{
(define id/current 0);当前单元格id号
           }|
 }
]

这样，单元格结构应为下边这个定义：

@codeblock|{
;定义单元格数据结构：
(struct cell (id bitmap)
  #:transparent
  #:mutable)
           }|

这里@var{#:mutable}时必须的，因为单元格字段数据需要是可变的；@var{#:transparent}是可选的，考虑到程序调试的时候想检查一下结构字段的值，所以加上这个关键字。

@section[#:tag "make-cell/define-cell-struct-var"]{定义单元格结构值}

构造好了结构，就可以用构造的结构来定义结构值，单元格结构值用如下函数产生：

@codeblock|{
;生成单元格数据：
(define (make-cell id)
  (cell id
        (split-picture-by-id id)))
        }|

这里@racket{split-picture-by-id}需要根据指定id分割图片。由于有之前定义的@racket{split-picture-by-row&col}函数和@racket{id->r&c}函数，这里实现根据指定@var{id}分割图片的函数就很简单了。如下：

@codeblock|{
;根据指定id分割图片：
(define (split-picture-by-id id)
  (define-values (row col) (id->r&c id))
  (split-picture-by-row&col picture row col))
}|
