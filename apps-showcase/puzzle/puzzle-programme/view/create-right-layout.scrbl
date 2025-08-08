;create-right-layout.scrbl
;创建右侧布局。

#lang scribble/manual

@title[#:tag "gui/create-right-layout"]{创建右侧布局}

先看代码，再作分析。

@codeblock|{
;创建右侧布局：
(define (create-right-layout p)
  (let ([vp (new vertical-panel%
                 [parent p]
                 [style '(border)])])
    (set! canvas/puzzle
          (new puzzle-canvas%
               [parent vp]
               [paint-callback
                (lambda (c dc)
                  (draw-puzzle-picture dc))]))))
}|

只要看完过前边的章节，对这个函数内容应该很清楚，就不再细说。

只是，这里出现了一个新的类——@racket[puzzle-canvas%]——自定义的拼图画布类。接下来就进入一个新的天地——探讨如何用Racket创建一个新的类，这个类是拼图的重头戏，之前所有对图片所作的操作就为了通过这个类来展示。

@section[#:tag "puzzle-canvas/class"]{类}

@bold{类}（class）表达式表示一个类的值，就像一个 @racket[lambda] 表达式一样：

@codeblock|{
(class superclass-expr decl-or-expr ...)
}|

@racket[superclass-expr] 为新类的基类。 每个 @racket[decl-or-expr] 既是一个声明，关系到对方法、字段和初始化参数；也是一个表达式，每次求值就实例化类。 换句话说，与方法之类的构造器不同，类具有与字段和方法声明交错的初始化表达式。

按照惯例，类名以 % 结束。@bold{内置根类是 @racket[object%]}。

比如创建一个类：

@codeblock|{
;拼图画布类：
(define puzzle-canvas%
  (class canvas%
    (super-new)))
}|

这里新创建的类@racket[puzzle-canvas%]的基类为@racket[canvas%]，在创建类的时候同时用@racket[(super-new)]初始化基类。这样就创建了一个新的类，其功能现在与@racket[canvas%]一致，后边可以通过进一步添加类字段或成员函数来对其进行扩展。

类的完整定义如下：

@codeblock|{
(class* superclass-expr (interface-expr ...)
  class-clause
  ...)
 
class-clause	 	=	(inspect inspector-expr)
 	 	|	 	(init init-decl ...)
 	 	|	 	(init-field init-decl ...)
 	 	|	 	(field field-decl ...)
 	 	|	 	(inherit-field maybe-renamed ...)
 	 	|	 	(init-rest id)
 	 	|	 	(init-rest)
 	 	|	 	(public maybe-renamed ...)
 	 	|	 	(pubment maybe-renamed ...)
 	 	|	 	(public-final maybe-renamed ...)
 	 	|	 	(override maybe-renamed ...)
 	 	|	 	(overment maybe-renamed ...)
 	 	|	 	(override-final maybe-renamed ...)
 	 	|	 	(augment maybe-renamed ...)
 	 	|	 	(augride maybe-renamed ...)
 	 	|	 	(augment-final maybe-renamed ...)
 	 	|	 	(private id ...)
 	 	|	 	(abstract id ...)
 	 	|	 	(inherit maybe-renamed ...)
 	 	|	 	(inherit/super maybe-renamed ...)
 	 	|	 	(inherit/inner maybe-renamed ...)
 	 	|	 	(rename-super renamed ...)
 	 	|	 	(rename-inner renamed ...)
 	 	|	 	method-definition
 	 	|	 	definition
 	 	|	 	expr
 	 	|	 	(begin class-clause ...)
 	 	 	 	 
init-decl	 	=	id
 	 	|	 	(renamed)
 	 	|	 	(maybe-renamed default-value-expr)
 	 	 	 	 
field-decl	 	=	(maybe-renamed default-value-expr)
 	 	 	 	 
maybe-renamed	 	=	id
 	 	|	 	renamed
 	 	 	 	 
renamed	 		=	(internal-id external-id)
 	 	 	 	 
method-definition	=	(define-values (id) method-procedure)
 	 	 	 	 
method-procedure	=	(lambda kw-formals expr ...+)
 	 	|	 	(case-lambda (formals expr ...+) ...)
 	 	|	 	(#%plain-lambda formals expr ...+)
 	 	|	 	
(let-values ([(id) method-procedure] ...)
  method-procedure)
 	 	|	 	
(letrec-values ([(id) method-procedure] ...)
  method-procedure)
 	 	|	 	
(let-values ([(id) method-procedure] ...+)
  id)
 	 	|	 	
(letrec-values ([(id) method-procedure] ...+)
  id)
 	 	|	 	
(chaperone-procedure method-procedure wrapper-proc
                     other-arg-expr ...)

}|

内容非常丰富。但是如果不涉及接口，就用@racket[class]来定义。

@section[#:tag "puzzle-canvas/object"]{对象}

通过 @racket[new] 表实例化类，返回的值即为类的@bold{对象}（object）。

@codeblock|{
(new class-expr (id by-name-expr) ...)
}|

实例化类在前边章节中已经使用多很多次了。它创建 @racket[class-expr] 的值（必须是一个类）的实例, 每个 @var[by-name-expr] 的值都作为相应 @racket[id] 的副名参数提供。

初始化参数、字段声明和表达式可以以类（class） 中的任何顺序出现，并且它们可以与方法声明交织在一起。类中表达式的相对顺序决定了实例化过程中的求值顺序。

方法声明的相对顺序对求值没有影响，因为方法在类实例化之前被完全定义。

Racket创建对象的方式有三种：
@itemlist[#:style 'ordered
 @item{@bold{make-object} 过程用 by-position 初始化参数创建一个新对象；}
@item{@bold{new} 表用 by-name 初始化参数创建一个新对象；}
@item{@bold{instantiate} 表用 by-position 和 by-name 初始化参数创建一个新对象。}
]

在求值表达式的过程中，超类声明的初始化必须通过使用 @racket[super-make-object] 过程、 @racket[super-new] 表或 @racket[super-instantiate] 表求值一次。

从超类继承的字段在超类的初始化过程被调用之前不会被初始化。 
@section[#:tag "puzzle-canvas/method"]{方法}

方法是什么概念？和函数之间有什么区别？

函数和方法都是一段代码块，通过名字来进行调用。它们都能将数据（参数）传递到本体里去进行处理，然后返回一些数据（返回值），也可以没有返回值（void）。所有传递给函数的数据都是显式传递的。但方法跟一个类对象相关联，其中的数据是隐式传递的，而且方法可以操作类内部的数据。方法的声明使用与 Racket 函数相同的语法，但方法不能作为独立函数访问。因此，方法是特定于类而存在的，没有类就谈不上方法。

Racket中，方法的定义方式之一如下：

@codeblock|{
(define/public id expr)
}|

来通过定义@racket[field]字段@var[over]及@racket[puzzle-canvas%]方法来扩展它的功能。如下：

@codeblock|{
;拼图画布类：
(define puzzle-canvas%
  (class canvas%
    (super-new)

    ;类字段：
    (field [over #f]);游戏结束标志
    
    ;设置游戏重启：
    (define/public (replay)
      (set! over #f))))
}|

这里定义了一个@racket[public]类型的方法，目的是设置游戏结束标志（@var[over]）。

Racket类中的每个 public, override, augment, pubment, overment, augride, public-final, override-final, augment-final 和 private 子句都声明了一个或多个方法名。

@itemlist[
 @item{用 public、 pubment 或 public-final 声明的方法在一个类中引入了一个新的方法。 这个方法必须不存在于超类中，否则当类的表达式被求值时就会引发 exn:fail:object exception is raised 异常。
  @itemlist[
 @item{一个用 public 声明的方法可以在一个使用 override、overment 或 override-final 的子类中被覆盖。 }
 @item{一个用 pubment 声明的方法可以在一个使用 augment、augride 或 augment-final 的子类中被增强。}
 @item{一个用 public-final 声明的方法不能在子类中被重载或增强。}]}
  @item{用 override、overment 或 override-final 声明的方法会覆盖已经存在于超类中的定义。 如果该方法不存在，当类表达式被求值时，将引发 exn:fail:object exception is raised 异常。
  @itemlist[
 @item{用 override 声明的方法可以在使用 override、overment 或 override-final 的子类中再次被覆盖。}
 @item{一个用 overment 声明的方法可以在一个使用 augment、 augride 或 augment-final 的子类中被增强。}
 @item{一个用 override-final 声明的方法不能在子类中被进一步重载或增强。}]}
 @item{用 augment、 augride 或 augment-final 声明的方法是对已经存在于超类中的定义进行增强。 如果该方法不存在，当类表达式被求值时，将引发 exn:fail:object exception is raised 。
  @itemlist[
 @item{用 augment 声明的方法可以在使用 augment、 augride 或 augment-final 的子类中被进一步增强。}
 @item{一个用 augride 声明的方法可以在一个使用 override、 overment 或 override-final 的子类中被覆盖。(这样的覆盖只是替换了增强，而不是替换了被增强的方法)。}
 @item{一个用 augment-final 声明的方法不能在子类中被重写或进一步增强。}]}
 @item{用 private 声明的方法在类表达式之外是不可访问的,不能被重载,也不能重载超类中的方法。}
 ]

如果有在其它语言中使用类的经历，会发现Racket对类的创建是非常细致的，其强大的操作能力就可见一斑了。

类的方法在其内部使用和函数的求值类似，如下：
@codeblock|{
(method-id arg ... . arg-list-expr)
}|

如果在类之外调用，则采用消息传递方式，有 send、 send/apply 和 send/keyword-apply 三种方式，如下：

@codeblock|{
(send obj-expr method-id arg ...)
(send obj-expr method-id arg ... . arg-list-expr)
(send/apply obj-expr method-id arg ... arg-list-expr)
(send/keyword-apply obj-expr method-id
                    keyword-list-expr value-list-expr
                    arg ... arg-list-expr)
}|

还有一种动态调用的方式。即在 obj 上调用名称与 method-name 匹配的方法, 并传递所有给定的 v 和 kw-arg ，如下：

@codeblock|{
(dynamic-send	obj	 	 	 	 
 	 	method-name	 	 	 	 
 	 	v ...	 	 	 	 
 	 	#:<kw> kw-arg ...)	
}|

如果觉得一个一个调用方法麻烦，也可以采用集中给出类对象，然后再调用方法的方式，这样代码就可以得到简化。如下：
@codeblock|{
(send* obj-expr msg ...+)
}|

类似的还有@racket[send+]。也可以用@racket[with-method]将方法绑定一个局部标识，这样就可以像使用普通函数一样使用方法。

@section[#:tag "puzzle-canvas/field"]{字段}

类中定义字段方式如下：

@codeblock|{
(field field-decl ...)
}|

类中的每个 @racket[field]、@racket[init-field] 和非方法 @racket[define-values] 子句都为该类声明一个或多个新字段。

用 @racket[field] 或 @racket[init-field] 声明的字段是公共的。 公有字段可以被子类使用 @racket[inherit-field] 访问和修改。 公有字段也可以在类外通过 @racket[class-field-accessor] 进行访问， 并且可以通过 @racket[class-field-mutator] 进行变异。用 @racket[init-field] 声明的字段既是一个公共字段也是一个初始化变量。

用 @racket[define-values] 声明的字段只能在类内访问。

@racket[inherit-field] 声明使一个由超类定义的公共字段可以在类表达式中直接访问。它并不控制继承，而只是控制类表达式中的词法范围。如果指定的字段没有在超类中定义，那么当类表达式被评估时就会引发 exn:fail:object exception is raised 。

使用@racket[get-field]函数从 @var[obj-expr] 的值中提取具有(外部)名称 @var[id] 的字段。

使用@racket[set-field!]函数将具有(外部)名称 @var[id] 的字段从 @var[obj-expr] 的值设置为 @racket[expr] 的值。

@section[#:tag "puzzle-canvas/init"]{初始化变量}

类的初始化变量，用 init、init-field 和 init-rest 声明，为类的每个对象实例化。

初始化变量可以被用于字段的初始值表达式、初始化参数的缺省值表达式以及初始化表达式中。

只有用 init-field 声明的初始化变量可以从方法中访问；从方法中访问任何其他初始化变量都是一个语法错误。

@section[#:tag "puzzle-canvas/interface"]{接口}

接口是一系列方法的声明，是一些方法特征的集合，一个接口只有方法的特征没有方法的实现，因此这些方法可以在不同的地方被不同的类实现，而这些实现可以具有不同的行为（功能）。其主要作用是定义一个类应该具有哪些方法，并不做具体实现，具体实现由接口实现类完成。其与继承的区别在于，@bold{继承}是"是不是"的关系，而@bold{接口}则是"有没有"的关系。

使用接口的意义如下：
@itemlist[#:style 'ordered
@item{@bold{多态性}：接口允许实现多态性，从而可以使用一个接口的引用来引用不同的具体类的对象，从而实现代码的灵活性和可扩展性。}
@item{@bold{解耦合}：使用接口可以实现代码的解耦合，从而降低代码之间的依赖性，使得每个部分可以独立修改和维护。}
@item{@bold{代码复用}：接口还可以实现代码的复用。通过定义通用的接口，可以在不同的具体类中实现相同的方法，从而提高了代码的可复用性。}
@item{@bold{实现规范}：接口还可以用来定义一组规范，让不同的具体类来遵循此规范。这样可以确保不同的类都实现了相同的方法，以满足特定的要求或约定。其实接口本身就是一种约定。}
@item{@bold{灵活性}：接口允许在不改变类的继承结构的情况下引入新的功能。这是因为一个类可以实现多个接口，从而获得多个不同的功能。}
 ]
通过定义通用的接口，可以编写更加灵活、可维护和可扩展的代码，从而提高开发效率和代码质量。 

Racket 中的接口通过使用 @racket[interface] 表创建，它只声明需要去实现的接口的方法名称。 接口可以扩展其它接口，这意味着接口的实现会自动实现扩展接口。即：

@codeblock|{
(interface (superinterface-expr ...) id ...)
}|

实现接口的类采用@racket[class*]来创建，实现的类里必须具有接口内的方法的实现（这就是规则）。

@section[#:tag "puzzle-canvas/mixin"]{混合}

在Racket中， 类（class）是一种表达表，而不是其它有些语言里的一个顶级的声明， 这样使得一个 class 表可以嵌套在任何词法范围内，包括 lambda（λ）。

一个 @racket[class] 表嵌套在lambda（λ）主体内的结果就形成了一个 混合（mixin），这样就实现了一个类的扩展，而且是一种参数化方式的扩展。当把任何一个类作为@racket[lambda]的参数值提供后，函数体内对@racket[class]定义的所有属性及方法均成为这个传入类的扩展方法。

混合的优势在于，可以很容易地将它们结合起来以创建新的类，其共享的实现不存在一个继承层次，即没有多重继承的歧义。

到这里，基本上了解Racket类的主要内容（其它还有特质、序列化等等概念），可以自由使用类进行编程设计了。不过，拼图程序只需要用到一点点类的特性就可以实现了。

@section[#:tag "puzzle-canvas/design-class"]{设计一个类}

类一般用来描述一个问题领域的概念，而概念是把事物的共同本质特点抽象出来形成。因此在创建一个类前，需要搞清楚要描述的概念，要形成对要描述的领域的认知，这样创建出来的类的属性、方法才具有针对性。

可以考虑这样：
@itemlist[
@item{首先，类的标识对应于类名称，应该是一个描述类的概念性名词。}
@item{其次，类应该具有特定的数据（属性），需要分析这些特定的数据应该有哪些？}
@item{然后，考虑针对要创建的类需要解决哪些问题（方法）？}
@item{可以在明确领域的前提下（目标清晰，名称描述准确），边分析概念的内涵，边扩展类的属性及方法。}
]

对于要实现的类@racket[puzzle-canvas%]来说，目标是非常清楚的，就是要实现一个画布，这个画布用来做拼图的。

这个类继承自@racket[canvas%]，当然@racket[canvas%]所具有的属性和方法也是@racket[puzzle-canvas%]的属性和方法。但对于拼图这个领域来说，要分析的特定领域是@racket[canvas%]之外的有关拼图的属性及方法。

需要哪些属性呢？暂时还不清楚（游戏结束标志@var[over]是后来添加的，并不是一开始就清楚），留待后边需要的时候添加。

需要哪些方法？根据之前游戏的规划，需要根据用户的鼠标交互作出拼图图片的调整来实现恢复打乱的图块。因此需要取得绘图环境的方法、取得鼠标指针坐标点的方法、取得鼠标事件的方法。

通过以上的分析，可以得到对@racket[puzzle-canvas%]类的第一步扩展。如下：

@codeblock|{
;拼图画布类：
(define puzzle-canvas%
  (class canvas%
    (inherit get-dc
             get-x
             get-y)

    (super-new)

    ;类字段：
    (field [over #f]);游戏结束标志
    
    ;设置游戏重启：
    (define/public (replay)
      (set! over #f))

    ;覆盖定义鼠标事件：
    (define/override (on-event event)
      void)
    ))
}|

这里@racket[get-dc]（取得绘图环境）、@racket[get-x]（取得鼠标指针x坐标）、@racket[get-y]（取得鼠标指针y坐标）在@racket[canvas%]已经定义，功能够用，为了方便，直接用@racket[inherit]引入（当然不用引入也可以，采用向@var[this]发消息的方式直接调用）。

同时，使用@racket[define/override]覆盖超类中的@racket[on-event]方法，准备对其进行重写。这个内容留待后边根据程序编写进程来一步一步扩充。

接下来就可以通过新创建的类来实现与用户的互操作了。