;adjust-picture-size.scrbl
;调整显示图片大小

#lang scribble/manual

@title[#:tag "adjust-picture-size"]{调整显示图片大小}

前面我们取得了我们需要的源图片,也取得了我们调整的图片大小,接下来我们就将源图片调整到我们需要的大小。

我们用@racket{adjust-picture-size}来将源图片调整到给定的大小的图片，这个图片将是我们实际操作来显示及拼图的基础图片，后边会用得比较多。
我们先看代码,再做解释：

@codeblock|{
(define (adjust-picture-size)
  (let* ([w width/picture]
         [h height/picture]
         [pic/adjust (make-object bitmap% w h)]
         [w/s (send source get-width)]
         [h/s (send source get-height)]
         [scale/a (get-scale/adjust w h w/s h/s)]
         [w/sc/a (/ w scale/a)]
         [h/sc/a (/ h scale/a)]
         [dc/adjust (send pic/adjust make-dc)])
    (send dc/adjust set-scale scale/a scale/a)
    (send dc/adjust draw-bitmap-section-smooth
          source
          0 0 w/sc/a h/sc/a
          (/ (- w/s w/sc/a) 2) (/ (- h/s h/sc/a) 2)
          w/sc/a h/sc/a)
    (set! picture pic/adjust)))
}|

这段代码和之前的代码相比，就有点长了。

不过我们分段看，也没有多复杂。我们把它分为四部分来看：

@itemlist[
@item{@bold{第一部分}（第1行），定义一个函数，标识为@racket{adjust-picture-size}。}
@item{@bold{第二部分}（第2~10行），进行一系列值绑定。有关@racket{let*}的细节后边来详述，关键的是要直到这个值绑定过程是可以求值的，包括对函数都可以在这里绑定到一个标识（这和@racket{lambda}意义是一致的）。}
@item{@bold{第三部分}（第11~12行），每行都执行一个对@var{dc/adjust}对象的类成员函数的求值，这部分包含面向对象编程的内容。关于类的内容，我们在以后有专门的部分讲述。这里先了解两个方面的内容：
@itemlist[
@item{一是类对象的@bold{创建}。在第二部分通过@racket{(make-object bitmap% w h)}中创建了一个类@racket{bitmap%}的对象（Racket的类名一般在后边带一个%，当然如果是你自定义的类，名称也可自选），然后将创建的类对象绑定到@var{dc/adjust}，后边我们就可以通过这个绑定标识来引用类对象。这里的函数@racket{make-object}就是用来创建类对象的。}
@item{二是类对象的@bold{消息传递}。Racket的类成员函数调用是采用消息传递机制，即用@racket{send}向类对象发送消息，消息内容为成员函数及其参数，成员函数的返回值即为发送消息函数@racket{send}的返回值。@racket{set-scale}和@racket{draw-bitmap-section-smooth}都是类@racket{bitmap%}的成员函数，只是@racket{draw-bitmap-section-smooth}的参数比较多。这里涉及类的有三个函数：@racket{(send pic/adjust make-dc)}向@var{pic/adjust}发送消息@racket{make-dc}来创建一个绘图环境@var{dc/adjust}，以便后边进行绘图操作；@racket{(send dc/adjust set-scale scale/a scale/a)}绘图环境@var{dc/adjust}发送消息@racket{set-scale}设定一个绘图比例（x、y方向比例均为@var{scale/a}）；@racket{(send dc/adjust draw-bitmap-section-smooth ……)}向绘图环境@var{dc/adjust}发送消息@racket{draw-bitmap-section-smooth}在@var{dc/adjust}中绘制调整后的图片。}
 ]}

@item{@bold{第四部分}（第13行），将取得的调整后尺寸的图片绑定到@var{picture}标识以备后边程序进行操作。}
]

看了这个函数代码各各部分的说明，我们可以看出这个函数的设计目的就如前边说的那样，是将源图片调整到给定的大小，思路是根据我们需要的目标图像大小以及来源图片大小的长宽比来创建目标图像。长宽比以及图片尺寸的计算均是在@racket{let*}中实现的，这是Racket实现局部绑定的一种方式。
虽然内部@racket{define}可用于局部绑定，但Racket提供了三种局部绑定表，给予绑定方面的更多控制：

@itemlist[
@item{并行绑定@racket{let}；}
@item{顺序绑定@racket{let*}；}
@item{和递归绑定@racket{letrec}。}
]

它们绑定一组标识，对应某个表达式的结果，以在@racket{let}（或@racket{let*}，或@racket{letrec}）主体中使用。绑定的表结构为（以let为例，其它类似）：

@codeblock|{
(let ([id expr] ...) body ...+)
}|

@itemlist[
@item{@bold{并行绑定@racket{let}：}在右边的@racket{expr}里没有的@var{id}被绑定于任何id，但在@racket{body}中所有的@var{id}都能找到，@var{id}必须不同于其它彼此。}
@item{@bold{顺序绑定@racket{let*}：}不同的是，每个@var{id}可在以后的@racket{expr}及@racket{body}中找到。此外，@var{id}不需要有彼此不同，最近的一个可见，其它重复的被忽略。}
@item{@bold{递归绑定@racket{letrec}：}@racket{letrec}绑定是递归的，其@var{id}绑定在所有其它@racket{expr}——甚至更早的@racket{expr}内存在。@racket{letrec}表中的@racket{expr}经常是用于递归的以及互相递归的@racket{lambda}函数。}
]

除了以上三种典型的局部绑定之外，Racket还有命名@racket{let}绑定以及多值绑定：@racket{let-values}、@racket{let*-values}、@racket{letrec-values}，后边有部分涉及再进行讲解。

这里还调用了一个自定义函数：@racket{(get-scale/adjust w h w/s h/s)}，用于取得调整的比例值。

我们先看代码，再做解释：

@codeblock|{
(define (get-scale/adjust w h w/s h/s)
  (let ([scale/w (/ w w/s)]
        [scale/h (/ h h/s)])
    (if (> scale/w scale/h) scale/w scale/h)))
}|

@racket{get-scale/adjust}函数不是简单地将原图片宽（高）与调整图片宽（高）进行相比取得比例值，如果这样，结果就可能出现画布上下或左右不能填满的情况。这里的算法考虑了源图片与调整图片宽窄的情况，以取得的长宽比例值大小来确定使用哪个比例值，保证了调整后的图片在长宽方向均填满（当然，可能会有一部分原图片内容在调整图片范围之外）。

这里的局部绑定采用的是@racket{let}（并行绑定）实现的。

另外，@racket{if}在Racket里用于实现条件分支，表达形式为：

@codeblock|{
(if test-expr then-expr else-expr)
}|

另外还有@racket{cond}可以实现多个条件选择式分支、@racket{when}和@racket{unless}可实现单条件单选择式分支，根据程序实现需求做不同的选择。这些内容留待我们后续有相应代码的时候介绍。

其它还有@racket{case}可以实现多个复合条件的选择分支，而@racket{match}的模式匹配方式实现的分支将非常灵活。

从这里我们可以看出，Racket语言是如此的灵活，以至于可以进行无思维障碍的编程。这可以区别于其它很多计算机语言，使用这些语言编程的时候总会受到这样那样的限制，顾忌较多，使得思维得不到释放，表达起来不容易得心应手。包括给标识命名，大多数语言都有不同程度的限制，但Racket就几乎没有限制，这样在表达的时候就能够用自然语言的方式（甚至直接用中文）实现编程。像上边代码中@var{w/sc/a}这个进行比例计算之后的调整图片宽度，以及@racket{draw-bitmap-section-smooth}这个用于绘制图像选取块并保持平滑的函数，都可以看出其灵活性及方便性。

到此我们已经准备好了目标图片，接下来我们将把图片按单元格行数和列数分割成小块的单元格图片。
