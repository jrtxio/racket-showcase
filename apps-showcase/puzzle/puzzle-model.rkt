;data.rkt
;拼图。

#lang racket

(require racket/draw)

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

;定义主要数据：
(define source void);原图
(define picture void);图片对象
(define height/picture 0);图片高度
(define width/picture 0);图片宽度
(define height/cell 0);单元高度
(define width/cell 0);单元宽度
(define rows 3);行数
(define cols 3);列数
(define cells (make-hash));单元格散列表
(define gap 1);单元格显示间隙
(define puzzle-picture void);混合的图片
(define id/current 0);当前单元格id号
(define id-list/blend void);混合后的id号列表

;定义单元格数据结构：
(struct cell (id bitmap)
  #:transparent
  #:mutable
  )

;取得源图片：
(define (get-source-picture path)
  (set! source (read-bitmap path)))

;设置图片尺寸：
(define (set-picture-size! w h)
  (set! width/picture w)
  (set! height/picture h))

;取得调整比例：
(define (get-scale/adjust w h w/s h/s)
  (let ([scale/w (/ w w/s)]
        [scale/h (/ h h/s)])
    (if (> scale/w scale/h) scale/w scale/h)))

;将源图片调整到给定的大小：
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
    (set! picture pic/adjust)
    (set-cell-size!)))

;设置单元格尺寸：
(define (set-cell-size!)
  (set! height/cell
        (floor (/ height/picture rows)))
  (set! width/cell
        (floor (/ width/picture cols))))

;根据指定id分割图片：
(define (split-picture-by-id id)
  (define-values (row col) (id->r&c id))
  (split-picture-by-row&col picture row col))

;根据指定行、列分割图片：
(define (split-picture-by-row&col pic row col)
  (let* ([pic/cell (make-bitmap width/cell height/cell)]
         [dc/pic (send pic/cell make-dc)]
         [xl (* col width/cell)]
         [yt (* row height/cell)]
         [xr (+ xl width/cell)]
         [yb (+ yt height/cell)])
    (send dc/pic draw-bitmap-section
          pic
          0 0
          xl yt xr yb)
    pic/cell))
          
;生成单元格数据：
(define (make-cell id)
  (cell id
        (split-picture-by-id id)))

;分割图片到单元格散列表：
(define (split-picture-to-cells)
  (for-each
   (lambda (cell)
     (hash-set! cells
                (cell-id cell)
                cell))
   (split-picture-to-list)))

;分割图片到列表：
(define (split-picture-to-list)
  (define (split/in l r c)
    (if (>= r rows)
        l
        (if (>= c cols)
            (split/in l (+ r 1) 0)
            (split/in
             (append l
                     (list (make-cell
                            (r&c->id r c))))
             r (+ c 1)))))
  (split/in null 0 0))

;绘制单元格表为图片：
(define (draw-cells dc)
  (hash-for-each cells
                 (lambda (id cell)
                   ( draw-cell id dc))))

;绘制单元格：
(define (draw-cell id dc)
  (let-values ([(row col) (id->r&c id)])
    (let ([bitmap (cell-bitmap
                   (hash-ref cells id))]
          [dest-x (* col (+ width/cell gap))]
          [dest-y (* row (+ height/cell gap))])
      (send dc draw-bitmap bitmap
            dest-x dest-y))))

;打乱列表中单元格表顺序：
(define (blend-cells)
  ;从剩余成员中随机抽取一个未调换过的：
  (define (swap-two-cells ids/unused)
    (if (< (length ids/unused) 2)
        ;保存混合后的id列表（记录混合后的状态）：
        (set! id-list/blend
              (hash-map cells
                        (lambda (k v)
                                (cell-id v))))
        (let* ([id/i (list-ref
                      ids/unused
                      (random 0 (length ids/unused)))]
               [ids/unused (remove id/i ids/unused)]
               [id/j (list-ref
                      ids/unused
                      (random 0 (length ids/unused)))]
               [ids/unused (remove id/j ids/unused)])
          (swap-cell id/i id/j)
          (swap-two-cells ids/unused))))
  (swap-two-cells (hash-keys cells)))

;交换列表中的两个单元格：
(define (swap-cell id/target id/source)
  (let* ([cell/t (hash-ref cells id/target)]
         [cell/s (hash-ref cells id/source)]
         [swap/t (cell (cell-id cell/s)
                       (cell-bitmap cell/s))]
         [swap/s (cell (cell-id cell/t)
                       (cell-bitmap cell/t))])
    (hash-set! cells id/target swap/t)
    (hash-set! cells id/source swap/s)))

;设置单元格行数：
(define (set-rows! num)
  (set! rows num))

;设置单元格列数：
(define (set-cols! num)
  (set! cols num))

;设置拼图图片：
(define (set-puzzle-picture!)
  (let* ([pic (make-bitmap width/picture
                           height/picture)]
         [dc (send pic make-dc)])
    (draw-cells dc)
    (set! puzzle-picture pic)))

;重置拼图图片：
(define (reset-puzzle-picture)
  ;设置单元格尺寸：
  (set-cell-size!)
  ;分割图片到表：
  (hash-clear! cells);清空单元格列表
  (set! id/current 0);重置当前单元格id号
  (split-picture-to-cells)
  ;混合图片：
  (blend-cells)
  ;初始化拼图图片：
  (set-puzzle-picture!))

;绘制拼图图片：
(define (draw-puzzle-picture dc)
  (send dc clear)
  (send dc draw-bitmap
        puzzle-picture 0 0))

;绘制目标图片：
(define (draw-target-picture dc)
  (let-values ([(w/d h/d) (send dc get-size)])
    (define s (/ w/d width/picture))
    (send dc set-scale s s)
    (send dc draw-bitmap picture 0 0)))

;将id转换为行列号(返回row、col)：
(define (id->r&c id)
  (if (>= id 0)
      (quotient/remainder id cols)
      (values -1 -1)))

;行列号转id号：
(define (r&c->id row col)
  (if (and (>= row 0)
           (>= col 0))
      (+ (* row cols) col)
      -1))

;根据坐标位置判断行列号(返回row、col)：
(define (x&y->r&c x y)
  (let* ([gh (+ height/cell gap)]
         [gw (+ width/cell gap)]
         [row (quotient y gh)]
         [col (quotient x gw)]
         [max-x/gap (- (* (+ col 1) gw) gap)]
         [min-x/gap (- max-x/gap width/cell)]
         [max-y/gap (- (* (+ row 1) gh) gap)]
         [min-y/gap (- max-y/gap height/cell)])
    (if (and (and (>= x min-x/gap)
                  (< x max-x/gap))
             (and (>= y min-y/gap)
                  (< y max-y/gap)))
        (values row col)
        (values -1 -1))))

;根据坐标位置获取id：
(define (x&y->id x y)
  (let-values ([(r c) (x&y->r&c x y)])
    (r&c->id r c)))

;根据拖动的坐标位置获取单元格id:
(define (drag-x&y->id x y)
  (x&y->id x y))

;绘制空白单元格：
(define (draw-blank-cell id dc)
  (let-values ([(row col) (id->r&c id)])
    (let ([x (* col (+ width/cell gap))]
          [y (* row (+ height/cell gap))]
          [pen/old (send dc get-pen)])
      (send dc set-pen "white" 0 'solid)
      (send dc draw-rectangle
            x y
            width/cell height/cell)
      (send dc set-pen pen/old))))

;在指定坐标位置绘制指定单元格：
(define (draw-cell-to-x&y x y id dc)
  (let ([x0 (- x (/ width/cell 2))]
        [y0 (- y (/ height/cell 2))]
        [cell (hash-ref cells id)])
    (send dc draw-bitmap
          (cell-bitmap cell)
          x0 y0)))

;在指定单元格位置绘制指定单元格内容：
(define (draw-cell-to-id/target id/t id dc)
  (let-values ([(r/t c/t) (id->r&c id/t)])
    (let ([x0/t (* c/t (+ width/cell gap))]
          [y0/t (* r/t (+ height/cell gap))]
          [cell (hash-ref cells id)])
      (send dc draw-bitmap
            (cell-bitmap cell)
            x0/t y0/t))))

;绘制单元格外框：
(define (draw-cell-outline id color dc)
  (let-values ([(r c) (id->r&c id)])
    (let ([x (* c (+ width/cell gap))]
          [y (* r (+ height/cell gap))]
          [pen/old (send dc get-pen)]
          [brush/old (send dc get-brush)])
      (send dc set-pen color 2 'solid)
      (send dc set-brush "white" 'transparent)
      (send dc draw-rectangle x y width/cell height/cell)
      (send dc set-pen pen/old)
      (send dc set-brush brush/old))))

;绘制焦点单元格：
(define (draw-focus-cell dc)
  (draw-cell-outline id/current "red" dc))

;绘制失焦单元格：
(define (draw-lose-focus-cell dc)
  (draw-cell id/current dc))

;取得当前单元格id：
(define (get-current-id)
  id/current)

;设置当前单元格id：
(define (set-current-id! id)
  (set! id/current id))

;指定的id有效：
(define (has-id? id)
  (hash-has-key? cells id))

;判断是否成功完成拼图：
(define (success?)
  (define s #t);成功完成标志
  ;检查复原情况的函数：
  (define (id=old? key value)
    (unless (= key (cell-id value))
      (set! s #f)))
  ;逐个检查是否复原：
  (hash-for-each cells id=old?)
  s)

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

#|
;重新设置单元格列表内图片：
(define (reset-cell-list)
  ;更新单元格内图片：
  (define (refresh-cell-picture id cell)
    (set-cell-bitmap!
     cell
     (split-picture-by-id (cell-id cell))))
  (hash-for-each cells refresh-cell-picture))
|#

;恢复单元格初始列表：
(define (recover-cells-hash)
  ;恢复单元格：
  (define (recover-cell id c)
    (let* ([id/old (list-ref id-list/blend id)]
           [pic/old (split-picture-by-id id/old)])
      (hash-set! cells id (cell id/old pic/old))))
  (hash-for-each cells recover-cell))

;恢复拼图图片：
(define (recover-puzzle-picture)
  (recover-cells-hash)
  (set-puzzle-picture!))

;测试：
(module+ test
  (define in (build-path
              (current-directory)
              "test"
              "test.jpg"))
  in;显示图片路径
  (get-source-picture in);取得原始图片
  ;source
  ;(struct? source)
  ;(send source get-width)
  ;(send source get-height)
  ;测试调整图片尺寸：
  (set-picture-size! 600 600)
  (adjust-picture-size);调整图片，取得拼图图片
  ;picture
  ;height/picture
  ;width/picture

  ;测试获取单元格图片：
  ;(split-picture-by-id 1)

  ;测试生成单元的数据结构：
  ;(define cd (make-cell 3))
  ;cd
  ;(cell-id cd)
  ;(cell-bitmap cd)

  ;测试设置单元格表：
  ;(split-picture-to-list)
  (split-picture-to-cells)
  ;(blend-cells)
  cells
  ;测试合成图片（带间隙）：
  ;(define pic/cells (make-bitmap (* cols width/cell) (* rows height/cell)))
  ;(draw-cells (send pic/cells make-dc))
  ;(set-puzzle-picture!)
  ;(reset-puzzle-picture)
  ;puzzle-picture
  ;(x&y->id 100 150)
  )