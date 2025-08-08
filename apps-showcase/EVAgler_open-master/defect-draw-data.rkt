#lang racket
;defect-draw-data.rkt
;绘制标记。

(provide interaction-flag
         begin-stamp-interaction
         end-stamp-interaction
         abort-stamp-interaction
         set-stamp-point
         message-end?
         get-interaction-message
         increase-interaction-flag
         set-stamp-base
         set-stamp-scale
         set-current-picture-id
         set-output-flag

         draw-stamp/defects
         draw-stamp/defect
         draw-current-stamp/defect
         draw-temp-stamp
         get-picture/defect-section)

(require racket/draw)

(require "task-struct-data.rkt"
         "app-data.rkt")

;公共数据：=========================================================
(define flag/interaction #f) ;序号,其他数字值表示会话列表序号
(define id/picture #f);当前源图片序号(#f表示未设置）
(define current-defect (void)) ;当前缺陷对象
(define temp-stamp (void));转换用的临时缺陷标记结构对象
(define x/offset 0) ;当前图片点相对源图片偏移量x值
(define y/offset 0) ;当前图片点相对源图片偏移量y值
(define scale/stamp 1) ;缺陷标志绘图比例值
(define list/defects (void));缺陷列表控件（用于更新缺陷列表显示）
(define flag/output #f) ;输出绘图标志，#t表示处于输出状态（默认为非输出状态）

;标记图形结构：========================================================
;方形：
(struct struct/square
  (center-x center-y half-width)
  #:mutable #:transparent)
;矩形：
(struct struct/rectangle
  (start-x start-y
           end-x end-y
           third-x third-y)
  #:mutable #:transparent)
;圆形：
(struct struct/circle
  (center-x center-y half-width)
  #:mutable #:transparent)

;会话交互提示：=====================================================
;方块标记会话列表：
(define interaction/square
  (list
   "点击要绘制的方块的中心点。"
   "点击要绘制的方块的边缘点。"))

;矩形标记会话列表：
(define interaction/rectangle
  (list
   "点击矩形短边中心点。"
   "点击矩形对面短边的中心点以确定矩形长度。"
   "点击以确定矩形宽度。"))

;圆形标记会话列表：
(define interaction/circle
  (list
   "点击圆形的中心点。"
   "点击圆形的边缘点。"))

;会话公共函数：======================================================
;设置会话序号：
(define interaction-flag
  (case-lambda
    [()
     flag/interaction]
    [(f)
     (set! flag/interaction f)]))

;设置当前图片序号：
(define (set-current-picture-id id)
  (set! id/picture id))

;设置输出标志：
(define (set-output-flag output)
  (set! flag/output output))

;开始交互绘图：
(define (begin-stamp-interaction id defect listbox)
  (set-output-flag #f) ;设置输出标致为假（为显示状态）
  (set! id/picture id) ;图片序号
  (set! current-defect defect) ;取得当前编辑的缺陷结构对象
  (set! list/defects listbox) ;取得缺陷列表控件
  (interaction-flag 0) ;初始化会话序号
  (create-stamp) ;创建一个标记对象
  (status-message ;显示初始化会话消息
   (get-interaction-message)))

;结束交互绘图：
(define (end-stamp-interaction)
  (save-defect-to-defects)
  (send list/defects set
        (get-defect-descriptions id/picture)
        (get-defect-grades id/picture))
  (interaction-flag #f)
  (status-message "绘图结束。"))

;终止交互绘图：
(define (abort-stamp-interaction)
  (interaction-flag #f)
  (status-message "绘图被终止。"))

;设置会话点值：
(define (set-stamp-point x y)
  (when (not (void? current-defect))
    (case (node-defect-stamp-type current-defect)
      [("方形")
       (set-stamp/square-point x y)]
      [("矩形")
       (set-stamp/rectangle-point x y)]
      [("圆形")
       (set-stamp/circle-point x y)])))

;取得当前阶段会话提示信息内容：
(define (get-interaction-message)
  (when flag/interaction
    (case(node-defect-stamp-type current-defect)
      [("方形")
       (list-ref interaction/square flag/interaction)]
      [("矩形")
       (list-ref interaction/rectangle flag/interaction)]
      [("圆形")
       (list-ref interaction/circle flag/interaction)])))

;判断是否在消息末尾：
(define (message-end?)
  (let ([messages
         (case (node-defect-stamp-type current-defect)
           [("方形") interaction/square]
           [("矩形") interaction/rectangle]
           [("圆形") interaction/circle])])
    (if (>= flag/interaction (length messages))
        #t #f)))

;根据当前标记类型创建标记：
(define (create-stamp)
  (case (node-defect-stamp-type current-defect)
    [("方形")
     (set! temp-stamp (struct/square 0 0 0))]
    [("矩形")
     (set! temp-stamp (struct/rectangle 0 0 0 0 0 0))]
    [("圆形")
     (set! temp-stamp (struct/circle 0 0 0))]))

;增加会话序号计数：
(define (increase-interaction-flag)
  (interaction-flag
   (+ flag/interaction 1)))

;方形绘图会话函数：----------------------------------------
;设置方形会话点值：
(define (set-stamp/square-point x y)
  (case flag/interaction
    [(0)
     (set-square-center x y)]
    [(1)
     (begin
       (set-square-half-width x y)
       (save-square-to-stamp))]))

;设置stamp/square中心点x、y值：
(define (set-square-center x y)
  (set-struct/square-center-x! temp-stamp x)
  (set-struct/square-center-y! temp-stamp y))

;设置stamp/square的half-width值：
(define (set-square-half-width x y)
  (let* ([x/center (struct/square-center-x temp-stamp)]
         [y/center (struct/square-center-y temp-stamp)]
         [half/x (abs (- x x/center))]
         [half/y (abs (- y y/center))])
    (set-struct/square-half-width!
     temp-stamp
     (if (> half/x half/y) half/x half/y))))

;保存stamp/square结构对象到缺陷标记哈希表：
(define (save-square-to-stamp)
  (node-defect-stamp
   current-defect
   (hash 'x (x/current->source
             (struct/square-center-x temp-stamp))
         'y (y/current->source
             (struct/square-center-y temp-stamp))
         'half-width (v/current->source
                      (struct/square-half-width temp-stamp)))))

;绘制方形（绘图时要将源图值转换为当前值）:
(define (draw-stamp/square dc stamp)
  (let* ([x (x/source->current (hash-ref stamp 'x))]
         [y (y/source->current (hash-ref stamp 'y))]
         [half-width (v/source->current (hash-ref stamp 'half-width))]
         [width (+ half-width half-width)])
    (send dc draw-rectangle
          (- x half-width) (- y half-width) width width)))

;圆形绘图会话函数：-------------------------------------
;设置圆形会话点值：
(define (set-stamp/circle-point x y)
  (case flag/interaction
    [(0)
     (set-circle-center x y)]
    [(1)
     (begin
       (set-circle-radius x y)
       (save-circle-to-stamp))]))

;设置stamp/circle中心点x、y值：
(define (set-circle-center x y)
  (set-struct/circle-center-x! temp-stamp x)
  (set-struct/circle-center-y! temp-stamp y))

;设置stamp/circle的half-width值：
(define (set-circle-radius x y)
  (let* ([x/center (struct/circle-center-x temp-stamp)]
         [y/center (struct/circle-center-y temp-stamp)]
         [half/x (abs (- x x/center))]
         [half/y (abs (- y y/center))])
    (set-struct/circle-half-width!
     temp-stamp
     (if (> half/x half/y) half/x half/y))))

;保存stamp/circle结构对象到缺陷标记哈希表：
(define (save-circle-to-stamp)
  (node-defect-stamp
   current-defect
   (hash 'x (x/current->source
             (struct/circle-center-x temp-stamp))
         'y (y/current->source
             (struct/circle-center-y temp-stamp))
         'half-width (v/current->source
                      (struct/circle-half-width temp-stamp)))))

;绘制圆形（绘图时要将源图值转换为当前值）:
(define (draw-stamp/circle dc stamp)
  (let* ([x (x/source->current (hash-ref stamp 'x))]
         [y (y/source->current (hash-ref stamp 'y))]
         [half-width (v/source->current (hash-ref stamp 'half-width))]
         [width (+ half-width half-width)])
    (send dc draw-ellipse
          (- x half-width) (- y half-width) width width)))

;矩形绘图会话函数：--------------------------------------
;设置矩形会话点值：
(define (set-stamp/rectangle-point x y)
  (case flag/interaction
    [(0)
     (set-rectangle-start x y)]
    [(1)
     (set-rectangle-end x y)]
    [(2)
     (begin
       (set-rectangle-third x y)
       (save-rectangle-to-stamp))]))

;设置stamp/rectangle起点x、y值：
(define (set-rectangle-start x y)
  (set-struct/rectangle-start-x! temp-stamp x)
  (set-struct/rectangle-start-y! temp-stamp y))

;设置stamp/rectangle终点x、y值：
(define (set-rectangle-end x y)
  (set-struct/rectangle-end-x! temp-stamp x)
  (set-struct/rectangle-end-y! temp-stamp y))

;设置stamp/rectangle第三点x、y值：
(define (set-rectangle-third x y)
  (set-struct/rectangle-third-x! temp-stamp x)
  (set-struct/rectangle-third-y! temp-stamp y))

;保存stamp/rectangle结构对象到缺陷标记哈希表：
(define (save-rectangle-to-stamp)
  (let* ([x/s (struct/rectangle-start-x temp-stamp)]
         [y/s (struct/rectangle-start-y temp-stamp)]
         [x/e (struct/rectangle-end-x temp-stamp)]
         [y/e (struct/rectangle-end-y temp-stamp)]
         [x/t (struct/rectangle-third-x temp-stamp)]
         [y/t (struct/rectangle-third-y temp-stamp)]
         [a (atan (- y/e y/s) (- x/e x/s))]
         [b (atan (- x/t x/s) (- y/t y/s))]
         [l/se (sqrt (+ (expt (- x/e x/s) 2)
                        (expt (- y/e y/s) 2)))]
         [l/st (sqrt (+ (expt (- x/t x/s) 2)
                        (expt (- y/t y/s) 2)))]
         [w/2 (abs (* (cos (+ a b)) l/st))]
         [x x/s]
         [y y/s]
         [width (exact-floor l/se)]
         [half-width (exact-floor w/2)]
         [rotation (- a)])
    (node-defect-stamp
     current-defect
     (hash 'x (x/current->source x)
           'y (y/current->source y)
           'width (v/current->source width)
           'half-width (v/current->source half-width)
           'rotation rotation))))

;绘制矩形（绘图时要将源图值转换为当前值）:
(define (draw-stamp/rectangle dc stamp)
  (let* ([x (x/source->current (hash-ref stamp 'x))]
         [y (y/source->current (hash-ref stamp 'y))]
         [width (v/source->current (hash-ref stamp 'width))]
         [half-width (v/source->current (hash-ref stamp 'half-width))]
         [height (+ half-width half-width)]
         [rotation (hash-ref stamp 'rotation)])
    (let-values ([(xo/old yo/old) (send dc get-origin)]
                 [(ro/old) (send dc get-rotation)])
      (send dc set-origin x y)
      (send dc set-rotation rotation)
      (send dc draw-rectangle
            0 (- half-width)
            width height)
      (send dc set-origin xo/old yo/old)
      (send dc set-rotation ro/old))))

;操作函数：--------------------------------------------------
;保存缺陷结构对象到缺陷列表：
(define (save-defect-to-defects)
  (let ([pic (get-picture-by-source-id id/picture)])
    (picture-node-defects
     pic ;保存缺陷对象到缺陷列表
     (append (picture-node-defects pic)
             (list current-defect)))))

;取得当前缺陷等级：
(define (get-current-defect-grade)
  (if (not (void? current-defect))
      (node-defect-grade current-defect)
      #f))

;为缺陷标志设置比例值：
(define (set-stamp-scale scale)
  (set! scale/stamp scale))

;将当前显示图片片段基点值传递给缺陷标记:
(define (set-stamp-base bx by)
  (set! x/offset bx)
  (set! y/offset by))

;绘图：--------------------------------------------------------
;绘制当前图片所有缺陷标记：
(define (draw-stamp/defects dc)
  (let ([defects
          (get-picture-defects id/picture)])
    (when (not (empty? defects))
      (for ([defect defects])
        (draw-stamp/defect dc defect)))))

;绘制缺陷标记图:
(define (draw-stamp/defect dc defect)
  (when (not (void? defect))
    (let ([type (node-defect-stamp-type defect)]
          [grade (node-defect-grade defect)]
          [stamp (node-defect-stamp defect)])
      (when (and (non-empty-string? type)
                 (non-empty-string? grade)
                 (not (hash-empty? stamp)))
        (begin
          (send dc set-pen
                (get-stamp-color grade)
                (v/source->current 10)
                'solid)
          (send dc set-brush "black" 'transparent)
          (draw-stamp dc type stamp))))))

;绘制临时标记：
(define (draw-temp-stamp dc)
  (when (not (void? current-defect))
    ;设置笔及画刷：
    (send dc set-pen
          (get-stamp-color (node-defect-grade current-defect))
          (v/source->current 10)
          'solid)
    (send dc set-brush "black" 'transparent)
    ;绘图：
    (if (and (string=? (node-defect-stamp-type current-defect)
                       "矩形")
             (= flag/interaction 1))
        (draw-temp/rectangle-line dc)
        (draw-current-stamp/defect dc))))

;绘制临时线段：
(define (draw-temp/rectangle-line dc)
  (let ([x1 (struct/rectangle-start-x temp-stamp)]
        [y1 (struct/rectangle-start-y temp-stamp)]
        [x2 (struct/rectangle-end-x temp-stamp)]
        [y2 (struct/rectangle-end-y temp-stamp)])
    (send dc draw-line x1 y1 x2 y2)))

;取得绘图颜色:
(define (get-stamp-color grade)
  (case grade
    [("一般") "yellow"]
    [("严重") "orange"]
    [("危急") "red"]))

;绘制当前标记图:
(define (draw-current-stamp/defect dc)
  (when (not (void? current-defect))
    (draw-stamp/defect dc current-defect)))

;绘制标记图：
(define (draw-stamp dc type stamp)
  (case type
    [("方形") (draw-stamp/square dc stamp)]
    [("矩形") (draw-stamp/rectangle dc stamp)]
    [("圆形") (draw-stamp/circle dc stamp)]))

;转换当前图片x点值到源图片:
(define (x/current->source x)
  (+ x/offset
     (exact-floor (/ x scale/stamp))))

;转换当前图片y点值到源图片:
(define (y/current->source y)
  (+ y/offset
     (exact-floor (/ y scale/stamp))))

;转换当前图片值到源图片：
(define (v/current->source v)
  (exact-floor (/ v scale/stamp)))

;转换源图片x点值到当前图片:
(define (x/source->current x)
  (if flag/output
      x
      (exact-floor
       (* (- x x/offset) scale/stamp))))

;转换源图片y点值到当前图片:
(define (y/source->current y)
  (if flag/output
      y
      (exact-floor
       (* (- y y/offset) scale/stamp))))

;转换源图片值到当前图片：
(define (v/source->current v)
  (if flag/output
      v
      (exact-floor
       (* v scale/stamp))))

;其他绘图函数：=========================================================
;取得缺陷图片片段（默认图片为800*450，高宽比约=0.618。图片不小于标记的3倍。）：
(define (get-picture/defect-section source defect)
  (let-values ([(x y w h)
                (get-defect-section-size defect)])
    (let* ([w/source (send source get-width)]
           [h/source (send source get-height)]
           [x/section (if (> w w/source)
                          0
                          (cond
                            [(< x 0) 0]
                            [(> (+ x w) w/source) (- w/source w)]
                            [else x]))]
           [y/section (if (> h h/source)
                          0
                          (cond
                            [(< y 0) 0]
                            [(> (+ y h) h/source) (- h/source h)]
                            [else y]))]
           [w/section (if (> w w/source) w/source w)]
           [h/section (if (> h h/source) h/source h)]
           [dc/section (send (make-object bitmap% w/section h/section) make-dc)]
           [dc/source (send source make-dc)])
      (draw-stamp/defect dc/source defect)
      (send dc/section draw-bitmap-section
            (send dc/source get-bitmap)
            0 0
            x/section y/section w/section h/section)
      (send dc/section get-bitmap))))

;取得缺陷片段尺寸值（x,y,w,h）：
(define (get-defect-section-size defect)
  (case (node-defect-stamp-type defect)
    [("方形") (get-defect/square-section-size defect)]
    [("矩形") (get-defect/rectangle-section-size defect)]
    [("圆形") (get-defect/circle-section-size defect)]))

;取得方形缺陷片段尺寸值：
(define (get-defect/square-section-size defect)
  (let* ([stamp (node-defect-stamp defect)]
         [x/stamp (hash-ref stamp 'x)]
         [y/stamp (hash-ref stamp 'y)]
         [w/stamp (* (hash-ref stamp 'half-width) 2)]
         [h/section (if (> (* w/stamp 3) 450)
                        (* w/stamp 3)
                        450)]
         [w/section (if (> (* w/stamp 3) 450)
                        (exact-floor (/ h/section 0.618))
                        800)]
         [x/section (exact-floor (- x/stamp (/ w/section 2)))]
         [y/section (exact-floor (- y/stamp (/ h/section 2)))])
    (values x/section y/section w/section h/section)))

;取得圆形缺陷片段尺寸值：
(define (get-defect/circle-section-size defect)
  (let* ([stamp (node-defect-stamp defect)]
         [x/stamp (hash-ref stamp 'x)]
         [y/stamp (hash-ref stamp 'y)]
         [w/stamp (* (hash-ref stamp 'half-width) 2)]
         [h/section (if (> (* w/stamp 3) 450)
                        (* w/stamp 3)
                        450)]
         [w/section (if (> (* w/stamp 3) 450)
                        (exact-floor (/ h/section 0.618))
                        800)]
         [x/section (exact-floor (- x/stamp (/ w/section 2)))]
         [y/section (exact-floor (- y/stamp (/  h/section 2)))])
    (values x/section y/section w/section h/section)))

;取得矩形缺陷片段尺寸值：
(define (get-defect/rectangle-section-size defect)
  (let* ([stamp (node-defect-stamp defect)]
         [x (hash-ref stamp 'x)]
         [y (hash-ref stamp 'y)]
         [wh (hash-ref stamp 'half-width)]
         [w (hash-ref stamp 'width)]
         [a (hash-ref stamp 'rotation)]
         ;以下角度计算中由于绘图坐标为反向，故需取负值（a已经取过负）：
         [x1 (+ x (* wh (sin (- (* pi 3/2) a))))]
         [y1 (+ y (* wh (cos (- (* pi 3/2) a))))]
         [x2 (+ x (* w (cos (- a))) (* wh (sin (- (* pi 3/2) a))))]
         [y2 (+ y (* w (sin (- a))) (* wh (cos (- (* pi 3/2) a))))]
         [x3 (+ x (* w (cos (- a))) (* wh (sin (- (* pi 1/2) a))))]
         [y3 (+ y (* w (sin (- a))) (* wh (cos (- (* pi 1/2) a))))]
         [x4 (+ x (* wh (sin (- (* pi 1/2) a))))]
         [y4 (+ y (* wh (cos (- (* pi 1/2) a))))]
         [x/min (min x1 x2 x3 x4)]
         [x/max (max x1 x2 x3 x4)]
         [y/min (min y1 y2 y3 y4)]
         [y/max (max y1 y2 y3 y4)]
         [w/stamp (exact-floor (- x/max x/min))]
         [h/stamp (exact-floor (- y/max y/min))]
         [check-w/stamp-3/2
          (lambda ()
            (if (> (* w/stamp 3/2) 800)
                (* w/stamp 3/2)
                800))]
         [check-w/stamp-3/2-h/stamp-n
          (lambda (n)
            (if (> (* w/stamp 3/2)
                   (* h/stamp n) 0.618)
                (* w/stamp 3/2)
                (/ (* h/stamp n) 0.618)))]
         [check-h/stamp-n
          (lambda (n)
            (if (> (* h/stamp n) 450)
                (if (> (/ (* h/stamp n) 0.618) 800)
                    (check-w/stamp-3/2-h/stamp-n n)
                    (check-w/stamp-3/2))
                (check-w/stamp-3/2)))]
         [w/section (exact-floor
                     (cond
                       [(and (< (/ h/stamp w/stamp) 0.618)
                             (>= (/ h/stamp w/stamp) (* 0.618 1/2)))
                        (check-h/stamp-n 2)]
                       [(< (/ h/stamp w/stamp) (* 0.618 1/2))
                        (check-h/stamp-n 3)]
                       [else
                        (if (> (* h/stamp 3/2) 450)
                            (if (> (/ (* h/stamp 3/2) 0.618) 800)
                                (/ (* h/stamp 3/2) 0.618) 800)
                            800)]))]
         [h/section (exact-floor (* w/section 0.618))]
         [x/section (exact-floor (- x/min (* (- w/section w/stamp) 1/2)))]
         [y/section (exact-floor (- y/min (* (- h/section h/stamp) 1/2)))])
    (values x/section y/section w/section h/section)))
