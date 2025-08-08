#lang racket
;canvas-magnifier.rkt
;放大器画布。

(provide canvas/magnifier%)

(require racket/gui)
(require "defect-draw-data.rkt"
         "app-data.rkt")

(define canvas/magnifier%
  (class canvas%
    (inherit get-dc
             get-width
             get-height
             set-cursor)
    
    (field [source (void)];初始化源图片
           [bitmap (void)];当前图片
           [scale/refer #f];参考比例
           [scale/section 1];片段比例

           [canvas/thumbnail (void)];缩略图画布
           
           [timer/draw (void)];绘图刷新延迟时钟
           [can-draw #f];是否可以绘图的标志
           [delay/timer 0];时钟间隔
           )

     ;设置源图片:
    (define/public (set-source pic)
      (set! source pic))
    
    ;设置当前图片：
    (define (set-bitmap)
      (set! bitmap (get-picture/magnifier))
      ;将片段基点值传递给缺陷标记:
      (set-stamp-base (get-bx/section)
                      (get-by/section)))

    ;设置缩略图画布:
    (define/public (set-canvas/thumbnail canvas)
      (set! canvas/thumbnail canvas))

    ;设置参考比例：
    (define/public (set-scale/refer refer)
      (set! scale/refer refer))
    
    ;设置片段比例：
    (define (set-scale/section)
      (if (not scale/refer)
          (set! scale/section 1) ;保持源图
          (set! scale/section
                (let* ([w/canvas (get-width)]
                       [h/canvas (get-height)]
                       [w/source (send source get-width)]
                       [h/source (send source get-height)]
                       [sc/w/c-s (/ w/canvas w/source)]
                       [sc/h/c-s (/ h/canvas h/source)]
                       [scale (if (> sc/w/c-s sc/h/c-s)
                                  sc/w/c-s sc/h/c-s)]
                       [v/canvas (if (> sc/w/c-s sc/h/c-s)
                                     w/canvas h/canvas)]
                       [v/source (if (> sc/w/c-s sc/h/c-s)
                                     w/source h/source)])
                  (- 1
                     (* (- 1
                           (/ v/canvas v/source))
                        scale/refer))))))

     ;取得片段比例:
    (define/public (get-scale/section)
      scale/section)
    
    ;取得源图片片段基点x值:
    (define (get-bx/section)
      (exact-floor
       (/ (send canvas/thumbnail get-bx/focuse)
          (send canvas/thumbnail get-scale))))

    ;取得源图片片段基点y值:
    (define (get-by/section)
      (exact-floor
       (/ (send canvas/thumbnail get-by/focuse)
          (send canvas/thumbnail get-scale))))
    
    ;取得源图片片段宽度:
    (define (get-width/section)
      (exact-floor
       (/ (send canvas/thumbnail get-width/focuse)
          (send canvas/thumbnail get-scale))))

    ;取源图片片段高度:
    (define (get-height/section)
      (exact-floor
       (/ (send canvas/thumbnail get-height/focuse)
          (send canvas/thumbnail get-scale))))
    
    ;取得放大器图片（放大器图片按1:1直接拷贝自源图片）:
    (define (get-picture/magnifier)
      (let ([dc/magnifier
             (send (make-object bitmap%
                     (get-width)
                     (get-height))
                   make-dc)])
        (let-values ([(x/s y/s)
                      (send dc/magnifier get-scale)])
          (send dc/magnifier set-scale
                scale/section scale/section)
          ;截取并绘制位图：
          (send dc/magnifier
                draw-bitmap-section
                source 0 0
                (get-bx/section) (get-by/section)
                (get-width/section) (get-height/section))
          (send dc/magnifier set-scale x/s y/s))
        ;绘制缺陷标记：
        (draw-stamp/defects dc/magnifier)
        ;返回位图：
        (send dc/magnifier get-bitmap)))
    
    ;更新字段值:
    (define (update-field)
      (set-scale/section)
      (set-stamp-scale scale/section) ;为缺陷标志设置比例值
      (when (not (void? canvas/thumbnail))
        (set-bitmap)))

    ;绘制图片：
    (define (draw-bitmap dc)
      (when (not (void? bitmap))
        (send dc draw-bitmap bitmap 0 0))
      (void))
      
    ;重写on-paint：
    (define/override (on-paint)
      (let ([dc (get-dc)])
        ;更新字段值:
        (update-field)
        ;绘制图片：
        (draw-bitmap dc)))
        
    ;重写on-size：
    (define/override (on-size width height)
      (when (not (void? canvas/thumbnail))
        (send canvas/thumbnail on-paint))
      (set! delay/timer ;绘图并取得绘图所花时间
            (get-time (on-paint)))
      (when (not (void? canvas/thumbnail)) ;设置缩略图画布时钟间隔
        (send canvas/thumbnail set-delay/timer delay/timer)))

    ;重写on-event，处理鼠标事件：
    (define/override (on-event event)
      (case (send event get-event-type)
        [(left-down);按下鼠标左键
         (when (interaction-flag)
           (begin
             (set-stamp-point (send event get-x)
                              (send event get-y))
             ;进行下一个交互过程：
             (increase-interaction-flag)
             (if (message-end?)
                 (begin
                   (end-stamp-interaction) ;结束交互
                   (set-cursor (make-object cursor% 'arrow));恢复光标样式
                   (when (not (void? bitmap))
                     (draw-current-stamp/defect ;绘制当前缺陷标记
                      (send bitmap make-dc))
                     (on-paint)))
                 ;进行下一个交互提示：
                 (status-message
                  (get-interaction-message)))))]
        [(motion);移动鼠标
         (when interaction-flag
           (set-stamp-point (send event get-x)
                            (send event get-y))
           (when can-draw
             (on-paint)
             (draw-temp-stamp (get-dc))
             (set! can-draw #f)))]
        [(enter) ;进入画布
         (cond
           [(interaction-flag)
            (set-cursor (make-object cursor% 'cross)) ;改变鼠标样式
            (set! timer/draw
                  (new timer% ;创建时钟
                       [notify-callback
                        (lambda ()
                          (when (not can-draw)
                            (set! can-draw #t)))]
                       [interval delay/timer]))]
           [else
            (set-cursor (make-object cursor% 'arrow))])]
        [(leave) ;离开画布
         (set-cursor (make-object cursor% 'arrow));恢复光标样式
         (when (not (void? timer/draw));停止计时器
           (send timer/draw stop))]))
    
    
    ;重写on-char处理键盘事件：
    (define/override (on-char event)
      (when (and (interaction-flag)
                 (eq? (send event get-key-code)
                      'escape)) ;按Esc键放弃绘图
        ;终止绘图交互：
        (abort-stamp-interaction)
        ;重绘图形：
        (on-paint)))
    
    (super-new)
))
    