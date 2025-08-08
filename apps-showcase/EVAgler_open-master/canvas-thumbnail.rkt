#lang racket
;canvas-thumbnail.rkt
;缩略图画布。

(provide canvas/thumbnail%)

(require racket/gui)

(define canvas/thumbnail%
  (class canvas%
    (inherit get-dc
             get-width
             get-height
             min-height)
        
    (field [source (void)];初始化源图片
           [bitmap (void)];当前图片
           [scale 1];缩略图缩小倍数（相对于源图片）
           
           [width/focuse 0];焦点宽度
           [height/focuse 0];焦点高度
           [bx/focuse 0];焦点基点x值
           [by/focuse 0];焦点基点y值

           [clicked-flag #f];鼠标点击开关标志，#t焦点可移动

           [canvas/magnifier (void)];放大器画布
           [timer/draw (void)];绘图刷新延迟时钟
           [can-draw #f];是否可以绘图的标志
           [delay/timer 0];时钟间隔
           )
    
    ;设置源图片：
    (define/public (set-source pic)
      (set! source pic))
    
    ;设置当前图片:
    (define (set-bitmap)
      (set! bitmap (get-picture/thumbnail)))
            
    ;取得缩略图图片：
    (define (get-picture/thumbnail)
      (when (not (void? source))
        (let ([dc (send (make-object bitmap%
                          (get-width)
                          (get-height))
                        make-dc)])
          (send dc set-scale scale scale)
          (send dc draw-bitmap source 0 0)
          (send dc get-bitmap))))

    ;设置放大器画布:
    (define/public (set-canvas/magnifier canvas)
      (set! canvas/magnifier canvas))

    ;设置缩小倍数:
    (define (set-scale)
      (when (not (void? source))
        (set! scale
              (/ (get-width)
                 (send source get-width)))))
      
    ;取得缩小倍数:
    (define/public (get-scale)
      scale)
    
    ;取得缩略图高度:
    (define (set-height)
      (when (not (void? source))
        (min-height
         (exact-floor
          (* (send source get-height) scale)))))
    
    ;设置焦点宽度：
    (define (set-width/focus)
      (when (not (void? canvas/magnifier))
        (set! width/focuse
              (exact-floor
               (*
                (/
                 (send canvas/magnifier get-width)
                 (send canvas/magnifier get-scale/section))
                scale)))))

    ;返回width/focuse:
    (define/public (get-width/focuse)
      width/focuse)
    
    ;设置焦点高度:
    (define (set-height/focus)
      (set! height/focuse
            (exact-floor
             (*
              (/ (send canvas/magnifier get-height)
                 (send canvas/magnifier get-scale/section))
              scale))))

    ;返回height/focuse:
    (define/public (get-height/focuse)
      height/focuse)
    
    ;设置焦点基点x值：
    (define (set-bx/focuse! px)
      (set! bx/focuse
            (let ([w/2 (exact-floor (/ width/focuse 2))])
              (if (>= px w/2)
                  (if (<= px (- (get-width) w/2))
                      (- px w/2)
                      (- (get-width) width/focuse))
                  0))))

    ;返回bx/focuse：
    (define/public (get-bx/focuse)
      bx/focuse)

    ;设置焦点基点y值:
    (define (set-by/focuse! py)
      (set! by/focuse
            (let ([h/2 (exact-floor (/ height/focuse 2))])
              (if (>= py h/2)
                  (if (<= py (- (get-height) h/2))
                      (- py h/2)
                      (- (get-height) height/focuse))
                  0))))

    ;返回by/focuse:
    (define/public (get-by/focuse)
      by/focuse)
    
    ;重写on-event,处理鼠标事件：
    (define/override (on-event event)
      (cond
        [(send event button-up? 'left);左键单击
         (on-left-click)]
        [clicked-flag ;处理鼠标移动事件
         (on-move event)]))

    ;设置时钟间隔：
    (define/public (set-delay/timer delay)
      (set! delay/timer delay))

    ;处理左键单击事件：
    ;检查鼠标键up,设置clicked-flag值。
    (define (on-left-click)
      (if clicked-flag
          (begin
            (set! clicked-flag #f)
            (when (not (void? timer/draw));停止计时器
              (send timer/draw stop)))
          (begin
            (set! clicked-flag #t)
            (set! timer/draw ;创建计时器
                  (new timer%
                       [notify-callback
                        (lambda ()
                          (when (not can-draw)
                            (set! can-draw #t)))]
                       [interval delay/timer])))))

    ;处理鼠标移动事件：
    (define (on-move event)
      ;取得焦点基点值:
      (set-bx/focuse! (send event get-x))
      (set-by/focuse!(send event get-y))
      ;重绘画布内容:
      (on-paint)
      ;重绘放大器画布内容：
      (when can-draw
        (send canvas/magnifier on-paint)
        (set! can-draw #f)))

    ;绘制焦点：
    (define (draw-focuse)
      (let ([dc (get-dc)])
        (send dc set-pen "yellow" 2 'short-dash)
        (send dc set-brush "black" 'transparent)
        (send dc
              draw-rectangle
              bx/focuse by/focuse
              width/focuse height/focuse)))

    ;绘制缩略图：
    (define (darw-bitmap)
      (when (not (void? bitmap))
        (send (get-dc) draw-bitmap bitmap 0 0)))

    ;更新字段值：
    (define (update-field)
      (when (not (void? canvas/magnifier))
        (set-scale)
        (set-bitmap)
        (set-height)
        (set-width/focus)
        (set-height/focus)))
    
    ;重写on-paint方法：
    (define/override (on-paint)
      (update-field)
      (darw-bitmap)
      (draw-focuse))
    
    (super-new)
    ))