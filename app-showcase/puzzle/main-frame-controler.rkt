;main-frame-controler.rkt
;主框架视图控制器

#lang racket/gui

(require "puzzle-model.rkt"
         "main-frame-ids.rkt")

(provide
 canvas/target-callback
 button/exit-callback
 text/row-callback
 text/col-callback
 choice/r&c-callback
 button/select-picture-callback
 button/blend-cells-callback
 button/play-again-callback
 init-main-frame
 show-main-frame
 msg-mouse-pos
 msg-string
 )

;;控件响应函数=================================
;目标画布响应函数：
(define (canvas/target-callback control dc)
  (draw-target-picture dc))

;退出按钮响应函数：
(define (button/exit-callback control event)
  (send main-frame on-exit))

;text/row控件响应函数：
(define (text/row-callback control event)
  (text/r&c-callback/in control "行"))

;text/col控件响应函数：
(define (text/col-callback control event)
  (text/r&c-callback/in control "列"))

;行列控件响应函数模板：
(define-syntax-rule (text/r&c-callback/in control r/c)
  (let [(v (string->number (send control get-value)))]
    (cond
      [(not v)
       (send control set-value "")
       (msg-string
        (format "输入错误！~a数应该是数字。" r/c))]
      [(> v 0)
       (msg-string (format "设置为~a~a。" v r/c))
       (unless (= (send choice/r&c get-selection) 0)
         (send choice/r&c set-selection 0))]
      [else
       (send control set-value "")
       (msg-string
        (format "输入错误！~a必须大于0。" r/c))])))

;choice/r&c行列选择框响应函数：
(define (choice/r&c-callback control event)
  (let ([i (send control get-selection)])
    (case i
      [(1) (set-r&c-text-value 3 3)]
      [(2) (set-r&c-text-value 5 5)]
      [(3) (set-r&c-text-value 8 8)]
      [(4) (set-r&c-text-value 10 10)]
      [(5) (set-r&c-text-value 13 13)]
      [(6) (set-r&c-text-value 15 15)])))

;设置行/列text-field%控件值：
(define (set-r&c-text-value r c)
  ;设置行/列控件值：
  (send text/row set-value (number->string r))
  (send text/col set-value (number->string c))
  (msg-string (format "设置为~a行，~a列。" r c)))

;显示鼠标坐标：
(define (msg-mouse-pos x y)
  (send message/status set-label
        (format "当前光标位置：x=~a，y=~a" x y)))

;显示子串信息：
(define (msg-string str)
  (send message/status set-label str))

;button/select-picture选择图片按钮响应函数：
(define (button/select-picture-callback control event)
  ;根据用户输入取得源图片：
  (let* ([extension "*.jp*g;*.png"]
         [filters '(("JPG图片" "*.jpg")
                    ("JPEG图片" "*.jpeg")
                    ("PNG图片" "*.png"))]
         [in (get-file "请选择你需要的图片"
                        main-frame
                        (current-directory)
                        #f
                        extension
                        null
                        filters)])
    (get-source-picture in)
    (adjust-picture-size))
  ;重置拼图：
  (reset-puzzle-picture)
  ;绘制目标图片：
  (draw-target-picture (send canvas/target get-dc))
  ;绘制拼图图片：
  (draw-puzzle-picture (send canvas/puzzle get-dc))
  ;设置拼图画布重玩：
  (send canvas/puzzle replay))

;button/blend-cells混合图片按钮响应函数：
(define (button/blend-cells-callback control event)
  (when (setted-r&c-value?)
    #|
    ;初始化宽高：
    (set-picture-size! (send canvas/puzzle get-width)
                       (send canvas/puzzle get-height))
    ;初始化行列数：
    (string->number (send text/row get-value))
    (string->number (send text/col get-value))
    |#
    ;重置混合图片：
    (reset-puzzle-picture)
    ;绘制拼图图片：
    (draw-puzzle-picture (send canvas/puzzle get-dc))
    ;设置拼图画布重玩：
    (send canvas/puzzle replay)))

;判断设置行列数是否成功，如果不成功返回#f：
(define (setted-r&c-value?)
  (let ([r (string->number (send text/row get-value))]
        [c (string->number (send text/col get-value))])
    (cond
      [(and (and r c) (> r 0) (> c 0))
       (set-rows! r)
       (set-cols! c)
       #t]
      [(and (not r) (not c))
       (msg-string "行数及列数必须是数字。")
       #f]
      [(and (<= r 0) (<= c 0))
       (msg-string "行数及列数必须是大于0的数字。")
       #f]
      [(not r)
       (msg-string "行数必须是数字。")
       #f]
      [(not c)
       (msg-string "列数必须是数字。")
       #f]
      [(<= r 0)
       (msg-string "行数必须是大于0的数字。")
       #f]
      [(<= c 0)
       (msg-string "列数必须是大于0的数字。")
       #f])))
                        
;button/play-again再来一次按钮响应函数：
(define (button/play-again-callback control event)
  (recover-puzzle-picture)
  ;重新显示混合图片：
  (send canvas/puzzle refresh-now draw-puzzle-picture)
  (send canvas/puzzle replay)
  (msg-string "再来一次！"))

;;==============================================
;初始化主界面：
(define (init-main-frame)
  ;初始化宽高：
  (set-picture-size! (send canvas/puzzle get-width)
                     (send canvas/puzzle get-height))
  ;初始化行列数：
  (set-rows! (string->number (send text/row get-value)))
  (set-cols! (string->number (send text/col get-value)))
  ;初始化拼图数据：
  (init-puzzle-data)
  ;绘制目标图片：
  (set-target-size);设置目标图片画布尺寸
  (draw-target-picture (send canvas/target get-dc))
  ;绘制拼图图片：
  (draw-puzzle-picture (send canvas/puzzle get-dc))
  ;设置拼图画布重玩：
  (send canvas/puzzle replay))

;设置目标图片画布尺寸：
(define (set-target-size)
  (let* ([w/p (send canvas/puzzle get-width)]
         [h/p (send canvas/puzzle get-height)]
         [w/t (send canvas/target get-width)]
         [h/t (floor (* h/p (/ w/t w/p)))])
    (send canvas/target min-height h/t)))

;显示主界面：
(define (show-main-frame)
  (send main-frame show #t))
