;main-frame.rkt
;拼图GUI界面：
#lang racket/gui

(require "puzzle-model.rkt"
         "main-frame-ids.rkt"
         "main-frame-controler.rkt"
         "puzzle-canvas-class.rkt")

(provide create-main-frame)

;创建视图及布局=============================
;创建界面主框架：
(define (create-main-frame)
  (set-main-frame
   (new frame%
        [label "拼图"]
        [width 1000]
        [height 600]
        [style '(no-resize-border
                 no-system-menu)]))
  (create-layout)
  )

;创建布局：
(define (create-layout)
  (let ([hp (new horizontal-pane%
                 [parent main-frame])])
    (create-left-layout hp)
    (create-right-layout hp)))

;创建左侧布局：
(define (create-left-layout p)
  (let ([vp (new vertical-panel%
                 [parent p]
                 [alignment '(center top)]
                 [min-width 300]
                 [stretchable-width #f])])
    ;创建目标图画布：
    (set-canvas/target
     (new canvas%
          [parent vp] 
          [horiz-margin 3]
          [paint-callback
           canvas/target-callback]))
    ;创建命令功能区：
    (create-command-area vp)
    ;创建状态显示区：
    (create-status-area vp)))

;创建命令功能区：
(define (create-command-area p)
  (let ([vp (new vertical-panel%
                 [parent p]
                 [alignment '(center top)])])
    ;创建行列设置组：
    (create-row&col-set-group vp)
    ;创建拼图命令按钮组：
    (create-puzzle-command-button-group vp)
    (set-button/exit
     (new button%
          [label "退出"]
          [parent vp]
          [callback button/exit-callback]))))

;创建行列设置组：
(define (create-row&col-set-group p)
  (let ([gp (new group-box-panel%
                 [label "拼图排列"]
                 [parent p]
                 [min-height 30]
                 [stretchable-height #f])])
    (let ([gp/hp (new horizontal-pane%
                      [parent gp])])
      (set-text/row
       (new text-field%
            [label "行数"]
            [parent gp/hp]
            [init-value "3"]
            [callback text/row-callback]))
      (set-text/col
       (new text-field%
            [label "列数"]
            [parent gp/hp]
            [init-value "3"]
            [callback text/col-callback])))
    (let ([gp/hp-r&c (new horizontal-pane%
                          [parent gp])])
      (set-choice/r&c
       (new choice%
            [label "行列"]
            [parent gp/hp-r&c]
            [stretchable-width #t]
            [choices '("自定义行列数"
                       "3行3列"
                       "5行5列"
                       "8行8列"
                       "10行10列"
                       "13行13列"
                       "15行15列")]
            [callback choice/r&c-callback])))))
  
;创建拼图命令按钮组：
(define (create-puzzle-command-button-group p)
  (let ([gp (new group-box-panel%
                 [label "拼图命令"]
                 [parent p]
                 [stretchable-height #f])])
    (create-button set-button/select-picture gp "选择..."
                   button/select-picture-callback)
    (create-button set-button/blend-cells gp "混合拼图"
                   button/blend-cells-callback)
    (create-button set-button/play-again gp "再来一次"
                   button/play-again-callback)))

;定义创建命令功能区按钮的宏：
(define-syntax-rule (create-button n p l prc)
  (n
   (new button%
        [label l]
        [parent p]
        [stretchable-width #t]
        [callback prc])))

;创建状态显示区：
(define (create-status-area p)
  (let ([vp (new vertical-panel%
                 [parent p]
                 [alignment '(left top)]
                 [min-height 30]
                 [stretchable-height #f])])
    (set-message/status
     (new message%
          [label "准备就绪。"]
          [parent vp]))))

;创建右侧布局：
(define (create-right-layout p)
  (let ([vp (new vertical-panel%
                 [parent p]
                 [style '(border)])])
    (set-canvas/puzzle
     (new puzzle-canvas%
          [parent vp]
          [paint-callback
           (lambda (c dc)
             (draw-puzzle-picture dc))]))))

;测试========================================
(module+ test
  (create-main-frame)
  (send main-frame show #t)
  (init-main-frame)
  #|
  ;取得混合的图片：
  (define in (build-path
              (current-directory)
              "test"
              "test.jpg"))
  (adjust-picture-size (read-bitmap in)
                     (send canvas/puzzle get-width)
                     (send canvas/puzzle get-height))
  (reset-puzzle-picture)
  (define dc (send canvas/puzzle get-dc))
  (draw-puzzle-picture dc)
  (draw-cell-to-x&y 127 77 3 dc)
|#
  )
