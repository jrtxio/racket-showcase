;main-frame-ids.rkt
;主框架视图控件标识。

#lang racket

(provide set-main-frame
         main-frame
         set-canvas/target
         canvas/target
         set-canvas/puzzle
         canvas/puzzle
         set-button/select-picture
         button/select-picture
         set-button/blend-cells
         button/blend-cells
         set-button/play-again
         button/play-again
         set-button/exit
         button/exit
         set-text/row
         text/row
         set-text/col
         text/col
         set-choice/r&c
         choice/r&c
         set-message/status
         message/status)

;视图控件标识:
(define main-frame void);主框架
(define canvas/target void);目标图片画布
(define canvas/puzzle void);拼图画布
(define button/select-picture void);选择图片按钮
(define button/blend-cells void);混合拼图按钮
(define button/play-again void);再来一次按钮
(define button/exit void);退出按钮
(define text/row void);行数设置按钮
(define text/col void);列数设置按钮
(define choice/r&c void);行列数选择按钮
(define message/status void);信息状态显示条

;设置构件标识值：
(define (set-main-frame val) (set! main-frame val))
(define (set-canvas/target val) (set! canvas/target val))
(define (set-canvas/puzzle val) (set! canvas/puzzle val))
(define (set-button/select-picture val) (set! button/select-picture val))
(define (set-button/blend-cells val) (set! button/blend-cells val))
(define (set-button/play-again val) (set! button/play-again val))
(define (set-button/exit val) (set! button/exit val))
(define (set-text/row val) (set! text/row val))
(define (set-text/col val) (set! text/col val))
(define (set-choice/r&c val) (set! choice/r&c val))
(define (set-message/status val) (set! message/status val))
