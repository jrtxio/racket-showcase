;test.rkt
;测试

#lang racket/gui

;创建界面主框架：
(define main-frame
  (new frame%
       [label "拼图"]
       [width 1000]
       [height 600]))

(send main-frame show #t)

;定义退出按钮：
(define button/exit
  (new button%
       [parent main-frame]
       [label "退出"]
       ;按钮点击的回调函数：
       [callback
        (lambda (button event)
          (send main-frame on-exit))]))