#lang racket
;dialog-about.rkt
;关于对话框。

(provide show-dialog/about)

(require racket/gui)

(require "view-common.rkt")

;对话框控件：=======================================
(define dialog/about void) ;对话框

;操作对话框函数：===================================
;显示对话框（同时传入父框架窗口和对话框需调用回传的函数）：
(define (show-dialog/about parent)
  ;创建对话框：
  (set! dialog/about
        (create-dialog/about parent))
  ;显示对话框：
  (send dialog/about show #t))
  
;对话框响应函数：===================================
;响应确定按钮：
(define (on-click-button/ok)
  (send dialog/about on-exit))

;创建对话框结构的函数：==============================
;;;创建对话框：
(define (create-dialog/about parent)
  (let ([dialog (new dialog%
                     [label "关于"]
                     [parent parent]
                     [border  5]
                     [min-width 400]
                     [stretchable-width #f])])
    ;软件信息窗格：
    (create-pane/messages dialog)
    ;软件说明窗格：
    (create-pane/explain dialog)
    ;对话框按钮：
    (create-buttons dialog)
    dialog));返回对话框

;软件信息窗格：
(define (create-pane/messages parent)
  (let ([messages
         (new vertical-pane%
              [parent parent]
              [alignment (list 'center 'center)]
              [min-height 0]
              [stretchable-height #f])])
    (new message%
         [label "EVAgle"]
         [parent messages])
    (new message%
         [label "（架空输电线路无人机巡检影像数据处理软件）"]
         [parent messages])
    (new message%
         [label "版本号：V1.0"]
         [parent messages])
    (new message%
         [label "作者：张勇"]
         [parent messages])
    (new message%
         [label "微信号：kangzi_great"]
         [parent messages])
    (new message%
         [label "QQ号：346442406"]
         [parent messages])))

;软件说明窗格：
(define (create-pane/explain parent)
  (let ([explain
         (new pane%
              [parent parent])])
    (create-text/explain explain)))

;软件说明显示编辑框：
(define (create-text/explain parent)
  (let ([explain
         (new text-field%
              [label #f]
              [parent parent]
              [style (list 'multiple)]
              [enabled #f]
              [min-height 100]
              [stretchable-height #t])])
    (send explain set-value
          "本软件是根据《架空输电线路多旋翼无人机巡检影像拍摄指导手册》（国家电网有限公司2019年6月V2版）设计，同时参考了《缺陷等级划分》内容，实现了无人机电力线路巡检照片数据处理过程一体化、自动化，操作简单、方便。同时可一步完成响应图像文件命名转存、报告输出等，为数据处理标准化提供了强有力的工具。")
    explain));返回对象值

;;按钮窗格：
(define (create-buttons parent)
  (let ([pane (create-pane/buttons parent)])
    (create-text-button
     "确定" pane on-click-button/ok)))