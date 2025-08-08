#lang racket
;create-view-data.rkt
;创建视图的通用函数。

(provide create-dialog
         create-separator-line/horizontal
         create-separatot-line/vertical
         create-pane/buttons
         create-text-button
         create-menu
         create-menu-item
         create-menu-separator
         create-toolbar
         create-toolbar-button)

(require racket/gui)

;创建对话框：
(define (create-dialog title parent)
  (new dialog%
       [label title]
       [parent parent]
       [border  5]))

;创建横向分隔板：
(define (create-separator-line/horizontal parent)
  (new panel%
       [parent parent]
       [style (list 'border)]
       [border 0]
       [min-height 1]
       [stretchable-height #f]))

;创建竖向分隔板：
(define (create-separatot-line/vertical parent)
  (new panel%
       [parent parent]
       [style (list 'border)]
       [horiz-margin 3]
       [min-width 1]
       [stretchable-width #f]))

;创建按钮的窗格：
(define (create-pane/buttons parent)
  (new horizontal-pane%
       [parent parent]
       [alignment (list 'center 'center)]
       [stretchable-height #f]))

;创建文本命令按钮：
(define (create-text-button title parent proc)
  (new button%
       [label title]
       [parent parent]
       [callback
        (lambda (button event)
          (proc))]))

;创建菜单：
(define (create-menu title parent)
  (new menu%
       [label title]
       [parent parent]))

;创建菜单项的宏：
(define-syntax-rule (create-menu-item name p l proc)
  (set! name
  (new menu-item%
       [label l]
       [parent p]
       [callback (lambda (item event)
                   (proc))])))

;创建菜单分隔线：
(define (create-menu-separator parent)
  (new separator-menu-item%
       [parent parent]))

;创建工具栏：
(define (create-toolbar parent)
  (new horizontal-panel%
       [parent parent]
       [alignment (list 'left 'top)]
       [style (list 'border)]
       [min-height 0]
       [stretchable-height #f]))

;创建工具栏按钮的宏：
(define-syntax-rule (create-toolbar-button n p l proc)
  (set! n
    (new button%
         [label l]
         [parent p]
         [callback (lambda ( button event)
                     (proc))])))