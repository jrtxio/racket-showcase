#lang racket
;dialog-node-set.rkt
;节点名设置对话框。

(provide show-dialog/node/set)

(require racket/gui)

(require "app-data.rkt"
         "view-common.rkt"
         "task-struct-data.rkt"
         "tower-type-data.rkt")

;父窗口回调函数=====================================
(define callback/parent void) ;回调函数
(define current-picture-id #f) ;当前图片序号

;对话框控件：=======================================
(define dialog/node/set void) ;对话框
(define canvas/type void) ;节点图片画布
(define list/types void) ;节点类型列表
(define text/type void) ;节点类型编辑框

;操作对话框函数：===================================
;显示对话框（同时传入父框架窗口和对话框需调用回传的函数）：
(define (show-dialog/node/set parent callback id/picture)
  (set! current-picture-id id/picture) ;传入当前使用的图片序号
  (set! callback/parent callback) ;回调函数
  ;创建对话框：
  (set! dialog/node/set
        (create-dialog/node/set parent))
  ;初始化对话框:
  (init-dialog)
  ;显示对话框:
  (send dialog/node/set show #t))

;初始化设置任务对话框:
(define (init-dialog)
  (let ([tp (task-tower-type)])
    (when (non-empty-string? tp)
      ;设置列表:
      (send list/types set
            (type/tower-node-ids tp)
            (type/tower-node-tags tp)
            (type/tower-node-names tp))
      ;设置列表列宽：
      (send list/types
            set-column-width 2 150 150 200)
      ;设置初始选择项：
      (send list/types set-selection
            (get-list/types-selection))
      ;设置图片：
      (draw-picture-to-canvas
       canvas/type
       (get-type/tower-bitmap-path (task-tower-type))))))

;根据源图片列表序号取得对应节点名在当前塔类型节点名列表的序号:
(define (get-list/types-selection)
  (let* ([node-name
          (get-node-name current-picture-id)]
         [node-names
          (type/tower-node-names (task-tower-type))]
         [index (index-of node-names node-name)])
    (if index index 0)))

;在画布上绘制塔图片：
(define (draw-tower-picture)
  (when (non-empty-string? (task-tower-type))
    (draw-picture-to-canvas
     canvas/type
     (get-type/tower-bitmap-path (task-tower-type)))))

;对话框响应函数：===================================
;响应list/types事件:
(define (on-event-list/types)
  (when (send list/types get-selection)
      (let ([node-name
             (list-ref
              (type/tower-node-names (task-tower-type))
              (send list/types get-selection))])
      (send text/type set-value node-name))))

;单击图片按钮:
(define (on-click-button/picture)
  (draw-tower-picture))

;单击确定按钮，保存选择的节点名称:
(define (on-click-button/ok)
  (let ([sel/type (send list/types get-selection)]
        [name/node (send text/type get-value)])
    (if sel/type
        (when (not (void? callback/parent))
          (callback/parent name/node) ;用回调函数传回主窗口设置值
          (status-message
           (string-append
            "已经为"
            (picture-source-name
             (get-picture-by-source-id current-picture-id))
            "设置节点名:" name/node)))
        (status-message
         (string-append
          "未能对"
          (picture-source-name
           (get-picture-by-source-id current-picture-id))
          "设置节点名。"))))
  (send dialog/node/set on-exit))

;单击放弃按钮:
(define (on-click-button/cancel)
  (when (not (void? callback/parent))
    (callback/parent "")) ;用回调函数传回值
  (send dialog/node/set on-exit))

;创建对话框结构的函数：==============================
;;;创建对话框：
(define (create-dialog/node/set parent)
  (let ([dialog (create-dialog "设置节点名" parent)])
    ;创建工作区：
    (create-pane/work dialog)
    dialog)) ;返回对话框对象

;;;对话框工作区窗格：
(define (create-pane/work parent)
  (let ([work (new horizontal-pane%
                   [parent parent])])
    (create-pane/left work)
    (create-pane/right work)))

;;对话框左窗格:
(define (create-pane/left parent)
  (let ([left 
         (new pane%
              [parent parent]
              [min-width 600]
              [stretchable-width #f])])
    (set! canvas/type (create-type/canvas left))))

;塔类型图片显示画布：
(define (create-type/canvas parent)
  (new canvas%
       [parent parent]
       [style (list 'border 'no-autoclear)]
       [paint-callback
        (lambda (canvas dc)
          (draw-tower-picture))]))

;;对话框右窗格：
(define (create-pane/right parent)
  (let ([right (new vertical-pane%
       [parent parent]
       [min-width 260]
       [stretchable-width #f])])
    (set! list/types (create-list/types right))
    (set! text/type (create-text/type right))
    (create-buttons right)))

;塔类型节点列表：
(define  (create-list/types parent)
  (new list-box%
       [label "杆塔节点名称："]
       [choices null]
       [parent parent]
       [min-height 400]
       [stretchable-height #f]
       [style (list 'single
                    'vertical-label
                    'variable-columns
                    'column-headers)]
       [columns (list "序号" "节点标志" "节点名称")]
       [callback (lambda (c e)
                   (on-event-list/types))]))

;塔类型节点名称编辑框：
(define (create-text/type parent)
  (new text-field%
       [label #f]
       [parent parent]))

;创建按钮区：
(define (create-buttons parent)
  (let ([buttons (create-pane/buttons parent)])
    (create-text-button
     "显示图片" buttons on-click-button/picture)
    (create-text-button
     "确定" buttons on-click-button/ok)
    (create-text-button
     "清空" buttons on-click-button/cancel)))
