#lang racket
;dilaog-task-set.rkt
;任务设置对话框。

(provide show-dialog/task/set)

(require racket/gui)

(require "app-data.rkt"
         "view-common.rkt"
         "task-struct-data.rkt"
         "tower-type-data.rkt")

;父窗口回调函数=====================================
(define callback/parent void)

;对话框控件：=======================================
(define dialog/task/set void) ;对话框
(define canvas/task void) ;全塔照片
(define canvas/type void) ;塔型图片
(define text/source void) ;图片源文件夹
(define button/open void) ;打开源文件夹按钮
(define text/path void) ;任务文件夹路径
(define combo/type void) ;塔杆类型
(define combo/voltage void) ;电压等级
(define combo/line void) ;线路名称
(define combo/tower void) ;杆塔号
(define combo/company void) ;检查公司
(define combo/date void) ;检查日期
(define message/bar void) ;信息显示条
(define button/picture void) ;图片按钮
(define button/ok void) ;确定按钮
(define button/cancel void) ;放弃按钮

;操作对话框函数：===========================================
;显示对话框（同时传入父框架窗口和对话框需调用回传的函数）：
(define (show-dialog/task/set parent callback)
  (set! callback/parent callback)
  ;
  (set! dialog/task/set (create-dialog/task/set parent))
  ;初始化设置任务对话框:
  (init-dialog)
  ;显示设置任务对话框:
  (send dialog/task/set show #t))

;初始化设置任务对话框：
(define (init-dialog)
  ;如果是新任务,激活路径控件以供设置
  (if (or (void? (task-task-path))
          (not (non-empty-string? (task-source-folder))))
      (enable-controls-about-path #t)
      (enable-controls-about-path #f))
  ;设置任务路径:
  (let ([path (task-task-path)]
        [folder (task-source-folder)])
    (if (not (path? path))
        (begin
          (send text/path set-value "")
          (send text/source set-value ""))
        (begin
          (send text/path
                set-value
                (path->string path))
          ;设置源文件夹:
          (if (not (non-empty-string? folder))
              (send text/source set-value "")
              (send text/source set-value folder)))))
  ;设置杆塔类型：
  (let ([type (task-tower-type)])
    (if (not (non-empty-string? type))
        (send combo/type set-value "")
        (send combo/type set-value type)))
  ;设置电压等级:
  (let ([voltage (task-voltage-grade)])
    (if (not (non-empty-string? voltage))
        (send combo/voltage set-value "")
        (send combo/voltage set-value voltage)))
  ;设置线路名称:
  (let ([line (task-line-name)])
    (if (not (non-empty-string? line))
        (send combo/line set-value "")
        (send combo/line set-value line)))
  ;设置杆塔号:
  (let ([number (task-tower-number)])
    (if (not (non-empty-string? number))
        (send combo/tower set-value "")
        (send combo/tower set-value number)))
  ;设置公司名称:
  (let ([company (task-company-name)])
    (if (not (non-empty-string? company))
        (send combo/company set-value "")
        (send combo/company set-value company)))
  ;设置检查日期:
  (let ([date (task-inspect-date)])
    (if (not (non-empty-string? date))
        (send combo/date set-value "")
        (send combo/date set-value date))))

;设置任务参数：
(define (set-task-property)
  ;设置杆塔图片列表（该项必须在前，后边会对field进行更改）：
  (set-tower-pictures-property)
  ;设置其他属性：
  (task-source-folder ;设置源文件夹
   (send text/source get-value))
  (task-task-path ;设置任务路径
   (string->path
    (send text/path get-value)))
  (task-tower-type ;设置塔类型
   (send combo/type get-value))
  (task-voltage-grade ;设置电压等级
   (send combo/voltage get-value))
  (task-line-name ;设置线路名称
   (send combo/line get-value))
  (task-tower-number ;设置杆塔号
   (send combo/tower get-value))
  (task-company-name ;设置公司名称
   (send combo/company get-value))
  (task-inspect-date ;设置检查日期
   (send combo/date get-value)))

;根据对话框修改情况设置杆塔图片列表:
(define (set-tower-pictures-property)
  (when (not (void? (task-object)))
    ;为新建项目,设置图片列表:
    (when (or (void? (task-task-path))
              (not (non-empty-string?
                    (task-source-folder))))
      (task-tower-pictures
       (get-pictures-by-filenames
        (directory-list
         (build-path
          (send text/path get-value)
          (send text/source get-value))))))))

;显示任务全塔图片(第一张图片,全塔):
(define (show-task-tower-picture)
  (let ([p (send text/path get-value)]
        [f (send text/source get-value)])
    (when (and (non-empty-string? p)
               (non-empty-string? f))
      (let ([pf (build-path p f)])
        (draw-picture-to-canvas
         canvas/task
         (build-path pf
                     (car (directory-list pf))))))))

;显示杆塔类型图片:
(define (show-tower-type-picture)
  (let ([tp (send combo/type get-value)])
    (when (non-empty-string? tp)
      (draw-picture-to-canvas
       canvas/type
       (get-type/tower-bitmap-path tp)))))

;激活对话框路径相关控件：
(define (enable-controls-about-path enabled)
  (send text/source enable enabled)
  (send button/open enable enabled)
  (send text/path enable enabled))

;检查任务属性结构的关键值(初创时)是否已经就位:
(define (task-ok?)
  (and
   (non-empty-string?
    (send text/source get-value));源文件夹名
   (non-empty-string?
    (send text/path get-value));工作文件路径
   (non-empty-string?
    (send combo/type get-value))));塔杆类型名称

;设置任务文件路径相关信息：
(define (set-dialog-path path)
  (let-values ([(path/task folder/source result)
                (split-path path)])
    ;设置任务文件路径:
    (send text/path
          set-value
          (path->string path/task))
    ;设置源文件夹名称：
    (send text/source
          set-value
          (path->string folder/source))))

;绘制全塔图片和塔类型图片：
(define (draw-task-and-type-picture)
  (show-task-tower-picture) ;显示任务全塔图片
  (show-tower-type-picture)) ;显示塔类型图片

;对话框响应函数：==========================================
;响应显示图片按钮：
(define (on-click-button/picture)
  (draw-task-and-type-picture))

;单击确定按钮：
(define (on-click-button/ok)
  (if (task-ok?)
      (begin
        (set-task-property) ;设置任务属性
        (status-message "任务属性设置完成。")
        (callback/parent) ;初始化框架视图
        (send dialog/task/set on-exit) ;退出对话框
        )
      (status-message "请检查并完善工作文件夹选择及塔杆类型选择。"
                      message/bar)))

;单击放弃按钮：
(define (on-click-button/cancel)
  (callback/parent)
  (send dialog/task/set on-exit) ;退出对话框
  (status-message "放弃，任务属性设置未保存更新。")
  )

;打开图片源文件夹按钮：
(define (on-click-button/open)
  ;取得完整路径：
  (let ([path
         (get-directory "选择图片源文件夹"
                        dialog/task/set)])
    (when (path? path)
      (set-dialog-path path);设置对话框路径相关信息
      (show-task-tower-picture))));设置任务塔全身图片

;处理combo/type回调：
(define (on-field-combo/type)
  (show-tower-type-picture));绘制塔类型图片

;创建对话框结构的函数：=====================================
;;创建对话框：
(define (create-dialog/task/set parent)
  (let ([dialog (create-dialog "设置任务" parent)])
    ;创建图片显示区：
    (create-pane/pictures dialog)
    ;创建分隔：
    (create-separator-line/horizontal dialog)
    ;创建工作区：
    (create-pane/work dialog)
    ;创建信息：
    (create-message-bar dialog)
    ;创建按钮区：
    (create-buttons dialog)
    dialog));返回对话框

;;对话框图片显示区：----------------------------------------
(define (create-pane/pictures parent)
  (let ([pictures
         (new horizontal-pane%
              [parent parent]
              [alignment (list 'center 'center)])])
    (set! canvas/task
          (create-canvas
           "全塔照片"
           (create-pane/picture pictures)))
    (set! canvas/type
          (create-canvas
           "塔型图片"
           (create-pane/picture pictures)))))

;创建图片显示区：
(define (create-pane/picture parent)
  (new vertical-pane%
       [parent parent]
       [alignment (list 'center 'bottom)]
       [border 3]
       [min-width 300]
       [stretchable-width #f]))

;创建图片画布：
(define (create-canvas title parent)
  ;创建画布：
  (define canvas
    (new canvas%
         [parent parent]
         [style (list 'border
                      'no-autoclear)]
         [min-height 200]
         [stretchable-height #f]
         [paint-callback
          (lambda (canvas dc) ;显示比较慢，暂不考虑即时显示图片
            #|(draw-task-and-type-picture)|# void)]))
  ;创建标签：
  (new message%
       [label title]
       [parent parent])
  canvas)

;;对话框工作区：------------------------------------------
(define (create-pane/work parent)
  (let ([pane/work
         (new vertical-pane%
              [parent parent]
              [alignment (list 'center 'top)]
              [horiz-margin 20])])
    ;创建各工作字段行：
    (create-work/control/source pane/work)
    (create-work/control/path pane/work)
    (set! combo/type
     (create-control/combo
      "塔杆类型:"
      on-field-combo/type
      (type/tower-type-names)
      pane/work))
    (set! combo/voltage
          (create-control/combo "电压等级：" void null pane/work))
    (set! combo/line
     (create-control/combo "线路名称：" void null pane/work))
    (set! combo/tower
          (create-control/combo "杆塔号：" void null pane/work))
    (set! combo/company
          (create-control/combo "检查公司：" void null pane/work))
    (set! combo/date
     (create-control/combo "检查日期：" void null pane/work))))

;创建控件容器行窗格:
(define (create-work/pane/row parent)
  (new horizontal-pane%
       [parent parent]
       [alignment (list 'center 'center)]
       [min-height 30]
       [stretchable-height #f]))

;创建控件标题窗格:
(define (create-work/pane/title parent)
  (new pane%
       [parent parent]
       [alignment (list 'right 'center)]
       [min-width 100]
       [stretchable-width #f]))

;创建控件实体窗格:
(define (create-work/pane/control parent)
  (new horizontal-pane%
       [parent parent]
       [alignment (list 'left 'center)]
       [min-width 320]
       [stretchable-width #f]))

;创建图片源文件夹控件:
(define (create-work/control/source parent)
  (let* ([panel/row (create-work/pane/row parent)]
         [pane/title (create-work/pane/title panel/row)]
         [pane/control (create-work/pane/control panel/row)])
    ;标题:
    (new message%
         [label "图片源文件夹:"]
         [parent pane/title])
    ;控件:
    (set! text/source
          (new text-field%
               [label #f]
               [parent pane/control]
               [init-value ""]
               [min-width 220]
               [stretchable-width #f]))
    ;文件打开按钮：
    (set! button/open
          (new button%
               [label "打开……"]
               [parent pane/control]
               [callback (lambda (button event)
                           (on-click-button/open))]))))

;创建任务文件夹路径控件:
(define (create-work/control/path parent)
  (let* ([pane/row (create-work/pane/row parent)]
         [pane/title (create-work/pane/title pane/row)]
         [pane/control (create-work/pane/control pane/row)])
    ;标题:
    (new message%
         [label "任务文件夹路径："]
         [parent pane/title])
    (set! text/path
          (new text-field%
               [label #f]
               [parent pane/control]
               [init-value ""]
               [min-width 300]
               [stretchable-width #f]))))

;创建combo控件：
(define (create-control/combo title proc list/choices parent)
  (let* ([panel/row (create-work/pane/row parent)]
         [pane/title (create-work/pane/title panel/row)]
         [pane/control (create-work/pane/control panel/row)])
    ;标题：
    (new message%
         [label title]
         [parent pane/title])
    ;控件：
    (new combo-field%
         [label #f]
         [choices list/choices]
         [parent pane/control]
         [min-width 300]
         [stretchable-width #f]
         [callback (lambda (c e) (proc))])))

;创建信息显示：-------------------------------------------
;创建信息显示条：
(define (create-message-bar parent)
  (set! message/bar
        (new message%
             [label ""]
             [parent parent]
             [stretchable-width #t])))

;创建按钮区：---------------------------------------------
;创建按钮区：
(define (create-buttons parent)
  (let ([pane (create-pane/buttons parent)])
    (set! button/picture
          (create-text-button
           "显示图片" pane on-click-button/picture))
    (set! button/ok
          (create-text-button
           "确定" pane on-click-button/ok))
    (set! button/cancel
          (create-text-button
           "放弃" pane on-click-button/cancel))))
