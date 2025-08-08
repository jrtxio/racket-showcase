#lang racket
;frame.rkt
;程序主框架视图。

(provide show-frame/main
         init-frame/main
         get-frame/main
         get-message-control)

(require racket/gui)

(require "app-data.rkt"
         "view-common.rkt"
         "task-struct-data.rkt"
         "defect-draw-data.rkt"
         "tower-type-data.rkt"
         "defect-type-data.rkt"
         "document-task-data.rkt"
         "document-output-data.rkt"
         "report-output-data.rkt"
         "pdf-common.rkt"
         "defects-output-data.rkt"
         "defect-picture-report.rkt"
         "dialog-task-set.rkt"
         "dialog-node-set.rkt"
         "dialog-about.rkt"
         "canvas-magnifier.rkt"
         "canvas-thumbnail.rkt")

;视图控件：=======================================
(define frame/main void) ;主框架
;菜单项：
(define menu/task/new void) ;新工作任务
(define menu/task/open void) ;打开工作任务
(define menu/task/close void) ;关闭工作任务
(define menu/task/save void) ;保存工作任务
(define menu/task/set void) ;设置工作任务
(define menu/task/exit void) ;退出
(define menu/output/nodes void) ;输出节点图片
(define menu/output/defects void) ;输出缺陷图片
(define menu/output/report void) ;输出处理报告
(define menu/output/report-pictures void) ;输出处理报告描述图片
(define menu/output/defects-table void) ;输出缺陷统计表
(define menu/output/defect-pictures-report void) ;根据缺陷图片生成报告（原图）
(define menu/output/defect-pictures/zip-report void) ;根据缺陷图片生成报告（压缩图）
(define menu/output/table-pictures/defect void) ;缺陷图片生成统计表
(define menu/tools/tower-types void) ;转换塔类型文件
(define menu/tools/defect-types void) ;转换缺陷类型文件
(define menu/tools/config-set void) ;设置软件默认配置
(define menu/help/about void) ;关于
;工具条按钮：
(define toolbar/standard/new void) ;新建任务
(define toolbar/standard/open void) ;打开任务
(define toolbar/standard/close void) ;关闭任务
(define toolbar/standard/save void) ;保存任务
(define toolbar/standard/set void) ;设置任务
;视图控件：
(define message-bar void) ;消息显示条
(define tab/pictures void) ;卡片面板
(define list/pictures void) ;图片名称显示列表
(define list/defects void) ;缺陷描述列表
(define canvas/magnifier void) ;放大器画布
(define canvas/thumbnail void) ;缩略图画布
(define check/scale void) ;激活比例选择框
(define slider/scale void) ;比例设置滑块
(define group/defect-grade void) ;缺陷等级组面板
(define radio/defect-grade void) ;缺陷等级单选按钮组
(define radio/stamp-type void) ;缺陷标记类型单选按钮组
(define group/defect-description void) ;缺陷标记类型单选按钮组
(define text/defect-edit void) ;缺陷编辑输入框
(define button/defect-draw void) ;缺陷编辑绘制按钮
(define list/defect-groups void) ;缺陷类别列表
(define list/defect-components void) ;缺陷部件列表
(define list/defect-types void) ;缺陷类型列表

;操作视图数据函数：================================================
;视图变更：-------------------------------------------------
;显示框架视图：
(define (show-frame/main)
  ;创建框架视图：
  (create-frame/main)
  ;显示框架视图：
  (send frame/main show #t)
  ;显示图片：
  (let ([picture (app-current-picture)])
    ;初始化画布类对象:
    (send canvas/thumbnail set-source picture)
    (send canvas/thumbnail set-canvas/magnifier canvas/magnifier)
    (send canvas/magnifier set-source picture)
    (send canvas/magnifier set-canvas/thumbnail canvas/thumbnail)
    ;重新显示图片:
    (redraw-picture)))

;激活控件：
(define (enable-controls)
  (if (void? (task-object))
      (begin
        (send toolbar/standard/close enable #f)
        (send toolbar/standard/save enable #f)
        (send toolbar/standard/set enable #f)
        (send menu/task/close enable #f)
        (send menu/task/save enable #f)
        (send menu/task/set enable #f)
        ;输出菜单：
        (send menu/output/nodes enable #f)
        (send menu/output/defects enable #f)
        (send menu/output/report enable #f)
        (send menu/output/report-pictures enable #f)
        (send menu/output/defects-table enable #f))
      (begin
        (if (void? (task-task-path))
            (begin
              (send toolbar/standard/save enable #f)
              (send menu/task/save enable #f))
            (begin
              (send toolbar/standard/save enable #t)
              (send menu/task/save enable #t)))
        (send toolbar/standard/close enable #t)
        (send toolbar/standard/set enable #t)
        (send menu/task/close enable #t)
        (send menu/task/set enable #t)
        ;输出菜单
        (send menu/output/nodes enable #t)
        (send menu/output/defects enable #t)
        (send menu/output/report enable #t)
        (send menu/output/report-pictures enable #t)
        (send menu/output/defects-table enable #t)))
  ;设置缺陷编辑控件组
  (enable-controls/defect))

;激活缺陷编辑控件：
(define (enable-controls/defect)
  (if (void? (task-object))
      (begin
        (send group/defect-grade enable #f)
        (send group/defect-description enable #f))
      (case (send tab/pictures get-selection)
        [(0)
         (if (non-empty-string?
              (get-node-name
               (get-list/pictures-selection)))
             (begin
               (send group/defect-grade enable #t)
               (send group/defect-description enable #t))
             (begin
               (send group/defect-grade enable #f)
               (send group/defect-description enable #f)))]
        [(1)
         (if (empty? (get-node-names))
             (begin
               (send group/defect-grade enable #f)
               (send group/defect-description enable #f))
             (begin
               (send group/defect-grade enable #t)
               (send group/defect-description enable #t)))]
        [(2)
         (if (empty? (get-defect-names))
             (begin
               (send group/defect-grade enable #f)
               (send group/defect-description enable #f))
             (begin
               (send group/defect-grade enable #t)
               (send group/defect-description enable #t)))])))

;激活比例选择框并重新显示图片：
(define (check/scale-enable control)
  ;激活滑块控件：
  (send slider/scale enable
        (send control get-value))
  ;更新图片显示：
  (redraw-picture))

;更新视图画布图片：
(define (update-picture)
  (let ([picture (app-current-picture)])
    (send canvas/thumbnail set-source picture)
    (send canvas/magnifier set-source picture))
  (redraw-picture))

;重绘图片：
(define (redraw-picture)
  ;设置画布显示片段比例(如滑块未激活，给#f):
  (send canvas/magnifier
        set-scale/refer
        (if (send slider/scale is-enabled?)
            (get-slider-scale)
            #f))
  ;绘制图片：
  (send canvas/thumbnail on-paint)
  (send canvas/magnifier on-paint))

;数据处理：----------------------------------------------------
;初始化框架视图：
(define (init-frame/main)
  (send tab/pictures set-selection 0);选择第一个tab卡片
  ;初始化图片列表框控件：
  (init-list/pictures)
  ;初始化缺陷编辑区：
  (init-defect-group)
  ;设置控件激活状态:
  (enable-controls))

;初始化list/pictures控件：
(define (init-list/pictures)
  (if (void? (task-object))
      (begin
        ;清空列表框
        (send list/pictures clear)
        (send list/defects clear))
      (if (empty? (task-tower-pictures))
          ;图片列表为空，设置封面图片：
          (begin
            (send list/pictures clear);清除图片列表框内容
            (send list/defects clear);清除缺陷列表框内容
            (set-current-picture-by-cover)
            (set-current-picture-id #f);清空当前操作的图片序号
            (update-picture))
          ;设置列表内容：
          (set-list/pictures-content))))

;初始化缺陷描述编辑区列表内容：
(define (init-defect-group)
  ;清空缺陷编辑框：
  (send text/defect-edit set-value "")
  ;设置缺陷描述列表头宽度：
  (send list/defect-types
        set-column-width 0 120 50 260)
  ;设置缺陷分组列表：
  (send list/defect-groups set
        (get-defect/group-names))
  (send list/defect-groups set-selection 0)
  ;主动响应分组列表事件：
  (on-event-list/defect-groups))

;设置list/pictures的显示列表：
(define (set-list/pictures-content)
  (send list/pictures clear);清除列表框内容
  (set-list/pictures-header) ;设置表头
  (names/pictures-to-list/picture) ;显示列表框内容
  ;初始化列表选择项：
  (when (> (send list/pictures get-number) 0)
    (send list/pictures set-selection 0);选择第一条
    (list/pictures-select)))

;根据卡片面板选择情况将图片相应名称列表注入图片列表框：
(define (names/pictures-to-list/picture)
  (case (send tab/pictures get-selection)
    [(0)
     (let ([names/source (get-source-names)]
           [names/node (get-node/source-names)])
       (when (and
              (not (empty? names/source))
              (not (empty? names/node)))
         (send list/pictures set names/source names/node)))]
    [(1)
     (let ([names/node (get-node-names)])
       (when (not (empty? names/node))
         (send list/pictures set names/node)))]
    [(2)
     (let ([names/defect (get-defect-names)])
       (when (not (empty? names/defect))
         (send list/pictures set names/defect)))]))

;设置list/pictuers表头：
(define (set-list/pictures-header)
  (case (send tab/pictures get-selection)
    [(0)
     (begin
       (if (= (length (send list/pictures get-column-labels))
              1)
           (begin
             (send list/pictures set-column-label 0 "源图片名")
             (send list/pictures append-column "图片节点名"))
           (begin
             (send list/pictures set-column-label 0 "源图片名")
             (send list/pictures set-column-label 1 "图片节点名")))
       (send list/pictures set-column-width 0 60 30 100)
       (send list/pictures set-column-width 1 150 100 250))]
    [(1 2)
     (begin
       (if (= (length (send list/pictures get-column-labels))
              1)
           (send list/pictures set-column-label 0 "图片节点名")
           (begin
             (send list/pictures delete-column 1)
             (send list/pictures set-column-label 0 "图片节点名")))
       (send list/pictures set-column-width 0 200 100 250))]))

;取得以源图片序号表示的list/pictures选择项：
(define (get-list/pictures-selection)
  (let ([sel (send list/pictures get-selection)])
    (case (send tab/pictures get-selection)
      [(0) sel]
      [(1) (id/node->id/source sel)]
      [(2) (id/defect->id/source sel)])))

;对图片列表进行选择：
(define (list/pictures-select)
  ;设置缺陷编辑控件组激活状态:
  (enable-controls/defect)
  (let* ([id/source (get-list/pictures-selection)]
         [bmp (get-picture-bitmap
               (get-picture-by-source-id id/source))])
    (when (not (void? bmp))
      (app-current-picture bmp) ;更新系统当前图片
      (set-current-picture-id id/source)
      (update-picture)) ;更新显示图片
    ;设置缺陷列表控件：
    (set-list/defects-content id/source)))

;设置缺陷列表控件
(define (set-list/defects-content id/source)
  (if (empty? (picture-node-defects
               (get-picture-by-source-id
                id/source)))
      (send list/defects clear)
      (send list/defects set
            (get-defect-descriptions id/source)
            (get-defect-grades id/source))))

;检查任务属性是否完善：
(define (task-property-perfect?)
  (and (non-empty-string? (task-voltage-grade))
       (non-empty-string? (task-line-name))
       (non-empty-string? (task-tower-number))
       (non-empty-string? (task-company-name))
       (non-empty-string? (task-inspect-date))))

;取得滑块变化比例：
(define (get-slider-scale)
  (let ([value (send slider/scale get-value)]
        [min/slider 0] ;滑块最小值
        [max/slider 10]) ;滑块最大值
    (/ (- max/slider value)
       (- max/slider min/slider))))

;设置缺陷等级单选按钮组严重等级:
(define (set-radio/defect-grade-select)
  (let* ([sel/group
          (send list/defect-groups get-selection)]
         [sel/component
          (send list/defect-components get-selection)]
         [id/type
          (send list/defect-types get-selection)]
         [str/grade
          (list-ref
           (get-defect/type-grades sel/group sel/component)
           id/type)]
         [sel/radio
          (case str/grade
            [("一般") 0]
            [("严重") 1]
            [("危急") 2]
            [else #f])])
    (send radio/defect-grade set-selection sel/radio)))

;创建当前缺陷对象实例：
(define (get-current-defect)
  (create-new-defect ;缺陷结构对象
   (send list/defect-groups get-string-selection) ;缺陷类别
   (send list/defect-components get-string-selection) ;缺陷部件
   (send list/defect-types get-string-selection) ;缺陷类型
   (send text/defect-edit get-value) ;缺陷描述
   (send radio/defect-grade get-item-label ;缺陷等级
         (send radio/defect-grade get-selection))
   (send radio/stamp-type get-item-label ;缺陷标记类型
         (send radio/stamp-type get-selection))
   (make-hash)))

;内外部调用函数：-----------------------------------------------
;取得frame/main视图：
(define (get-frame/main)
  frame/main)

;取得提示信息显示控件：
(define (get-message-control)
  message-bar)

;任务设置对话框回调函数：
(define (callback/dialog-task-set)
  (init-frame/main))

;节点名设置对话框回调函数：
(define (callback/dialog-node-set name/node)
  (let ([sel/picture (get-list/pictures-selection)])
    (send list/pictures set-string sel/picture name/node 1);设置到列表框
    (set-node-name sel/picture name/node)));保存

;框架响应函数：===================================================
;响应菜单及按钮：------------------------------------------------------
;创建新任务：
(define (new-task)
  (task-object (create-new-task)) ;新建任务
  (status-message "新建任务成功，但当前所有属性均未设置。现在进入设置任务对话框进行任务设置。")
  (show-dialog/task/set frame/main init-frame/main))

;打开任务文件：
(define (open-task)
  (let ([filepath (get-file
                   "打开项目文件"
                   frame/main
                   (if (or (void? (task-object))
                           (void (task-task-path)))
                       #f
                       (task-task-path))
                   #f
                   "tsk"
                   null
                   '(("任务文件" "*.tsk")
                     ("所有文件" "*.*")))])
    (if filepath
        (begin
          (read-task-document filepath)
          ;Todo:这里还应该比较当前图片文件夹里图片与任务文件里的图片是否一致
          ;(compare pathnew pathold)
          ;重新设置任务路径为当前文件路径：
          (let-values ([(p f b)
                        (split-path filepath)])
            (task-task-path p))
          (init-frame/main) ;初始化框架视图
          (status-message
           (string-append
            "已经打开文件："
            (path->string filepath))))
        (status-message "未打开新的文件。"))))

;关闭任务文件：
(define (close-task)
  (task-object (void)) ;初始化任务对象
  (init-frame/main) ;初始化框架视图
  (set-current-picture-by-cover)
  (update-picture)
  (status-message "工作任务关闭。"))

;保存任务文件：
(define (save-task)
  (let ([task (task-object)])
    (if (void? task)
        (status-message
         "任务未建立，不能完成保存。")
        (let ([filepath (get-default-file-path)])
          (write-task-document filepath)
          (status-message
           (string-append
            "已经将任务文件保存至:"
            (path->string filepath)))))))

;设置任务:
(define (set-task)
  (when (not (void? (task-object)));显示设置任务对话框:
    (show-dialog/task/set frame/main init-frame/main)))

;退出程序：
(define (exit-app)
  (send frame/main on-exit))

;输出节点图片：
(define (output-nodes)
  (output-node-pictures))

;输出缺陷图片：
(define (output-defects)
  (if (task-property-perfect?)
      (output-defect-pictures) ;输出缺陷图片
      (show-property-not-perfect-message-box)))

;输数据处理报告:
(define (output-report)
  (if (task-property-perfect?)
      (begin
        (status-message  "输出数据处理报告……")
        (set-a4-paper)
        (output-doc-by-procedure
         (create-dc/pdf-report frame/main)
         output-page/report)
        (status-message  "数据处理报告输出完成。"))
      (show-property-not-perfect-message-box)))

;显示项目数据设置不完整消息框：
(define (show-property-not-perfect-message-box)
  (show-simple-message-box
   "项目属性数据设置不完整，请用项目设置对话框完善相关信息。"))

;输出处理报告的缺陷描述及图片部分:
(define (output-report/pictures)
  (if (task-property-perfect?)
      (begin
        (status-message  "输出缺陷描述及图片……")
        (set-a4-paper)
        (output-doc-by-procedure
         (create-dc/pdf-report/pictures frame/main)
         output-page/report/pictures)
        (status-message  "缺陷描述及图片输出完成。"))
      (show-property-not-perfect-message-box)))

;输出缺陷统计表：
(define (output-table/defects)
  (if (task-property-perfect?)
      (begin
        (status-message  "正在输出缺陷统计表……")
        (ouput-xslx/defects-table)
        (status-message  "缺陷统计表输出完毕。"))
      (show-property-not-perfect-message-box)))

;转换塔类型文件：
(define (convert-tower-types)
  (sample-tower-type-file)
  (status-message "塔类型文件转换完毕。"))

;转换缺陷类型文件:
(define (convert-defect-types)
  (sample-defect-type-file)
  (status-message "缺陷类型文件转换完毕。"))

;设置软件默认配置：
(define (set-app-config)
  void)

;软件关于对话框：
(define (about)
  (show-dialog/about frame/main))

;响应视图控件：-------------------------------------------------
;响应tab/pictures控件事件：
(define (on-tab/pictures-event control event)
  (init-list/pictures));初始化list/pictures

;响应图片列表事件:
(define (on-list/pictures-event control event)
  (case (send event get-event-type)
    [(list-box-dclick)
     (when (and
            (= (send tab/pictures get-selection) 0) ;源图片面板
            (get-list/pictures-selection)) ;选择有效
       (show-dialog/node/set
        frame/main
        callback/dialog-node-set
        (get-list/pictures-selection))
       ;激活缺陷编辑控件：
       (enable-controls/defect))]
    [(list-box)
     (list/pictures-select)]))

;响应list/defects列表框事件：
(define (on-event-list/defects control event)
  ;处理双击事件，删除列表项：
  (when (eq? (send event get-event-type)
             'list-box-dclick)
    (let ([id/defect (send list/defects get-selection)]
          [id/picture (get-list/pictures-selection)])
      (when (and id/defect
                 (eq? (show-interaction-message-box
                       "双击该列表框将执行删除列表项。是否确定要删除?")
                      'yes))
        (begin
          ;删除指定项：
          (delete-picture-defect id/picture id/defect)
          ;如果缺陷列表为空并在缺陷列表卡片上：
          (if (empty? (get-picture-defects id/picture))
              (if (= (send tab/pictures get-selection) 2)
                  ;重置图片列表框：
                  (set-list/pictures-content)
                  ;重置缺陷列表：
                  (set-list/defects-content id/picture))
              ;重置缺陷列表：
              (set-list/defects-content id/picture))
          ;重绘图片：
          (redraw-picture))))))

;响应比例滑块控件事件：
(define (on-event-slider/scale)
  (redraw-picture))

;响应button/defect-draw事件：
(define (on-event-button/defect-draw)
  (if (and
       (send radio/defect-grade get-selection)
       (non-empty-string?
        (send text/defect-edit get-value)))
      (begin
        ;开始绘图会话绘图
        (begin-stamp-interaction
          (get-list/pictures-selection) ;当前图片序号
          (get-current-defect) ;当前缺陷结构对象
          list/defects) ;传递缺陷列表框，以便更新缺陷列表
        (send canvas/magnifier focus)) ;将焦点放到画布
      ;给出对话框提示信息：
      (show-simple-message-box "必须选择缺陷等级且输入缺陷描述。")))

;响应缺陷类别列表控件事件：
(define (on-event-list/defect-groups)
  (let ([id (send list/defect-groups get-selection)])
    (when id
      (begin
        (send list/defect-components set
              (get-defect/component-names id))
        (send list/defect-components set-selection 0)
        ;主动响应部件列表事件:
        (on-event-list/defect-components)))))

;响应部件列表控件事件:
(define (on-event-list/defect-components)
  (let ([group-id (send list/defect-groups get-selection)]
        [component-id (send list/defect-components get-selection)])
    (when (and group-id component-id)
      (begin
        (send list/defect-types set
              (get-defect/type-descriptions group-id component-id)
              (get-defect/type-grades group-id component-id))
        ;清空编辑框内容：
        (send text/defect-edit set-value "")))))

;响应缺陷类型列表控件事件:
(define (on-event-list/defect-types)
  (send text/defect-edit set-value "");清空编辑框内容
  (let ([str/group
         (send list/defect-groups get-string-selection)]
        [str/component
         (send list/defect-components get-string-selection)]
        [str/type
         (send list/defect-types get-string-selection)]
        [id/type (send list/defect-types get-selection)])
    ;设置缺陷描述内容：
    (send text/defect-edit set-value
          (string-append str/group str/component str/type))
    ;设置缺陷等级单选按钮组严重等级：
    (set-radio/defect-grade-select)
    (send button/defect-draw enable #t)));激活绘图按钮

;构建框架视图：===================================================
;创建框架视图：
(define (create-frame/main)
  ;创建框架窗口：
  (create-frame)
  ;创建菜单栏：
  (create-menubar frame/main)
  ;创建工具栏：
  (create-pane/toolbars frame/main)
  ;创建工作区窗格：
  (create-pane/work frame/main))

;创建框架视图窗口：
(define (create-frame)
  (set! frame/main 
        (new frame%
             [label "EVAgle-架空输电线路无人机巡检影像拍摄数据处理程序"]
             [width 800] 
             [height 600])))

;定义菜单栏-----------------------------------------------------
;定义菜单条：
(define (create-menubar parent)
  (let ([menubar
         (new menu-bar%
              [parent parent])])
    (create-menu/task menubar) ;工作任务菜单
    (create-menu/output menubar) ;输出菜单
    (create-menu/tools menubar) ;工具菜单
    (create-menu/help menubar))) ;帮助菜单

;创建工作任务菜单：
(define (create-menu/task parent)
  (let ([task (create-menu "&T工作任务" parent)])
    (create-menu-item menu/task/new task
                      "&N新工作任务……" new-task)
    (create-menu-item menu/task/open task
                      "&O打开工作任务……" open-task)
    (create-menu-item menu/task/close task
                      "&C关闭工作任务" close-task)
    (create-menu-item menu/task/save task
                      "&S保存工作任务" save-task)
    (create-menu-item menu/task/set task
                      "&T设置工作任务……" set-task)
    (create-menu-separator task)
    (create-menu-item menu/task/exit task
                      "&E退出" exit-app)))

;输出任务菜单：
(define (create-menu/output parent)
  (let ([output (create-menu "&O输出任务" parent)])
    (create-menu-item menu/output/nodes output
                      "&N输出节点图片" output-nodes)
    (create-menu-item menu/output/defects output
                      "&D输出缺陷图片" output-defects)
    (create-menu-item menu/output/report output
                      "&R输出处理报告" output-report)
    (create-menu-item menu/output/report-pictures output
                      "&P输出处理报告描述图片" output-report/pictures)
    (create-menu-item menu/output/defects-table output
                      "&T输出缺陷统计表" output-table/defects)
    (create-menu-separator output)
    (create-menu-item
     menu/output/defect-pictures-report output
     "&T根据缺陷图片生成报告（原图）" output-report-by-pictures/defect)
    (create-menu-item
     menu/output/defect-pictures/zip-report output
     "&T根据缺陷图片生成报告（压缩图）" output-report-by-pictures/defect/zip)
    (create-menu-item
     menu/output/table-pictures/defect output
     "&T根据缺陷图片生成统计表" output-table-by-pictures/defect)))

;工具菜单：
(define (create-menu/tools parent)
  (let ([tools (create-menu "&L工具" parent)])
    (create-menu-item menu/tools/tower-types tools
                      "&T转换塔类型文件" convert-tower-types)
    (create-menu-item menu/tools/defect-types tools
                      "&D转换缺陷类型文件" convert-defect-types)
    (create-menu-separator tools)
    (create-menu-item menu/tools/config-set tools
                      "&S设置软件默认配置……" set-app-config)))

;帮助菜单：
(define (create-menu/help parent)
  (let ([help (create-menu "&H帮助" parent)])
    (create-menu-item menu/help/about help
                      "&A关于……" about)))

;定义工具栏：--------------------------------------------
;定义工具栏区:
(define (create-pane/toolbars parent)
  (let ([toolbars
         (new pane%
              [parent parent]
              [min-height 0]
              [stretchable-height #f])])
    (create-toolbar/standard toolbars)));标准工具栏

;标准工具栏：
(define (create-toolbar/standard parent)
  (let ([standard (create-toolbar parent)])
    (create-toolbar-button toolbar/standard/new standard
                           "新建任务" new-task)
    (create-toolbar-button toolbar/standard/open standard
                           "打开任务" open-task)
    (create-toolbar-button toolbar/standard/close standard
                           "关闭任务" close-task)
    (create-toolbar-button toolbar/standard/save standard
                           "保存任务" save-task)
    (create-toolbar-button toolbar/standard/set standard
                           "设置任务" set-task)
    (create-pane/message standard))) ;创建消息显示条

;状态信息窗格:-------------------------------------------
;状态信息窗格：
(define (create-pane/message parent)
  (let ([message 
         (new horizontal-pane%
              [parent parent]
              [alignment (list 'left 'center)]
              [stretchable-width #t])])
    (create-pane/message-message-title message)
    (create-pane/message-message-content message)))

;创建状态信息标题：
(define (create-pane/message-message-title parent)
  (new message%
       [label "    EVAgler提示 >>"]
       [parent parent]
       [min-width 0]
       [stretchable-width #f]))

;创建状态信息标题：
(define (create-pane/message-message-content parent)
  (set! message-bar
        (new message%
             [label "您好！EVAgler欢迎您!"]
             [parent parent]
             [stretchable-width #t])))

;定义工作区：----------------------------------------------
;;;创建工作区窗格:
(define (create-pane/work parent)
  (let ([work
         (new horizontal-pane%
              [parent parent]
              [alignment (list 'center 'top)]
              [border 3])])
    (create-pane/work/left work) ;左侧窗格
    (create-separatot-line/vertical work) ;竖向分隔板
    (create-panel/work/middle work) ;中间板格
    (create-separatot-line/vertical work) ;竖向分隔板
    (create-pane/work/right work))) ;右侧窗格

;;创建左侧窗格：---------------------
;创建左侧窗格
(define (create-pane/work/left parent)
  (let ([pane
         (new vertical-pane%
              [parent parent]
              [min-width 230]
              [stretchable-width #f])])
    (create-tab/pictures pane)))

;创建卡片面板
(define (create-tab/pictures parent)
  (let ([tab
         (new tab-panel%
              [choices '("源图片"
                         "节点图片"
                         "缺陷图片")]
              [parent parent]
              [callback
               (lambda (c e)
                 (on-tab/pictures-event c e))])])
    (set! tab/pictures tab) ;设置为全局值
    (create-list/pictures tab) ;图片名称显示列表
    (create-list/defects tab))) ;缺陷描述列表

;创建图片名称显示列表：
(define (create-list/pictures parent)  
  (set! list/pictures
        (new list-box%
             [label "图片列表"]
             [parent parent]
             [choices null]
             [style (list 'single
                          'vertical-label
                          'variable-columns
                          'column-headers)]
             [columns (list "原图片名" "节点图片名")]
             [callback (lambda (c e)
                         (on-list/pictures-event c e))])))

;创建缺陷描述列表:
(define (create-list/defects parent)
  (set! list/defects
        (new list-box%
             [label "缺陷列表"]
             [choices null]
             [parent parent]
             [style (list 'single
                          'vertical-label
                          'variable-columns
                          'column-headers)]
             [columns (list "缺陷描述" "缺陷等级")]
             [min-height 160]
             [stretchable-height #f]
             [callback (lambda (c e)
                         (on-event-list/defects c e))]))
  (send list/defects ;设置列宽
        set-column-width 0 130 60 200))

;;创建中间窗格:---------------------------
(define (create-panel/work/middle parent)
  (let ([panel
         (new panel%
              [parent parent]
              [style (list 'border)]
              [border 1])])
    (create-canvas/magnifier panel)))

;放大器画布（工作区中间）:
(define (create-canvas/magnifier parent)
  (set! canvas/magnifier
        (new canvas/magnifier%
             [parent parent])))

;;工作区右侧窗格:------------------------------------
(define (create-pane/work/right parent)
  (let ([pane
         (new vertical-pane%
              [parent parent]
              [min-width 200]
              [stretchable-width #f])])
    (create-panel/thumbnail pane) ;缩略图窗格
    (create-pane/scale pane) ;比例窗格
    (create-separator-line/horizontal pane) ;横向分隔条
    (create-group/defect-grade pane) ;缺陷等级组面板
    (create-group/defect-description pane))) ;缺陷描述组面板

;缩略图窗格：------------
(define (create-panel/thumbnail parent)
  (let ([panel
         (new panel%
              [parent parent]
              [style (list 'border)]
              [alignment (list 'center 'top)]
              [min-height 0]
              [stretchable-height #f])])
    (create-canvas/thumbnail panel)))  ;缩略图画布

;缩略图画布:
(define (create-canvas/thumbnail parent)
  (set! canvas/thumbnail
        (new canvas/thumbnail%
             [parent parent]
             [stretchable-height #f])))

;比例窗格:-----------------
(define (create-pane/scale parent)
  (let ([pane
         (new horizontal-pane%
              [parent parent]
              [alignment (list 'left 'top)]
              [min-height 0]
              [stretchable-height #f])])
    (create-check/scale pane) ;激活比例选择框
    (create-slider/scale pane))) ;比例设置滑块

;激活比例选择框：
(define (create-check/scale parent)
  (set! check/scale
        (new check-box%
             [label ""]
             [parent parent]
             [value #t]
             [callback (lambda (c e)
                         (check/scale-enable c))])))

;比例设置滑块：
(define (create-slider/scale parent)
  (set! slider/scale
        (new slider%
             [label ""]
             [min-value 0]
             [max-value 10]
             [parent parent]
             [init-value 0]
             [style (list 'horizontal
                          'plain)]
             [callback (lambda (c e)
                         (on-event-slider/scale))])))

;缺陷等级组面板：------------------
(define (create-group/defect-grade parent)
  (let ([group
         (new group-box-panel%
              [label "缺陷表示"]
              [parent parent]
              [alignment (list 'left 'top)]
              [stretchable-height #f])])
    (set! group/defect-grade group) ;设为全局值
    (create-radio/defect-grade group) ;缺陷等级单选按钮组
    (create-radio/stamp-type group))) ;缺陷标记类型单选按钮组

;缺陷等级单选按钮组：
(define (create-radio/defect-grade parent)
  (set! radio/defect-grade
        (new radio-box%
             [label "等级："]
             [choices (list "一般" "严重" "危急")]
             [parent parent]
             [style (list 'horizontal)])))

;缺陷标记类型单选按钮组：
(define (create-radio/stamp-type parent)  
  (set! radio/stamp-type
        (new radio-box%
             [label "标记："]
             [choices (list "方形" "矩形" "圆形")]
             [parent parent]
             [style (list 'horizontal)])))

;缺陷描述组面板：----------------------
(define (create-group/defect-description parent)
  (let ([group
         (new group-box-panel%
              [label "缺陷描述编辑区"]
              [parent parent])])
    (set! group/defect-description group) ;设为全局值
    (create-pane/defect-edit group) ;缺陷描述编辑窗格
    (create-list/defect-groups group) ;缺陷类别列表
    (create-list/defect-components group) ;缺陷部件列表
    (create-list/defect-types group))) ;缺陷类型列表

;缺陷描述编辑窗格:
(define (create-pane/defect-edit parent)
  (let ([pane 
         (new horizontal-pane%
              [parent parent]
              [stretchable-height #f])])
    (create-text/defect-edit pane) ;缺陷编辑输入框
    (create-button/defect-draw pane))) ;缺陷编辑绘制按钮

;缺陷编辑输入框:
(define (create-text/defect-edit parent)
  (set! text/defect-edit
        (new text-field%
             [label #f]
             [parent parent]
             [min-height 30]
             [stretchable-height #f])))

;缺陷编辑绘制按钮:
(define (create-button/defect-draw parent)
  (set! button/defect-draw
        (new button%
             [label (read-bitmap
                     (get-icon-path "draw-flag.png") 'png)]
             [parent parent]
             [min-width 24]
             [min-height 24]
             [stretchable-width #f]
             [callback (lambda (c e)
                         (on-event-button/defect-draw))])))

;缺陷类别列表:
(define (create-list/defect-groups parent)
  (set! list/defect-groups
        (new list-box%
             [label "类别列表"]
             [choices null]
             [parent parent]
             [style (list 'single
                          'vertical-label)]
             [min-height 100]
             [stretchable-height #f]
             [callback (lambda (c e)
                         (on-event-list/defect-groups))])))

;缺陷部件列表:
(define (create-list/defect-components parent)
  (set! list/defect-components
        (new list-box%
             [label "部件列表"]
             [choices null]
             [parent parent]
             [style (list 'single
                          'vertical-label)]
             [min-height 100]
             [stretchable-height #f]
             [callback (lambda (c e)
                         (on-event-list/defect-components))])))

;缺陷类型列表:
(define (create-list/defect-types parent)
  (set! list/defect-types
        (new list-box%
             [label "类型列表"]
             [choices null]
             [columns (list "缺陷描述" "缺陷等级")]
             [parent parent]
             [style (list 'single
                          'vertical-label)]
             [callback (lambda (c e)
                         (on-event-list/defect-types))])))
