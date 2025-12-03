#lang racket/gui

;;; ============================================================================
;;; LCFU GUI Application (雷达配置工具)
;;;
;;; 图形界面主程序 - 重构版
;;; ============================================================================

(require racket/class racket/date racket/file racket/path)
(require "lcfu/protocol.rkt")
(require "lcfu/write-lib.rkt")
(require "lcfu/read-lib.rkt")

;;; ----------------------------------------------------------------------------
;;; 全局配置
;;; ----------------------------------------------------------------------------

(define *app-config*
  (make-hash
   '((target-ip . "192.168.1.10")
     (write-port . 8082)
     (read-port . 8083)
     (timeout . 5.0)
     (max-retries . 3)
     (write-block-size . 1024)
     (write-block-delay . 0.01)
     (read-block-size . 1024)
     (read-block-delay . 0.05))))

(define (config-get key)
  (hash-ref *app-config* key))

(define (config-set! key val)
  (hash-set! *app-config* key val))

;;; ----------------------------------------------------------------------------
;;; 日志管理
;;; ----------------------------------------------------------------------------

(define log-text-field #f)
(define log-buffer null)
(define MAX-LOG-LINES 1000)

(define (log-message type msg)
  (define current-date-val (current-date))
  (define seconds-str (format "~a:~a:~a"
                              (~r (date-hour current-date-val) #:min-width 2 #:pad-string "0")
                              (~r (date-minute current-date-val) #:min-width 2 #:pad-string "0")
                              (~r (date-second current-date-val) #:min-width 2 #:pad-string "0")))
  (define milli-str (number->string (current-milliseconds)))
  (define milli-part (substring (string-append (substring milli-str (max 0 (- (string-length milli-str) 3))) "000") 0 3))
  (define full-time (string-append seconds-str "." milli-part))
  (define prefix
    (case type
      [(step) "=="]
      [(success) "✓ "]
      [(error) "✗ "]
      [(warning) "⚠ "]
      [(info) "  "]
      [else "  "]))

  (define line (format "[~a] ~a~a" full-time prefix msg))
  
  ;; 添加到缓冲区
  (set! log-buffer (append log-buffer (list line)))
  (when (> (length log-buffer) MAX-LOG-LINES)
    (set! log-buffer (drop log-buffer (- (length log-buffer) MAX-LOG-LINES))))
  
  ;; 更新显示
  (when log-text-field
    (send log-text-field set-value (string-join log-buffer "\n"))
    ;; Scroll to bottom
    (let ([editor (send log-text-field get-editor)])
      (send editor set-position (send editor last-position)))))

(define (clear-log)
  (set! log-buffer null)
  (when log-text-field
    (send log-text-field set-value "")))

;;; ----------------------------------------------------------------------------
;;; UI 常量定义
;;; ----------------------------------------------------------------------------

(define LABEL-WIDTH 80)           ; 统一标签宽度
(define INPUT-WIDTH 220)          ; 文件输入框宽度(缩短)
(define BUTTON-WIDTH 80)          ; 浏览按钮宽度
(define SMALL-INPUT-WIDTH 70)     ; 小输入框宽度(缩短)
(define PANEL-SPACING 8)          ; 面板间距
(define FIELD-HEIGHT 24)          ; 字段高度

;;; ----------------------------------------------------------------------------
;;; 主窗口
;;; ----------------------------------------------------------------------------

(define main-frame 
  (new frame% 
       [label "雷达配置工具 v1.0"]
       [width 850]
       [height 700]
       [stretchable-width #f]
       [stretchable-height #t]))

;;; ----------------------------------------------------------------------------
;;; 顶部连接设置面板 - 合并所有配置到一行
;;; ----------------------------------------------------------------------------

(define connection-panel
  (new horizontal-panel%
       [parent main-frame]
       [alignment '(left center)]
       [stretchable-height #f]
       [min-height 45]
       [spacing 5]
       [border 10]))

(new message%
     [parent connection-panel]
     [label "目标IP:"])

(define ip-field
  (new text-field%
       [parent connection-panel]
       [label #f]
       [init-value (config-get 'target-ip)]
       [min-width 115]))

(new message%
     [parent connection-panel]
     [label "写端口:"])

(define write-port-field
  (new text-field%
       [parent connection-panel]
       [label #f]
       [init-value (number->string (config-get 'write-port))]
       [min-width 55]))

(new message%
     [parent connection-panel]
     [label "读端口:"])

(define read-port-field
  (new text-field%
       [parent connection-panel]
       [label #f]
       [init-value (number->string (config-get 'read-port))]
       [min-width 55]))

(define (update-config-from-fields)
  (config-set! 'target-ip (send ip-field get-value))
  (config-set! 'write-port (string->number (send write-port-field get-value)))
  (config-set! 'read-port (string->number (send read-port-field get-value))))

;;; ----------------------------------------------------------------------------
;;; 雷达类型选择面板
;;; ----------------------------------------------------------------------------

;; 雷达类型已经移到顶部连接面板,这个面板已删除

;;; ----------------------------------------------------------------------------
;;; 主工作区容器
;;; ----------------------------------------------------------------------------

(define main-container-panel
  (new vertical-panel%
       [parent main-frame]
       [alignment '(left top)]
       [spacing 10]
       [border 10]))

;;; === 写入和读取面板容器 ===
(define write-read-container
  (new horizontal-panel%
       [parent main-container-panel]
       [alignment '(left top)]
       [spacing 15]
       [stretchable-height #f]))

;;; ----------------------------------------------------------------------------
;;; 辅助函数: 创建统一的文件选择行
;;; ----------------------------------------------------------------------------

(define (create-file-row parent label-text init-value browse-callback [editable? #f])
  (define row-panel
    (new horizontal-panel%
         [parent parent]
         [alignment '(left center)]
         [stretchable-height #f]
         [spacing 5]))
  
  (new message%
       [parent row-panel]
       [label label-text]
       [min-width LABEL-WIDTH]
       [auto-resize #f])
  
  (define field
    (new text-field%
         [parent row-panel]
         [label #f]
         [init-value init-value]
         [min-width INPUT-WIDTH]
         [enabled editable?]))
  
  (new button%
       [parent row-panel]
       [label "浏览..."]
       [min-width BUTTON-WIDTH]
       [callback (lambda (btn evt) (browse-callback field))])
  
  field)

;;; ----------------------------------------------------------------------------
;;; 辅助函数: 创建统一的设置行
;;; ----------------------------------------------------------------------------

(define (create-setting-row parent label-text value-str unit-text)
  (define row-panel
    (new horizontal-panel%
         [parent parent]
         [alignment '(left center)]
         [stretchable-height #f]
         [spacing 5]))
  
  (new message%
       [parent row-panel]
       [label label-text]
       [min-width LABEL-WIDTH]
       [auto-resize #f])
  
  (define field
    (new text-field%
         [parent row-panel]
         [label #f]
         [init-value value-str]
         [min-width SMALL-INPUT-WIDTH]))
  
  (new message%
       [parent row-panel]
       [label unit-text])
  
  field)

;;; ----------------------------------------------------------------------------
;;; 写入面板
;;; ----------------------------------------------------------------------------

(define write-panel
  (new group-box-panel%
       [parent write-read-container]
       [label " 写入配置 (Write) "]
       [alignment '(left top)]
       [spacing PANEL-SPACING]))

(define write-content-panel
  (new vertical-panel%
       [parent write-panel]
       [alignment '(left top)]
       [spacing PANEL-SPACING]
       [border 5]))

;; 文件信息显示需要提前定义
(define write-file-info-text #f)
(define write-file-data #f)

; 从配置文件中解析雷达类型
(define (parse-lidar-type-from-config config-data)
  (let ([len (bytes-length config-data)])
    (if (>= len 5)  ; 确保文件至少有5个字节
        (bytes-ref config-data 4)  ; 第5个字节（索引为4）是雷达类型
        0)))  ; 文件太小，无法读取雷达类型

;; 浏览写入文件的回调函数
(define (browse-write-file field)
  (define path (get-file "选择配置文件" main-frame))
  (when path
    (send field set-value (path->string path))
    (set! write-file-data (file->bytes path))
    (define size (bytes-length write-file-data))
    (define crc (calculate-crc32 write-file-data))
    (define lidar-type (parse-lidar-type-from-config write-file-data))
    (define lidar-type-str
      (case lidar-type
        [(1) "FW192SB"]
        [(2) "IFW192S"]
        [(3) "FW192S-A"]
        [(4) "IFN56"]
        [else "未知"]))
    (send write-file-info-text set-label
          (format "文件大小: ~a, CRC32: 0x~a, 类型: ~a"
                  (format-file-size size)
                  (string-upcase
                    (let* ([crc-str (number->string crc 16)]
                           [padding-len (- 8 (string-length crc-str))])
                      (if (> padding-len 0)
                          (string-append (make-string padding-len #\0) crc-str)
                          crc-str)))
                  lidar-type-str))
    (log-message 'info (format "选择写入文件: ~a (~a), 类型: ~a"
                              (path->string path)
                              (format-file-size size)
                              lidar-type-str))))

;; 文件选择区域
(define write-file-field
  (create-file-row write-content-panel "配置文件:" "" browse-write-file))

;; 文件信息显示
(define write-info-panel
  (new horizontal-panel%
       [parent write-content-panel]
       [alignment '(left center)]
       [stretchable-height #f]
       [spacing 5]))

(new message%
     [parent write-info-panel]
     [label ""]
     [min-width LABEL-WIDTH])

(set! write-file-info-text
      (new message%
           [parent write-info-panel]
           [label "文件大小: --, CRC32: --"]
           [auto-resize #t]
           [font (make-object font% 9 'default 'normal 'normal)]))

;; 分隔线
(define write-separator
  (new horizontal-panel%
       [parent write-content-panel]
       [min-height 1]
       [stretchable-height #f]))

;; 设置区域
(define write-block-size-field
  (create-setting-row write-content-panel 
                      "块大小:" 
                      (number->string (config-get 'write-block-size))
                      "bytes"))

;; 按钮区域
(define write-button-panel
  (new horizontal-panel%
       [parent write-content-panel]
       [alignment '(center center)]
       [stretchable-height #f]
       [min-height 50]))

(define write-button
  (new button%
       [parent write-button-panel]
       [label "开始写入配置"]
       [min-width 200]
       [min-height 36]
       [callback (lambda (btn evt) (start-write-operation))]))

;;; ----------------------------------------------------------------------------
;;; 读取面板
;;; ----------------------------------------------------------------------------

(define read-panel
  (new group-box-panel%
       [parent write-read-container]
       [label " 读取配置 (Read) "]
       [alignment '(left top)]
       [spacing PANEL-SPACING]))

(define read-content-panel
  (new vertical-panel%
       [parent read-panel]
       [alignment '(left top)]
       [spacing PANEL-SPACING]
       [border 5]))

;; 读取相关变量提前定义
(define read-verify-data #f)

;; 浏览验证文件的回调函数
(define (browse-verify-file field)
  (define path (get-file "选择验证文件" main-frame))
  (when path
    (send field set-value (path->string path))
    (set! read-verify-data (file->bytes path))
    (log-message 'info (format "选择验证文件: ~a (~a)"
                              (path->string path)
                              (format-file-size (bytes-length read-verify-data))))))

;; 浏览保存文件的回调函数
(define (browse-save-file field)
  (define path (put-file "选择保存位置" main-frame))
  (when path
    (send field set-value (path->string path))))

;; 验证文件选择
(define read-verify-field
  (create-file-row read-content-panel "验证文件:" "" browse-verify-file))

;; 保存文件选择
(define read-save-field
  (create-file-row read-content-panel "保存文件:" "samples/config_readback.bin" browse-save-file #t))

;; 分隔线
(define read-separator
  (new horizontal-panel%
       [parent read-content-panel]
       [min-height 1]
       [stretchable-height #f]))

;; 设置区域
(define read-block-size-field
  (create-setting-row read-content-panel 
                      "块大小:" 
                      (number->string (config-get 'read-block-size))
                      "bytes"))

;; 按钮区域
(define read-button-panel
  (new horizontal-panel%
       [parent read-content-panel]
       [alignment '(center center)]
       [stretchable-height #f]
       [min-height 50]))

(define read-button
  (new button%
       [parent read-button-panel]
       [label "开始读取配置"]
       [min-width 200]
       [min-height 36]
       [callback (lambda (btn evt) (start-read-operation))]))

;;; ----------------------------------------------------------------------------
;;; 日志显示区域
;;; ----------------------------------------------------------------------------

(define log-container-panel
  (new vertical-panel%
       [parent main-container-panel]
       [alignment '(left top)]
       [stretchable-height #t]))

(define log-label-panel
  (new horizontal-panel%
       [parent log-container-panel]
       [alignment '(left center)]
       [stretchable-height #f]
       [spacing 10]))

(new message%
     [parent log-label-panel]
     [label "执行日志:"]
     [font (make-object font% 10 'default 'normal 'bold)])

(new button%
     [parent log-label-panel]
     [label "清空"]
     [min-width 70]
     [callback (lambda (btn evt) (clear-log))])

(define (save-log-to-file)
  (define path (put-file "保存日志" main-frame "log.txt"))
  (when path
    (call-with-output-file path
      #:exists 'replace
      (lambda (out)
        (for ([line log-buffer])
          (displayln line out))))
    (log-message 'success (format "日志已保存到: ~a" path))))

(new button%
     [parent log-label-panel]
     [label "保存日志"]
     [min-width 70]
     [callback (lambda (btn evt) (save-log-to-file))])

(set! log-text-field
      (new text-field%
           [parent log-container-panel]
           [label #f]
           [style '(multiple)]
           [enabled #f]
           [font (make-object font% 9 'modern)]))

;;; ----------------------------------------------------------------------------
;;; 状态栏
;;; ----------------------------------------------------------------------------

(define status-panel
  (new horizontal-panel%
       [parent main-frame]
       [alignment '(left center)]
       [stretchable-height #f]
       [border 10]
       [spacing 15]))

(define status-message
  (new message%
       [parent status-panel]
       [label "状态: 就绪"]
       [min-width 180]))

(define progress-gauge
  (new gauge%
       [parent status-panel]
       [label "进度:"]
       [range 100]
       [min-width 200]))

(define lidar-type-message
  (new message%
       [parent status-panel]
       [label "检测类型: --"]
       [min-width 150]))

(define (update-status msg)
  (send status-message set-label (format "状态: ~a" msg)))

(define (update-progress percent)
  (send progress-gauge set-value (inexact->exact (round percent))))

;;; ----------------------------------------------------------------------------
;;; 写入操作
;;; ----------------------------------------------------------------------------

(define write-in-progress #f)

(define (start-write-operation)
  (cond
    [write-in-progress
     (message-box "警告" "写入操作正在进行中" main-frame)]
    
    [(not write-file-data)
     (message-box "错误" "请先选择配置文件" main-frame)]
    
    [else
     (update-config-from-fields)
     (set! write-in-progress #t)
     (send write-button enable #f)
     (send read-button enable #f)
     (update-status "写入中...")
     (update-progress 0)
     (clear-log)
     
     (log-message 'step "━━━ 开始写入操作 ━━━")
     (log-message 'info (format "目标设备: ~a:~a" 
                               (config-get 'target-ip)
                               (config-get 'write-port)))
     (log-message 'info (format "文件大小: ~a" 
                               (format-file-size (bytes-length write-file-data))))
     
     (thread
      (lambda ()
        (with-handlers 
            ([exn:lcfu:timeout? 
              (lambda (e)
                (log-message 'error (format "超时错误: ~a" (exn-message e)))
                (queue-callback (lambda () (operation-complete #f))))]
             [exn:lcfu:protocol? 
              (lambda (e)
                (log-message 'error (format "协议错误: ~a" (exn-message e)))
                (queue-callback (lambda () (operation-complete #f))))]
             [exn:lcfu:io? 
              (lambda (e)
                (log-message 'error (format "网络错误: ~a" (exn-message e)))
                (queue-callback (lambda () (operation-complete #f))))]
             [exn? 
              (lambda (e)
                (log-message 'error (format "未知错误: ~a" (exn-message e)))
                (queue-callback (lambda () (operation-complete #f))))])
          
          (lcfu-write-config 
           (config-get 'target-ip)
           (config-get 'write-port)
           write-file-data
           #:max-block-size (string->number (send write-block-size-field get-value))
           #:timeout (config-get 'timeout)
           #:max-retries (config-get 'max-retries)
           #:log-callback (lambda (type msg)
                           (queue-callback (lambda () (log-message type msg))))
           #:progress-callback (lambda (current total)
                                (queue-callback 
                                 (lambda () 
                                   (update-progress (* 100 (/ current total)))))))
          
          (queue-callback (lambda ()
                           (log-message 'success "━━━ 写入操作完成 ━━━")
                           ; 从写入的配置文件解析雷达类型并更新显示
                           (let* ([lidar-type (parse-lidar-type-from-config write-file-data)]
                                  [lidar-type-str
                                   (case lidar-type
                                     [(1) "FW192SB"]
                                     [(2) "IFW192S"]
                                     [(3) "FW192S-A"]
                                     [(4) "IFN56"]
                                     [else "未知"])]
                                  [label-text (format "写入类型: ~a" lidar-type-str)])
                             (send lidar-type-message set-label label-text))
                           (operation-complete #t))))))]))

;;; ----------------------------------------------------------------------------
;;; 读取操作
;;; ----------------------------------------------------------------------------

(define read-in-progress #f)

(define (start-read-operation)
  (define save-path (send read-save-field get-value))
  (cond
    [read-in-progress
     (message-box "警告" "读取操作正在进行中" main-frame)]

    [(string=? save-path "")
     (message-box "错误" "请指定保存文件名" main-frame)]

    [else
     (update-config-from-fields)
     (set! read-in-progress #t)
     (send write-button enable #f)
     (send read-button enable #f)
     (update-status "读取中...")
     (update-progress 0)
     (clear-log)
     
     (log-message 'step "━━━ 开始读取操作 ━━━")
     (log-message 'info (format "目标设备: ~a:~a" 
                               (config-get 'target-ip)
                               (config-get 'read-port)))
     (when read-verify-data
       (log-message 'info "将验证数据匹配"))
     
     (thread
      (lambda ()
        (with-handlers 
            ([exn:lcfu:timeout? 
              (lambda (e)
                (log-message 'error (format "超时错误: ~a" (exn-message e)))
                (queue-callback (lambda () (operation-complete #f))))]
             [exn:lcfu:protocol? 
              (lambda (e)
                (log-message 'error (format "协议错误: ~a" (exn-message e)))
                (queue-callback (lambda () (operation-complete #f))))]
             [exn:lcfu:io? 
              (lambda (e)
                (log-message 'error (format "网络错误: ~a" (exn-message e)))
                (queue-callback (lambda () (operation-complete #f))))]
             [exn:lcfu:verify? 
              (lambda (e)
                (log-message 'error (format "验证错误: ~a" (exn-message e)))
                (queue-callback (lambda () (operation-complete #f))))]
             [exn? 
              (lambda (e)
                (log-message 'error (format "未知错误: ~a" (exn-message e)))
                (queue-callback (lambda () (operation-complete #f))))])
          
          (let-values ([(read-data lidar-type)
                        (lcfu-read-config 
                         (config-get 'target-ip)
                         (config-get 'read-port)
                         #:max-read-size (string->number (send read-block-size-field get-value))
                         #:timeout (config-get 'timeout)
                         #:max-retries (config-get 'max-retries)
                         #:verify-data read-verify-data
                         #:log-callback (lambda (type msg)
                                         (queue-callback (lambda () (log-message type msg))))
                         #:progress-callback (lambda (current total)
                                              (queue-callback 
                                               (lambda () 
                                                 (update-progress (* 100 (/ current total)))))))])
            
            ; 确保保存路径的目录存在
            (let ([save-dir (path-only (string->path save-path))])
              (when (and save-dir (not (directory-exists? save-dir)))
                (letrec ([create-dirs (lambda (dir)
                                        (let ([parent-dir (path-only dir)])
                                          (unless (or (not parent-dir) (directory-exists? parent-dir))
                                            (create-dirs parent-dir))
                                          (unless (directory-exists? dir)
                                            (make-directory dir))))])
                  (create-dirs save-dir))))
            (call-with-output-file save-path
              #:exists 'replace
              (lambda (out) (write-bytes read-data out)))
            
            (queue-callback
             (lambda ()
               (log-message 'success (format "文件已保存: ~a" save-path))
               (send lidar-type-message set-label
                     (format "读取类型: ~a"
                            (case lidar-type
                              [(0) "未知"]
                              [(1) "FW192SB"]
                              [(2) "IFW192S"]
                              [(3) "FW192S-A"]
                              [(4) "IFN56"]
                              [else (format "未知(~a)" lidar-type)])))
               (log-message 'success "━━━ 读取操作完成 ━━━")
               (operation-complete #t)))))))]))

;;; ----------------------------------------------------------------------------
;;; 操作完成处理
;;; ----------------------------------------------------------------------------

(define (operation-complete success?)
  (set! write-in-progress #f)
  (set! read-in-progress #f)
  (send write-button enable #t)
  (send read-button enable #t)
  (update-progress 100)
  (update-status (if success? "操作成功" "操作失败"))
  
  (when success?
    (message-box "成功" "操作成功完成!" main-frame '(ok))))

;;; ----------------------------------------------------------------------------
;;; 启动应用
;;; ----------------------------------------------------------------------------

(log-message 'info "雷达配置工具已启动")
(log-message 'info (format "当前配置 - IP: ~a, 写端口: ~a, 读端口: ~a"
                          (config-get 'target-ip)
                          (config-get 'write-port)
                          (config-get 'read-port)))

(send main-frame center)
(send main-frame show #t)