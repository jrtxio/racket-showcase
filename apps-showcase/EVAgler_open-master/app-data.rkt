#lang racket

;app-data.rkt
;程序数据。

(provide  get-picture/cover
          app-current-picture
          set-current-picture-by-cover
          get-icon-path
          access-app-main-frame
          set-message-control
          status-message
          show-simple-message-box
          show-interaction-message-box
          draw-picture-to-canvas
          get-time
          
          get-config-file
          read-app-config
          config/get-main-frame-state
          config/set-main-frame-state
          config/get-main-frame-size
          config/set-main-frame-size)

(require racket/draw
          racket/gui
          json)

;主框架窗口：
(define app-main-frame (void))
;信息显示控件：
(define message-bar (void))
;当前的图片：
(define current-picture (void))
;app的运行参数（为hasheq表，采用json进行存储）：
(define app-config-parameter (make-hash))

;===================================================
;取得封面图片:
(define (get-picture/cover)
  (read-bitmap
   (build-path (current-directory)
               "surpport"
               "cover.png")))

;存取当前图片：
(define app-current-picture
  (case-lambda
    [()
     current-picture]
    [(picture)
     (set! current-picture picture)]))

;初始化当前图片为封面：
(define (set-current-picture-by-cover)
  (app-current-picture (get-picture/cover)))

;取得图标文件夹：
(define (get-icon-path icon-file)
  (let ([path (build-path
               (current-directory)
               "surpport"
               "icon"
               icon-file)])
    (path->string path)))

;设置主框架视图：
(define access-app-main-frame
  (case-lambda
    [()
     app-main-frame]
    [(frame)
     (set! app-main-frame frame)]))

;设置信息显示控件：
(define (set-message-control control)
  (set! message-bar control))

;设置主框架状态条信息：
(define (status-message message [parent message-bar])
  (when (not (void? parent))
    (send parent set-label message)))

;显示简单的信息提示对话框：
(define (show-simple-message-box message)
  (message-box "信息提示"
               message
               app-main-frame))

;显示交互对话框：
(define (show-interaction-message-box msg)
  (message-box "交互信息"
               msg
               app-main-frame
               (list 'yes-no)))

;将给定图片绘制到指定画布:
(define (draw-picture-to-canvas canvas path)
  (let* ([w/c (send canvas get-width)]
         [h/c (send canvas get-height)]
         [pic (read-bitmap
               (path->string path))]
         [w/p (send pic get-width)]
         [h/p (send pic get-height)]
         [scale
          (if (> w/p h/p)
              (/ w/c w/p) (/ h/c h/p))]
         [dc (send canvas get-dc)])
    (send dc set-scale scale scale)
    (send dc erase)
    (send dc draw-bitmap pic 0 0)))

;取得函数运行时间：
(define-syntax-rule (get-time proc)
  (let ([por/old (current-output-port)]
        [por/str (open-output-string)])
    (current-output-port por/str)
    (time proc)
    (define str (get-output-string por/str))
    (current-output-port por/old)
    (string->number
     (car
      (regexp-match
       #rx"[0-9]+"
       (car
        (regexp-match
         #rx"real time: [0-9]+"
         str)))))))

;操作程序运行配置参数(函数名统一包含config)：========================================
;取得程序运行参数文件名：
(define (get-config-file)
  (build-path (current-directory)
              "surpport"
              "config.jsn"))

;读取程序配置文件：
(define (read-app-config)
  (let ([cf (get-config-file)])
    (when (directory-exists? cf)
      (with-input-from-file cf
        (lambda ()
          (set! app-config-parameter
                (read-json)))))))

;写入程序配置文件：
(define (save-app-config)
  (with-output-to-file (get-config-file)
    (lambda ()
      (write-json app-config-parameter))
    	#:exists 'replace))

;设置配置主窗口状态：
(define (config/set-main-frame-state)
  (hash-set! app-config-parameter
             'main-frame-state
             (send app-main-frame
                   is-maximized?))
  (save-app-config))

;取得配置主窗口状态：
(define (config/get-main-frame-state)
  (send app-main-frame maximize
        (hash-ref app-config-parameter
                  'main-frame-state #f)))

;设置主窗口框架尺寸：
(define (config/set-main-frame-size)
  (hash-set! app-config-parameter
             'main-frame-x
             (send app-main-frame
                   get-x))
  (hash-set! app-config-parameter
             'main-frame-y
             (send app-main-frame
                   get-y))
  (hash-set! app-config-parameter
             'main-frame-width
             (send app-main-frame
                   get-width))
  (hash-set! app-config-parameter
             'main-frame-height
             (send app-main-frame
                   get-height))
  (save-app-config))

;取得主窗口框架尺寸：
(define (config/get-main-frame-size)
  (send app-main-frame move
        (hash-ref app-config-parameter
                  'main-frame-x 0)
        (hash-ref app-config-parameter
                  'main-frame-y 0))
  (send app-main-frame min-width
        (hash-ref app-config-parameter
                  'main-frame-width 800))
  (send app-main-frame min-height
        (hash-ref app-config-parameter
                  'main-frame-height 600)))
