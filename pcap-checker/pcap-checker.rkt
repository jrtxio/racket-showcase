#lang racket/gui

(require racket/system
         racket/port)

(define frame (new frame% [label "PCAP 报文分析器"] [width 600] [height 500]))

;; 创建主面板
(define main-panel (new vertical-panel% [parent frame]))

;; 文件选择面板
(define file-panel (new horizontal-panel% [parent main-panel]))

;; 输入字段:选择 PCAP 文件
(define pcap-path
  (new text-field%
       [parent file-panel]
       [label "PCAP 文件路径"]
       [min-width 300]))

(define choose-btn
  (new button%
       [parent file-panel]
       [label "选择 PCAP 文件"]
       [callback
        (lambda (button event)
          (define path (get-file))
          (when path 
            (send pcap-path set-value (path->string path))))]))

;; 多行文本框用于显示输出
(define output-text (new text%))
(define output-view
  (new editor-canvas%
       [parent main-panel]
       [editor output-text]
       [min-height 300]
       [stretchable-height #t]))

;; 分析按钮
(define run-btn
  (new button%
       [parent main-panel]
       [label "运行分析"]
       [callback
        (lambda (button event)
          (define pcap (send pcap-path get-value))
          (define python-script "pcap_checker.py")
          
          (cond
            [(string=? pcap "")
             (message-box "错误" "请选择PCAP文件" frame)]
            [(not (file-exists? pcap))
             (message-box "错误" "PCAP文件不存在" frame)]
            [(not (file-exists? python-script))
             (message-box "错误" "找不到Python脚本: pcap_analyzer.py" frame)]
            [else
             (send output-text erase)
             (send output-text insert (format "正在分析: ~a\n\n" pcap))
             
             ;; 尝试调用Python脚本
             (with-handlers 
               ([exn:fail? (lambda (e)
                            (send output-text insert 
                                  (format "错误: ~a\n" (exn-message e))))])
               
               ;; 添加调试信息
               (send output-text insert "开始调用Python脚本...\n")
               (send output-text insert (format "当前工作目录: ~a\n" (current-directory)))
               
               ;; 尝试确定Python命令
               (define python-cmd "python")  ; 既然命令行用python有效,就直接用python
               
               (send output-text insert (format "使用Python命令: ~a\n" python-cmd))
               (send output-text insert (format "执行命令: ~a ~a ~a --src-port 1234 --dst-port 4321 --track-coord-repeat\n" 
                                               python-cmd python-script pcap))
               
               ;; 设置工作目录为Python脚本所在目录
               (define script-dir (path-only (path->complete-path python-script)))
               (when script-dir
                 (current-directory script-dir)
                 (send output-text insert (format "切换工作目录到: ~a\n" script-dir)))
               
               (define-values (proc stdout stdin stderr)
                 (subprocess #f #f #f
                             python-cmd
                             (file-name-from-path python-script)  ; 只用文件名,不用完整路径
                             pcap
                             "--src-port" "1234"
                             "--dst-port" "4321"
                             "--track-coord-repeat"))
               
               ;; 关闭stdin
               (close-output-port stdin)
               
               ;; 获取输出
               (define result (port->string stdout))
               (define error-msg (port->string stderr))
               
               ;; 等待进程完成
               (define exit-code (subprocess-wait proc))
               
               ;; 显示调试信息
               (send output-text insert (format "进程退出码: ~a\n" exit-code))
               
               ;; 显示结果
               (if (and (string=? result "") (string=? error-msg ""))
                   (send output-text insert "Python脚本没有输出任何内容\n")
                   (begin
                     (unless (string=? result "")
                       (send output-text insert (format "标准输出:\n~a\n" result)))
                     (unless (string=? error-msg "")
                       (send output-text insert (format "错误输出:\n~a\n" error-msg)))))
               
               (send output-text insert "\n分析完成。"))]))]))

;; 显示窗口
(send frame show #t)