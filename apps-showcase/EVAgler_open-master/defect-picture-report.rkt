#lang racket
;defect-picture-report.rkt
;缺陷文件出报告。

(provide output-report-by-pictures/defect
         output-report-by-pictures/defect/zip
         output-table-by-pictures/defect)


(require racket/gui/base)

(require "app-data.rkt"
         "pdf-common.rkt"
         ;"xlsx-common.rkt"
         )

;当前任务路径：
(define task-path (void))
;执行图片压缩标志（默认采用图片压缩）：
(define zip-flag #t)

;缺陷报告：======================================================
;根据缺陷图片生成报告（原图）：
(define (output-report-by-pictures/defect)
  (set! zip-flag #f)
  (output-picture-report))

;根据缺陷图片生成报告（压缩图）：
(define (output-report-by-pictures/defect/zip)
  (set! zip-flag #t)
  (output-picture-report))

;输出缺陷图片报告：
(define (output-picture-report)
  (when (eq? (show-start-note) 'yes)
    (get-task-path)
    (set-a4-paper)
    (let ([dc
           (create-output-dc/pdf (get-report-path)
                                 (access-app-main-frame))])
      (when (send dc ok?)
        (status-message "输出缺陷描述及图片……")
        (output-doc-by-procedure
         dc
         draw-defect-pictures)
        (status-message "缺陷描述及图片输出完成。")))))

;显示开始提示信息：
(define (show-start-note)
  (show-interaction-message-box
   "请确保文件夹内包含\"一般缺陷\"、\"严重缺陷\"、\"危急缺陷\"等分类文件夹，且检查文件名是否规范。"))

;取得任务路径：
(define (get-task-path)
  (set! task-path
        (get-directory "请选择任务文件夹" (access-app-main-frame))))

;绘制所有缺陷图片：
(define (draw-defect-pictures dc)
  (set-position-height 0) ;初始化绘图起点
  (let ([path task-path])
    (when path
      (let ([numbers (list "一" "二" "三")]
            [grades (list "一般缺陷" "严重缺陷" "危急缺陷")])
        (for/list ([n numbers]
                   [grade grades])
          (status-message
           (string-append "开始处理" grade "……"))
          (draw-subsection-title dc grade n)
          (let* ([filenames (get-defect-filenames path grade)]
                 [names (get-defect-names filenames)])
            (when (not (empty? filenames))
              (for/list ([n (in-naturals 1)]
                         [filename filenames]
                         [name names])
                (status-message
                 (string-append "正在处理："
                                (path->string filename)))
                (draw-defect-name dc name n)
                (draw-paragraph/bitmap
                 dc
                 (get-picture path grade filename))))))))))

;取得缺陷文件：
(define (get-defect-filenames path/task grade)
  (let ([path
         (build-path path/task
                     grade)])
    (directory-list path)))

;取得文件名列表：
(define (get-defect-names filenames)
  (map (lambda (filename)
         (path->string
          (path-replace-extension filename #"")))
       filenames))

;取得文件对应图片：
(define (get-picture path grade filename)
  (let* ([filepath
          (build-path path grade filename)]
         [bitmap
          (read-bitmap filepath)])
    (if zip-flag
        (get-zip-picture bitmap);压缩图片        
        bitmap)));返回原图

;取得压缩图片（压缩图片标准大小为3200*2100）：
(define (get-zip-picture source)
  (let* ([w/source (send source get-width)]
         [h/source (send source get-height)]
         [w/standard 3200]
         [h/standard 2100]
         [w/scale (/ w/standard w/source)]
         [h/scale (/ h/standard h/source)]
         [scale (if (> w/scale h/scale)
                    h/scale w/scale)]
         [w/bitmap (exact-floor (* w/source scale))]
         [h/bitmap (exact-floor (* h/source scale))]
         [dc (send
              (make-object bitmap% w/bitmap h/bitmap)
              make-dc)])
    (send dc set-scale scale scale)
    (send dc draw-bitmap source 0 0)
    (send dc get-bitmap)))

;绘制标题：
(define (draw-subsection-title dc title n)
  (let ([str
         (string-append "（" n "）、" title)])
    (draw-paragraph/text dc str)))

;绘制缺陷图片名：
(define (draw-defect-name dc name n)
  (let ([str
         (string-append
          (number->string n) "、" name)])
    (draw-paragraph/text dc str)))

;取得缺陷报告路径：
(define (get-report-path)
  (let-values ([(name)
                (if zip-flag
                    "缺陷报告（压缩）.pdf"
                    "缺陷报告（非压缩）.pdf")]
               [(p folder b) (split-path task-path)])
    (build-path  task-path
                 (string-append
                  (path->string folder)
                  "_" name))))

;缺陷统计表：======================================================
;根据缺陷图片生成统计表：
(define (output-table-by-pictures/defect)
  (when (eq? (show-start-note) 'yes)
    (get-task-path)
    #|
    ;取得表格行数据：
    (get-lines-data (get-defect-lines))
    ;输出表格：
    (ouput-xslx-table
     (get-table-path)
     create-defects-table)
    |#
    (status-message "缺陷统计表完成。")))

;取得缺陷报告路径：
(define (get-table-path)
  (let-values ([(name) "缺陷统计表.xlsx"]
               [(p folder b) (split-path task-path)])
    (build-path  task-path
                 (string-append
                  (path->string folder)
                  "_" name))))

;取得所有缺陷表格行：
(define (get-defect-lines)
  (define lines null) ;初始化表格行
  (let ([path task-path])
    (when path
      (let ([numbers (list "一" "二" "三")]
            [grades (list "一般缺陷" "严重缺陷" "危急缺陷")])
        (for/list ([n/t numbers]
                   [grade grades])
          (status-message
           (string-append "开始处理" grade "……"))
          (set! lines
                (append lines
                        (list (get-subsection-line n/t grade))))
          (let* ([filenames (get-defect-filenames path grade)])
            (for/list ([n (in-naturals 1)]
                       [filename filenames])
              (status-message
               (string-append "正在处理："
                              (path->string filename)))
              (set! lines
                    (append
                     lines
                     (list (syntax-defect-name n filename))))))))))
  lines)

;创建次节标题：
(define (get-subsection-line n grade)
  (list (string-append "（" n "）、" grade)
        "" "" "" "" "" "" "" "" ""))

;解析缺陷图片名，取得缺陷表行数据：
;例：#path(10kV柳开线N01号塔-小号横担侧挂点螺栓销钉退出-危急缺陷)
(define (syntax-defect-name n filename)
  (let* ([picture (path->string filename)]
         [name (path->string
                (path-replace-extension filename #""))]
         [name/split (string-split name "-")]
         [grade (last name/split)]
         [tower (string-join
                 (take name/split (- (length name/split) 2))
                 "-")])
    (let*-values ([(voltage tower) (get-mach-str #rx"[0-9]*[kKvV]+" tower)])
      (let* ([tower/split (string-split tower "线")]
             [line (string-append
                    (string-join
                     (take tower/split (- (length tower/split) 1))
                     "线")
                    "线")]
             [number (string-trim (last tower/split) "塔")])
        (list (number->string n) ;序号
              "" ;项目单位
              voltage ;电压等级
              line ;线路名称
              number ;杆塔号
              "" ;缺陷类别
              "" ;缺陷精度
              grade ;缺陷等级
              "" ;发现时间
              picture))))) ;缺陷图像

;取得匹配的文字片段，并将剩下的片段重置给基础字符串：
(define (get-mach-str reg str)
  (let ([result (first (regexp-match reg str))])
    (values result (string-replace str result ""))))
