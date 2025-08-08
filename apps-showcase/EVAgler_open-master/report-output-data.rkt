#lang racket
;report-output-data.rkt
;报告输出。

(provide create-dc/pdf-report
         create-dc/pdf-report/pictures
         output-page/report
         output-page/report/pictures)

(require racket/draw)

(require "task-struct-data.rkt"
         "document-output-data.rkt"
         "defect-draw-data.rkt"
         "app-data.rkt"
         "pdf-common.rkt")

;应用函数：======================================================
;创建报告的dc/pdf:
(define (create-dc/pdf-report parent)
  (create-output-dc/pdf
   (get-report-file-path)
   parent))

;创建报告图片的dc/pdf:
(define (create-dc/pdf-report/pictures parent)
  (create-output-dc/pdf
   (get-report/pictures-file-path)
   parent))

;取得输出报告文件完整路径:
(define (get-report-file-path)
  (get-output-report-file-path "_处理报告"))

;取得输出报告图片文件完整路径:
(define (get-report/pictures-file-path)
  (get-output-report-file-path "_说明图片"))

;按输出类型取得输出报告文件完整路径:
(define (get-output-report-file-path name/type)
  (if (void? (task-object))
      #f
      (path->string
       (path-add-extension
        (build-path (task-task-path)       
                    (string-append
                     (task-source-folder)
                     name/type))
        ".pdf"))))
 
;转化缺陷类型为字符串：
(define (types->string types result)
  (if (empty? types)
      result
      (if (non-empty-string? result)
          (types->string
           (cdr types)
           (string-append result "、" (car types)))
          (types->string
           (cdr types)
           (string-append result (car types))))))

;报告内容：====================================================================
;输出报告页:
(define (output-page/report dc)
  (set-position-height 0);初始化绘图起点
  (increase-position-height 30);标题上增加空行
  (draw-title dc (get-title/task))
  (draw-title dc "无人机精细化巡视报告")
  (status-message "输出基本情况……")
  (draw-title/section dc "一、巡视基本情况")
  (draw-paragraph/text
   dc
   (string-append
    (task-inspect-date) (task-company-name) "（公司）完成"
    (task-voltage-grade) (task-line-name) "杆塔无人机精细化巡视，发现"
    "缺陷总计 " (number->string (get-defect/total-number)) " 处，其中"
    "一般缺陷 " (number->string (get-defect/normal-number)) " 处，"
    "严重缺陷 " (number->string (get-defect/serious-number)) " 处，"
    "危急缺陷 " (number->string (get-defect/critical-number)) " 处。"
    "发现的缺陷包括：" (types->string (get-pictures-defect-types) "") "等"
    " " (number->string (length (get-total-defects/types))) " 类。"))
  (draw-title/section dc "二、缺陷情况统计")
  (draw-title/subsection dc "(一)按缺陷等级统计:")
  (let ([n/normal (get-defect/normal-number)]
        [n/serious (get-defect/serious-number)]
        [n/critical (get-defect/critical-number)])
    (draw-paragraph/text
     dc
     (string-append
      "缺陷总共： " (number->string (get-defect/total-number)) " 处。"))
    (draw-paragraph/text
     dc
     (string-append
      "1、一般缺陷共计： " (number->string n/normal) " 处。"))
    (when (not (= n/normal 0))
      (draw-paragraph/text
       dc
       (string-append
        "    位置为：" (types->string (get-defect/node-names/normal) "") "。"))
      (draw-paragraph/text
       dc
       (string-append
        "    对应来源图片名为：" (types->string (get-defect/source-names/normal) "") "。")))
    (draw-paragraph/text
     dc
     (string-append
      "2、严重缺陷共计： " (number->string n/serious) " 处。"))
    (when (not (= n/serious 0))
      (draw-paragraph/text
       dc
       (string-append
        "    位置为：" (types->string (get-defect/node-names/serious) "") "。"))
      (draw-paragraph/text
       dc
       (string-append
        "    对应来源图片名为：" (types->string (get-defect/source-names/serious) "") "。")))
    (draw-paragraph/text
     dc
     (string-append
      "3、危急缺陷共计： " (number->string n/critical) " 处。"))
    (when (not (= n/critical 0))
      (draw-paragraph/text
       dc
       (string-append
        "    位置为：" (types->string (get-defect/node-names/critical) "") "。"))
      (draw-paragraph/text
       dc
       (string-append
        "    对应来源图片名为：" (types->string (get-defect/source-names/critical) "") "。"))))
  (draw-title/subsection dc "(二)按缺陷类别统计:")
  (draw-paragraph/text
   dc
   (string-append
    "缺陷类别总共计 " (number->string (length (get-total-defects/types))) " 类，如下："))
  (draw-paragraph-types dc);输出缺陷类型
  (status-message "输出、缺陷描述及图片……")
  (draw-title/section dc "三、缺陷描述及图片")
  (draw-defect-description-and-bitmap dc))

;取得任务名称标题:
(define (get-title/task)
  (string-append (task-voltage-grade)
                 (task-line-name)))

;绘制缺陷类别段落：
(define (draw-paragraph-types dc)
  (for ([n (in-naturals 1)]
        [defect (get-total-defects/types)]
        [type (get-pictures-defect-types)])
    (begin
      (draw-paragraph/text
       dc
       (string-append
        (number->string n)  "、"  type "：共 " (number->string (list-ref defect 0)) " 处。"))
     (draw-paragraph/text
      dc
      (string-append
       "    位置为：" (extend-str/defects-to-string (list-ref defect 1) "") "。"))
     (draw-paragraph/text
       dc
       (string-append
        "    对应来源图片名为：" (extend-str/defects-to-string (list-ref defect 2) "") "。")))))

;展开str/defects（来自get-total-defects/types取得的列表里的字串列表）为字串：
(define (extend-str/defects-to-string str/defects result)
  (if (empty? str/defects)
      result
      (if (non-empty-string? result)
          (extend-str/defects-to-string
           (cdr str/defects)
           (string-append result "、" (car str/defects)))
          (extend-str/defects-to-string
           (cdr str/defects)
           (car str/defects)))))

;绘制缺陷描述及缺陷图片（默认图片为800*450，高宽比约=0.618）：
(define (draw-defect-description-and-bitmap dc)
  (set-output-flag #t) ;设置输出标记
  (define n 1) ;序号：
  (for ([picture (get-defect-pictures)])
    (let ([source (read-bitmap (get-source-file-path picture))])
      (for ([defect (picture-node-defects picture)])
        ;显示输出进度：
        (status-message (string-append "正在处理" (get-defect-filename picture defect)))
        ;输出缺陷描述：
        (draw-paragraph/text
         dc
         (string-append (number->string n) "、" (get-defect-filename picture defect)))
        ;输出缺陷图片片段:
        (draw-paragraph/bitmap
         dc
         (get-picture/defect-section (copy-bitmap source) defect))
        (set! n (+ n 1)))))
  (set-output-flag #f))

;拷贝一张图片的副本：
(define (copy-bitmap source)
  (let ([dc (send (make-object bitmap%
          (send source get-width)
          (send source get-height))
        make-dc)])
    (send dc draw-bitmap source 0 0)
    (send dc get-bitmap)))

;输出输出处理报告描述图片：
(define (output-page/report/pictures dc)
  (set-position-height 0);初始化绘图起点
  (draw-defect-description-and-bitmap dc))