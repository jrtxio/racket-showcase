#lang racket
;document-task-data.rkt
;对任务结构数据进行文档处理。

(provide sample-xml
         elements->list
         
         get-default-file-path
         read-task-document
         write-task-document)

(require xml)

(require "task-struct-data.rkt"
         "xml-common.rkt")

;操作XML文档=======================================================
;辅助操作函数------------------------------------------------------
;简化任务文件:
(define (sample-task-file)
  (let* ([path (string->path "E:\\Documents\\程序设计\\架空输电线路无人机巡检影像数据处理\\sample\\泰安公司110kV丰磁观支线无人机巡视资料\\#001无人机巡视资料\\2019年无人机巡视资料\\")]
         [in-path (build-path path
                              "11月无人机巡视资料-test.tsk")]
         [out-path (build-path path
                               "11月无人机巡视资料.tsk")])
    (sample-xml in-path out-path)))

;取得当前默认文件路径：
(define (get-default-file-path)
  (let* ([path (task-task-path)]
         [file (task-source-folder)]
         [ext "tsk"])
    (build-path path
                (string-append file "." ext))))

;读取并解析任务文件---------------------------------------------
(module+ test
;测试程序:
(define (test-read-task-document)
  (let* ([path (string->path "E:\\Documents\\程序设计\\架空输电线路无人机巡检影像数据处理\\sample\\泰安公司110kV丰磁观支线无人机巡视资料\\#001无人机巡视资料\\2019年无人机巡视资料\\")]
         [file-path (build-path path
                                "11月无人机巡视资料.tsk")])
    (read-task-document file-path)))
)

;读取任务文件:
(define (read-task-document path)
  ;创建新的struct/task结构对象:
  (task-object (create-new-task))
  ;读取任务文件到任务结构对象：
  (read-task
   (element-content
    (read-xml-document path))))

;读入任务并解析到任务结构对象:
(define (read-task elements)
  (for ([e elements])
    (case (element-name e)
      [(task-path)
       (task-task-path
        (string->path
         (pcdata-string
          (first (element-content e)))))]
      [(source-folder)
       (task-source-folder
        (pcdata-string
         (first (element-content e))))]
      [(tower-type)
       (task-tower-type
        (pcdata-string
         (first (element-content e))))]
      [(line-name)
       (task-line-name
        (pcdata-string
         (first (element-content e))))]
      [(voltage-grade)
       (task-voltage-grade
        (pcdata-string
         (first (element-content e))))]
      [(tower-number)
       (task-tower-number
        (pcdata-string
         (first (element-content e))))]
      [(company-name)
       (task-company-name
        (pcdata-string
         (first (element-content e))))]
      [(inspect-date)
       (task-inspect-date
        (pcdata-string
         (first (element-content e))))]
      [(tower-pictures)
       (task-tower-pictures
        (elements->list
         (element-content e)
         null
         read-picture))])))

;读入图片结构:
(define (read-picture elements)
  ;定义一个新的struct/picture结构对象:
  (define p (create-new-picture))
  (for ([e elements])
    (case (element-name e)
      [(source-name)
       (picture-source-name
        p
        (pcdata-string
         (first (element-content e))))]
      [(ext-name)
       (picture-ext-name
        p
        (pcdata-string
         (first (element-content e))))]
      [(node-name)
       (picture-node-name
        p
        (pcdata-string
         (first (element-content e))))]
      [(node-defects)
       (picture-node-defects
        p
        (elements->list
         (element-content e)
         null
         read-defect))]))
  ;返回struct/picture结构对象:
  p)

;读取缺陷结构:
(define (read-defect elements)
  ;定义一个新的struct/defect结构对象:
  (define d (create-new-defect))
  ;根据列表元素设置结构对象:
  (for ([e elements])
    (case (element-name e)
      [(defect-group)
       (node-defect-group
        d
        (pcdata-string
         (first (element-content e))))]
      [(defect-companent)
       (node-defect-companent
        d
        (pcdata-string
         (first (element-content e))))]
      [(defect-type)
       (node-defect-type
        d
        (pcdata-string
         (first (element-content e))))]
      [(defect-description)
       (node-defect-description
        d
        (pcdata-string
         (first (element-content e))))]
      [(defect-grade)
       (node-defect-grade
        d
        (pcdata-string
         (first (element-content e))))]
      [(stamp-type)
       (node-defect-stamp-type
        d
        (pcdata-string
         (first (element-content e))))]
      [(defect-stamp)
       (node-defect-stamp
        d
        (read-stamp
         (element-content e)
         (make-hash)))]))
  ;返回struct/defect结构对象:
  d)

;读取缺陷标记:
(define (read-stamp elements result)
  (if (empty? elements)
      result
      (let ([e (car elements)])
        (read-stamp
         (cdr elements)
         (begin
           (hash-set! result
                      (element-name e)
                      (string->number
                       (pcdata-string
                       (first
                        (element-content e)))))
           result)))))
                  
;写入XML任务文件---------------------------------------------
(module+ test
;测试程序:
(define (test-write-task-document)
  (let* ([path (string->path "E:\\Documents\\程序设计\\架空输电线路无人机巡检影像数据处理\\sample\\泰安公司110kV丰磁观支线无人机巡视资料\\#001无人机巡视资料\\2019年无人机巡视资料\\")]
         [file-path (build-path path
                                "11月无人机巡视资料-write.tsk")])
    (write-task-document (test-read-task-document)
                         file-path)))
)

;写入任务文件:
(define (write-task-document path)
  (when (not (void? (task-object)))
    (call-with-output-file path
      (lambda (out)
        (write-string
         (write-task)
         out))
      #:exists 'replace)))

;写入任务结构:
(define (write-task)
  (string-append
   "<task>"
   "<task-path>"
   (path->string
    (task-task-path))
   "</task-path>"
   "<source-folder>"
   (task-source-folder)
   "</source-folder>"
   "<tower-type>"
   (task-tower-type)
   "</tower-type>"
   (if (non-empty-string? (task-line-name))
       (string-append
        "<line-name>"
        (task-line-name)
        "</line-name>")
       "")
   (if (non-empty-string? (task-voltage-grade))
       (string-append
        "<voltage-grade>"
        (task-voltage-grade)
        "</voltage-grade>")
       "")
   (if (non-empty-string? (task-tower-number))
       (string-append
        "<tower-number>"
        (task-tower-number)
        "</tower-number>")
       "")
   (if (non-empty-string? (task-company-name))
       (string-append
        "<company-name>"
        (task-company-name)
        "</company-name>")
       "")
   (if (non-empty-string? (task-inspect-date))
       (string-append
        "<inspect-date>"
        (task-inspect-date)
        "</inspect-date>")
       "")
   (let ([pictures (task-tower-pictures)])
     (if (empty? pictures)
         ""
         (string-append
          "<tower-pictures>"
          (apply string-append
                 (map (lambda (p)
                        (write-struct/picture p))
                      pictures))
          "</tower-pictures>")))
   "</task>"))

;写入struct/picture结构对象:
(define (write-struct/picture object)
  (string-append
   "<tower-picture>"
   "<source-name>"
   (picture-source-name object)
   "</source-name>"
   "<ext-name>"
   (picture-ext-name object)
   "</ext-name>"
   (if (non-empty-string? (picture-node-name object))
       (string-append
        "<node-name>"
        (picture-node-name object)
        "</node-name>")
       "")
   (let ([defects (picture-node-defects object)])
     (if (empty? defects)
         ""
         (string-append
          "<node-defects>"
          (apply string-append
                 (map (lambda (d)
                        (write-struct/defect d))
                      defects))
          "</node-defects>")))
   "</tower-picture>"))

;写入struct/defect结构对象:
(define (write-struct/defect object)
  (string-append
   "<node-defect>"
   "<defect-group>"
   (node-defect-group object)
   "</defect-group>"
   "<defect-companent>"
   (node-defect-companent object)
   "</defect-companent>"
   "<defect-type>"
   (node-defect-type object)
   "</defect-type>"
   "<defect-description>"
   (node-defect-description object)
   "</defect-description>"
   "<defect-grade>"
   (node-defect-grade object)
   "</defect-grade>"
   "<stamp-type>"
   (node-defect-stamp-type object)
   "</stamp-type>"
   "<defect-stamp>"
   (write-hash-stamp
    (node-defect-stamp object))
   "</defect-stamp>"
   "</node-defect>"))

;写入defect-stamp哈希表:
(define (write-hash-stamp stamp)
  (apply string-append
         (hash-map stamp
                   (lambda (key value)
                     (string-append
                      "<" (symbol->string key) ">"
                      (number->string value)
                      "</"(symbol->string key) ">")))))

(module+ test
  ;测试:
  ;(test-read-task-document)
  ;(write-task)
  )