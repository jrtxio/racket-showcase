#lang racket
;xml-common.rkt
;通用xml文档操作函数。

(provide sample-xml
         read-xml-document
         elements->list
         get-pcdata-string)

(require xml)

;整理XML文件为简化格式:
(define (sample-xml in-path out-path)
  (call-with-output-file out-path
    (lambda (out)
      (write-string
       (call-with-input-file in-path
         (lambda (in)
           (read-and-group-lines in "")))
       out))
    #:exists 'replace))

;读取文件返回第一级元素:
(define (read-xml-document path)
   (document-element
    (call-with-input-file path
      (lambda (in)
        (read-xml in)))))

;读入每一行进行组合简化:
(define (read-and-group-lines in str)
  (let ([s (read-line in)])
    (if (eq? s eof)
        str
        (read-and-group-lines
         in
         (string-append
          str
          (string-trim s))))))

;递归读取元素并解析成列表:
(define (elements->list elements result proc)
  (if (empty? elements)
      result
      (elements->list
       (cdr elements)
       (append result
               (list (proc
                      (element-content
                       (car elements)))))
       proc)))

;取得元素的字串内容(拥有单个元素时):
(define (get-pcdata-string e)
  (pcdata-string
   (first (element-content e))))