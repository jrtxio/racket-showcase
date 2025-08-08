#lang racket
;defects-output-data.rkt
;缺陷统计表输出。

(provide ouput-xslx/defects-table)

;(require simple-xlsx)

(require "task-struct-data.rkt"
         ;"xlsx-common.rkt"
         )

;=========================================================
;输出xslx格式的缺陷汇总表：
(define (ouput-xslx/defects-table)
  #|
  ;取得表格行数据：
  (get-lines-data
   (make-lines-data (get-defect-pictures) 0 null 1))
  ;输出表格：
  (ouput-xslx-table
   (get-table/defects-path)
   create-defects-table)|#
  void
)

;取得缺陷数据统计表完整路径：
(define (get-table/defects-path)
  (if (void? (task-object))
      #f
      (path->string
       (path-add-extension
        (build-path (task-task-path)       
                    (string-append
                     (task-source-folder)
                     "_缺陷数据统计表"))
        ".xlsx"))))

;制作行列表：
(define (make-lines-data pictures/defect id/defect result n)
  (if (empty? pictures/defect)
      result
      (if (>= id/defect
              (length
               (picture-node-defects (car pictures/defect))))
          (make-lines-data (cdr pictures/defect) 0 result n)
          (make-lines-data
           pictures/defect
           (+ id/defect 1)
           (append result
                   (list
                    (make-line-data
                     (car pictures/defect)
                     id/defect n)))
           (+ n 1)))))


;取得缺陷行数据：
(define (make-line-data picture id/defect n)
  (let ([defect (list-ref
                 (picture-node-defects picture)
                 id/defect)])
    (list n
          (task-company-name)
          (task-voltage-grade)
          (task-line-name)
          (task-tower-number)
          (node-defect-group defect)
          "" ;该数据未知，暂设置为空
          (node-defect-grade defect)
          (task-inspect-date)
          (get-defect-filename picture defect))))