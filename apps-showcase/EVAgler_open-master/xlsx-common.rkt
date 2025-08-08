#lang racket
;xlsx-common.rkt
;xlsx通用函数。

(provide ouput-xslx-table
         get-lines-data
         create-defects-table)

(require simple-xlsx)

;表格行数据：
(define data/lines null)

;根据proc输出xslx格式的汇总表：
(define (ouput-xslx-table path proc)
  (let ([xlsx (new xlsx%)])
    (proc xlsx)
    (write-xlsx-file xlsx path)))

;创建缺陷统计数据表：
(define (create-defects-table xlsx)
  (let ([name/sheet "缺陷数据统计表"])
    ;添加缺陷数据：
    (send xlsx add-data-sheet
          #:sheet_name name/sheet
          #:sheet_data data/lines)
    ;添加表格线：
    (send xlsx add-data-sheet-cell-style!
          #:sheet_name name/sheet
          #:cell_range
          (string-append
           "A1-J"
           (number->string (length data/lines)))
          #:style '( (borderStyle . thin)
                     (borderColor . "black")))
    ;将表头文字居中：
    (send xlsx add-data-sheet-cell-style!
          #:sheet_name name/sheet
          #:cell_range "A1-J1"
          #:style '( (horizontalAlign . center) ))))

;取得缺陷所有行数据：
(define (get-lines-data lines)
  (set! data/lines
        (append
         (list (get-header-data))
         lines)))

;取得表头数据：
(define (get-header-data)
  (list "序号" "项目单位" "电压等级" "线路名称" "杆塔号"
        "缺陷类别" "缺陷精度" "缺陷等级" "发现时间" "缺陷图像"))
