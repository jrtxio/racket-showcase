#lang racket
;task-struct-data.rkt
;项目数据。

(provide create-new-task
         task-object
         task-source-folder
         task-task-path
         task-tower-type
         task-voltage-grade
         task-line-name
         task-tower-number
         task-inspect-date
         task-company-name
         task-tower-pictures
         
         create-new-picture         
         picture-source-name
         picture-ext-name
         picture-node-name
         picture-node-defects
         
         create-new-defect
         node-defect-group
         node-defect-companent
         node-defect-type
         node-defect-description
         node-defect-grade
         node-defect-stamp-type
         node-defect-stamp

         id/node->id/source
         id/defect->id/source
         get-defect-descriptions
         get-defect-grades
         get-picture-by-source-id
         get-picture-defects
         get-source-names
         get-node/source-names
         get-node-names
         get-defect-names
         get-node-name
         set-node-name
         get-node-pictures
         get-defect-pictures
         get-picture-bitmap
         get-pictures-by-filenames
         delete-picture-defect
         get-defect-filename

         get-defect/total-number
         get-defect/normal-number
         get-defect/serious-number
         get-defect/critical-number
         get-defect/node-names/normal
         get-defect/node-names/serious
         get-defect/node-names/critical
         get-defect/source-names/normal
         get-defect/source-names/serious
         get-defect/source-names/critical
         get-pictures-defect-types
         get-total-defects/types
         )

(require racket/draw)

;操作任务结构=============================================================
;任务结构属性：-----------------------------------------------------------
(struct struct/task
  (source-folder ;源文件夹
   task-path ;任务路径
   tower-type ;杆塔类型
   voltage-grade ;电压等级
   line-name ;线路名称
   tower-number ;杆塔编号
   company-name ;负责检查的公司名称
   inspect-date ;检查日期
   tower-pictures);杆塔图片列表（所有）
  #:mutable #:transparent)

;定义任务值：
(define task (void))

;制作一个任务：
(define (create-new-task)
  (struct/task
   "" (void) "" "" "" "" "" "" null))

;设置任务结构对象：
(define task-object
  (case-lambda
    [()
     task]
    [(t)
     (set! task t)]))

;设置源文件夹：
(define task-source-folder
  (case-lambda
    [()
     (if (void? task)
         ""
         (struct/task-source-folder task))]
    [(name)
     (set-struct/task-source-folder! task name)]))

;设置任务文件路径:
(define task-task-path
  (case-lambda
    [()
     (if (void? task)
         (void)
         (struct/task-task-path task))]
    [(path)
     (set-struct/task-task-path! task path)]))

;设置塔杆类型：
(define task-tower-type
  (case-lambda
    [()
     (if (void? task)
         ""
         (struct/task-tower-type task))]
    [(type)
     (set-struct/task-tower-type! task type)]))

;设置电压等级:
(define task-voltage-grade
  (case-lambda
    [()
     (if (void? task)
         ""
         (struct/task-voltage-grade task))]
    [(voltage)
     (set-struct/task-voltage-grade! task voltage)]))

;设置线路名称：
(define task-line-name
  (case-lambda
    [()
     (if (void? task)
         ""
         (struct/task-line-name task))]
    [(line)
     (set-struct/task-line-name! task line)]))

;设置杆塔号：
(define task-tower-number
  (case-lambda
    [()
     (if (void? task)
         ""
         (struct/task-tower-number task))]
    [(number)
     (set-struct/task-tower-number! task number)]))

;设置公司名称：
(define task-company-name
  (case-lambda
    [()
     (if (void? task)
         ""
         (struct/task-company-name task))]
    [(name)
     (set-struct/task-company-name! task name)]))

;设置检查日期：
(define task-inspect-date
  (case-lambda
    [()
     (if (void? task)
         ""
         (struct/task-inspect-date task))]
    [(date)
     (set-struct/task-inspect-date! task date)]))

;设置源图片表:
(define task-tower-pictures
  (case-lambda
    [()
     (if (void? task)
         null
         (struct/task-tower-pictures task))]
    [(pictures)
     (set-struct/task-tower-pictures! task pictures)]))

;图片属性------------------------------------------------------
;图片属性结构:
(struct struct/picture
  (source-name ;源图片名
   ext-name ;图片扩展名（图片文件类型）
   node-name ;节点名称
   node-defects) ;缺陷
  #:mutable #:transparent)

;创建新的图片结构对象：
(define (create-new-picture)
  (struct/picture "" "" "" null))

;根据图片文件名创建图片结构：
(define (create-new-picture-by-name-and-ext file)
  (let ([split-file
         (string-split (path->string file) ".")])
    (struct/picture (list-ref split-file 0)
                    (list-ref split-file 1)
                    ""
                    null)))

;设置源图片名：
(define picture-source-name
  (case-lambda
    [(picture)
     (if (void? picture)
         ""
         (struct/picture-source-name picture))]
    [(picture name)
     (set-struct/picture-source-name! picture name)]))

;设置源图片扩展名:
(define picture-ext-name
  (case-lambda
    [(picture)
     (if (void? picture)
         ""
         (struct/picture-ext-name picture))]
    [(picture ext)
     (set-struct/picture-ext-name! picture ext)]))

;设置图片节点名称：
(define picture-node-name
  (case-lambda
    [(picture)
     (if (void? picture)
         ""
         (struct/picture-node-name picture))]
    [(picture name)
     (set-struct/picture-node-name! picture name)]))

;设置图片节点缺陷列表:
(define picture-node-defects
  (case-lambda
    [(picture)
     (if (void? picture)
         null
         (struct/picture-node-defects picture))]
    [(picture defects)
     (set-struct/picture-node-defects! picture defects)]))

;缺陷属性------------------------------------------------------
;节点缺陷属性结构:
(struct struct/defect
  (defect-group ;缺陷类别
    defect-companent ;缺陷部件
    defect-type ;缺陷类型
    defect-description ;缺陷描述
    defect-grade ;缺陷等级
    stamp-type ;标记类型（方形、矩形、圆形三种）
    defect-stamp) ;缺陷标记
  #:mutable #:transparent)

;创建struct/defect对象：
(define create-new-defect
  (case-lambda
    [()
    (struct/defect "" "" "" "" "" "" (make-hash))]
    [(description group companent type grade stamp-type stamp)
     (struct/defect
      description group companent type grade stamp-type stamp)]))

;设置缺陷类别：
(define node-defect-group
  (case-lambda
    [(defect)
     (struct/defect-defect-group defect)]
    [(defect group)
     (set-struct/defect-defect-group! defect group)]))

;设置缺陷部件：
(define node-defect-companent
  (case-lambda
    [(defect)
     (struct/defect-defect-companent defect)]
    [(defect companent)
     (set-struct/defect-defect-companent! defect companent)]))

;设置缺陷类型：
(define node-defect-type
  (case-lambda
    [(defect)
     (struct/defect-defect-type defect)]
    [(defect type)
     (set-struct/defect-defect-type! defect type)]))

;设置缺陷描述：
(define node-defect-description
  (case-lambda
    [(defect)
     (struct/defect-defect-description defect)]
    [(defect description)
     (set-struct/defect-defect-description! defect description)]))

;设置缺陷等级：
(define node-defect-grade
  (case-lambda
    [(defect)
     (struct/defect-defect-grade defect)]
    [(defect grade)
     (set-struct/defect-defect-grade! defect grade)]))

;设置标记类型:
(define node-defect-stamp-type
  (case-lambda
    [(defect)
     (struct/defect-stamp-type defect)]
    [(defect type)
     (set-struct/defect-stamp-type! defect type)]))

;设置缺陷标记：
(define node-defect-stamp
  (case-lambda
    [(defect)
     (struct/defect-defect-stamp defect)]
    [(defect stamp)
     (set-struct/defect-defect-stamp! defect stamp)]))

;缺陷标记------------------------------------------------------
;方形缺陷标记属性哈希表定义方式:
;表名：hash/stamp
;方形：
;x ;左上角x值
;y ;左上角y值
;width ;宽
;矩形：
;x1
;y1
;x2
;y2
;x3
;y3
;x4
;y4
;圆形：
;x
;y
;width
;height

;应用函数：========================================================
;节点序号转换为图片序号:
(define (id/node->id/source id/node)
  (name/node->id/source
   (picture-node-name
    (get-picture-by-node-id id/node))))

;节点序号转换为图片序号:
(define (id/defect->id/source id/defect)
  (name/node->id/source
   (picture-node-name
    (get-picture-by-defect-id id/defect))))

;根据节点名取得图片序号：
(define (name/node->id/source name/node)
  (index-of (task-tower-pictures)
            (get-picture-by-node-name name/node)))

;根据节点名称取得图片结构对象:
(define (get-picture-by-node-name name/node)
  (findf (lambda (picture)
           (string=?
            (picture-node-name picture)
            name/node))
         (task-tower-pictures)))

;根据源图片列表序号取得图片结构对象:
(define (get-picture-by-source-id id/source)
  (let ([pictures (task-tower-pictures)])
    (if (empty? pictures)
        (void)
        (list-ref (task-tower-pictures)
                  id/source))))

;根据节点列表序号取得图片结构对象:
(define (get-picture-by-node-id id/node)
  (list-ref (get-node-pictures) id/node))

;根据缺陷节点列表序号取得图片结构对象:
(define (get-picture-by-defect-id id/defect)
  (list-ref (get-defect-pictures) id/defect))

;取得所有具有节点名称的图片结构对象:
(define (get-node-pictures)
  (make-node-pictures (task-tower-pictures)
                      null))

;制作所有具有节点名称的图片结构对象列表:
(define (make-node-pictures pictures result)
  (if (empty? pictures)
      result
      (let ([pic (car pictures)])
        (if (non-empty-string? (picture-node-name pic))
            (make-node-pictures (cdr pictures)
                                (append result (list pic)))
            (make-node-pictures (cdr pictures)
                                result)))))

;取得所有具有有缺陷的图片结构对象:
(define (get-defect-pictures)
  (make-defect-pictures (task-tower-pictures)
                        null))

;制作所有具有有缺陷的图片结构对象列表:
(define (make-defect-pictures pictures result)
  (if (empty? pictures)
      result
      (let ([pic (car pictures)])
        (if (empty? (picture-node-defects pic))
            (make-defect-pictures (cdr pictures)
                                  result)
            (make-defect-pictures (cdr pictures)
                                  (append result (list pic)))))))

;根据源图片序号取得缺陷描述列表:
(define (get-defect-descriptions id/source)
  (map (lambda (defect)
         (node-defect-description defect))
       (get-picture-defects id/source)))


;取得缺陷等级列表:
(define (get-defect-grades id/source)
  (map (lambda (defect)
         (node-defect-grade defect))
       (get-picture-defects id/source)))

;取得指定图片缺陷列表：
(define (get-picture-defects id/source)
  (picture-node-defects
   (get-picture-by-source-id id/source)))

;取得源图片名列表:
(define (get-source-names)
  (map
   (lambda (picture)
     (struct/picture-source-name picture))
   (task-tower-pictures)))


;取得源图片名列表相对应的节点名列表:
(define (get-node/source-names)
  (map
   (lambda (picture)
     (struct/picture-node-name picture))
   (task-tower-pictures)))

;取得节点图片名列表:
(define (get-node-names)
  (map (lambda (picture)
         (struct/picture-node-name picture))
       (get-node-pictures)))

;取得有缺陷的节点图片名列表:
(define (get-defect-names)
  (map (lambda (picture)
         (picture-node-name picture))
       (get-defect-pictures)))

;根据源图片序号取得图片节点名:
(define (get-node-name id/source)
  (picture-node-name
   (get-picture-by-source-id id/source)))

;取得图片实体:
(define (get-picture-bitmap picture)
  (if (void? picture)
      (void)
      (let* ([name (string-append
                    (picture-source-name picture)
                    "."
                    (picture-ext-name picture))]
             [path (build-path (task-task-path)
                               (task-source-folder)
                               name)])
        (read-bitmap path))))

;根据文件名列表创建图片结构对象列表:
(define (get-pictures-by-filenames filenames)
  (map (lambda (file)
         (create-new-picture-by-name-and-ext file))
       filenames))

;为source-id对应图片设置节点名:
(define (set-node-name id/source name/node)
  (picture-node-name
   (get-picture-by-source-id id/source)
   name/node))

;取得缺陷输出图片名称：
(define (get-defect-filename picture defect)
  (string-append (task-voltage-grade)
                 (task-line-name)
                 (task-tower-number)
                 "-"
                 (picture-node-name picture)
                 (node-defect-description defect)
                 "-"
                 (picture-source-name picture)))

;删除指定的缺陷结构对象：
(define (delete-picture-defect id/source id/defect)
  (let* ([picture (get-picture-by-source-id id/source)]
         [defects (picture-node-defects picture)]
         [defect (list-ref defects id/defect)])
    (picture-node-defects picture
                          (remove defect defects))))

;统计函数：-----------------------------------------------------------
;缺陷总计数量：
(define (get-defect/total-number)
  (defect/total-number (task-tower-pictures) 0))

;统计缺陷总计：
(define (defect/total-number pictures result)
  (if (empty? pictures)
      result
      (defect/total-number
        (cdr pictures)
        (+ result
           (length (picture-node-defects
                    (car pictures)))))))

;一般缺陷数量：
(define (get-defect/normal-number)
  (defect/grade-number (task-tower-pictures) 0 0 "一般"))

;严重情况数量：
(define (get-defect/serious-number)
  (defect/grade-number (task-tower-pictures) 0 0 "严重"))

;危急情况数量：
(define (get-defect/critical-number)
  (defect/grade-number (task-tower-pictures) 0 0 "危急"))

;求指定缺陷等级的数量：
(define (defect/grade-number pictures id/defect result grade)
  (if (empty? pictures)
      result
      (if (empty? (picture-node-defects (car pictures)))
          (defect/grade-number (cdr pictures) 0 result grade)
          (if (>=
               id/defect
               (length (picture-node-defects (car pictures))))
              (defect/grade-number (cdr pictures) 0 result grade)
              (if (string=?
                   (node-defect-grade
                    (list-ref (picture-node-defects (car pictures)) id/defect))
                   grade)
                  (defect/grade-number pictures (+ id/defect 1) (+ result 1) grade)
                  (defect/grade-number pictures (+ id/defect 1) result grade))))))

;取得缺陷类型列表：
(define (get-pictures-defect-types)
  (remove-duplicates ;清除重复的列表成员
   (pictures-defect-types
    (task-tower-pictures) 0 null)))

;构造缺陷类型列表：
(define (pictures-defect-types pictures id/defect result )
  (if (empty? pictures)
      result
      (if (empty? (picture-node-defects (car pictures)))
          (pictures-defect-types (cdr pictures) 0 result)
          (if (>=
               id/defect
               (length (picture-node-defects (car pictures))))
              (pictures-defect-types (cdr pictures) 0 result)
              (let ([defect (list-ref
                             (picture-node-defects (car pictures))
                             id/defect)])
                (pictures-defect-types
                 (cdr pictures)
                 (+ id/defect 1)
                 (append result
                         (list
                          (string-append
                           (node-defect-companent defect)
                           (node-defect-type defect))))))))))

;一般缺陷节点名列表：
(define (get-defect/node-names/normal)
  (get-defect-pictures-names
   (task-tower-pictures) 0 null "一般" picture-node-name))

;严重缺陷节点名列表：
(define (get-defect/node-names/serious)
  (get-defect-pictures-names
   (task-tower-pictures) 0 null "严重" picture-node-name))

;危急缺陷节点名列表：
(define (get-defect/node-names/critical)
  (get-defect-pictures-names
   (task-tower-pictures) 0 null "危急" picture-node-name))

;一般缺陷源图片名列表：
(define (get-defect/source-names/normal)
  (get-defect-pictures-names
   (task-tower-pictures) 0 null "一般" picture-source-name))

;严重缺陷源图片名列表：
(define (get-defect/source-names/serious)
 (get-defect-pictures-names
   (task-tower-pictures) 0 null "严重" picture-source-name))

;危急缺陷源图片名列表：
(define (get-defect/source-names/critical)
  (get-defect-pictures-names
   (task-tower-pictures) 0 null "危急" picture-source-name))

;取得指定名称及等级列表：
(define (get-defect-pictures-names pictures id/defect result grade proc)
  (if (empty? pictures)
      result
      (if (empty? (picture-node-defects (car pictures)))
          (get-defect-pictures-names (cdr pictures) 0 result grade proc)
          (if (or
               (>=
               id/defect
               (length (picture-node-defects (car pictures))))
               (not (string=?
                   (node-defect-grade
                    (list-ref (picture-node-defects (car pictures)) id/defect))
                   grade)))
              (get-defect-pictures-names (cdr pictures) 0 result grade proc)
              (get-defect-pictures-names
               (cdr pictures)
               (+ id/defect 1)
               (append result (list (proc (car pictures))))
               grade proc)))))

;取得缺陷类型对应的数量、节点名列表、原图片名列表而组成的列表：
(define (get-total-defects/types)
  (map (lambda (type/defect)
         (let-values
             ([(number names/node names/source)
               (total-defects/types (get-defect-pictures) type/defect 0
                                    0 null null)])
           (list number names/node names/source)))
       (get-pictures-defect-types)))

;取得缺陷类型列表对应的类型数量、节点名列表、源图片名列表：
(define (total-defects/types  pictures/defect type/defect id/defect
                              number names/node name/source)
  (if (empty? pictures/defect)
      (values number names/node name/source)
      (let ([defects (picture-node-defects (car pictures/defect))])
      (if (>= id/defect (length defects))
          (total-defects/types (cdr pictures/defect) type/defect 0
                               number names/node name/source)
          (if (not (string=?
                    (string-append
                     (node-defect-companent (list-ref defects id/defect))
                     (node-defect-type (list-ref defects id/defect)))
                    type/defect))
              (total-defects/types pictures/defect type/defect (+ id/defect 1)
                                   number names/node name/source)
              (total-defects/types pictures/defect type/defect (+ id/defect 1)
                                   (+ number 1)
                                   (append
                                    names/node
                                    (list (picture-node-name
                                           (car pictures/defect))))
                                   (append
                                    name/source
                                    (list (picture-source-name
                                           (car pictures/defect))))))))))