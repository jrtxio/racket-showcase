#lang racket
;defect-type-data.rkt
;处理塔缺陷XML文件数据。
;注意:
;1、文件编码格式需确保为UTF-8格式,否则加载的时候将无法解析。

(provide sample-defect-type-file
         types/defect-object

         get-defect/group-names
         get-defect/component-names
         get-defect/type-descriptions
         get-defect/type-grades)
         
(require xml)

(require "xml-common.rkt")

;塔缺陷类型组列表对象(该值在本文件最后一行初始化):
(define types/defect null)

;数据结构:-------------------------------------
;定义塔缺陷数据结构:
(struct struct/defect/group;缺陷分组
  (name components)
  #:mutable #:transparent);
(struct struct/defect/component;缺陷部件
  (name types)
  #:mutable #:transparent)
(struct struct/defect/type;缺陷类型
  (description grade)
  #:mutable #:transparent)

;缺陷类型文件路径名:
(define tower-defects-filepath
  (build-path
   (current-directory)
   "surpport"
   "defect-type"
   "defect-type-sample.xml"))

;辅助操作作函数:-----------------------------------
;简化缺陷类型XML文件:
(define (sample-defect-type-file)
  (let* ([path (build-path
                (current-directory)
                "surpport"
                "defect-type")]
         [in-path (build-path path
                              "defect-type.xml")]
         [out-path (build-path path
                               "defect-type-sample.xml")])
    (sample-xml in-path out-path)))

;读取并解析缺陷类型文件-------------------------------
;读取缺陷分组列表:
(define (read-groups path)
  (elements->list
   (element-content
    (read-xml-document path))
   null
   read-group))

;读入并解析到缺陷分组结构对象:
(define (read-group elements)
  ;定义一个新的struct/defect/group结构对象:
  (define g (struct/defect/group "" null))
  (for ([e elements])
    (case (element-name e)
      [(name)
       (set-struct/defect/group-name!
        g (get-pcdata-string e))]
      [(components)
       (set-struct/defect/group-components!
        g
        (elements->list
         (element-content e)
         null
         read-component))]))
  ;返回struct/defect/group结构对象:
  g)

;读入并解析到缺陷部件结构对象:
(define (read-component elements)
  ;定义一个新的struct/defect/component结构对象:
  (define c (struct/defect/component "" null))
  (for ([e elements])
    (case (element-name e)
      [(name)
       (set-struct/defect/component-name!
        c (get-pcdata-string e))]
      [(types)
       (set-struct/defect/component-types!
        c
        (elements->list
         (element-content e)
         null
         read-type))]))
  ;返回struct/defect/component结构对象:
  c)

;读入并解析到缺陷类型结构对象:
(define (read-type elements)
  ;定义一个新的struct/defect/type结构对象:
  (define t (struct/defect/type "" ""))
  (for ([e elements])
    (case (element-name e)
      [(description)
       (set-struct/defect/type-description!
        t (get-pcdata-string e))]
      [(grade)
       (set-struct/defect/type-grade!
        t (get-pcdata-string e))]))
  ;返回struct/defect/type结构对象:
  t)

;操作函数:-----------------------------------------
;设置缺陷类型组列表对象:
(define types/defect-object
  (case-lambda
    [()
     types/defect]
    [(path)
     (set! types/defect (read-groups path))]))

;设置缺陷组名:
(define defect-group-name
  (case-lambda
    [(group)
     (struct/defect/group-name group)]
    [(group name)
     (set-struct/defect/group-name! group name)]))

;设置缺陷组部件列表:
(define defect-group-components
  (case-lambda
    [(group)
     (struct/defect/group-components group)]
    [(group components)
     (set-struct/defect/group-components! group components)]))

;设置部件名:
(define defect-component-name
  (case-lambda
    [(component)
     (struct/defect/component-name component)]
    [(component name)
     (set-struct/defect/component-name! component name)]))

;设置部件缺陷类型列表:
(define defect-component-types
  (case-lambda
    [(component)
     (struct/defect/component-types component)]
    [(component types)
     (set-struct/defect/component-types! component types)]))

;设置类型描述:
(define defect-type-description
  (case-lambda
    [(type)
     (struct/defect/type-description type)]
    [(type decsription)
     (set-struct/defect/type-description! type decsription)]))

;设置缺陷等级:
(define defect-type-grade
  (case-lambda
    [(type)
     (struct/defect/type-grade type)]
    [(type grade)
     (set-struct/defect/type-grade! type grade)]))

;应用函数:--------------------------------------------------
;取得缺陷分组名列表：
(define (get-defect/group-names)
  (map (lambda (group)
         (defect-group-name group))
       (types/defect-object)))

;通过缺陷组id取得缺陷部件名称列表:
(define (get-defect/component-names group-id)
  (map (lambda (component)
         (defect-component-name component))
       (get-defect/components group-id)))

;根据缺陷组列表序号取得缺陷部件列表:
(define (get-defect/components group-id)
  (defect-group-components
    (list-ref (types/defect-object) group-id)))

;通过缺陷组id及部件id取得缺陷类型描述列表:
(define (get-defect/type-descriptions group-id component-id)
  (map (lambda (type)
         (defect-type-description type))
       (get-defect/types group-id component-id)))

;通过缺陷组id及部件id取得缺陷等级列表:
(define (get-defect/type-grades group-id component-id)
  (map (lambda (type)
         (defect-type-grade type))
       (get-defect/types group-id component-id)))

;通过缺陷组id及部件组id取得缺陷类型列表:
(define (get-defect/types group-id component-id)
  (defect-component-types
    (list-ref
     (get-defect/components group-id)
     component-id)))

;初始化types/defect对象----------------------------------
(types/defect-object tower-defects-filepath)