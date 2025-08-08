#lang racket
;tower-type-data.rkt
;处理塔类型XML文件数据。
;注意：
;1、文件编码格式需确保为UTF-8格式，否则加载的时候将无法解析。

(provide sample-tower-type-file

         types/tower-object
         types/tower-filepath
         
         type/tower-type-names
         type/tower-node-ids
         type/tower-node-tags
         type/tower-node-names
         get-type/tower-bitmap-path)

(require xml)

(require "xml-common.rkt")

;塔类型结构列表对象(该值在本文件最后一行初始化):
(define types/tower null)

;数据结构：-------------------------------------
;定义塔型数据结构：
(struct struct/type/tower;塔类型
  (id name bitmap nodes)
  #:mutable #:transparent)
(struct struct/type/node;拍摄节点
  (id tag name)
  #:mutable #:transparent)

;塔类型文件路径名:
(define tower-types-filepath
  (build-path
   (current-directory)
   "surpport"
   "tower-type"
   "tower-type-sample.xml"))

;取得塔类型文件路径名:
(define types/tower-filepath
  (case-lambda
    [()
     tower-types-filepath]
    [(path)
     (set! tower-types-filepath path)]))

;辅助操作作函数：-----------------------------------
;简化杆塔类型XML文件:
(define (sample-tower-type-file)
  (let* ([path (build-path
                (current-directory)
                "surpport"
                "tower-type")]
         [in-path (build-path path
                              "tower-type.xml")]
         [out-path (build-path path
                               "tower-type-sample.xml")])
    (sample-xml in-path out-path)))

;读取并解析塔类型文件-------------------------------
;读取塔列表：
(define (read-towers path)
  (elements->list
   (element-content
    (read-xml-document path))
   null
   read-tower))

;读入塔元素并解析到塔结构对象:
(define (read-tower elements)
  ;定义一个新的struct/type/tower结构对象:
  (define t (struct/type/tower "" "" "" null))
  (for ([e elements])
    (case (element-name e)
      [(id)
       (set-struct/type/tower-id!
        t (get-pcdata-string e))]
      [(name)
       (set-struct/type/tower-name!
        t (get-pcdata-string e))]
      [(bitmap)
       (set-struct/type/tower-bitmap!
        t (get-pcdata-string e))]
      [(nodes)
       (set-struct/type/tower-nodes!
        t
        (elements->list
         (element-content e)
         null
         read-node))]))
  ;返回struct/type/tower结构对象:
  t)

;读入节点元素并解析到节点结构对象:
(define (read-node elements)
  ;定义一个新的struct/type/node结构对象:
  (define n (struct/type/node "" "" ""))
  (for ([e elements])
    (case (element-name e)
      [(id)
       (set-struct/type/node-id!
        n (get-pcdata-string e))]
      [(tag)
       (set-struct/type/node-tag!
        n (get-pcdata-string e))]
      [(name)
       (set-struct/type/node-name!
        n (get-pcdata-string e))]))
  ;返回struct/type/node结构对象:
  n)

;应用函数：-----------------------------------------
;设置塔类型列表对象:
(define types/tower-object
  (case-lambda
    [()
     types/tower]
    [(path)
     (set! types/tower (read-towers path))]))

;取得塔类型列表:
(define (type/tower-type-names)
  (map
   (lambda (tower)
     (struct/type/tower-name tower))
   (types/tower-object)))

;根据塔类型取得塔节点名称列表:
(define (type/tower-node-names type-name)
  (let ([nodes (struct/type/tower-nodes
                (get-type/tower-by-name type-name))])
    (map (lambda (node)
           (struct/type/node-name node)) nodes)))

;根据塔类型名取得塔节点编号：
(define (type/tower-node-ids type-name)
  (let ([nodes (struct/type/tower-nodes
                (get-type/tower-by-name type-name))])
    (map (lambda (node)
           (struct/type/node-id node)) nodes)))

;根据塔类型名取得塔节点标记:
(define (type/tower-node-tags type-name)
  (let ([nodes (struct/type/tower-nodes
                (get-type/tower-by-name type-name))])
    (map (lambda (node)
           (struct/type/node-tag node)) nodes)))

;根据塔类型名取得塔结构对象：
(define (get-type/tower-by-name name)
  (findf (lambda (tower)
           (string=?
            (struct/type/tower-name tower)
            name))
         (types/tower-object)))

;根据塔类型名取得塔图片路径:
(define (get-type/tower-bitmap-path name)
  (build-path
   (current-directory)
   "surpport"
   "tower-type"
   (struct/type/tower-bitmap
    (get-type/tower-by-name name))))

;初始化types/tower对象----------------------------------
(types/tower-object tower-types-filepath)