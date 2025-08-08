#lang racket

;; 导入 json 库
(require json)

;; 1. 创建示例数据
(define sample-data
  (hash 'name "MyApp"
        'version "1.0"
        'settings (hash 'theme "dark"
                        'language "zh"
                        'favorites (list 1 2 3))
        'users (list
                (hash 'id 1 'name "Alice" 'active #t)
                (hash 'id 2 'name "Bob" 'active #f))))

;; 2. 写入 JSON 文件
(define (save-json-file data filename)
  (call-with-output-file filename
    (lambda (out)
      (write-json data out))
    #:exists 'replace))

;; 3. 读取 JSON 文件
(define (load-json-file filename)
  (if (file-exists? filename)
      (call-with-input-file filename
        (lambda (in)
          (define content (read-json in))
          (if (eof-object? content)
              (hash) ; 返回空哈希表
              content)))
      (hash))) ; 文件不存在返回空哈希表

;; 4. 测试代码
(define (run-json-demo)
  (printf "1. 原始数据结构：\n~a\n\n" sample-data)

  ;;保存到文件
  (save-json-file sample-data "config.json")

  ;; 从文件读取
  (define loaded-data (load-json-file "config.json"))
  (printf "3. 从文件读取数据：\n~a\n\n" loaded-data)

  ;; 演示数据访问
  (printf "4. 演示访问示例：\n")
  (printf "应用名称：~a\n" (hash-ref loaded-data 'name))
  (printf "主体设置：~a\n" (hash-ref (hash-ref loaded-data 'settings) 'theme))
  (printf "收藏列表：~a\n" (hash-ref (hash-ref loaded-data 'settings) 'favorites))

  ;; 修改数据
  (printf "\n5. 修改数据示例：\n")
  (define updated-data
    (hash-set loaded-data 'version "2.0"))

  ;; 保存修改后的数据
  (save-json-file updated-data "config.json")
  (printf "数据已更新，版本改为 2.0\n")

  ;; 验证更新
  (define final-data (load-json-file "config.json"))
  (printf "新版本号：~a\n" (hash-ref final-data 'version)))

;; 运行演示
(run-json-demo)