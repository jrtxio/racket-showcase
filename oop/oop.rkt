#lang racket/gui

;; ===== 1. 定义类 =====
(define person%  ; 类名通常以 % 结尾
  (class object%  ; 继承自 object%(基类)
    
    ;; 初始化参数
    (init-field name age)  ; 定义字段并接收初始化参数
    
    ;; 私有字段
    (field [id (random 1000)])  ; 私有字段,外部不能直接访问
    
    ;; 方法定义
    (define/public (greet)  ; 公共方法
      (format "你好,我是~a,今年~a岁" name age))
    
    (define/public (get-id)  ; 获取私有字段的方法
      id)
    
    (define/public (set-age new-age)  ; 设置年龄的方法
      (set! age new-age))
    
    ;; 构造函数(可选)
    (super-new)))  ; 调用父类构造函数

;; ===== 2. 创建对象 =====
(define person1 (new person% [name "小明"] [age 20]))
(define person2 (new person% [name "小红"] [age 18]))

;; ===== 3. 调用方法 =====
;; 使用 send 向对象发送消息
(displayln (send person1 greet))        ; "你好,我是小明,今年20岁"
(displayln (send person2 greet))        ; "你好,我是小红,今年18岁"
(displayln (send person1 get-id))       ; 显示随机ID

;; 修改对象状态
(send person1 set-age 21)
(displayln (send person1 greet))        ; "你好,我是小明,今年21岁"

;; ===== 4. 继承示例 =====
(define student%
  (class person%  ; 继承自 person%
    
    (init-field school)  ; 学生特有的字段
    
    ;; 重写父类方法
    (define/override (greet)
      (format "~a,我在~a上学" (super greet) school))  ; 调用父类方法
    
    ;; 新增方法
    (define/public (study subject)
      (format "~a正在学习~a" (get-field name this) subject))  ; 修复:使用 get-field 获取 name
    
    (super-new)))

(define student1 (new student% [name "小李"] [age 16] [school "清华大学"]))
(displayln (send student1 greet))        ; "你好,我是小李,今年16岁,我在清华大学上学"
(displayln (send student1 study "数学"))  ; "小李正在学习数学"

;; ===== 5. GUI中的实际应用 =====
(define frame (new frame% [label "面向对象演示"] [width 400] [height 200]))

;; 文本显示区域
(define display-text (new text%))
(define display-canvas (new editor-canvas% [parent frame] [editor display-text]))

;; 按钮:展示person1信息
(define show-person1-btn
  (new button%
       [parent frame]
       [label "显示小明信息"]
       [callback (lambda (button event)
                   (send display-text insert (send person1 greet))
                   (send display-text insert "\n"))]))

;; 按钮:展示student1信息
(define show-student1-btn
  (new button%
       [parent frame]
       [label "显示小李信息"]
       [callback (lambda (button event)
                   (send display-text insert (send student1 greet))
                   (send display-text insert "\n"))]))

;; 按钮:清空显示
(define clear-btn
  (new button%
       [parent frame]
       [label "清空"]
       [callback (lambda (button event)
                   (send display-text erase))]))

(send frame show #t)