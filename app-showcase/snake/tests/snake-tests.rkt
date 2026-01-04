#lang racket

;; 加载游戏核心代码
(require "../src/snake.rkt")
(require rackunit)

;; 测试套件
(define snake-tests
  (test-suite
   "贪吃蛇游戏核心功能测试"
   
   ;; 测试1：游戏状态初始化
   (test-case "测试游戏状态初始化" 
     (check-equal? (length (get-state 'snake)) 3 "蛇初始长度应为3")
     (check-equal? (get-state 'direction) 'right "蛇初始方向应为右")
     (check-equal? (get-state 'score) 0 "初始分数应为0")
     (check-equal? (get-state 'game-running?) #f "游戏初始状态应为未运行"))
   
   ;; 测试2：重启游戏功能
   (test-case "测试重启游戏功能" 
     ;; 先修改一些状态
     (set-state! 'score 10)
     (set-state! 'game-over? #t)
     (set-state! 'game-running? #f)
     
     ;; 重启游戏
     (restart-game)
     
     ;; 检查状态是否重置
     (check-equal? (length (get-state 'snake)) 3 "重启后蛇长度应为3")
     (check-equal? (get-state 'direction) 'right "重启后蛇方向应为右")
     (check-equal? (get-state 'score) 0 "重启后分数应为0")
     (check-equal? (get-state 'game-running?) #t "重启后游戏应为运行状态")
     (check-equal? (get-state 'game-over?) #f "重启后游戏不应为结束状态"))
   
   ;; 测试3：蛇重叠检测
   (test-case "测试蛇重叠检测" 
     ;; 设置一个蛇身重叠的情况
     (set-state! 'snake (list (cons 10 10) (cons 9 10) (cons 10 10)))
     (check-true (snake-overlap? (cons 10 10)) "应检测到蛇身重叠")
     (check-false (snake-overlap? (cons 5 5)) "应检测到非蛇身位置")
     )
   
   ;; 测试4：食物生成
   (test-case "测试食物生成" 
     ;; 确保生成的食物不在蛇身上
     (set-state! 'snake (list (cons 10 10) (cons 9 10) (cons 8 10)))
     (define food (generate-food))
     (check-false (snake-overlap? food) "生成的食物不应在蛇身上")
     (check-true (<= 0 (car food) (sub1 GRID-COUNT)) "食物X坐标应在有效范围内")
     (check-true (<= 0 (cdr food) (sub1 GRID-COUNT)) "食物Y坐标应在有效范围内")
     )
   
   ;; 测试5：蛇移动逻辑
   (test-case "测试蛇移动逻辑" 
     ;; 设置初始状态
     (set-state! 'snake (list (cons 10 10) (cons 9 10) (cons 8 10)))
     (set-state! 'direction 'right)
     (set-state! 'food (cons 12 10))
     
     ;; 保存初始状态
     (define initial-snake (get-state 'snake))
     (define initial-score (get-state 'score))
     
     ;; 执行一次移动
     (move-snake)
     
     ;; 检查移动结果
     (check-not-equal? (get-state 'snake) initial-snake "蛇应该移动了")
     (check-equal? (car (get-state 'snake)) (cons 11 10) "蛇头应该向右移动了一格")
     (check-equal? (get-state 'score) initial-score "没吃到食物时分数不应变化")
     )
   
   ;; 测试6：吃食物逻辑
   (test-case "测试吃食物逻辑" 
     ;; 设置蛇头即将吃到食物的状态
     (set-state! 'snake (list (cons 10 10) (cons 9 10) (cons 8 10)))
     (set-state! 'direction 'right)
     (set-state! 'food (cons 11 10))
     (set-state! 'score 0)
     
     ;; 保存初始长度和分数
     (define initial-length (length (get-state 'snake)))
     (define initial-score (get-state 'score))
     
     ;; 执行一次移动
     (move-snake)
     
     ;; 检查结果
     (check-equal? (length (get-state 'snake)) (add1 initial-length) "吃到食物后蛇长度应增加1")
     (check-equal? (get-state 'score) (add1 initial-score) "吃到食物后分数应增加1")
     (check-not-equal? (get-state 'food) (cons 11 10) "吃到食物后应生成新食物")
     )
   
   ;; 测试7：边界碰撞检测
   (test-case "测试边界碰撞检测" 
     ;; 设置蛇头在边界上的状态
     (set-state! 'snake (list (cons (sub1 GRID-COUNT) 10) (cons (- GRID-COUNT 2) 10) (cons (- GRID-COUNT 3) 10)))
     (set-state! 'direction 'right)
     (set-state! 'game-over? #f)
     (set-state! 'game-running? #t)
     
     ;; 执行一次移动，应该触发边界碰撞
     (move-snake)
     
     ;; 检查结果
     (check-equal? (get-state 'game-over?) #t "碰到边界后游戏应结束")
     (check-equal? (get-state 'game-running?) #f "碰到边界后游戏应停止运行")
     )
   
   ;; 测试8：自身碰撞检测
   (test-case "测试自身碰撞检测" 
     ;; 设置蛇头即将碰到自身的状态
     (set-state! 'snake (list (cons 10 10) (cons 9 10) (cons 9 11) (cons 10 11) (cons 10 10)))
     (set-state! 'direction 'down)
     (set-state! 'game-over? #f)
     (set-state! 'game-running? #t)
     
     ;; 执行一次移动，应该触发自身碰撞
     (move-snake)
     
     ;; 检查结果
     (check-equal? (get-state 'game-over?) #t "碰到自身后游戏应结束")
     (check-equal? (get-state 'game-running?) #f "碰到自身后游戏应停止运行")
     )
   
   ;; 测试9：方向改变逻辑
   (test-case "测试方向改变逻辑" 
     ;; 设置初始方向
     (set-state! 'direction 'right)
     
     ;; 测试方向改变
     (set-state! 'direction 'left) ; 改变方向为左
     (check-equal? (get-state 'direction) 'left "方向应该可以改变为左")
     
     ;; 测试正常方向改变
     (set-state! 'direction 'up)
     (check-equal? (get-state 'direction) 'up "方向应该可以改变为上")
     
     (set-state! 'direction 'down)
     (check-equal? (get-state 'direction) 'down "方向应该可以改变为下")
     )
   )
  )

;; 运行测试
(module+ test
  (require rackunit/text-ui)
  (run-tests snake-tests))
