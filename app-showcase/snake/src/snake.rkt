#lang racket/gui

(require racket/random)

;; 游戏常量定义
(provide GRID-SIZE GRID-COUNT WINDOW-WIDTH WINDOW-HEIGHT CONTROL-HEIGHT GAME-SPEED)

(provide game-state get-state set-state! snake-overlap? generate-food move-snake restart-game)

(define GRID-SIZE 20)
(define GRID-COUNT 20)
(define WINDOW-WIDTH (* GRID-SIZE GRID-COUNT))
(define WINDOW-HEIGHT (* GRID-SIZE GRID-COUNT))
(define CONTROL-HEIGHT 50)
(define GAME-SPEED 150) ; 毫秒

;; 游戏状态定义
(define game-state
  (let ((initial-snake (list (cons 10 10) (cons 9 10) (cons 8 10))))
    (box
     (list
      (cons 'snake initial-snake)
      (cons 'direction 'right)
      (cons 'food (cons (random GRID-COUNT) (random GRID-COUNT)))
      (cons 'score 0)
      (cons 'high-score 0)
      (cons 'game-running? #f)
      (cons 'game-paused? #f)
      (cons 'game-over? #f))))) 

;; 获取游戏状态值
(define (get-state key)
  (cdr (assoc key (unbox game-state))))

;; 设置游戏状态值
(define (set-state! key value)
  (set-box! game-state
            (cons (cons key value)
                  (filter (lambda (pair) (not (equal? (car pair) key)))
                          (unbox game-state)))))

;; 检查位置是否与蛇身重叠
(define (snake-overlap? pos)
  (not (not (member pos (get-state 'snake) equal?))))

;; 生成新的食物位置
(define (generate-food)
  (let loop ()
    (let ((new-food (cons (random GRID-COUNT) (random GRID-COUNT))))
      (if (snake-overlap? new-food)
          (loop)
          new-food))))

;; 绘制蛇
(define (draw-snake dc)
  (send dc set-brush "green" 'solid)
  (send dc set-pen "darkgreen" 1 'solid)
  (for-each
   (lambda (pos)
     (let ((x (* GRID-SIZE (car pos)))
           (y (* GRID-SIZE (cdr pos))))
       (send dc draw-rectangle x y GRID-SIZE GRID-SIZE)))
   (get-state 'snake)))

;; 绘制食物
(define (draw-food dc)
  (let ((food (get-state 'food))
        (x (* GRID-SIZE (car (get-state 'food))))
        (y (* GRID-SIZE (cdr (get-state 'food)))))
    (send dc set-brush "red" 'solid)
    (send dc set-pen "darkred" 1 'solid)
    (send dc draw-ellipse x y GRID-SIZE GRID-SIZE)))

;; 绘制分数
(define (draw-scores dc)
  (send dc set-font (make-font #:size 12 #:weight 'bold))
  (send dc set-text-foreground "black")
  (send dc draw-text (format "Score: ~a" (get-state 'score)) 10 (- WINDOW-HEIGHT 20))
  (send dc draw-text (format "High Score: ~a" (get-state 'high-score)) 150 (- WINDOW-HEIGHT 20)))

;; 绘制游戏结束界面
(define (draw-game-over dc)
  (send dc set-font (make-font #:size 24 #:weight 'bold))
  (send dc set-text-foreground "red")
  (send dc draw-text "GAME OVER" 100 150)
  (send dc set-font (make-font #:size 16 #:weight 'normal))
  (send dc draw-text "Press 'R' to restart" 120 180))

;; 绘制暂停界面
(define (draw-paused dc)
  (send dc set-font (make-font #:size 24 #:weight 'bold))
  (send dc set-text-foreground "blue")
  (send dc draw-text "PAUSED" 130 150))

;; 主绘图函数
(define (on-paint canvas dc)
  (let* ((width (send canvas get-width))
         (height (send canvas get-height)))
    (send dc clear)
    ;; 绘制背景
    (send dc set-brush "lightgray" 'solid)
    (send dc set-pen "gray" 1 'solid)
    (send dc draw-rectangle 0 0 width height)
    
    ;; 绘制网格
    (send dc set-pen "gray" 1 'solid)
    (for ((i (in-range (add1 GRID-COUNT))))
      (let ((x (* i GRID-SIZE))
            (y (* i GRID-SIZE)))
        (send dc draw-line x 0 x height)
        (send dc draw-line 0 y width y)))
    
    ;; 绘制游戏元素
    (draw-snake dc)
    (draw-food dc)
    (draw-scores dc)
    
    ;; 绘制游戏状态
    (if (get-state 'game-over?)
        (draw-game-over dc)
        (when (get-state 'game-paused?)
          (draw-paused dc)))))

;; 移动蛇
(define (move-snake)
  (let* ((snake (get-state 'snake))
         (head (car snake))
         (dir (get-state 'direction))
         (new-head
          (case dir
            ((up) (cons (car head) (sub1 (cdr head))))
            ((down) (cons (car head) (add1 (cdr head))))
            ((left) (cons (sub1 (car head)) (cdr head)))
            ((right) (cons (add1 (car head)) (cdr head))))))
    
    ;; 检查碰撞
    (cond
      ;; 碰到边界
      ((or (< (car new-head) 0)
           (>= (car new-head) GRID-COUNT)
           (< (cdr new-head) 0)
           (>= (cdr new-head) GRID-COUNT))
       (set-state! 'game-running? #f)
       (set-state! 'game-over? #t))
      ;; 碰到自身
      ((member new-head (cdr snake) equal?)
       (set-state! 'game-running? #f)
       (set-state! 'game-over? #t))
      ;; 正常移动
      (else
       (let ((new-snake (cons new-head snake)))
         ;; 检查是否吃到食物
         (if (equal? new-head (get-state 'food))
             ;; 吃到食物，增加分数，生成新食物，不删除尾部
             (begin
               (set-state! 'score (add1 (get-state 'score)))
               (when (> (get-state 'score) (get-state 'high-score))
                 (set-state! 'high-score (get-state 'score)))
               (set-state! 'food (generate-food))
               (set-state! 'snake new-snake))
             ;; 没吃到食物，删除尾部
             (set-state! 'snake (reverse (cdr (reverse new-snake))))))))))

;; 自定义 canvas 类，重写 on-char 方法处理键盘事件
(define my-canvas%
  (class canvas%
    (super-new)
    
    (define/override (on-char event)
      (let ((key-code (send event get-key-code)))
        (case key-code
          ;; 方向键控制
          ((up down left right)
           (when (and (get-state 'game-running?) (not (get-state 'game-over?)))
             (let ((current-dir (get-state 'direction)))
               ;; 防止反向移动
               (unless (or (and (eq? current-dir 'up) (eq? key-code 'down))
                           (and (eq? current-dir 'down) (eq? key-code 'up))
                           (and (eq? current-dir 'left) (eq? key-code 'right))
                           (and (eq? current-dir 'right) (eq? key-code 'left)))
                 (set-state! 'direction key-code)))))  
          ;; 空格键暂停/继续
          ((#\space)
           (when (and (get-state 'game-running?) (not (get-state 'game-over?)))
             (set-state! 'game-paused? (not (get-state 'game-paused?)))))
          ;; R键重新开始
          ((#\r #\R)
           (restart-game)
           (send this focus))
          ;; S键开始游戏
          ((#\s #\S)
           (when (not (get-state 'game-running?))
             (set-state! 'game-running? #t)
             (set-state! 'game-paused? #f))))))))



;; 重启游戏
(define (restart-game)
  (set-state! 'snake (list (cons 10 10) (cons 9 10) (cons 8 10)))
  (set-state! 'direction 'right)
  (set-state! 'food (generate-food))
  (set-state! 'score 0)
  (set-state! 'game-running? #t)
  (set-state! 'game-paused? #f)
  (set-state! 'game-over? #f))

;; 定时器回调函数
(define (on-tick canvas)
  (when (and (get-state 'game-running?) (not (get-state 'game-paused?)) (not (get-state 'game-over?)))
    (move-snake)
    (send canvas refresh)))

;; 创建主窗口
(define frame (new frame% [label "贪吃蛇游戏"]
                   [width WINDOW-WIDTH]
                   [height (+ WINDOW-HEIGHT CONTROL-HEIGHT)]
                   [style '(no-resize-border)]))

;; 创建垂直面板
(define panel (new vertical-panel% [parent frame]
                   [spacing 0]))

;; 创建控制面板
(define control-panel (new horizontal-panel% [parent panel]
                          [spacing 10]
                          [alignment '(center center)]
                          [min-height CONTROL-HEIGHT]))

;; 创建开始按钮
(define start-button
  (new button% [parent control-panel]
       [label "开始游戏 (S)"]
       [callback (lambda (btn event)
                   (set-state! 'game-running? #t)
                   (set-state! 'game-paused? #f)
                   (set-state! 'game-over? #f)
                   (send canvas focus))])
  )

;; 创建暂停按钮
(define pause-button
  (new button% [parent control-panel]
       [label "暂停/继续 (Space)"]
       [callback (lambda (btn event)
                   (when (get-state 'game-running?) 
                     (set-state! 'game-paused? (not (get-state 'game-paused?)))))]
       [enabled #t])
  )

;; 创建重新开始按钮
(define restart-button
  (new button% [parent control-panel]
       [label "重新开始 (R)"]
       [callback (lambda (btn event) 
                   (restart-game)
                   (send canvas focus))])
  )

;; 创建游戏画布
(define canvas (new my-canvas% [parent panel]
                    [min-width WINDOW-WIDTH]
                    [min-height WINDOW-HEIGHT]
                    [paint-callback (lambda (canvas dc) (on-paint canvas dc))]
                    [style '(border)]))

;; 绑定键盘事件
(send canvas focus)

;; 创建定时器
(define timer (new timer% [notify-callback (lambda () (on-tick canvas))]
                   [interval GAME-SPEED]))

;; 创建一个监控定时器，用于检查窗口是否关闭
(define monitor-timer 
  (new timer% [notify-callback (lambda () 
                                 (unless (send frame is-shown?) 
                                   (send timer stop) 
                                   (send monitor-timer stop) 
                                   (exit)))]
              [interval 100]))

;; 显示窗口后手动触发一次重绘
(send frame center)
(send frame show #t)
(send canvas refresh)


