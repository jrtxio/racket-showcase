;puzzle-canvas-class.rkt
;拼图画布类

#lang racket/gui

(require "main-frame-controler.rkt"
         "puzzle-model.rkt")

(provide puzzle-canvas%)

;拼图画布类：
(define/contract puzzle-canvas%
  ;puzzle-canvas%类的合约：
  (class/c
   (inherit [get-dc
             (->m (or/c (is-a?/c dc<%>) #f))]
            [get-x
             (->m real?)]
            [get-y
             (->m real?)])
   (override [on-event
              (->m (is-a?/c mouse-event%) void?)]))
  (class canvas%
    (inherit get-dc
             get-x
             get-y)
    
    (super-new)

    ;类字段：
    (field (dragging #f);单元格拖动状态
           (over #f));游戏结束标志

    ;覆盖定义鼠标事件方法：
    (define/override (on-event event)
      ;如果游戏结束，不再处理鼠标事件：
      (unless over
        (let ([dc (get-dc)]
              [x (send event get-x)]
              [y (send event get-y)])
          (msg-mouse-pos x y);显示鼠标位置信息。
          ;根据事件情况进行动态显示：
          (cond
            ;移动但不拖拽：
            [(and (send event moving?)
                  (not (send event dragging?)))
             (flag-cell-and-set-current x y dc)]
            ;拖拽并移动：
            [(and (send event moving?)
                  (send event dragging?))
             (move-cell-and-simulate-swap x y dc)]
            ;释放鼠标左键：
            [(and (send event button-up? 'left)
                  dragging)
             (swap-cell-current-to-target x y dc)]
            [else void]))))

    ;鼠标移动但不拖拽，
    ;对鼠标位置单元格进行标记并记录为当前单元格：
    (define/private (flag-cell-and-set-current x y dc)
      (let ([id (x&y->id x y)])
        (unless (= id (get-current-id))
          (draw-puzzle-picture dc)
          (define id/old (get-current-id))
          (draw-lose-focus-cell dc)
          (when (has-id? id)
            (set-current-id! id)
            (draw-focus-cell dc)))))

    ;拖拽并移动，
    ;移动当前单元格，在进入其它单元格时模拟调换场景：
    (define/private (move-cell-and-simulate-swap x y dc)
      (let ([id/target (drag-x&y->id x y)]
            [id (get-current-id)])
        (cond
          [(has-id? id/target)
           ;绘制两个单元格调换状态：
           (draw-puzzle-picture dc)
           (draw-blank-cell id/target dc)
           (draw-cell-to-id/target id id/target dc)
           (draw-cell-to-x&y x y id dc)]
          [else
           ;绘制当前单元格拖动状态：
           (draw-puzzle-picture dc)
           (draw-blank-cell id dc)
           (draw-cell-to-x&y x y id dc)])
        ;设置单元格拖动状态为#t：
        (unless dragging
          (set! dragging #t))))

    ;释放鼠标左键，
    ;将当前单元格与目标单元格调换：
    (define/private (swap-cell-current-to-target x y dc)
      (let ([id/target (drag-x&y->id x y)])
        (when (has-id? id/target)
          (let ([id (get-current-id)])
            ;将两个单元格调换：
            (swap-cell id/target id)
            (set-puzzle-picture!)
            (draw-puzzle-picture dc)
            ;检查是否成功拼合图片：
            (when (success?)
              (msg-string "恭喜！成功完成拼图！")
              (set! over #t))))
        ;设置单元格拖动状态为#f：
        (when dragging
          (set! dragging #f))))

    ;设置游戏重启：
    (define/public (replay)
      (set! over #f))
    ))
