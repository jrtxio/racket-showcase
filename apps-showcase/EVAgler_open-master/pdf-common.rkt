#lang racket
;pdf-common.rkt
;PDF文件输出通用函数。
;输出页面为A4；
;字体大小自动设置。

(provide output-doc-by-procedure
         create-output-dc/pdf
         set-a4-paper
         draw-title
         draw-title/section
         draw-title/subsection
         draw-paragraph/text
         draw-paragraph/bitmap
         set-position-height
         increase-position-height)

(require racket/draw)

;定义当前页面绘制点高度位置:
(define position-height 0)

;输出文档:
(define (output-doc-by-procedure dc proc)
  (when (send dc ok?)
    (send dc start-doc "开始输出PDF文件……")
    (send dc start-page)
    (proc dc) ;输出页面内容
    (send dc end-page)
    (send dc end-doc)))

;创建用于输出的pdf-dc:
(define (create-output-dc/pdf filepath [parent #f])
  (new pdf-dc%
       [interactive #f]
       [parent parent]
       [use-paper-bbox #f]
       [as-eps #f]
       [width #f]
       [height #f]
       [output filepath]))

;设置页面为A4大小:
(define (set-a4-paper)
  (let ([ps (current-ps-setup)])
    (send ps set-paper-name "A4 210 x 297 mm")
    (send ps set-margin 64 48)))

;设置标题字体:
(define (get-title-font)
  (make-font
    #:size 18 #:weight 'bold))

;设置节标题字体：
(define (get-title/section-font)
  (make-font
   #:size 14 #:weight 'bold))

;设置小节标题字体:
(define (get-title/subsection-font)
  (make-font
   #:size 12 #:weight 'bold))
  
;设置段落字体:
(define (get-paragraph-font)
  (make-font #:size 12))

;检查页末状态：
(define (check-end-page dc h h/size)
  (when (> (+ position-height h) h/size)
    (begin
      (send dc end-page)
      (send dc start-page)
      ;初始化绘图起点
      (set! position-height 0))))

;将页面当前绘制点高度增值:
(define (increase-position-height height)
  (let ([gap 10])
    (set! position-height
          (+ position-height height gap))))

;取得页面边距:
(define (get-paper-margin)
  (let ([h/m (box 0.0)]
        [v/m (box 0.0)])
    (send (current-ps-setup)
          get-margin h/m v/m)
    (values (unbox h/m) (unbox v/m))))

;绘制标题:
(define (draw-title dc str)
  (send dc set-font (get-title-font))
  (let*-values
      ([(w/str h/str d/str a/str)
        (send dc get-text-extent str)]
       [(w/paper h/paper) (send dc get-size)])
    (send dc draw-text
          str
          (exact-floor
           (/ (- w/paper w/str) 2))
          position-height)
    (increase-position-height h/str)))

;写节标题：
(define (draw-title/section dc str)
  (send dc set-font (get-title/section-font))
  (draw-string-line dc str))

;写小节标题:
(define (draw-title/subsection dc str)
  (send dc set-font (get-title/subsection-font))
  (draw-string-line dc str))

;写段落内容：
(define (draw-paragraph/text dc str)
  (send dc set-font (get-paragraph-font))
  (let*-values ([(w/size h/size) (send dc get-size)]
                [(lines)
                 (string->lines
                  (string-append "    " str) ;在首行前边添加4个空格
                  0 null dc w/size)])
    (for-each (lambda (line)
                (draw-string-line dc line))
              lines)))

;对段落字串进行行划分：
(define (string->lines str n result dc width)
  (if (>= n (string-length str))
      (append result (list str))
      (let-values ([(w/str h/str d/str a/str)
                    (send dc get-text-extent
                          (substring str 0 n))])
        (if (> w/str width)
            (string->lines (substring str n)
                           0
                           (append
                            result
                            (list (substring str 0 (- n 1))))
                           dc width)
            (string->lines str (+ n 1) result dc width)))))

;写一行字串内容：
(define (draw-string-line dc str)
  (let-values ([(w/str h/str d/str a/str)
                (send dc get-text-extent str)]
               [(w/size h/size) (send dc get-size)])
    (check-end-page dc h/str h/size)
    (send dc draw-text str 0 position-height)
    (increase-position-height h/str)))

;绘制图片：
(define (draw-paragraph/bitmap dc source)
  (let*-values ([(w/size h/size) (send dc get-size)])
    (let* ([w/source (send source get-width)]
           [h/source (send source get-height)]
           [sc (if (> w/source w/size)
                   (/ w/size w/source)
                   1)]
           [h/output (exact-floor (* h/source sc))])
      (check-end-page dc h/output h/size)
      (send dc set-origin 0 position-height)
      (let-values ([(w/s h/s) (send dc get-scale)])
        (send dc set-scale sc sc)
        (send dc draw-bitmap source 0 0)
        (send dc set-scale w/s h/s))
      (send dc set-origin 0 0)
      (increase-position-height h/output))))

;设置绘图纵向起点：
(define (set-position-height y)
  (set! position-height y))