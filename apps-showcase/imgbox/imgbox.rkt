#lang racket

(require 2htdp/image
         racket/cmdline
         (only-in racket/draw read-bitmap))

(define women (read-bitmap "face.jpg"))


(define (image-to-box img/file width)
  (define (f x y) (/ (-  x y) 2))
  (let*-values ([(img) (if (string? img/file) (read-bitmap img/file) img/file)]
                [(w) (image-width img)] ; return img width and assign to w
                [(h) (image-height img)] ; return img height and assign to h
                [(∂w ∂h) (if (> w h) (values (f w h) 0) (values 0 (f h w)))]
                [(∂w1 ∂h1) (values (+ ∂w (f w width)) (+ ∂h (f h width)))])
    (crop ∂w1 ∂h1 width width img)))

(define (normalize-name filename width)
  (string-replace filename "." (format "-~a." width)))

(image-to-box women 300)

;; run from command line, racket face.rkt face.jpg 400
(command-line
 #:args (filename width)
 (save-image (image-to-box filename (string->number width))
             (normalize-name filename width)))



