#lang racket/gui

(require racket/class)

(define folder-path
  (current-directory))

(define filenames
  (for/list ([filename (directory-list folder-path)]
             #:when (equal? (path-get-extension filename) #".txt"))
    (path->string filename)))

(define window
  (new frame%
       [label "Text Viewer"]
       [width 800]
       [height 600]))

(define panel
  (new vertical-panel%
       [parent window]))

(define search-box
  (new text-field%
       [parent panel]
       [label #f]
       [callback (lambda (sb e)
                   (define text (send sb get-value))

                   (send list-box clear)
                   (for ([filename filenames]
                         #:when (string-contains? filename text))
                     (send list-box append filename)))]))

(define list-box
  (new list-box%
       [parent panel]
       [label #f]
       [choices filenames]
       [callback (lambda (tb e)
                   (define selection (send tb get-string-selection))
                   (define filename (and selection (build-path folder-path selection)))
                   (when filename
                     (define lines
                       (with-handlers ([exn:fail? (lambda (_) '(""))])
                         (file->lines filename)))
                     (define dc (send canvas get-dc))
                     (send dc clear)
                     (for ([line (in-list lines)]
                           [i (in-naturals)])
                       (send dc draw-text line 10 (* i 16)))))]))

(define canvas
  (new canvas%
       [parent panel]))

(send window show #t)