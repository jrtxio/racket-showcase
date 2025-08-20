#lang racket

(define (start-tcp-client [host "localhost"] [port 8080])
  (printf "连接到 ~a:~a\n" host port)
  (with-handlers ([exn:fail? (lambda (e)
                              (printf "连接错误: ~a\n" (exn-message e)))])
    (define-values (in out) (tcp-connect host port))
    
    ;; 创建读取线程
    (thread
     (lambda ()
       (with-handlers ([exn:fail? void])
         (let loop ()
           (define response (read-line in))
           (unless (eof-object? response)
             (printf "~a\n" response)
             (loop))))))
    
    ;; 主线程处理输入
    (let loop ()
      (printf "> ")
      (define input (read-line))
      (unless (equal? input "quit")
        (fprintf out "~a\n" input)
        (flush-output out)
        (loop)))
    
    (close-input-port in)
    (close-output-port out)))

;; 启动客户端
(start-tcp-client)