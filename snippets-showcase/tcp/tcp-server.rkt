#lang racket

(define (start-tcp-server [port 8080])
  (define listener (tcp-listen port 5 #t))
  (printf "服务器启动在端口 ~a\n" port)
  
  (let loop ()
    (define-values (in out) (tcp-accept listener))
    (thread
     (lambda ()
       (printf "新客户端连接\n")
       (with-handlers ([exn:fail? (lambda (e)
                                   (printf "错误: ~a\n" (exn-message e)))])
         (let process-client ()
           (define line (read-line in))
           (unless (eof-object? line)
             (printf "收到: ~a\n" line)
             (fprintf out "服务器回复: ~a\n" line)
             (flush-output out)
             (process-client))))
       (printf "客户端断开连接\n")
       (close-input-port in)
       (close-output-port out)))
    (loop)))

;; 启动服务器
(start-tcp-server)