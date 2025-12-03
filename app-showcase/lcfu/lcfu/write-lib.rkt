#lang racket

;;; ============================================================================
;;; LCFU Write Protocol Library (LCFU 写协议库)
;;; 
;;; 提供写入配置的核心功能,带进度回调
;;; ============================================================================

(provide (all-defined-out))

(require racket/udp racket/bytes)
(require "protocol.rkt")

;;; ----------------------------------------------------------------------------
;;; 协议常量 - Big Endian
;;; ----------------------------------------------------------------------------

(define LCFU-HEADER #xAAAA5555)

(define CMD-HANDSHAKE-REQ    #x0001)
(define CMD-HANDSHAKE-RESP   #x8001)
(define CMD-UPGRADE-REQ      #x0002)
(define CMD-UPGRADE-RESP     #x8002)
(define CMD-ERASE-REQ        #x0003)
(define CMD-ERASE-RESP       #x8003)
(define CMD-DATA-REQ         #x0004)
(define CMD-DATA-RESP        #x8004)
(define CMD-COMPLETE-REQ     #x0005)
(define CMD-COMPLETE-RESP    #x8005)
(define CMD-INTEGRITY-REQ    #x0006)
(define CMD-INTEGRITY-RESP   #x8006)

(define STATUS-SUCCESS #x00)
(define STATUS-FAILURE #x01)

;;; ----------------------------------------------------------------------------
;;; 协议包构建与解析
;;; ----------------------------------------------------------------------------

(define (build-write-packet cmd-id [data #f])
  (let* ([data-len (if data (bytes-length data) 0)]
         [packet (make-bytes (+ 8 data-len))])
    (bytes-copy! packet 0 (u32->bytes-be LCFU-HEADER))
    (bytes-copy! packet 4 (u16->bytes-be cmd-id))
    (bytes-copy! packet 6 (u16->bytes-be data-len))
    (when data
      (bytes-copy! packet 8 data))
    packet))

(define (parse-write-response resp)
  (unless (and resp (>= (bytes-length resp) 8))
    (raise (exn:lcfu:protocol "响应包太短" (current-continuation-marks))))
  
  (let ([header (bytes->u32-be resp 0)])
    (unless (= header LCFU-HEADER)
      (raise (exn:lcfu:protocol 
             (format "响应头错误: 期望 0x~a, 实际 0x~a" 
                     (number->string LCFU-HEADER 16)
                     (number->string header 16))
             (current-continuation-marks))))
    
    (let ([cmd (bytes->u16-be resp 4)]
          [len (bytes->u16-be resp 6)])
      (values cmd len (if (> len 0) (subbytes resp 8 (+ 8 len)) #f)))))

;;; ----------------------------------------------------------------------------
;;; 写协议主函数
;;; ----------------------------------------------------------------------------

(define (lcfu-write-config target-ip 
                          write-port 
                          file-data
                          #:max-block-size [max-block-size 1024]
                          #:block-delay [block-delay 0.01]
                          #:timeout [timeout 5.0]
                          #:max-retries [max-retries 3]
                          #:log-callback [log-fn void]
                          #:progress-callback [progress-fn void])
  "写入配置文件到雷达设备
   返回: #t 成功, 抛出异常表示失败"
  
  (define file-size (bytes-length file-data))
  (define sock (udp-open-socket))
  (udp-bind! sock #f 0)
  
  (with-handlers ([exn? (lambda (e) 
                         (udp-close sock)
                         (raise e))])
    
    ;; 步骤 1: 握手
    (log-fn 'step "步骤 1/6: 握手...")
    (send-packet sock target-ip write-port
                (build-write-packet CMD-HANDSHAKE-REQ) log-fn)
    (let ([resp (receive-response sock timeout log-fn)])
      (let-values ([(cmd len data) (parse-write-response resp)])
        (unless (and (= cmd CMD-HANDSHAKE-RESP) data 
                    (= (bytes-ref data 0) STATUS-SUCCESS))
          (raise (exn:lcfu:protocol "握手失败" (current-continuation-marks))))
        (log-fn 'success "✓ 握手成功")))
    (progress-fn 1 6)
    
    ;; 步骤 2: 升级请求
    (log-fn 'step "步骤 2/6: 升级请求...")
    (send-packet sock target-ip write-port
                (build-write-packet CMD-UPGRADE-REQ (u32->bytes-be file-size))
                log-fn)
    (define max-recv
      (let ([resp (receive-response sock timeout log-fn)])
        (let-values ([(cmd len data) (parse-write-response resp)])
          (unless (and (= cmd CMD-UPGRADE-RESP) data (= len 4))
            (raise (exn:lcfu:protocol "升级响应错误" (current-continuation-marks))))
          (bytes->u32-be data 0))))
    (log-fn 'success (format "✓ 升级接受,最大块: ~a bytes" max-recv))
    (progress-fn 2 6)
    
    ;; 步骤 3: Flash 擦除
    (log-fn 'step "步骤 3/6: Flash 擦除 (请等待)...")
    (send-packet sock target-ip write-port
                (build-write-packet CMD-ERASE-REQ) log-fn)
    (let ([resp (receive-response sock 10.0 log-fn)])
      (let-values ([(cmd len data) (parse-write-response resp)])
        (unless (and (= cmd CMD-ERASE-RESP) data 
                    (= (bytes-ref data 0) STATUS-SUCCESS))
          (raise (exn:lcfu:protocol "擦除失败" (current-continuation-marks))))
        (log-fn 'success "✓ Flash 擦除成功")))
    (progress-fn 3 6)
    
    ;; 步骤 4: 数据传输
    (log-fn 'step "步骤 4/6: 数据传输...")
    (define actual-block-size (min max-block-size max-recv))
    (define total-blocks (quotient (+ file-size actual-block-size -1) actual-block-size))
    
    (let data-loop ([block-id 1] [offset 0] [retry-count 0])
      (when (< offset file-size)
        (let* ([remaining (- file-size offset)]
               [block-size (min actual-block-size remaining)]
               [block-data (subbytes file-data offset (+ offset block-size))]
               [payload (bytes-append (u16->bytes-be block-id) block-data)])
          
          (with-handlers ([exn:lcfu:timeout? 
                          (lambda (e)
                            (if (< retry-count max-retries)
                                (begin
                                  (log-fn 'warning (format "⚠ 块 ~a 超时,重试..." block-id))
                                  (sleep 0.5)
                                  (data-loop block-id offset (+ retry-count 1)))
                                (raise e)))]
                         [exn:lcfu:io? 
                          (lambda (e)
                            (if (< retry-count max-retries)
                                (begin
                                  (log-fn 'warning (format "⚠ 块 ~a IO错误,重试..." block-id))
                                  (sleep 0.5)
                                  (data-loop block-id offset (+ retry-count 1)))
                                (raise e)))])
            
            (log-fn 'info (format "发送块 ~a/~a (~a%)" 
                                 block-id total-blocks
                                 (exact->inexact (round (* 100 (/ offset file-size))))))
            
            (send-packet sock target-ip write-port
                        (build-write-packet CMD-DATA-REQ payload) log-fn)
            
            (let ([resp (receive-response sock timeout log-fn)])
              (let-values ([(cmd len data) (parse-write-response resp)])
                (unless (and (= cmd CMD-DATA-RESP) data (= len 2))
                  (raise (exn:lcfu:protocol "数据响应错误" (current-continuation-marks))))
                
                (let ([next-block-id (bytes->u16-be data 0)])
                  (if (= next-block-id (+ block-id 1))
                      (begin
                        (progress-fn (+ 3 (/ block-id total-blocks)) 6)
                        (sleep block-delay)
                        (data-loop (+ block-id 1) (+ offset block-size) 0))
                      (if (< retry-count max-retries)
                          (begin
                            (log-fn 'warning (format "⚠ 块 ~a 确认失败,重传..." block-id))
                            (sleep 0.5)
                            (data-loop block-id offset (+ retry-count 1)))
                          (raise (exn:lcfu:protocol 
                                 (format "块 ~a 重试次数超限" block-id)
                                 (current-continuation-marks)
                                 '())))))))))))
    
    (log-fn 'success "✓ 数据传输完成")
    (progress-fn 4 6)
    
    ;; 步骤 5: 传输完成
    (log-fn 'step "步骤 5/6: 传输完成...")
    (send-packet sock target-ip write-port
                (build-write-packet CMD-COMPLETE-REQ) log-fn)
    (let ([resp (receive-response sock timeout log-fn)])
      (let-values ([(cmd len data) (parse-write-response resp)])
        (unless (and (= cmd CMD-COMPLETE-RESP) data 
                    (= (bytes-ref data 0) STATUS-SUCCESS))
          (raise (exn:lcfu:protocol "完成确认失败" (current-continuation-marks))))
        (log-fn 'success "✓ 传输完成确认")))
    (progress-fn 5 6)
    
    ;; 步骤 6: 完整性校验
    (log-fn 'step "步骤 6/6: 数据完整性校验...")
    (define crc32 (calculate-crc32 file-data))
    (log-fn 'info (format "CRC32: 0x~a" (number->string crc32 16)))
    (send-packet sock target-ip write-port
                (build-write-packet CMD-INTEGRITY-REQ (u32->bytes-be crc32))
                log-fn)
    (let ([resp (receive-response sock timeout log-fn)])
      (let-values ([(cmd len data) (parse-write-response resp)])
        (unless (and (= cmd CMD-INTEGRITY-RESP) data 
                    (= (bytes-ref data 0) STATUS-SUCCESS))
          (raise (exn:lcfu:protocol "完整性校验失败" (current-continuation-marks))))
        (log-fn 'success "✓ 数据完整性校验通过")))
    (progress-fn 6 6)
    
    (udp-close sock)
    #t))