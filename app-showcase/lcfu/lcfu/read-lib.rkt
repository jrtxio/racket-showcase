#lang racket

;;; ============================================================================
;;; LCFU Read Protocol Library (LCFU 读协议库)
;;; 
;;; 提供读取配置的核心功能,带进度回调
;;; ============================================================================

(provide (all-defined-out))

(require racket/udp racket/bytes)
(require "protocol.rkt")

;;; ----------------------------------------------------------------------------
;;; 协议常量 - Little Endian
;;; ----------------------------------------------------------------------------

(define LCFU-FLAG-REQ  #xA5A5A5A5)
(define LCFU-FLAG-RESP #x5A5A5A5A)

(define CMD-DATA     #x00)
(define CMD-COMPLETE #x01)

;;; ----------------------------------------------------------------------------
;;; 协议包构建与解析
;;; ----------------------------------------------------------------------------

(define (build-read-request cmd addr-shift len)
  (define packet (make-bytes 15))
  (bytes-copy! packet 0 (u32->bytes-le LCFU-FLAG-REQ))
  (bytes-set! packet 4 cmd)
  (bytes-copy! packet 5 (u32->bytes-le addr-shift))
  (bytes-copy! packet 9 (u32->bytes-le len))
  (let ([sum (calculate-checksum packet 13)])
    (bytes-copy! packet 13 (u16->bytes-le sum)))
  packet)

(define (parse-read-response data)
  (when (< (bytes-length data) 36)
    (raise (exn:lcfu:protocol
           (format "响应包太短: ~a bytes" (bytes-length data))
           (current-continuation-marks))))
  
  (define flag (bytes->u32-le data 0))
  (unless (= flag LCFU-FLAG-RESP)
    (raise (exn:lcfu:protocol
           (format "响应标志错误: 0x~a" (number->string flag 16))
           (current-continuation-marks))))

  (define total-len (bytes->u32-le data 4))  ; 文件总大小在第4-7字节
  (define raw-lidar-type (bytes-ref data 8)) ; 雷达类型在第8字节
  ; 验证雷达类型是否在有效范围内 (1-4)，如果不是，使用默认值 1
  (define lidar-type
    (if (and (>= raw-lidar-type 1) (<= raw-lidar-type 4))
        raw-lidar-type
        0))  ; 对无效值使用0，不写日志因为log-fn在此函数中不可用
  (define data-end (- (bytes-length data) 3))
  (define payload (subbytes data 36 data-end))  ; payload从第36字节开始
  (define packet-index (bytes->u16-le data data-end))
  (define checksum (bytes-ref data (+ data-end 2)))

  (values total-len lidar-type payload packet-index checksum))

;;; ----------------------------------------------------------------------------
;;; 读协议主函数
;;; ----------------------------------------------------------------------------

(define (lcfu-read-config target-ip
                         read-port
                         #:max-read-size [max-read-size 1400]
                         #:timeout [timeout 5.0]
                         #:max-retries [max-retries 3]
                         #:log-callback [log-fn void]
                         #:progress-callback [progress-fn void]
                         #:verify-data [original-data #f])
  "从雷达设备读取配置文件
   返回: (values read-data lidar-type)
   如果提供 original-data,则会验证数据匹配"
  
  (define sock (udp-open-socket))
  (udp-bind! sock #f 0)
  
  (with-handlers ([exn? (lambda (e) 
                         (udp-close sock)
                         (raise e))])
    
    ;; 读取第一个包
    (log-fn 'step "读取初始数据包...")
    (define req-packet (build-read-request CMD-DATA 0 max-read-size))
    (send-packet sock target-ip read-port req-packet log-fn)

    (let ([resp (receive-response sock timeout log-fn)])
      (let-values ([(total-len detected-lidar-type payload packet-index checksum)
                    (parse-read-response resp)])

        ; 现在总是使用检测到的雷达类型
        (define actual-lidar-type detected-lidar-type)

        (log-fn 'info (format "文件总大小: ~a" (format-file-size total-len)))
        (log-fn 'info (format "雷达类型: ~a"
                             (case actual-lidar-type
                               [(1) "FW192SB"]
                               [(2) "IFW192S"]
                               [(3) "FW192S-A"]
                               [(4) "IFN56"]
                               [else (format "未知(~a)" actual-lidar-type)])))
        (log-fn 'info (format "首包大小: ~a bytes" (bytes-length payload)))

        ;; 如果提供了原始数据,检查大小
        (when (and original-data (not (= total-len (bytes-length original-data))))
          (log-fn 'warning "⚠ 警告: 读取大小与原始文件不匹配")
          (raise (exn:lcfu:protocol "文件大小不匹配" (current-continuation-marks))))

        ;; 准备读取缓冲区
        (define read-data (make-bytes total-len))
        (define first-len (bytes-length payload))
        (bytes-copy! read-data 0 payload)
        (progress-fn first-len total-len)
        
        ;; 读取剩余数据
        (let data-loop ([offset first-len] [packet-count 1] [retry-count 0])
          (cond
            [(>= offset total-len)
             (log-fn 'success (format "✓ 读取完成: ~a 个数据包" packet-count))]
            
            [(> retry-count max-retries)
             (raise (exn:lcfu:protocol
                    (format "偏移 ~a 重试次数超限" offset)
                    (current-continuation-marks)))]
            
            [else
             (let* ([remaining (- total-len offset)]
                    [request-len (min max-read-size remaining)]
                    [is-last (>= (+ offset request-len) total-len)]
                    [cmd (if is-last CMD-COMPLETE CMD-DATA)])
               
               (log-fn 'info (format "读取偏移 ~a/~a (~a%)" 
                                    offset total-len
                                    (exact->inexact (round (* 100 (/ offset total-len))))))
               
               (with-handlers ([exn:lcfu? 
                               (lambda (e)
                                 (if (< retry-count max-retries)
                                     (begin
                                       (log-fn 'warning (format "⚠ 读取失败,重试..."))
                                       (sleep 0.5)
                                       (data-loop offset packet-count (+ retry-count 1)))
                                     (raise e)))])
                 
                 (define req (build-read-request cmd offset request-len))
                 (send-packet sock target-ip read-port req log-fn)
                 
                 (let ([resp (receive-response sock timeout log-fn)])
                   (let-values ([(total-len2 lidar-type2 payload2 packet-index2 checksum2)
                                 (parse-read-response resp)])
                     (define payload-len (bytes-length payload2))
                     (when (= payload-len 0)
                       (raise (exn:lcfu:protocol "接收到空数据包" (current-continuation-marks))))
                     (bytes-copy! read-data offset payload2)
                     (progress-fn (+ offset payload-len) total-len)
                     (data-loop (+ offset payload-len) (+ packet-count 1) 0)))))]))
        
        ;; 验证数据 (如果提供了原始数据)
        (when original-data
          (log-fn 'step "验证数据匹配...")
          (if (bytes=? read-data original-data)
              (log-fn 'success "✓ 数据验证通过!")
              (begin
                (log-fn 'error "✗ 数据验证失败!")
                (raise (exn:lcfu:verify "数据不匹配" (current-continuation-marks))))))

        (udp-close sock)
        (values read-data detected-lidar-type)))))