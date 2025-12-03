#lang racket

;;; ============================================================================
;;; LCFU Protocol Common Module (LCFU 协议公共模块)
;;; 
;;; 提供写入和读取协议的公共功能
;;; ============================================================================

(provide (all-defined-out))

(require racket/udp racket/bytes)

;;; ----------------------------------------------------------------------------
;;; 字节序转换 - Big Endian (用于写协议)
;;; ----------------------------------------------------------------------------

(define (u16->bytes-be val)
  (bytes (bitwise-and (arithmetic-shift val -8) #xFF)
         (bitwise-and val #xFF)))

(define (u32->bytes-be val)
  (bytes (bitwise-and (arithmetic-shift val -24) #xFF)
         (bitwise-and (arithmetic-shift val -16) #xFF)
         (bitwise-and (arithmetic-shift val -8) #xFF)
         (bitwise-and val #xFF)))

(define (bytes->u16-be buf offset)
  (+ (arithmetic-shift (bytes-ref buf offset) 8)
     (bytes-ref buf (+ offset 1))))

(define (bytes->u32-be buf offset)
  (+ (arithmetic-shift (bytes-ref buf offset) 24)
     (arithmetic-shift (bytes-ref buf (+ offset 1)) 16)
     (arithmetic-shift (bytes-ref buf (+ offset 2)) 8)
     (bytes-ref buf (+ offset 3))))

;;; ----------------------------------------------------------------------------
;;; 字节序转换 - Little Endian (用于读协议)
;;; ----------------------------------------------------------------------------

(define (u16->bytes-le val)
  (bytes (bitwise-and val #xFF)
         (bitwise-and (arithmetic-shift val -8) #xFF)))

(define (u32->bytes-le val)
  (bytes (bitwise-and val #xFF)
         (bitwise-and (arithmetic-shift val -8) #xFF)
         (bitwise-and (arithmetic-shift val -16) #xFF)
         (bitwise-and (arithmetic-shift val -24) #xFF)))

(define (bytes->u16-le buf offset)
  (+ (bytes-ref buf offset)
     (arithmetic-shift (bytes-ref buf (+ offset 1)) 8)))

(define (bytes->u32-le buf offset)
  (+ (bytes-ref buf offset)
     (arithmetic-shift (bytes-ref buf (+ offset 1)) 8)
     (arithmetic-shift (bytes-ref buf (+ offset 2)) 16)
     (arithmetic-shift (bytes-ref buf (+ offset 3)) 24)))

;;; ----------------------------------------------------------------------------
;;; CRC32 校验
;;; ----------------------------------------------------------------------------

(define *crc32-table*
  (for/vector ([i (in-range 256)])
    (let loop ([c i] [k 0])
      (if (< k 8)
          (loop (if (odd? c)
                    (bitwise-xor (arithmetic-shift c -1) #xEDB88320)
                    (arithmetic-shift c -1))
                (+ k 1))
          c))))

(define (calculate-crc32 data)
  (let ([crc #xFFFFFFFF])
    (for ([byte (in-bytes data)])
      (set! crc (bitwise-xor 
                 (arithmetic-shift crc -8)
                 (vector-ref *crc32-table*
                            (bitwise-and (bitwise-xor crc byte) #xFF)))))
    (bitwise-xor crc #xFFFFFFFF)))

;;; ----------------------------------------------------------------------------
;;; 校验和计算 (用于读协议)
;;; ----------------------------------------------------------------------------

(define (calculate-checksum data len)
  (let loop ([i 0] [sum 0])
    (if (>= i len)
        (bitwise-and sum #xFFFF)
        (loop (+ i 1) (+ sum (bytes-ref data i))))))

;;; ----------------------------------------------------------------------------
;;; 异常类型定义 (统一的异常类型)
;;; ----------------------------------------------------------------------------

(struct exn:lcfu exn:fail () #:transparent)
(struct exn:lcfu:timeout exn:lcfu () #:transparent)
(struct exn:lcfu:protocol exn:lcfu () #:transparent)
(struct exn:lcfu:io exn:lcfu () #:transparent)
(struct exn:lcfu:verify exn:lcfu () #:transparent)

;;; ----------------------------------------------------------------------------
;;; 网络通信函数 (公共)
;;; ----------------------------------------------------------------------------

(define (send-packet sock target-ip target-port packet log-fn)
  "发送 UDP 数据包,带异常处理"
  (with-handlers ([exn:fail:network? 
                   (lambda (e)
                     (raise (exn:lcfu:io
                            (format "发送失败: ~a" (exn-message e))
                            (current-continuation-marks))))])
    (udp-send-to sock target-ip target-port packet)
    (log-fn 'info (format "→ 发送数据包 (~a bytes)" (bytes-length packet)))
    (sleep 0.05)
    #t))

(define (receive-response sock timeout-sec log-fn)
  "接收 UDP 响应,带超时处理"
  (define buf (make-bytes 8192))
  (with-handlers ([exn:fail:network? 
                   (lambda (e)
                     (raise (exn:lcfu:io
                            (format "接收失败: ~a" (exn-message e))
                            (current-continuation-marks))))])
    (define result
      (sync/timeout timeout-sec
                    (handle-evt (udp-receive!-evt sock buf)
                                (lambda (evt-result) evt-result))))
    
    (if result
        (let ([len (car result)])
          (log-fn 'info (format "← 接收 ~a bytes" len))
          (subbytes buf 0 len))
        (raise (exn:lcfu:timeout "等待响应超时" (current-continuation-marks))))))

;;; ----------------------------------------------------------------------------
;;; 辅助函数
;;; ----------------------------------------------------------------------------

(define (bytes->hex-string bs [max-len 64])
  (define len (min max-len (bytes-length bs)))
  (string-join
   (for/list ([i (in-range len)])
     (format "~a" (number->string (bytes-ref bs i) 16)))
   " "))

(define (format-file-size bytes)
  (cond
    [(< bytes 1024) (format "~a B" bytes)]
    [(< bytes (* 1024 1024)) (format "~a KB" (round (/ bytes 1024)))]
    [else (format "~a MB" (round (/ bytes 1024 1024)))]))