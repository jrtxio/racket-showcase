#lang racket
;document-output-data.rkt
;提供文件输出功能。

(provide get-source-file-path
         output-defect-pictures
         output-node-pictures)

(require racket/gui)

(require "task-struct-data.rkt"
         "app-data.rkt"
         "defect-draw-data.rkt")

;取得当前任务下指定文件夹：
(define (get-assign-path folder)
  (let ([task-path (task-task-path)])
    (if (void? task-path)
        (void)
        (build-path task-path folder))))

;取得节点文件夹路径：
(define (get-node-path)
  (get-assign-path "命名文件"))

;取得源文件夹路径:
(define (get-source-path)
  (get-assign-path
   (task-source-folder)))

;取得缺陷文件夹路径:
(define (get-defect-path)
  (get-assign-path "缺陷照片"))

;检查是否存在目标文件夹:
(define (check-assign-directory path)
  (let ([str/path (path->string path)])
    (when (directory-exists? str/path)
      ;询问是否覆盖已有的文件夹:
      (let ([result (message-box
                     "提示信息"
                     "节点文件夹已经存在,是否要继续输出操作?"
                     (access-app-main-frame)
                     (list 'yes-no))])
        (if (eq? result 'yes)
            (begin
              (clear-files str/path)
              (delete-directory str/path)
              (sleep 1);暂停线程1秒
              #t)
            #f)))))

;删除指定文件夹内所有文件：
(define (clear-files path)
  (for ([filepath (directory-list path)])
    (delete-file (build-path path filepath))))

;取得源图片文件完整路径：
(define (get-source-file-path picture)
  (path-add-extension
   (path->string
    (build-path (get-source-path)
                (picture-source-name picture)))
   (string-append
    "."
    (picture-ext-name picture))))

;取得节点文件完整路径:
(define (get-node-file-path picture)
  (path-add-extension
   (path->string
    (build-path (get-node-path)
                (string-append
                 (picture-node-name picture)
                 "_"
                 (picture-source-name picture))))
   (string-append
    "."
    (picture-ext-name picture))))

;取得缺陷文件完整路径:
(define (get-defect-file-path picture defect)
  (path-add-extension
   (path->string
    (build-path
     (get-defect-path)
     (get-defect-filename picture defect)))
   (string-append
    "."
    (picture-ext-name picture))))

;输出缺陷图片：
(define (output-defect-pictures)
  (let ([defect-path (get-defect-path)]
        [source-path (get-source-path)])
    ;检查是否存在目标文件夹并做处理:
    (when (check-assign-directory defect-path)
      (begin
        (make-directory defect-path)
        (set-output-flag #t) ;设置输出标志为真
        (status-message "开始输出缺陷图片......")
        (for ([picture (get-defect-pictures)])
          (for ([defect (picture-node-defects picture)])
            ;制作缺陷图片并输出:
            (let* ([path/bmp (get-source-file-path picture)]
                   [path/output (get-defect-file-path picture defect)]
                   [filename/output (get-defect-filename picture defect)]
                   [bitmap (make-object bitmap% path/bmp)]
                   [dc/bmp (send bitmap make-dc)])
              (status-message
               (string-append "正在输出缺陷图片:" filename/output))
              (draw-stamp/defect dc/bmp defect)
              (send bitmap save-file
                    path/output 'jpeg))))
        (set-output-flag #f) ;设置输出标志为假
        (status-message "缺陷文件输出完毕。")))))

;输出命名节点图片：
(define (output-node-pictures)
  (let ([node-path (get-node-path)]
        [source-path (get-source-path)])
    (when (check-assign-directory node-path)
      (begin
        (make-directory node-path)
        (status-message "开始输出节点图片......")
        (for ([picture (get-node-pictures)])
          (copy-file (get-source-file-path picture)
                     (get-node-file-path picture)))
        (status-message "节点文件输出完毕。")))))