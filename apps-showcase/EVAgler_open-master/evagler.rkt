#lang racket
;evalger.rkt
;EVAgler：无人机飞鹰。
;软件启动入口文件。

(require "app-data.rkt"
         "frame.rkt")

;读取程序运行配置：
(read-app-config)

;初始化当前图片为封面：
(set-current-picture-by-cover)

;显示主视图：
(show-frame/main)
;为公用值设置数据：
(access-app-main-frame (get-frame/main))
(set-message-control (get-message-control))
;初始化视图：
(init-frame/main)