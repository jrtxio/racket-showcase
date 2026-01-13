;; 一个简化的算术表达式求值器(教学示意)
(define (eval-expr expr env)
  (cond 
    ;; 数字直接返回
    ((number? expr) expr)
    ;; 符号从环境中查找
    ((symbol? expr) 
     (cdr (assoc expr env)))
    ;; 列表表示运算
    ((list? expr)
     (let ((op (eval-expr (first expr) env))
           (args (map (lambda (a) (eval-expr a env)) (rest expr))))
       (apply op args)))))

;; 环境中保存运算符绑定
(define simple-env (list (cons '+ +) (cons '* *) (cons '- -)))

(eval-expr 5 simple-env)           ; => 5
(eval-expr '(+ 2 3) simple-env)    ; => 5
(eval-expr '(* (+ 1 2) 4) simple-env) ; => 12