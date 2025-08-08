#lang racket


;; ----- code -----
(struct Scope (table parent))
(struct Closure (fun env))

(define op?
  (lambda (x)
    (memq x '(+ - * / = < >))))

(define new-env
  (lambda (env)
    (Scope (make-hash) env)))

(define assign
  (lambda (x v env)
    (hash-set! (Scope-table env) x v)))

(define ext-env
  (lambda (x v env)
    (let ([env+ (new-env env)])
      (assign x v env+)
      env+)))

(define assign*
  (lambda (x* v* env)
    (for ([x x*]
          [v v*])
      (assign x v env))))

(define ext-env*
  (lambda (x* v* env)
    (let ([env+ (new-env env)])
      (assign* x* v* env+)
      env+)))

(define lookup
  (lambda (x env)
    (cond
     [(not env) #f]
     [else
      (let ([v? (hash-ref (Scope-table env) x #f)])
        (cond
         [(not v?)
          (lookup x (Scope-parent env))]
         [else v?]))])))

(define env0
  (ext-env*
   '(+ - * / = < >)
   (list + - * / = < >)
   #f))

(define interp
  (lambda (exp env)
    (match exp
      [(? symbol? x)
       (let ([v (lookup x env)])
         (cond
          [(not v)
           (error "undefined variable" x)]
          [else v]))]
      [(? number? x) x]
      [(? boolean? x) x]
      [(? string? x) x]
      [`(lambda (,x* ...) ,e)
       (Closure exp env)]
      [`(let ([,x ,e1]) ,e2 ...)
       (let ([v1 (interp e1 env)])
         (interp `(begin ,@e2) (ext-env x v1 env)))]
      [`(define ,x ,e)
       (let ([v1 (interp e env)])
         (assign x v1 env))]
      [`(begin ,e1 ... ,en)
       (for ([e e1])
         (interp e env))
       (interp en env)]
      [`(if ,t ,e1 ,e2)
       (let ([tval (interp t env)])
         (if tval
             (interp e1 env)
             (interp e2 env)))]
      [`(cond ,clauses ...)
       (match (first clauses)
         [`(,test ,result)
          (let ([test-ok (or (eq? test 'else) (interp test env))])
            (if test-ok
                (interp result env)
                (interp `(cond ,@(rest clauses)) env)))])]
      [`(,f ,x* ...)
       (let ([fv (interp f env)]
             [xv* (map (lambda (x) (interp x env)) x*)])
         (match fv
           [(? procedure? p)
            (apply p xv*)]
           [(Closure `(lambda (,x* ...) ,e) env-save)
            (interp e (ext-env* x* xv* env-save))]))])))

(define r3
  (lambda (exp)
    (interp exp env0)))


;; ----- examples -----
(r3
 '(begin
    (define x 1)
    (define y 2)
    (+ x y)))
;; => 3


(r3
 '(begin
    (let ([x 1])
      (define f (lambda (y) (+ x y)))
      (let ([x 2])
        (f 0)))))
;; => 1


(r3
 '(begin
    (define x 1)
    (define f (lambda (y) (+ x y)))
    (let ([x 2])
      (f 0))))
;; => 1


(r3
 '(begin
    (define x 1)
    (define f (lambda (y) (+ x y)))
    (define x 2)
    (f 0)))
;; => 2


(r3
 '(begin
    (define fact
      (lambda (n)
        (cond
         [(= n 0) 1]
         [else
          (* n (fact (- n 1)))])))
    (fact 5)))
;; => 120


(r3
 '(begin
    (define fib
      (lambda (n)
        (cond
         [(< n 2) n]
         [else
          (+ (fib (- n 1)) (fib (- n 2)))])))
    (fib 9)))
;; => 34


(r3
 '(begin
    (define even
      (lambda (n)
        (cond
         [(= n 0) #t]
         [(= n 1) #f]
         [else  (odd (- n 1))])))
    (define odd
      (lambda (n)
        (cond
         [(= n 0) #f]
         [(= n 1) #t]
         [else  (even (- n 1))])))
    (even 42)))
;; => #t

(r3
 '(begin
    (define f
      (lambda (x y z)
        (+ x (* y z))))
    (f 1 2 3)))
;; => 7

(r3
 '(begin
    (define f
      (lambda (x y)
        (g y x)))
    (define g
      (lambda (x y)
        (- x y)))
    (f 1 2)))
;; => 1
