#lang racket
(require math/bigfloat)
(require math/number-theory)

(define sum
  (lambda (next first last term)
    (cond [(< last first) 0]
          [else (+ (term first) 
                   (sum next (next first) last term))])))
(define bfsum
  (lambda (next first last term)
    (cond [(< last first) (bf 0)]
          [else (bf+ (term first)
                     (bfsum next (next first) last term))])))

  

(define inc
  (lambda (x) (+ x 1)))

(define seq
  (lambda (f val)
    (lambda (x)
      (f x val))))


(define sum-series
  (lambda (deg term-f)
    (cond
      [(= deg 0) (lambda (x) (term-f 0 x) ) ]
      [else (add-procs (lambda (z) (term-f deg z))
                       (sum-series (- deg 1) term-f))])))




;;taylor series for exp[x]
;;1 + x + x^2/2 + x^3/3!...
;;let x = 1
(define term-exp
    (lambda (n val)
      (/ (expt val n) (factorial n))))

;cosx
;1 - x^2/2 + x^4/4! -x^6/6!...
(define term-cos
  (lambda (n val)
    (cond 
      [(= (modulo n 2) 0) (/ (expt val (* 2 n)) (factorial (* 2 n)))]
      [else (* -1 (/ (expt val (* 2 n)) (factorial (* 2 n)) ))])))


;;fibonacci with tail recursion
(define fib
  (lambda (n)
    (letrec ([iterate
             (lambda (n acc1 acc2)
               (cond
                 [(= 0 n) acc1]
                 [else (iterate (- n 1) acc2 (+ acc1 acc2))]))])
      (iterate n 0 1))))

;;fibonacci w/out tail recursion
(define bad-fib
  (lambda (n)
    (cond
      [(= n 0) 0]
      [(= n 1) 1]
      [else (+ (bad-fib (- n 1)) (bad-fib (- n 2)))])))

;;factorial w/ tail recursion
(define fac
  (lambda (n)
    (letrec ([fac-iter
              (lambda (n fac-acc)
                (cond
                  [(= 0 n) fac-acc]
                  [else (fac-iter (- n 1) (* fac-acc n))]))])
      (fac-iter n 1))))

;;factorial w/out tail recursion
(define bad-fac
  (lambda (n)
    (cond
      [(= 0 n) 1]
      [else (* n (bad-fac (- n 1)))])))

;;procedure add-proc
;; f(x) + g(x) = z(x)
(define f (lambda (x) (* 2 x)))
(define g (lambda (x) (* 3 x)))
(define z (lambda (x) ( + (g x) (f x))))
(define add-procs
  (lambda (proc1 proc2)
    (lambda (x) (+ (proc1 x) (proc2 x)))))


;composition of procedures: comp(x) = f(g(x))
(define comp
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

;scale function f by constant variable
(define const-var
  (lambda (f c)
    (lambda (x)
      (* c (f x)))))


;;limited precision
;;approximation based on ramanujan's series
#;(define pi-approx
  (/ 1
     (sum inc 0 100
      (lambda (k)
        (* 12
           (/ (* (expt -1 k) 
                 (factorial (* 6 k)) 
                 (+ 13591409 (* 545140134 k)))
              (* (factorial (* 3 k)) 
                 (expt (factorial k) 3) 
                 (expt 640320 (+ (* 3 k) (/ 3 2))))))))))

;;arbitrary precision
;;approximation of pi based on ramanujan's series
;(bf-precision 80000)
#;(define pi-digits
  (bf/ (bf "1.0")
     (bfsum inc 0 100
      (lambda (k)
        (bf* (bf 12)
           (bf/ (bf (* (expt -1 k) 
                       (factorial (*  6 k)) 
                       (+ 13591409 (* 545140134 k))))
                (bf* (bf (factorial (* 3 k))) 
                     (bf (expt (factorial k) 3)) 
                     (bfexpt (bf 640320) (bf (+ (* 3 k) (/ 3 2)))))))))))
