#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;; Begin Chapter 2
(define lat?
  (lambda (x)
    (cond [(null? x) #t]
          [(not (pair? (car x))) (lat? (cdr x))]
          [else #f] )))
;; End Chapter 2

;; Begin Chapter 3
(define member?
  (lambda (a lat)
    (cond [(null? lat) #f]
          [(eq? a (car lat)) #t]
          [else (member? a (cdr lat))] )))

(define rember
  (lambda (a lat)
    (cond 
      [(null? lat) '()]
      [(eq? a (car lat)) (cdr lat)]
      [else (cons (car lat) (rember a (cdr lat)))])))

(define firsts
  (lambda (llat)
    (cond  
      [(null? llat) null]
      [else (cons (caar llat) (firsts (cdr llat)))])))

;;redefined in chapter 8
#;(define insertR
  (lambda (new old lat)
    (cond 
      [(null? lat) null]
      [(eq? (car lat) old)  (cons (car lat) 
                                  (cons new (cdr lat)))]
      [else (cons (car lat) (insertR new old (cdr lat)))])))

;;redefined in chapter 8
#;(define insertL
  (lambda (new old lat)
    (cond
      [(null? lat) null]
      [(eq? (car lat) old) (cons new lat)]
      [else (cons (car lat) (insertL new old (cdr lat)))])))

;;redefined in chapter 8
#;(define subst
  (lambda (new old lat)
    (cond
      [(null? lat) null]
      [(eq? old (car lat)) (cons new (cdr lat))]
      [else (cons (car lat) (subst new old (cdr lat)))])))

(define subst2
  (lambda (new old1 old2 lat)
    (cond
      [(null? lat) null]
      [(or (eq? old1 (car lat)) 
           (eq? old2 (car lat))) (cons new (cdr lat))]
      [else (cons (car lat) (subst2 new old1 old2 (cdr lat)))])))

(define multirember
  (lambda (a lat)
    (cond
      [(null? lat) null]
      [(equal? a (car lat)) (multirember a (cdr lat))]
      [else (cons (car lat) (multirember a (cdr lat)))])))

;; procedures missing
;; End Chapter 3 

;begin chapter 4
(define o+
  (lambda (n m)
    (cond
      [(zero? m) n]
      [else (add1 (o+ n (sub1 m)))])))

(define j-
  (lambda (n m)
    (cond
      [(zero? m) n]
      [else (sub1 (j- n (sub1 m)))])))

(define o*
  (lambda (n m)
    (cond
      [(zero? m) 0]
      [else (o+ n  (o* n (sub1 m)))])))

(define tup+
  (lambda (tup1 tup2)
    (cond
      [(null? tup1) tup2]
      [(null? tup2) tup1]
      [else (cons (+ (car tup1) (car tup2))
                  (tup+ (cdr tup1) (cdr tup2)))])))
(define j>
  (lambda (n m)
    (cond
      [(zero? n) #f]
      [(zero? m) #t]
      [else (j> (sub1 n) (sub1 m))])))

(define j<
  (lambda (n m)
    (cond
      [(zero? m) #f]
      [(zero? n) #t]
      [else (j< (sub1 n) (sub1 m))])))

(define j=
  (lambda (n m)
    (cond
      [(zero? m) (zero? n)]
      [(zero? n) #f]
      [else (j= (sub1 n) (sub1 m))])))

(define pow
  (lambda (n m)
    (cond
      [(zero? m) 1]
      [(o* n (pow n (sub1 m)))]
      )))

(define oquotient
  (lambda (n m)
    (cond
      [(< n m) 0]
      [else (add1 (oquotient (- n m) m))])))

(define length
  (lambda (lat)
    (cond
      [(null? lat) 0]
      [else (add1 (length (cdr lat)))])))

(define pick
  (lambda (n lat)
    (cond
      [(zero? (sub1 n)) (car lat) ]
      [else (pick (sub1 n) (cdr lat))])))

(define rempick
  (lambda (n lat)
    (cond
      [(zero? (sub1 n)) (cdr lat)]
      [else (cons (car lat)
                  (rempick (sub1 n) 
                           (cdr lat)))])))

(define no-nums
  (lambda (lat)
    (cond
      [(null? lat) null]
      [(number? (car lat)) (no-nums (cdr lat))]
      [else (cons (car lat) (no-nums (cdr lat)))])))

(define all-nums
  (lambda (lat)
    (cond
      [(null? lat) null]
      [(number? (car lat))  (cons (car lat) (all-nums (cdr lat)))]
      [else (all-nums (cdr lat))])))

(define eqan?
  (lambda (a1 a2)
    (cond
      [(and (number? a1) (number? a2)) (= a1 a2)]
      [(or (number? a1) (number? a2)) #f]
      [else (eq? a1 a2)])))

(define occur
  (lambda (a lat)
    (cond
      [(null? lat) 0]
      [(eqan? a (car lat)) (add1 (occur a (cdr lat)))]
      [else (occur a (cdr lat))])))
;;missing procedures from
;; End chapter 4

;;Begin Chapter 5
(define rember*
  (lambda (a l)
    (cond
      [(null? l) null]
      [(atom? (car l)) 
       (cond
         [(eqan? a (car l)) 
          (rember* a (cdr l))]
         [else (cons (car l) 
                     (rember* a (cdr l)))])]
      [else (cons (rember* a (car l)) 
                  (rember* a (cdr l)))])))

(define insertR*
  (lambda (new old l)
    (cond
      [(null? l) null]
      [(atom? (car l)) 
       (cond
         [(eqan? old (car l))
          (cons old
                (cons new
                      (insertR* new old 
                                (cdr l))))]
         [else (cons (car l)
                     (insertR* new old 
                               (cdr l)))])]
      [else (cons (insertR* new old 
                            (car l))
                  (insertR* new old (cdr l)))])))

                       
                       
(define occur*
  (lambda (a l)
    (cond
      [(null? l) 0]
      [(atom? (car l))
       (cond 
         [(eq? (car l) a) (add1 (occur* a (cdr l)))]
         [else (occur* a (cdr l))])]
      [else (+ (occur* a (car l)) (occur* a (cdr l)))])))

(define subst*
  (lambda (old new l)
    (cond
      [(null? l) null]
      [(atom? (car l))
       (cond
         [(eq? (car l) old) 
          (cons new 
                (subst* old new (cdr l)))]
         [else (cons (car l) 
                     (subst* old new (cdr l)))])]
      [else 
       (cons (subst* old new (car l)) 
             (subst* old new (cdr l)))])))

(define insertL*
  (lambda (new old l)
    (cond
      [(null? l) null]
      [(atom? (car l))
       (cond
         [(eq? old (car l)) (cons new 
                                  (cons (car l)
                                        (insertL* new old (cdr l))))]
         [else (cons (car l) (insertL* new old (cdr l)))])]
      [else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))])))

(define member*
  (lambda (a l)
    (cond
      [(null? l) #f]
      [(atom? (car l))
       (or (eq? a (car l))
           (member* a (cdr l)))]
      [else (or (member* a (car l))
                (member* a (cdr l)))])))

(define leftmost
  (lambda (l)
    (cond
      [(atom? (car l)) (car l)]
      [else (leftmost (car l))])))

(define eqlist?
  (lambda (l1 l2)
    (cond
      [(and (null? l1) (null? l2)) #t]
      [(or (null? l1) (null? l2)) #f]
      [else (and (equal? (car l1) (car l2))
                 (eqlist? (cdr l1) (cdr l2)))])))
(define equal?
  (lambda (s1 s2)
    (cond
      [(and (atom? s1) (atom? s2)) (eqan? s1 s2)]
      [(or (atom? s1) (atom? s2)) #f]
      [else (eqlist? s1 s2)])))
;; End Chapter 5

;; Begin Chapter 6
(define numbered?
  (lambda (aexp)
    (cond
      [(atom? aexp) (number? aexp)]
      [else
       (and (numbered? (car aexp))
            (numbered? (caddr aexp)))])))

;;redefined in chapter 8
(define 1st-sub-exp
  (lambda (aexp)
    (cadr aexp)))
(define 2nd-sub-exp
  (lambda (aexp)
    (caddr aexp)))
(define operator
  (lambda (aexp)
    (car aexp)))

;;redefined in chapter 8
#;(define value
  (lambda (nexp)
    (cond
      [(atom? nexp) nexp]
      [(eq? (car nexp) '*)
       (* (value (cadr nexp))
          (value (caddr nexp)))]
      [(eq? (car nexp) '+)
       (+ (value (cadr nexp))
          (value (caddr nexp)))]
      [else
       (exp (value (cadr nexp))
            (value (caddr nexp)))])))

(define sero?
  (lambda (n)
    (null? n)))
(define edd1
  (lambda (n)
    (cons null n)))
(define zub1
  (lambda (n)
    (cdr n)))
(define pluz
  (lambda (n m)
    (cond
      [(null? n) m]
      [else (edd1 (zub1 n) m)])))
;; End Chapter 6


;; Begin CHAPTER 7
(define set?
  (lambda (lat)
    (cond
      [(null? lat) #t]
      [(member? (car lat) (cdr lat)) #f]
      [else (set? (cdr lat))])))

(define makeset
  (lambda (lat)
    (cond
      [(null? lat) null]
      [(member? (car lat) (cdr lat)) (makeset (cdr lat))]
      [else (cons (car lat) (makeset (cdr lat)))])))

(define makeset1
  (lambda (lat)
    (cond
      [(null? lat) null]
      [else
       (cons (car lat) 
             (makeset1 (multirember (car lat)
                                    (cdr lat))))])))

#;(define subset?
  (lambda (set1 set2)
    (cond
      [(null? set1) #t]
      [(member? (car set1) set2) (subset? (cdr set1) set2)]
      [else #f])))

(define subset?
  (lambda (set1 set2)
    (cond
      [(null? set1) #t]
      [else (and (member? (car set1) set2)
                 (subset? (cdr set1) set2))])))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

#;(define intersect?
  (lambda (set1 set2)
    (cond
      [(null? set1) #f]
      [(member? (car set1) set2) #t]
      [else (intersect? (cdr set1) set2)])))

(define intersect?
  (lambda (set1 set2)
    (cond
      [(null? set1) #f]
      [else (or (member? (car set1) set2)
                (intersect? (cdr set1) set2))])))

(define intersect
  (lambda (set1 set2)
    (cond
      [(null? set1) null]
      [(member? (car set1) set2)
       (cons (car set1) (intersect (cdr set1) set2))]
      [else (intersect (cdr set1) set2)])))

(define union
  (lambda (set1 set2)
    (cond
      [(null? set1) set2]
      [(member? (car set1) set2) 
       (union (cdr set1) set2)]
      [else (cons (car set1) 
                  (union (cdr set1) set2))])))

(define intersectall
  (lambda (s)
    (cond
      [(null? (cdr s)) (car s)]      
      [else (intersect (car s) (intersectall (cdr s)))])))

(define a-pair?
  (lambda (s)
    (cond
      [(atom? s) #f]
      [(null? s) #f]
      [(null? (cdr s)) #f]
      [else (null? (cddr s))])))

(define first (lambda (p) (car p)))
(define second (lambda (p) (cadr p)))
(define third (lambda (l) (caddr l)))

(define build (lambda (s1 s2) (cons s1 (cons s2 null))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revrel
  (lambda (rel)
    (cond
      [(null? rel) null]
      [else (cons (build (cadar rel) (caar rel)) 
                  (revrel (cdr rel)))])))
(define seconds
  (lambda (l)
    (cond
      [(null? l) null]
      [else (cons (cadar l) (seconds (cdr l)))])))

(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))

;; Begin Chapter 8 (lambda the ultimate)

#;(define rember-f
  (lambda (test? a l)
    (cond
      [(null? l) null]
      [(test? a (car l)) (cdr l)]
      [else (cons (car l)
                  (rember-f test? a (cdr l)))])))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define k 'salad)
(define eq?-salad (eq?-c k))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        [(null? l) null]
        [(test? a (car l)) (cdr l)]
        [else (cons (car l) ((rember-f test?) a (cdr l)))]))))

(define insertL-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
        [(null? lat) null]
        [(test? old (car lat)) (cons new lat)]
        [else (cons (car lat) 
                    ((insertL-f test?) new old (cdr lat)))]))))

(define insertR-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
        [(null? lat) null]
        [(test? old (car lat)) 
         (cons (car lat) (cons new (cdr lat)))]
        [else (cons (car lat) 
                    ((insertR-f test?) new old (cdr lat)))]))))

;;my version takes 'left or 'right rather
;;a lambda expression which inserts right or left
#;(define insert-g
  (lambda (test?)
    (lambda (side new old l)
      (cond
        [(null? l) null]
        [(test? old (car l))
         (cond
           [(eq? side 'left) (cons new l)]
           [else (cons (car l)
                       (cons new (cdr l)))])]
         [else (cons (car l) 
                     ((insert-g test?) side new old (cdr l)))]))))

;;book version passes lambda expression 'seq
;;which inserts to the left or right
(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        [(null? l) null]
        [(eq? old (car l)) (seq new old (cdr l))]
        [else (cons (car l)
                    ((insert-g seq) new old (cdr l)))]))))

;;by passing different lambda expressions to insert-g
;;we can easily redefine the following three functions

(define insertL
  (insert-g 
   (lambda (new old l) 
     (cons new (cons old l)))))

(define insertR
  (insert-g
   (lambda (new old l)
     (cons old (cons new l)))))

(define subst
  (insert-g
   (lambda (new old l)
     (cons new l))))

(define atom-to-function
 (lambda (x)
   (cond
     [(eq? x '+) + ]
     [(eq? x '*) * ]
     [else exp])))

(define value
  (lambda (nexp)
    (cond 
      [(atom? nexp) nexp]
      [else
       ((atom-to-function (cadr nexp))
        (value (car nexp))
        (value (caddr nexp)))])))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        [(null? lat) null]
        [(test? (car lat) a) 
         ((multirember-f test?) a 
                                (cdr lat))]
        [else (cons (car lat)
                    ((multirember-f test?) a 
                                           (cdr lat)))]))))

(define multirember-eq?
  (multirember-f eq?))





;; End Chapter 8

;; Begin Chapter 9
;; End Chapter 9
;; Begin Chapter 10
;; End Chapter 10