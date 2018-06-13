#lang dyn-cbpv

;; 
(define f ; () -> F A /\ (A) -> F A
  (case-lambda
    [()
     (print "no more args")
     (ret '())]
    [(x)
     (print "got one!")
     (ret '(x))]))

;; count-args : (? ...) -> F Num
(define-rec count-loop
  (λ (acc)
   (case-lambda
     [() acc]
     [(x) ((! count-loop) (+ 1 acc))])))

(define count-args
  (case-lambda
    [()]))

;; A little "slow" because of the reverse, but could fix this with
;; mutability
(define (dot-rest-loop f acc)
  (case-lambda
    [() (f (reverse acc))]
    [(x) (dot-rest-loop f (cons x acc))]))


(define (dot-rest f) ;: (U ((List ?) -> F A)) -> (? ...) -> F A
  (dot-rest-loop f '()))

(define (dot-rest-efficient f)
  (define hd (mut-nil))
  (define tl (box hd))
  ; Invariant: tl is pointing to a nil
  (define (loop)
    (case-lambda
      [()
       (freeze (unbox tl))
       (f hd)]
      [(x)
       (define cur-tl (unbox tl))
       (cons! cur-tl x (mut-nil))
       (loop)]))
  (loop))

(define-syntax-rule (λ-rest z e)
  (dot-rest (λ z e)))
