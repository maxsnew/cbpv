#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     syntax/transformer)
         syntax/parse/define
         (rename-in racket/base [if if-]))

(define (bool-) (error "bool-"))
(define-syntax-parser bool
;  [(_) (syntax-property #'(bool-) 'kind 'v)]
  [(_) (value-type this-syntax)])

(define-syntax-parser true
  [(_) (syntax-property #'#t 'type #'(bool))])

(define-syntax-parser false
  [(_) (syntax-property #'#f 'type #'(bool))])

(begin-for-syntax
  (define-syntax-class typed
    (pattern e:expr
             #:attr e- (local-expand #'e 'expression '())
             #:attr type (syntax-property (attribute e-) 'type)))
  (define-syntax-class v-type
    (pattern e:expr
             #:attr e- (local-expand #'e 'expression '())
             #:fail-unless (equal? 'v (syntax-property (attribute e-) 'kind)) "expected a value type"))
  (define-syntax-class c-type
    (pattern e:expr
             #:attr e- (local-expand #'e 'expression '())
             #:fail-unless (equal? 'c (syntax-property (attribute e-) 'kind)) "expected a computation type"))
  (define-syntax-class value
    (pattern e:typed 
             #:attr e- (attribute e.e-)
             #:attr type (attribute e.type)
             #:with _:v-type (attribute e.type)))
  (define-syntax-class computation
    (pattern e:typed 
             #:attr e- (attribute e.e-)
             #:attr type (attribute e.type)
             #:fail-unless (syntax-parse (attribute type) [_:c-type #t] [_ #f])
             "expected a computation type"))

  ;; TODO: make an optional failure continuation here
  (require racket/trace)
  (define (type=? t1 t2)
    ;; (define t1- (local-expand t1 'expression '()))
    ;; (define t2- (local-expand t2 'expression '()))
    ;; (displayln t1-)
    ;; (displayln t2-)
    (syntax-parse #`(#,t1 #,t2)
      #:literals (bool bool- #%plain-app)
      [((bool) (bool)) #t]
      #;[((#%plain-app bool-) (#%plain-app bool-)) #t]
      [else #f]))
  (define (computation-type syn)
    (syntax-property (quasisyntax/loc syn void) 'kind 'c))
  (define (value-type syn)
    (syntax-property (quasisyntax/loc syn void) 'kind 'v))
  )

(define-syntax-parser if
  [(_ v:value t:computation f:computation)
   #:fail-when (not (type=? (attribute v.type) #'(bool))) "if needs a bool, dawg"
   #:fail-when (not (type=? (attribute t.type) (attribute f.type))) "branches of an if gotta have the same type, dawg"
   (syntax-property #'(if- v.e- t.e- f.e-) 'type (attribute t.type))])

(define-syntax (define-computation-type syn)
  (syntax-parse syn
    [(_ name [e] ...)
     #`(define-syntax-parser name [e (computation-type this-syntax)] ...)]))
;; (define-syntax-parser F
;;   [(_ a:v-type) (computation-type this-syntax)])
(define-computation-type F
  [(_ a:v-type)])
(define-syntax-parser return
  [(_ v:value) (syntax-property #'v.e- 'type #'(F v.type))])

(define-syntax-parser bind
  #:literals (F let-values #%plain-lambda)
  [(_ (x:id e:computation) k)
   #:fail-unless (syntax-parse (attribute e.type)
                   #:literals (F)
                   [(F A) #t]
                   [_ #f])
   (format "~a must have type (F A)" #'e)
   #:with (F A) (attribute e.type)
   #:with x- (generate-temporary)
   #:with (#%plain-lambda (x--) (let-values _ k-))
   (local-expand #'(lambda (x-)
                     (let-syntax ([x (make-variable-like-transformer
                                      (syntax-property #'x- 'type #'A))])
                       k))
                 'expression '())
   (syntax-property #'(let ([x-- e.e-]) k-) 'type #'k.type)])

; (if (true) (return (true)) (false))
(bind (x (return (true))) (return x))
;(true)
#;
(begin-for-syntax
  (display (syntax-property (local-expand (syntax-property (local-expand #'(true) 'expression '()) 'type)
                          'expression '()) 'kind)))
