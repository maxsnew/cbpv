#lang turnstile

(require (rename-in racket/function (thunk thunk-)))
(define (force- f) (f))

(define-syntax-category kind)
;(define-syntax-category ctype)
;(define-syntax-category value)
;(define-syntax-category computation)

(define-base-kind vty)
(define-base-kind cty)

(begin-for-syntax
  (current-type? (λ (t) (or (vty? t) (cty? t)))))

(define-internal-type-constructor U)
(define-kinded-syntax (U B) ≫
  (⊢ B ≫ B- ⇐ cty)
  -----------------
  (⊢ (U- B-) ⇒ vty))

(define-typed-syntax (thunk e) ≫
  (⊢ e ≫ e- ⇒ B (⇒ ~cty))
  ----------------
  (⊢ e ⇒ (U B)))

(define-typed-syntax (force e) ≫
  (⊢ e ≫ e- ⇒ (~U B))
  -------------------
  (⊢ e- ⇒ B))

(define-typed-syntax (let (x e) e1) ≫
  (⊢ e ≫ e- ⇒ A (⇒ ~vty))
  ((x ≫ x- : A) ⊢ e1 ≫ e1- ⇒ B (⇒ ~cty))
  ------------------------
  (⊢ (let- ([x- e-]) e1-) ⇒ B))

(define-base-type bool : vty)
(define-internal-type-constructor F)
(define-kinded-syntax (F A) ≫
  (⊢ A ≫ A- ⇐ vty)
  --------------
  (⊢ (F- A-) ⇒ cty))

(define-typed-syntax (return v) ≫
  (⊢ v ≫ v- ⇒ A (⇒ ~vty))
  ------------------
  (⊢ (thunk- v-) ⇒ (F A)))

(define-for-syntax (phi B t)
  (syntax-parse B
    [(~F A)
     #`(thunk- (force- (force- #,t)))]
    #;[(~> A B)
     #`(lambda (x) (#,(phi #'B t) x))]))

(define-typed-syntax (bind (x:id e) e^) ≫
  (⊢ e ≫ e- ⇒ (~F A))
  ((x ≫ x- : A) ⊢ e^ ≫ e^- ⇒ B- (⇒ ~cty))
  -----------------
  (⊢ #,(phi #'B- #'(thunk- (let- ([x- (force- e-)]) e^-))) ⇒ B-))

(define-typed-syntax true
  (_:id ≫
   -----------------
   (⊢ #t ⇒ bool)))

(define-typed-syntax false
  (_:id ≫
   -----------------
   (⊢ #f ⇒ bool)))

(define-typed-syntax (if e e1 e2) ≫
  (⊢ e ≫ e- ⇐ bool)
  (⊢ e1 ≫ e1- ⇒ B2 (⇒ ~cty))
  (⊢ e2 ≫ e2- ⇐ B2)
  ------------------
  (⊢ (if- e- e1- e2-) ⇒ B2))

(define-internal-type-constructor ->)
(define-kinded-syntax (-> A B) ≫
  (⊢ A ≫ A- ⇐ vty)
  (⊢ B ≫ B- ⇐ cty)
  ----------------
  (⊢ (->- A- B-) ⇒ cty))

(define-typed-syntax (λ (x : A) e) ≫
  (⊢ A ≫ A- ⇐ vty)
  ((x ≫ x- : A-) ⊢ e ≫ e- ⇒ B (⇒ ~cty))
  --------------------------------------
  (⊢ (lambda (x-) e-) ⇒ (-> A- B)))

(define-typed-syntax (^ e1 e2) ≫
  (⊢ e1 ≫ e1- ⇒ (~-> A B))
  (⊢ e2 ≫ e2- ⇐ A)
  ----------------
  (⊢ (#%app- e1- e2-) ⇒ B))

(provide (rename-out (^ #%app)))

(define-base-type ⊥)
(define-typed-syntax (main e) ≫
  (⊢ e ≫ e- ⇒ (~F A))
  ----------------
  (⊢ (force- e-) ⇒ ⊥))

(module+ test
  (require
    turnstile/rackunit-typechecking
    rackunit)
  
  (check-type (return true) : (F bool))
  (check-type (return false) : (F bool))
  (typecheck-fail (return (return true)) #:with-msg "expected .* `vty")

  (check-type (thunk (return true)) : (U (F bool)))
  (check-type (force (thunk (return true))) : (F bool))
  (check-type (main (force (thunk (return true)))) : ⊥)

  (check-type (let (x true) (return x)) : (F bool))
  (check-equal? (main (force (thunk (let (x true) (return x))))) #t)

  (check-type (λ (x : bool) (return x)) : (-> bool (F bool)))
  (typecheck-fail (λ (x : bool) x) #:with-msg "expected .* `cty")
  (typecheck-fail (λ (x : (F bool)) x) #:with-msg "expected vty")
  
  (check-type (^ (λ (x : bool) (return x)) true) : (F bool))
  (check-equal? (main (^ (λ (x : bool) (return x)) true)) #t)
  (typecheck-fail (^ true true) #:with-msg "Expected ->"))

(if true (return false) (return true))
#;(if true false true)

(bind (x (return true)) (return x))
#;(bind (x (return true)) x)
#;(bind (x true) (return true))

(main (return true))
(main (bind (x (return true)) (return x)))
#;(main true)