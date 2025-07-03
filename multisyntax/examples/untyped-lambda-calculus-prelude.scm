;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Untyped lambda calculus using normal-order evaluation.
;;; 
;;; The following defines a Scheme-like pure untyped curried lambda
;;; calculus evaluated in normal order. "Pure" means there are only
;;; functions: numbers, pairs, booleans, etc. are Church encoded.
;;; Numbers are expanded into Church numerals by the expander.

;;; ;;;;;;;;;;;;;
;;; Fundamentals

(define-syntax splicing-begin
  (syntax-rules ()
    ((_ body ...) (splicing-let-syntax () body ...))))

(define I (lambda x x))

(define-syntax ∘
  ;; function composition
  (syntax-rules ()
    ((∘) I)
    ((∘ f g ...)
     (f (∘ g ...)))))

(define-syntax ∘←
  ;; postfix function composition
  (letrec-syntax ((R (syntax-rules ()
                       ((R () (acc ...))
                        (∘ acc ...))
                       ((R (head rest ...) (acc ...))
                        (R (rest ...) (acc ...))))))
    (syntax-rules ()
      ((∘← f ...) (R (f ...) ())))))

(splicing-let-syntax ((lambda lambda))
  ;; This binds `lambda` in the global syntatic environment into a
  ;; local, immutable syntatic environment.
  (define-syntax begin
    (syntax-rules ()
      ((begin) I)
      ((begin x y ...)
       ((lambda dummy (begin y ...)) x)))))

(splicing-let-syntax ((%lambda lambda))
  ;; This binds `%lambda` because `lambda` will be overridden in the
  ;; global syntatic environment.
  (define-syntax lambda
    (syntax-rules ()
      ((_ (formal1 formal-rest ...) body ...)
       (lambda formal1 (lambda (formal-rest ...) body ...)))
      ((_ (formal) body ...)
       (lambda formal body ...))
      ((_ formal body ...)
       (%lambda formal (begin body ...))))))

(define-syntax λ lambda)

(define-syntax let
  ;; Regular `let`. Named `let` is defined later.
  (syntax-rules ()
    ((let () body ...)
     ((λ (dummy) body ...) I))
    ((let ((name value) ...) body ...)
     ((λ (name ...) body ...) value ...))))

(define-syntax let* let)

(define Y
  ;; Y combinator.
  (lambda (f)
    (let ((recursor (λ (x) (f (x x)))))
      (recursor recursor))))

(define-syntax rec
  ;; Define a recursive function.
  (syntax-rules ()
    ((rec (name . formals) body ...)
     (Y (λ (name . formals) body ...)))))

(define-syntax υ rec)

(splicing-let-syntax ((%let let))
  ;; Named `let`.
  (define-syntax let
    (syntax-rules ()
      ((let ((name value) ...) body ...)
       (%let ((name value) ...) body ...))
      ((let name ((param first-binding) ...) body ...)
       ((υ (name param ...) body ...)
        first-binding ...)))))

(splicing-let-syntax ((%define define))
  (define-syntax define
    (syntax-rules ()
      ((define (name . args) body ...)
       (define name (rec (name . args) body ...)))
      ((define name body ...)
       (%define name (letrec ((name (begin body ...))) name))))))

(define-syntax binary-to-arbitrary
  ;; Convert a binary procedure to a syntatic procedure of arbitrary
  ;; arguments.
  (syntax-rules ()
    ((_ name binary)
     (define-syntax name
       (syntax-rules ...* ()
         ((_ x) x)
         ((_ x y ...*) (binary x (name y ...*))))))))

(define-syntax let-lowered
  (syntax-rules (define)
    ((_ ((name value) ...) (define d-name d-value ...))
     (splicing-begin
      (define d-name (let ((name value) ...) d-value ...))
      ...))))

;;; ;;;;;;;;;;;;
;;; untyped operations

(define (%cons car cdr)
  (λ selector (selector car cdr)))

(define (%car pair)
  (pair (λ (x y) x)))
(define (%cdr pair)
  (pair (λ (x y) y)))

(define %true (λ (x y) x))
(define %false (λ (x y) y))
(define (%and x y) (x y x))

(define %null (λ (f x) x))
(define (%null? value)
  (church-numeral (λ x %false) %true))

(define (%succ n) (λ (f x) (f (n f x))))

(define (%pred n)
  (%car (n (λ p (%cons (%cdr p) (%succ (%car p))))
           (%cons %null %null))))

(define (%- x y) (y pred) x)
(define (%<= x y) (%null? (%- x y)))
(define (%= x y) (%and (%<= x y) (%<= y x)))

;;; ;;;;;;;;;;;;;;;
;;; Typed pairs

(define (type-constructor tag)
  (λ value (%cons tag value)))
(define (type-predicate tag)
  (λ value (%= (%car value) tag)))
(define type-value %cdr)

(let-lowered ((tag %null)))

(let-lowered ((tag %null))
  (define null ((type-constructor tag) null))
  (define null? (type-predicate tag)))

(let-lowered ((tag (%succ %null)))
  (define (cons car cdr)
    ((type-constructor tag) (%cons car cdr)))
  (define pair? (type-predicate tag)))

(define (car cell)
  ((∘← type-value primitive-car) cell))

(define (cdr cell)
  ((∘← type-value primitive-cdr) cell))

