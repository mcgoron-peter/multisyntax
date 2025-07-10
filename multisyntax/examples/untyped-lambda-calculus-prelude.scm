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

(splicing-let-syntax ((%lambda lambda))
  ;; This binds `%lambda` because `lambda` will be overridden in the
  ;; global syntatic environment.
  (define-syntax lambda
    (syntax-rules ()
      ((_ (formal) body)
       (lambda formal body))
      ((_ (formal1 formal-rest ...) body)
       (lambda formal1 (lambda (formal-rest ...) body)))
      ((_ () body) (syntax-error "functions must have at least one argument"))
      ((_ formal body)
       (%lambda formal body)))))

(define-syntax λ lambda)

(define-syntax let
  ;; Regular `let`. Named `let` is defined later.
  (syntax-rules ()
    ((let () body ...)
     (error "let bindings must have at least one argument"))
    ((let ((name value) ...) body ...)
     ((λ (name ...) body ...) value ...))))

(define-syntax let* let)

(define Y
  ;; Y combinator.
  (λ (f)
    (let ((recursor (λ (x) (f (x x)))))
      (recursor recursor))))

(define-syntax rec
  ;; Define a recursive function.
  (syntax-rules ()
    ((rec (name formal1 formal-rest ...) body)
     (Y (λ (name formal1 formal-rest ...) body)))))

(define-syntax υ rec)

(splicing-let-syntax ((%let let))
  ;; Named `let`.
  (define-syntax let
    (syntax-rules ()
      ((let ((name value) ...) body)
       (%let ((name value) ...) body))
      ((let name ((param first-binding) ...) body)
       ((υ (name param ...) body)
        first-binding ...)))))

(splicing-let-syntax ((%define define))
  (define-syntax define
    (syntax-rules ()
      ((define (name . args) body ...)
       (%define name (rec (name . args) body ...)))
      ((define name body)
       (%define name (Y (λ (name) body)))))))

;;; Untyped primitives

(define %true (λ (x y) x))
(define %false (λ (x y) y))
(define (%and x y) (x y x))
(define (%or x y) (x x y))

(define (%cons car cdr) (λ selector (selector car cdr)))
(define (%car value) (value %true))
(define (%cdr value) (value %false))

(define %zero (%cons %true I))
(define (%succ n) (%cons %false n))
(define %zero? %car)
(define (%pred n)
  ((%zero? n) %zero (%cdr n)))

(define (%= n m)
  ((%and (%zero? n) (%zero? m))
   %true
   ((%or (%zero? n) (%zero? m))
    %false
    (%= (%pred n) (%pred m)))))

;;; Typed objects

(define (error payload) (%cons %zero payload))

(define boolean-tag (%succ %zero))
(define true (%cons boolean-tag %true))
(define false (%cons boolean-tag %false))

(define (if precondition on-true on-false)
  (let ((tag-of-precondition (%car precondition)))
    ((%= tag-of-precondition boolean-tag)
     ((%cdr precondition) on-true on-false)
     (error boolean-tag))))

(define (predicate-for-tag tag)
  (λ (value) (%cons boolean-tag (%= (%car value) tag))))

(define boolean? (predicate-for-tag boolean-tag))
(define error-object? (predicate-for-tag %zero))

(define-syntax define-record-type
  (syntax-rules ()
    ((_ tag constructor predicate (name accessor) ...)
     (splicing-begin
      (define predicate (predicate-for-tag tag))
      (define (constructor name ...)
        (%cons tag (λ selector (selector name ...))))
      (define (accessor value)
        (if (predicate value)
            ((%cdr value) (λ (name ...) name))
            (error tag)))
      ...))))

(define pair-tag (%succ (%succ %zero)))
(define-record-type pair-tag
  cons
  pair?
  (car car) (cdr cdr))

(define null-tag (%succ (%succ (%succ %zero))))
(define null (%cons null-tag I))
(define null? (predicate-for-tag null-tag))

;;; Boolean operations

(define (truthy? x)
  (if (boolean? x)
      x
      true))

(define (or2 x y)
  (if (truthy? x)
      x
      (if (truthy? y)
          y
          false)))

(define (and2 x y)
  (if (truthy? x)
      (if (truthy? y)
          y
          false)
      false))

(define-syntax binary-to-arbitrary
  (syntax-rules ()
    ((_ name base-case binary)
     (define-syntax name
       (syntax-rules ...* ()
         ((_) base-case)
         ((_ x y ...*) (binary x (name y ...*))))))))

(binary-to-arbitrary or false or2)
(binary-to-arbitrary and true and2)

(define (not x)
  (if (truthy? x)
      false
      true))

(define-syntax when
  (syntax-rules ()
    ((_ predicate body)
     (if predicate
         body
         (error boolean-tag)))))

(define-syntax cond
  (syntax-rules (else)
    ((_ (else body)) body)
    ((_ (predicate) rest ...)
     (or predicate (cond rest ...)))
    ((_ (predicate body) rest ...)
     (if predicate body (cond rest ...)))))

;;; Numbers

(define natural-tag (%succ (%succ (%succ (%succ %zero)))))
(define-record-type natural-tag
  %natural
  natural?
  (repr natural->repr))

(define zero (%natural null))
(define (zero? n)
  (when (natural? n)
    (null? (natural->repr n))))

(define (succ n)
  (when (natural? n)
    (%natural (cons null (natural->repr n)))))

(define (pred n)
  (when (natural? n)
    (let ((value (natural->repr n)))
      (if (null? n)
          (%natural n)
          (%natural (cdr n))))))

(define (+bin x y)
  (when (and (natural? x) (natural? y))
    (if (zero? y)
        x
        (+bin (succ x) (pred y)))))
(binary-to-arbitrary + zero +bin)

(define (-bin x y)
  (when (and (natural? x) (natural? y))
    (if (zero? y)
        x
        (-bin (pred x) (pred y)))))
(binary-to-arbitrary - zero -bin)

(define (*bin x y)
  (when (and (natural? x) (natural? y))
    (if (zero? y)
        zero
        (+bin x (*bin x (pred y))))))
(binary-to-arbitrary * (succ zero) *bin)

