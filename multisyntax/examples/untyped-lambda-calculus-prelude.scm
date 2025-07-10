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
       (define name (rec (name . args) body ...)))
      ((define name body)
       (%define name (Y (λ (name) body)))))))

(define (%cons car cdr) (λ selector (selector car cdr)))
(define (%car pair) (pair (λ (x y) x)))
(define (%cdr pair) (pair (λ (x y) y)))
