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

(splicing-let-syntax ((define-syntax* define-syntax))
  (define-syntax* define-syntax
    (syntax-rules (let-syntax letrec-syntax)
      ((_ name (let-syntax bindings body))
       (splicing-let-syntax bindings
         (define-syntax name body)))
      ((_ name (letrec-syntax bindings body))
       (splicing-letrec-syntax bindings
         (define-syntax name body)))
      ((_ name body)
       (define-syntax* name body)))))

(define-syntax ∘←
  ;; postfix function composition
  (let-syntax ((R (syntax-rules ()
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

(define (Y f)
  ;; Y combinator.
  (let ((recursor (λ (x) (f (x x)))))
    (recursor recursor)))

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

(define-syntax letrec
  (syntax-rules ()
    ((letrec ((name value) ...) body ...)
     (let ((name (λ (name ...) value)) ...)
       (let ((name (name name ...)) ...)
         body ...)))))

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

;;; Boolean logic. `#t` maps to this definition of true, and `#f` maps to
;;; this definition of false.
(define (true x y) x)
(define (false x y) y)
(define (not x) (x false true))

(define (binary-and x y) (x y false))
(binary-to-arbitrary and binary-and)

(define (binary-or x y) (x true y))
(binary-to-arbitrary or binary-or)

(define (if predicate truthy falsy)
  (predicate truthy falsy))

(define-syntax cond
  ;; Simplified cond.
  (syntax-rules (else)
    ((_ (predicate expression ...)
        rest ...)
     (if predicate
         (begin expression ...)
         (case rest ...)))
    ((_ (else expression ...))
     (begin expression ...))))

;;; Church numeral operations

(define (S n)
  (λ (f x) (f (n f x))))

(define (binary+ x y) (x S n))
(binary-to-arbitrary + binary+)

(define (binary* x y) (λ (f) (m (n f))))
(binary-to-arbitary * binary*)

(define (expt b n) (n b))

;;; Pairs and lists

(define (cons car cdr)
  (lambda (f) (f car cdr)))
(define null false)

(define-syntax list
  (syntax-rules ()
    ((list) null)
    ((list x y ...)
     (cons x (list y ...)))))

(define-syntax cons*
  (syntax-rules ()
    ((cons* x y) (cons x y))
    ((cons* x y z ...)
     (cons x (cons* y z ...)))))

(define (car pair) (pair true))
(define (cdr pair) (pair false))

(define (null? x) (x (λ (h t d) false) true))

(define (fold kons nil list)
  ;; This is a left-fold: i.e.
  ;; (kons (kons (... (kons (car list) nil))))
  ;; 
  ;; This is the fundamental list iterator that all other list functions
  ;; are defined with.
  (if (null? list)
      nil
      (fold kons (kons (car list) nil) (cdr list))))

(define (reverse-append list1 list2)
  (fold cons list2 list1))
(define (reverse list) (reverse-append list1 null))

(define (fold-right kons nil list)
  (fold kons nil (reverse list)))

(define (append list1 list2) (fold-right cons list2 list1))

(define (append-n list-of-lists)
  (fold-right (λ (list acc) (append list acc))
              null
              list-of-lists))

(let-lowered ((mapper (λ (car cdr) (cons (f car) cdr))))
  (define (map-reverse f list) (fold mapper null list))
  (define (map f list) (fold-right mapper null list)))

(define (any f list)
  (fold (λ (value previous) (or previous (f value))) false list))
(define (all f list)
  (fold (λ (value previous) (and previous (f value))) true list))

(define (length list) (fold (lambda (_ n) (+ n 1)) 0 list))

(define (iota-fold kons nil count start step)
  ;; `iota` cannot be defined in terms of `fold`s.
  (if (<= count 0)
      nil
      (iota-fold kons
                 (kons start nil)
                 (- count 1)
                 (+ start step)
                 step)))

(define (reverse-iota count start step)
  (iota-fold cons null count start step))
(define (iota count start step)
  (reverse (reverse-iota count start step)))

(define (make-list n fill)
  (iota-fold (λ (_ cdr) (cons fill cdr)) null n 0 1))

(define (list-tabulate n init-proc)
  (iota-fold (λ (i cdr) (cons (init-proc i) cdr)) null n 0 1))

(define (list-tail n list failure)
  (let ((length (length list)))
    (if (>= n length)
        (failure n)
        (iota-fold (lambda (_ list) list) n 0 1))))

(define (list-ref n list failure)
  (car (list-tail n list (lambda (n)
                           (cons (failure n) nil)))))

(define (list-drop list i)
  (iota-fold (λ (_ cdr) cdr) list i 0 1))

(define (list-take list i)
  (reverse (list-drop (reverse list) (- (length list) i))))

(define (list-take-right list i)
  (list-drop list (- (length list) i)))

(define (list-drop-right list i)
  (list-take list (- (length list) i)))

(define (last list default)
  (fold (λ (kar knil) kar) default list))

(define (transverse list-of-lists)
  (let ((init-list (list-tabluate (length (car list-of-lists)) null))
        (add-element
         (λ (el state)
           (let ((n (list-ref state 0))
                 (acc (list-ref state 1)))
             (list (+ n 1)
                   (append (list-take acc n)
                           (cons el (list-ref acc n))
                           (list-drop acc (+ n 1))))))))
    (fold-right (λ (l state)
                  (fold-right add-element l (list 0 state)))
                init-list
                list-of-lists)))

(define (foldn f init list-of-lists)
  (fold (λ (elements init)
          (letrec ((loop
                    (λ (f elements init)
                      (if (null? elements)
                          (f init)
                          (loop (f (car elements)) (cdr elements) init)))))
            (loop f elements init)))
        init
        (transverse list-of-lists)))

(define (list= = l1 l2)
  (fold-n (λ (e1 e2 result)
            (cond
              ((not result) result)
              ((= e1 e2) true)
              (else false)))
          (list l1 l2)))

