#| Copyright (c) Peter McGoron 2025
|
 | Licensed under the Apache License, Version 2.0 (the "License");
 | you may not use this file except in compliance with the License.
 | You may obtain a copy of the License at
 |
 |     http://www.apache.org/licenses/LICENSE-2.0
 |
 | Unless required by applicable law or agreed to in writing, software
 | distributed under the License is distributed on an "AS IS" BASIS,
 | WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 | See the License for the specific language governing permissions and
 | limitations under the License.
 |------------------------------------------------------------------------
 | Example implementation of macros for an untyped lambda calculus.
 | NOTE: `define-syntax` is deliberately different from RNRS, because
 | `define-syntax`, in effect, modifies bindings in the global syntatic
 | environment: it does not create new bindings.
 |
 | Syntax of the core:
 |
 |     TERM ::= (lambda ID TERM) | (TERM TERM+) | ID
 |
 | where `(TERM1 TERM2 TERM3 TERM4 ...)` is interpreted as
 | `((TERM1 TERM2) TERM3 TERM4 ...)`.
 |
 | Syntax with macros:
 |
 | TOPLEVEL ::= (define-syntax ID TFMR)
 |            | (define ID TERM)
 |            | (splicing-let-syntax ((ID TFMR) ...) TOPLEVEL ...)
 |            | (splicing-letrec-syntax ((ID TFMR) ...) TOPLEVEL ...)
 |            | EXPR
 | EXPR ::= (let-syntax ((ID TFMR) ...) EXPR)
 |        | (letrec-syntax ((ID TFMR) ...) EXPR)
 |        | (lambda ID EXPR)
 |        | (EXPR EXPR EXPR ...)
 |        | ID
 | TFMR ::= ID
 |        | (let-syntax ((ID TFMR) ...) TFMR)
 |        | (letrec-syntax ((ID TFMR) ...) TFMR)
 |        | (syntax-rules ID? (ID ...) ((ID . pattern) PROD) ... )
 |
 | The lexical environment is made up of either the symbol `variable`
 | (meaning that the identifier is some bound variable), a value
 | satisfying `transformer?` (a syntax transformer), or a symbol for a
 | primitive syntatic transformer (`'lambda` for `lambda`, etc).
 |#

(define-record-type <syntax-rules>
  ;; `clauses` is a list of cons cells, the car of each cell is the matcher
  ;; and the cdr of each cell is the producer.
  (wrap-syntax-rules clauses)
  transformer?
  (clauses unwrap-syntax-rules))

(define (empty-map) (hashmap free-identifier-comparator))

(define (inject-primitive name)
  (let ((id (generate-identifier name)))
    (add-substitution id
                      id
                      (generate-lexical-location name name))))

(define initial-environment
  (hashmap free-identifier-comparator
           (empty-wrap 'lambda) 'lambda
           (empty-wrap 'define) 'define
           (empty-wrap 'define-syntax) 'define-syntax
           (empty-wrap 'splicing-let-syntax) 'splicing-let-syntax
           (empty-wrap 'splicing-letrec-syntax) 'splicing-letrec-syntax
           (empty-wrap 'let-syntax) 'let-syntax
           (empty-wrap 'letrec-syntax) 'letrec-syntax
           (empty-wrap 'syntax-rules) 'syntax-rules))

(define (church-numeral stx)
  ;; Convert the exact non-negative integer `stx` into a Church numeral.
  (let ((function (generate-identifier 'f))
        (argument (generate-identifier 'x)))
    (list (inject-primitive 'lambda)
          function
          (list (inject-primitive'lambda)
                argument
                (let loop ((i stx))
                  (if (zero? i)
                      argument
                      (list function
                            (loop (- i 1)))))))))

(define (union-names env new-names tfmrs)
  ;; Add `new-names` bound to `tfmrs` in `env`, overriding previous
  ;; bindings.
  (hashmap-union (alist->hashmap free-identifier-comparator
                                 (map (lambda (name tfmr)
                                        (cons name tfmr))
                                      new-names tfmrs))
                 env))

(define (is? env stx id)
  ;; Return true if `stx` resolves to a location with value `id`, either
  ;; lexically or in the global environment.
  (let ((stx (unwrap-syntax stx)))
    (and (pair? stx)
         (identifier? (car stx))
         (let ((location (resolve (car stx))))
           (if (lexical-location? location)
               (eq? (lexical-location-value location) id)
               (eq? (hashmap-ref/default env (car stx) #f) id))))))

(define (identifier-is-transformer env stx)
  ;; Returns transformer if `stx` resolves to a syntax rules transformer,
  ;; lexically or in the global environment.
  (let ((stx (unwrap-syntax stx)))
    (cond
      ((not (pair? stx)) #f)
      ((not (identifier? (car stx))) #f)
      (else
       (let ((location (resolve (car stx))))
         (if (lexical-location? location)
             (let ((value (lexical-location-value location)))
               (and (transformer? value) value))
             (hashmap-ref env
                          (car stx)
                          (lambda () #f)
                          (lambda (x) (and (transformer? x) x)))))))))

(define (on-bindings stx)
  ;; Given (_ ((name value) ...) body ...), return 
  ;; 
  ;; 1. `name ...`,
  ;; 2. `tmp ...`, the same length as `name ...`, which are the names with
  ;;    new lexical locations.
  ;; 3. `value ...`.
  ;; 4. `body ...`
  ;; 
  (let* ((stx (unwrap-list stx))
         (binders (unwrap-list (syntax-cxr '(d a) stx)))
         (old-names (map syntax-car binders))
         (new-lls (generate-lexical-locations old-names)))
    (values old-names
            (map (lambda (old-name ll)
                   (add-substitution old-name old-name ll))
                 old-names
                 new-lls)
            (map (lambda (form) (syntax-cxr '(d a) form))
                 binders)
            (syntax-cxr '(d d) stx))))

(define (set-names-to-transformers! new-names tfmrs)
  (for-each (lambda (new-name tfmr)
              (set-lexical-location-value! (resolve new-name) tfmr))
            new-names tfmrs))

(define (let-syntax-expander env stx K)
  ;; Continuation-passing-style expansion of `let-syntax`. Expands the
  ;; body of the `let-syntax` form using the continuation `K`, with an
  ;; environment binding the transformers to names as defined by the
  ;; `let-syntax` declaration.
  (let*-values (((old-names new-names tfmrs body) (on-bindings stx))
                ((tfmrs) (map (lambda (stx) (expand-transformer env stx))
                              tfmrs)))
    (set-names-to-transformers! new-names tfmrs)
    (K old-names new-names body)))

(define (letrec-syntax-expander env stx K)
  ;; CPS expansion of `letrec-syntax`. See `let-syntax-expander`.
  (let*-values (((old-names new-names tfmrs body) (on-bindings stx))
                ((tfmrs)
                 (map (lambda (stx)
                        (expand-transformer env
                                            (add-substitution
                                             stx
                                             old-names
                                             new-names)))
                      tfmrs)))
    (set-names-to-transformers! new-names tfmrs)
    (K old-names new-names body)))

(define (eval-transformer name tfmr stx)
  ;; Try to match each pattern in `tfmr`, and when one matches, call the
  ;; producer on the matched data.
  (let loop ((tfmr (unwrap-syntax-rules tfmr)))
    (if (null? tfmr)
        (error "no matched pattern" name stx tfmr)
        (let ((matcher (caar tfmr))
              (producer (cdar tfmr)))
          (cond
            ((matcher stx)
             => (lambda (bindings)
                  (let ((return (producer bindings)))
                    return)))
            (else (loop (cdr tfmr))))))))

(define (macro-expand-expander name stx tfmr K)
  ;; Evaluate the transformer `tfmr` with `stx`, properly adding and
  ;; removing macro expansion timesteps. Pass the result to `K`.
  (let ((ts (generate-timestamp)))
    (K (add-timestamp (eval-transformer name
                                        tfmr
                                        (add-timestamp stx ts))
                      ts))))

(define (expand-expr env stx)
  ;; Expander of expressions (not toplevel statements).
  (let ((stx (unwrap-syntax stx)))
    (cond
      ((and (exact-integer? stx) (positive? stx))
       (church-numeral stx))
      ((self-syntax? stx) stx)
      ((identifier? stx) stx)
      ((is? env stx 'lambda)
       (let* ((bound (syntax-cxr '(d a) stx))
              (renamed (add-substitution
                        bound
                        bound
                        (generate-lexical-location (syntax->datum bound) 'variable)))
              (body (syntax-cxr '(d d a) stx)))
         (list (inject-primitive 'lambda)
               renamed
               (expand-expr env (add-substitution body bound renamed)))))
      ((is? env stx 'let-syntax)
       (let-syntax-expander env
                            stx
                            (lambda (old-names new-names body)
                              (expand-expr env
                                           (add-substitution
                                            (syntax-car body)
                                            old-names
                                            new-names)))))
      ((is? env stx 'letrec-syntax)
       (letrec-syntax-expander env
                               stx
                               (lambda (old-names new-names body)
                                 (expand-expr env
                                              (add-substitution
                                               (syntax-car body)
                                               old-names
                                               new-names)))))
      ((identifier-is-transformer env stx)
       => (lambda (tfmr)
            (macro-expand-expander (syntax->datum (syntax-car stx))
                                   stx
                                   tfmr
                                   (cut expand-expr env <>))))
      ((pair? stx)
       (cons (expand-expr env (car stx)) (expand-expr env (cdr stx))))
      (else (error "invalid syntax" stx)))))

(define (expand-syntax-rules ellipsis literals clauses)
  ;; Expand a `syntax-rules` transformer and wrap it as a `syntax-rules`
  ;; object.
  (define (operate clause)
    (let*-values (((clause) (unwrap-list clause))
                  ((literals) (unwrap-list literals))
                  ((matcher bindings _)
                   (compile-pattern literals
                                    (list-ref clause 0)
                                    ellipsis))
                  ((bindings)
                   (hashmap-map (lambda (key value)
                                  (values key (car value)))
                                bound-identifier-comparator
                                bindings)))
      (cons matcher (compile-producer literals
                                      (list-ref clause 1)
                                      bindings
                                      ellipsis))))
  (let ((clauses (unwrap-list clauses)))
    (wrap-syntax-rules (map operate clauses))))

(define (expand-transformer env stx)
  (let ((stx (unwrap-syntax stx)))
    (cond
      ((identifier? stx)
       (let ((value (resolve stx)))
         (if (lexical-location? value)
             (lexical-location-value value)
             (hashmap-ref env
                          stx
                          (lambda () (error "transformer not found" stx))))))
      ((identifier-is-transformer env stx)
       => (lambda (tfmr)
            (macro-expand-expander (syntax->datum (syntax-car stx))
                                   stx
                                   tfmr
                                   (cut expand-transformer <> stx))))
      ((is? env stx 'syntax-rules)
       (let ((stx (unwrap-list stx)))
         (if (identifier? (syntax-cxr '(d a) stx))
             (expand-syntax-rules (syntax-cxr '(d a) stx)
                                  (syntax-cxr '(d d a) stx)
                                  (syntax-cxr '(d d d) stx))
             (expand-syntax-rules #f
                                  (syntax-cxr '(d a) stx)
                                  (syntax-cxr '(d d) stx)))))
      ;; Although one could use splicing-let-syntax and splicing-letrec-syntax
      ;; to achieve similar behavior, the splicing variants would not have the
      ;; name bound during their expansion.
      ((is? env stx 'let-syntax)
       (let-syntax-expander env
                            stx
                            (lambda (old-names new-names body)
                              (expand-transformer env
                                                  (add-substitution
                                                   (syntax-car body)
                                                   old-names
                                                   new-names)))))
      ((is? env stx 'letrec-syntax)
       (letrec-syntax-expander env
                               stx
                               (lambda (old-names new-names body)
                                 (expand-transformer env
                                                     (add-substitution
                                                      (syntax-car body)
                                                      old-names
                                                      new-names)))))
      (else (error "invalid syntax for transformer" stx)))))

(define (accumulate-splicing globalenv body)
  ;; Expand each toplevel declaraion in `body` with the lexical environment
  ;; `lexenv` with an accumulated global environment `globalenv`.
  ;; 
  ;; Returns `(values globalenv acc)` which is the expanded body clauses
  ;; and the accumulated global environment.
  (let loop ((globalenv globalenv)
             (iter (unwrap-list body))
             (acc '()))
    (if (null? iter)
        (values globalenv (reverse acc))
        (let-values (((globalenv next)
                      (expand-toplevel globalenv (car iter))))
          (loop globalenv (cdr iter) (append-reverse next acc))))))

(define (expand-toplevel env stx)
  ;; Expands toplevel expressions with accumulated global environment
  ;; `env`.
  (let ((stx (unwrap-syntax stx)))
    (cond
      ((is? env stx 'define-syntax)
       (let* ((stx (unwrap-list stx))
              (name (syntax-cxr '(d a) stx))
              (tfmr (expand-transformer env (syntax-cxr '(d d a) stx))))
         (values (hashmap-set env name tfmr) '())))
      ((is? env stx 'splicing-let-syntax)
       (let-syntax-expander
        env
        stx
        (lambda (old-names new-names body)
          (accumulate-splicing env
                               (add-substitution body old-names new-names)))))
      ((is? env stx 'splicing-letrec-syntax)
       (letrec-syntax-expander
        env
        stx
        (lambda (old-names new-names body)
          (accumulate-splicing env
                               (add-substitution body old-names new-names)))))
      ((is? env stx 'define)
       (let* ((name (syntax-cxr '(d a) stx))
              (expanded-value (expand-expr env (syntax-cxr '(d d a) stx))))
         (values (hashmap-set env name 'variable)
                 (list (list (inject-primitive 'define)
                             name
                             expanded-value)))))
      ((identifier-is-transformer env stx)
       => (lambda (tfmr)
            (macro-expand-expander (syntax->datum (syntax-car stx))
                                   stx
                                   tfmr
                                   (lambda (stx)
                                     (expand-toplevel env stx)))))
      (else
       (values env (list (expand-expr env stx)))))))

(define (expand initenv stx)
  ;; Expand `stx`, which is a list of syntax forms, into a list of syntax
  ;; forms, with initial environment `initenv`. Returns the new environment
  ;; and the list of expanded forms.
  (define (fold globalenv stxlist acc)
    (if (null? stxlist)
        (values globalenv (reverse acc))
        (let-values (((globalenv next)
                      (expand-toplevel globalenv (car stxlist))))
          (fold globalenv (cdr stxlist) (append-reverse next acc)))))
  (fold initenv (unwrap-list stx) '()))

(define (alpha stx)
  (let ((stx (unwrap-syntax stx)))
    (cond
      ((pair? stx) (cons (alpha (car stx)) (alpha (cdr stx))))
      ((identifier? stx)
       (let ((loc (resolve stx)))
         (if (symbol? loc)
             loc
             (lexical-location->string loc))))
      (else stx))))

(define (debruijnize env stx free-variables)
  (let ((stx (unwrap-syntax stx)))
    (cond
      ((is? env stx 'lambda)
       (list 'lambda
             (debruijnize env
                          (syntax-cxr '(d d a) stx)
                          (cons (cons (syntax-cxr '(d a) stx)
                                      0)
                                (map (lambda (pair)
                                       (cons (car pair)
                                             (+ 1 (cdr pair))))
                                     free-variables)))))
      ((and (identifier? stx)
            (assoc stx free-variables bound-identifier=?))
       => cdr)
      ((identifier? stx) (syntax->datum stx))
      ((pair? stx)
       (cons (debruijnize env (car stx) free-variables)
             (debruijnize env (cdr stx) free-variables)))
      (else stx))))
