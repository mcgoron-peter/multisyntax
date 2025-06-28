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

(define (empty-map) (hashmap location-comparator))

(define initial-environment
  (hashmap location-comparator
           'lambda 'lambda
           'define 'define
           'define-syntax 'define-syntax
           'splicing-let-syntax 'splicing-let-syntax
           'splicing-letrec-syntax 'splicing-letrec-syntax
           'let-syntax 'let-syntax
           'letrec-syntax 'letrec-syntax
           'syntax-rules 'syntax-rules))

(define (church-numeral stx)
  ;; Convert the exact non-negative integer `stx` into a Church numeral.
  (let ((function (generate-identifier 'f))
        (argument (generate-identifier 'x)))
    (list (empty-wrap 'lambda)
          function
          (list (empty-wrap 'lambda)
                argument
                (let loop ((i stx))
                  (if (zero? i)
                      argument
                      (list function
                            (loop (- i 1)))))))))

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

(define (union-names env new-names tfmrs)
  ;; Add `new-names` bound to `tfmrs` in `env`, overriding previous
  ;; bindings.
  (hashmap-union (alist->hashmap location-comparator
                                 (map (lambda (name tfmr)
                                        (cons (resolve name) tfmr))
                                      new-names tfmrs))
                 env))

(define (is? env stx id)
  ;; Return true if `stx` in `env` is `eq?` to `id`.
  (let ((stx (unwrap-syntax stx)))
    (and (pair? stx)
         (identifier? (car stx))
         (let ((resolved (hashmap-ref/default env (resolve (car stx)) #f)))
           (eq? resolved id)))))

(define (identifier-is-transformer env stx)
  ;; Returns transformer if `stx` is a syntax-rules transformer in `env`.
  (let ((stx (unwrap-syntax stx)))
    (cond
      ((not (pair? stx)) #f)
      ((not (identifier? (car stx))) #f)
      ((hashmap-ref/default env (resolve (car stx)) #f)
       => (lambda (return)
            (and (transformer? return) return)))
      (else #f))))

(define (let-syntax-expander env stx K)
  ;; Continuation-passing-style expansion of `let-syntax`. Expands the
  ;; body of the `let-syntax` form using the continuation `K`, with an
  ;; environment binding the transformers to names as defined by the
  ;; `let-syntax` declaration.
  (let*-values (((old-names new-names tfmrs body) (on-bindings stx))
                ((tfmrs) (map (lambda (stx) (expand-transformer env stx))
                              tfmrs)))
    (K (union-names env new-names tfmrs)
       (add-substitution (syntax-cxr '(a) body)
                         old-names
                         new-names))))

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
    (K (union-names env new-names tfmrs)
       (add-substitution (syntax-cxr '(a) stx)
                         old-names
                         new-names))))

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

(define (macro-expand-expander name env stx tfmr K)
  ;; Evaluate the transformer `tfmr` with `stx`, properly adding and
  ;; removing macro expansion timesteps. Pass the result to `K`, which
  ;; is a function of one argument (not two like the `let-syntax-expander`
  ;; procedures).
  (let ((ts (generate-timestamp)))
    (K (add-timestamp (eval-transformer name
                                        tfmr
                                        (add-timestamp stx ts))
                      ts))))

(define (expand-expr env stx)
  ;; TODO: fix function application
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
                        (generate-lexical-location (syntax->datum bound))))
              (body (syntax-cxr '(d d a) stx)))
         (list (empty-wrap 'lambda)
               renamed
               (expand-expr
                (hashmap-set env (resolve renamed) 'variable)
                (add-substitution body bound renamed)))))
      ((is? env stx 'let-syntax)
       (let-syntax-expander env stx expand-expr))
      ((is? env stx 'letrec-syntax)
       (letrec-syntax-expander env stx expand-expr))
      ((identifier-is-transformer env stx)
       => (lambda (tfmr)
            (macro-expand-expander (syntax->datum (syntax-car stx))
                                   env
                                   stx
                                   tfmr
                                   (lambda (stx)
                                     (expand-expr env stx)))))
      ((pair? stx)
       (cons (expand-expr env (car stx)) (expand-expr env (cdr stx))))
      (else (error "invalid syntax" stx)))))

(define (expand-syntax-rules env ellipsis literals clauses)
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
       (hashmap-ref env (resolve stx) (lambda () (error "transformer not found" stx))))
      ((identifier-is-transformer env stx)
       => (lambda (tfmr)
            (macro-expand-expander (syntax->datum (syntax-car stx))
                                   env
                                   stx
                                   tfmr
                                   (lambda (stx)
                                     (expand-transformer env stx)))))
      ((is? env stx 'syntax-rules)
       (let ((stx (unwrap-list stx)))
         (if (identifier? (syntax-cxr '(d a) stx))
             (expand-syntax-rules env
                                  (syntax-cxr '(d a) stx)
                                  (syntax-cxr '(d d a) stx)
                                  (syntax-cxr '(d d d) stx))
             (expand-syntax-rules env
                                  #f
                                  (syntax-cxr '(d a) stx)
                                  (syntax-cxr '(d d) stx)))))
      ;; TODO: remove these, they are definable in terms of the splicing
      ;; versions.
      ((is? env stx 'let-syntax)
       (let-syntax-expander env stx expand-transformer))
      ((is? env stx 'letrec-syntax)
       (letrec-syntax-expander env stx expand-transformer))
      (else (error "invalid syntax for transformer" stx)))))

(define (accumulate-splicing globalenv lexenv body)
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
                      (expand-toplevel globalenv lexenv (car iter))))
          (loop globalenv (cdr iter) (append-reverse next acc))))))

(define (expand-toplevel globalenv lexenv stx)
  ;; Expands toplevel expressions with accumulated global environment
  ;; `globalenv`.
  (let ((stx (unwrap-syntax stx))
        (env (hashmap-union lexenv globalenv)))
    (cond
      ((is? env stx 'define-syntax)
       (let* ((stx (unwrap-list stx))
              (name (syntax-cxr '(d a) stx))
              (tfmr (expand-transformer env (syntax-cxr '(d d a) stx))))
         (values (hashmap-set globalenv (resolve name) tfmr) '())))
      ((is? env stx 'splicing-let-syntax)
       (let*-values (((old-names new-names tfmrs body)
                      (on-bindings stx))
                     ((tfmrs) (map (lambda (stx)
                                     (expand-transformer env stx))
                                   tfmrs)))
         (accumulate-splicing globalenv
                              (union-names lexenv new-names tfmrs)
                              body)))
      ((is? env stx 'splicing-letrec-syntax)
       (let*-values (((old-names new-names tfmrs body) (on-bindings stx))
                     ((tfmrs) (map (lambda (stx)
                                     (expand-transformer env
                                                         (add-substitution
                                                          stx
                                                          old-names
                                                          new-names)))
                                   tfmrs)))
         (accumulate-splicing globalenv
                              (union-names lexenv new-names tfmrs)
                              body)))
      ((is? env stx 'define)
       (let* ((name (syntax-cxr '(d a) stx))
              (expanded-value (expand-expr env (syntax-cxr '(d d a) stx))))
         (values (hashmap-adjoin globalenv (resolve name) 'variable)
                 (list (list (empty-wrap 'define)
                             name
                             expanded-value)))))
      ((identifier-is-transformer env stx)
       => (lambda (tfmr)
            (macro-expand-expander (syntax->datum (syntax-car stx))
                                   env
                                   stx
                                   tfmr
                                   (lambda (stx)
                                     (expand-toplevel globalenv lexenv stx)))))
      (else
       (values globalenv
               (list
                (expand-expr (hashmap-union lexenv globalenv) stx)))))))

(define (expand initenv stx)
  ;; Expand `stx`, which is a list of syntax forms, into a list of syntax
  ;; forms, with initial environment `initenv`. Returns the new environment
  ;; and the list of expanded forms.
  (define (fold globalenv stxlist acc)
    (if (null? stxlist)
        (values globalenv (reverse acc))
        (let-values (((globalenv next)
                      (expand-toplevel globalenv (empty-map) (car stxlist))))
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
