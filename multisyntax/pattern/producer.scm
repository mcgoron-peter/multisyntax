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
 | Pattern "producer" for syntax-rules and syntax-case.
 | Takes the output of a pattern match and the ellipses nesting data for
 | that pattern match and return a syntax object with the output of the
 | pattern match.
 |
 | This does not timestamp, because that is the expander's job.
 |------------------------------------------------------------------------
 | The following uses terms defined in "matcher.scm".
 |
 | The "real nesting level" of an identifier (`RNL(I)`) is the nesting level
 | from the pattern matcher.
 |
 | The "producer nesting level" of a pattern `P` (`PNL(P)`) relative to an
 | input pattern `P'`
 | is defined similarly to the ellipsis nesting level, except that it is
 | defined on a producer pattern, and each occurence of an identifier may
 | have a different producer nesting level. When the input pattern is not
 | specified, it is the entire input pattern.
 |
 | Let `P` be a pattern. Then a subpattern `P'` is closed relative to `P`
 | if
 |
 | * `P'` is an identifier and `PNL(P') >= RNL(P')` (relative to `P`), or
 | * `P'` is literal, or
 | * `P'` is a list, vector, or ellipsis pattern where all elements of `P'`
 |   are closed relative to `P'`.
 |
 | The output length of a subpattern `P'` of a pattern `P` is the length
 | of the `PNL(P')` (relative to `P`) nesting level of the matched form.
 |
 | A pattern producer `P` is well formed if
 |
 | 1. Each subpattern of `P` is closed relative to `P`, and
 | 2. For each ellipsis pattern `A ...` of `P`,
 |    1. At least one pattern in `A` is open relative to `A`.
 |    2. Each pattern `P'` open relative to `A` with `PNL(P') = 0` relative
 |       to `A` has the same output length (relative to `A`).
 |------------------------------------------------------------------------
 | A stricter pattern producer would only allow open identifiers inside of
 | an ellipsis pattern that are a part of the same ellipsis class (i.e.
 | they were contained in the same outermost ellipsis in the matcher).
 | This is relaxed to allow for `generate-temporaries` forms bound
 | with `with-syntax` to be useful. See the example in the R6RS Standard
 | Libraries, §12.7.
 |--------------------------------------------------------------------
 | Here's a smoke test for your syntax-rules pattern matcher:

      (define-syntax test
        (syntax-rules ()
          ((_ (x ...))
          (quote ((x (x ...)) ...)))))
      (test (1 2))

 | This outputs on Chez

     ((1 (1 2)) (2 (1 2)))

 | What does this do?

       (define-syntax test2
         (syntax-rules ()
           ((_ (x ...) ...)
            (quote (((x (x ...)) ...) ...)))))

       (test2 (1 2) (3 4))

 | On Chez it outputs

     (((1 (1 2)) (2 (3 4))) ((3 (1 2)) (4 (3 4))))

 | Surprising! To make sense of it, consider the rewrite

       (define-syntax test3
         (lambda (x)
           (syntax-case x ()
             ((_ (x ...) ...)
              (with-syntax ((((y ...) ...) #'((x ...) ...)))
                #'(quote (((x (y ...)) ...) ...)))))))

 | The outermost `...` is more ellipses than `y` was introduced with, so
 | it will be repeated. The `...` after that will select one of the
 | nestings of `y`, and the second `...` next to `y` will select one value
 | in one nesting of `y`.
 |
 | The easiest way to make sense of such patterns is to rewrite the
 | patterns so that each identifier in the production is used at most once.
 | This is done automatically.
 |#

(define (rewrite/temporaries pattern bindings)
  ;; Rewrite `pattern` such that all pattern-bound identifiers occur in
  ;; `pattern` at most once.
  (define appearances (set bound-identifier-comparator))
  (define (add-appearance! id)
    (set! appearances (set-adjoin appearances id)))
  (define (appears? id)
    (set-contains? appearances id))
  (define (add-temporary! id)
    (let ((new (generate-identifier (syntax->datum id))))
      (set! bindings (hashmap-adjoin bindings
                                     new
                                     (hashmap-ref bindings id)))
      new))
  (define (rewrite pattern)
    (let ((pattern (unwrap-syntax pattern)))
      (cond
        ((self-syntax? pattern) pattern)
        ((pair? pattern) (cons (rewrite (car pattern))
                               (rewrite (cdr pattern))))
        ((vector? pattern) (vector-map rewrite pattern))
        ((literal? pattern) pattern)
        ((actual-ellipsis? pattern) pattern)
        ((and (identifier? pattern) (appears? pattern))
         (add-temporary! pattern))
        ((identifier? pattern)
         (add-appearance! pattern)
         pattern)
        (else pattern))))
  (let ((pattern (rewrite pattern)))
    (values pattern bindings)))

(define bindings
  ;; Mapping bound identifiers to their real nesting level.
  (make-parameter #f))

(define all-bindings (make-parameter #f))

(define compile-producer
  ;; Enty point into the producer compiler.
  ;; 
  ;; This will rewrite the producer so that each bound identifier is used
  ;; at most once.
  (case-lambda
    ((literals pattern %bindings)
     (compile-producer literals pattern %bindings #f))
    ((literals pattern %bindings ellipsis)
     (parameterize ((matcher-input (vector ellipsis literals)))
       (let-values (((pattern %bindings)
                     (rewrite/temporaries pattern %bindings)))
         (parameterize ((bindings %bindings))
           (let-values (((producer open-bindings)
                         (compile pattern)))
             (if (not (hashmap-empty? open-bindings))
                 (error "pattern not closed" pattern)
                 (lambda (bindings)
                   (parameterize ((all-bindings bindings))
                     (producer bindings)))))))))))

(define (compile pattern)
  ;; Returns a procedure that will produce `pattern` given the bindings.
  (let ((pattern (unwrap-syntax pattern)))
    (cond
      ((self-syntax? pattern)
       (values (lambda (bindings) pattern)
               (empty-map)))
      ((pair? pattern) (compile-pair (unwrap-syntax (car pattern))
                                     (unwrap-syntax (cdr pattern))))
      ;; TODO: Vectors
      #;((vector? pattern) (compile-vector pattern))
      ((actual-ellipsis? pattern)
       (error "ellipsis in location where it is not allowed" pattern))
      ((literal? pattern)
       (values (lambda (bindings) pattern)
               (empty-map)))
      ((and (identifier? pattern)
            (hashmap-contains? (bindings) pattern))
       ;; Return 0 as the PNL of this identifier, because all identifiers
       ;; have a PNL of 0 to themselves.
       (values (lambda (bindings) (hashmap-ref bindings pattern))
               (hashmap bound-identifier-comparator pattern 0)))
      ((identifier? pattern)
       (values (lambda (bindings) pattern) (empty-map)))
      (else (error "not syntax" pattern)))))

;;; ;;;;;;;;;
;;; Lists
;;; ;;;;;;;;;

(define (list-of-ellipses patcdr)
  ;; Returns `(values i pat)`, where `i` is the number of found ellipses
  ;; and `pat` is the first non-ellipsis.
  (let loop ((i 0)
             (patcdr patcdr))
    (cond
      ((null? patcdr) (values i patcdr))
      ((not (pair? patcdr)) (values i patcdr))
      (else
       (let ((patcar (unwrap-syntax (car patcdr))))
         (if (actual-ellipsis? patcar)
             (loop (+ i 1) (unwrap-syntax (cdr patcdr)))
             (values i patcdr)))))))

(define (compile-pair patcar patcdr)
  (let*-values (((number-of-ellipses next) (list-of-ellipses patcdr))
                ((produce-next open-identifiers-next) (compile next)))
    (if (zero? number-of-ellipses)
        (let-values (((produce-car open-identifiers) (compile next)))
          (values
           (lambda (bindings)
             (cons (produce-car bindings) (produce-next bindings)))
           (hashmap-union open-identifiers open-identifiers-next)))
        (let-values (((produce-in-ellipsis open-identifiers-of)
                      (produce-ellipsis-list number-of-ellipses patcar)))
          (if (hashmap-empty? open-identifiers-of)
              (values
               ;; If the ellipses binding is completely closed, then pass
               ;; the pattern producer all the bindings (which is stored as
               ;; a parameter object).
               (lambda (bindings)
                 (append (produce-in-ellipsis (all-bindings))
                         (produce-next bindings)))
               open-identifiers-next)
              (values
               (lambda (bindings)
                 (append (produce-in-ellipsis bindings)
                         (produce-next bindings)))
               (hashmap-union open-identifiers-of
                              open-identifiers-next)))))))

;;; The following prodcures are related to "iterator" maps, which are the
;;; maps used to output ellipses productions.

(define (length+reverse list)
  (let loop ((i 0)
             (list list)
             (acc '()))
    (if (null? list)
        (values i acc)
        (loop (+ i 1) (cdr list) (cons (car list) acc)))))

(define (open-bindings open-identifiers bindings)
  ;; Return a map of open identifiers to a correctly ordered list of their
  ;; captured values. Will raise an error if there is their lengths are not
  ;; the same.
  (define length-of-each #f)
  (hashmap-map
   (lambda (open-identifier _)
     (let*-values (((rev-bound) (hashmap-ref bindings open-identifier))
                   ((length bound) (length+reverse rev-bound)))
       (cond
         ((not length-of-each)
          (set! length-of-each length)
          (values open-identifier bound))
         ((not (= length-of-each length))
          (error "length mismatch for open identifiers"
                 bindings
                 open-identifiers
                 bound))
         (else (values open-identifier bound)))))
   bound-identifier-comparator
   open-identifiers))

(define (next-binding open-bindings)
  ;; Returns a map of open identifiers that points to the next bound values
  ;; to output.
  (let ((returned (hashmap-map (lambda (identifier list)
                                 (values identifier (cdr list)))
                               bound-identifier-comparator
                               open-bindings)))
    returned))

(define (union/current-bindings bindings open-identifiers-map)
  ;; Return a map of bindings, where the open identifiers are assigned the
  ;; current bound value in iteration.
  (hashmap-union
   (hashmap-map (lambda (open-identifier list)
                  (values open-identifier (car list)))
                bound-identifier-comparator
                open-identifiers-map)
   bindings))

(define (bindings-finished? bindings)
  ;; Returns true if there are no more bound values to iterate over.
  (or (hashmap-empty? bindings)
      (hashmap-any? (lambda (_ values) (null? values)) bindings)))

(define (produce-ellipsis-list number-of-ellipses patcar)
  ;; Prepare a procedure that matches an ellipsis pattern in a list.
  (define (will-be-closed? identifier PNL)
    ;; Returns true if the appearance of this is closed appearing in this
    ;; ellipsis.
    (let ((RNL (hashmap-ref (bindings) identifier)))
      (>= PNL RNL)))
  (let*-values (((produce-part open-identifiers) (compile patcar))
                ((open-identifiers)
                 ;; Give the open identifiers their PNLs according to this
                 ;; pattern.
                 (hashmap-map (lambda (id old-PNL)
                                (values id (+ old-PNL number-of-ellipses)))
                              bound-identifier-comparator
                              open-identifiers))
                ((open-identifiers-to-return)
                 ;; Remove identifiers which that will become closed after
                 ;; exiting the ellipses.
                 (hashmap-remove will-be-closed? open-identifiers)))
    (when (hashmap-empty? open-identifiers)
      (error "ellipsis production does not have open identifiers" patcar))
    ;; TODO: Need to handle the case of sequential ellipses. They are
    ;; equivalent to
    ;;     x ... ... => {append ((x ...) ...)}
    ;;     x ... ... ... => {append {append (((x ...) ...) ...)}}
    ;; and so on where `append` is meta-level.
    (letrec ((iterate
              (lambda (bindings acc level)
                (if (= level number-of-ellipses)
                    (acc (produce-part bindings))
                    (do ((iterated (open-bindings open-identifiers bindings)
                                   (next-binding iterated)))
                        ((bindings-finished? iterated))
                      (let ((subbindings (union/current-bindings bindings
                                                                 iterated)))
                        (iterate subbindings
                                 acc
                                 (+ level 1))))))))
      (values (lambda (bindings)
                (let ((patterns (list-accumulator)))
                  (iterate bindings patterns 0)
                  (patterns (eof-object))))
              open-identifiers-to-return))))
