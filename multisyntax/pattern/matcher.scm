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
 | Backtracking recursive syntax pattern matcher for syntax-rules and
 | syntax-case.
 |
 | The "compiler" returns:
 |
 | 1. a procedure (the matcher) that will ake an input pattern and return
 |    either `#f` or a hashmap from identifiers to "matched values".
 | 2. a hashmap from identifiers to `(cons x y)`, where
 |    `x` is the "ellipsis nesting level" and `y` is the "ellipsis group".
 |    The hashmap checks identifier according to `bound-identifier=?`.
 | 3. A hashmap from ellipsis groups to ellipsis groups. These are the
 |    ellipsis groups that occur inside of that ellipsis group.
 |
 | The matcher is implemented as a recursive procedure that accumulates
 | the result map as it continues. When it encounters a template of the
 | form `template ...` in a vector or a list, `template` is matched with
 | an empty identifier hashmap. If `template` successfully matches,
 | the returned identifiers inside of that pattern are inserted into the
 | result map as elements of a list and then P is called again. This way,
 | multiple values from an ellipsis can be collected.
 |
 |------------------------------------------------------------------------
 | A "matched value" is defined as:
 | 
 | 1. A syntax object, or
 | 2. Reversed lists containing matched values from a repeated match.
 |    This occurs when an identifier has a non-zero ellipsis nesting level.
 |
 | A syntax object can also match a list. To disambiguate this, refer to
 | the ellipsis nesting level of an identifier.
 |
 |------------------------------------------------------------------------
 | The "ellipsis nesting level" of a syntax object is defined as:
 |
 | * The ellipsis nesting level of a syntax form passed to `compile-pattern`
 |   is 0.
 | * Whenever `template ...` is found in a list or a vector with
 |   ellipsis nesting level N, then `template` has nesting level `N + 1`.
 |
 |  For example,
 |
 | 1. The ellipsis nesting level of `(x y z)` is 0.
 | 2. The ellipsis nesting level of `(x ...)` is 0. Inside the form, the
 |    nesting level of `x` is 1.
 | 3. In `(let-values (((names ...) value) ...) body ...)`, the nesting
 |    level of `names` is 2, of `value` 1, and of `body` 1.
 |
 |------------------------------------------------------------------------
 | The "ellipsis group" is an integer with the property that whenever
 | `template ...` is found in a pattern, all identifiers in `template` at
 | the same nesting level have the same ellipsis group, and no other
 | identifiers have the same ellipsis group.
 |
 |------------------------------------------------------------------------
 | For an identifier with nesting level N, the result map at the end of
 | matching will have N levels of lists. For example:
 |
 |     (let-values (((name1 name2) value1) ((name3 name4) value2)) body)
 |
 | Will map
 |
 |     names -> ((name4 name3) (name2 name1))
 |     value -> (value2 value1)
 |     body -> (body)
 |#

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auxillary parameters
;;; 
;;; These use the unportable `empty-wrap` procedure to create auxillary
;;; syntax keywords (identifiers without timestamps and without an
;;; environment).

(define _ (empty-wrap '_))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parameter objects for the parser
;;; 
;;; The state of the pattern matcher is kept in parameter objects. This
;;; is to make the code simpler to read. The previous implementation was
;;; getting to four nested levels of procedures because it kept everything
;;; in lexical scope.

(define nesting-level
  ;; Current ellipsis nesting level.
  (make-parameter 0))

(define bindings
  ;; Parameter object that contains a box that contains a mapping from
  ;; identifiers to their nesting level.
  (make-parameter #f))

(define bound-here
  ;; Parameter object that contains a box that contains a map of
  ;; identifiers at the current nesting level to the empty list.
  ;; When this parameter is false, then the compiler is not in a ellipsis
  ;; nesting level.
  ;; 
  ;; The map is the default match bindings for those identifiers (i.e. zero
  ;; matches).
  (make-parameter #f))

(define ellipsis-group
  ;; Current ellipsis group.
  (make-parameter #f))

(define ellipsis-group-map
  ;; Map containing the ellipsis group tree.
  (make-parameter #f))

(define (set-parameter! parameter operation)
  ;; Set the box stored in `parameter` to the value returned by
  ;; `(operation value)`, where `value` is the value stored in the box.
  ;; 
  ;; This has no effect if `box` is falsy.
  (let ((box (parameter)))
    (when box
      (set-box! box (operation (unbox box))))))

(define (call/nesting-level procedure . args)
  ;; Invoke (procedure args ...) with a higher nesting level and an empty
  ;; `bound-here` map. Also creates a new ellipsis group.
  ;; 
  ;; Returns (values return map):
  ;; 
  ;; 1. `return` is the returned value from `(procedure args ...)`
  ;; 2. `map` is the map of identifiers at this nesting level to their
  ;;    default values (the empty list).
  (let ((old-bound-here-box (bound-here))
        (outer-ellipsis-group (ellipsis-group)))
    (parameterize ((nesting-level (+ (nesting-level) 1))
                   (bound-here (box (empty-map)))
                   (ellipsis-group (generate-unique-integer)))
      (when outer-ellipsis-group
        (set-parameter! ellipsis-group-map
                        (cute hashmap-update!/default
                              <>
                              outer-ellipsis-group
                              (cute cons (ellipsis-group) <>)
                              '())))
      (set-parameter! ellipsis-group-map
                      (cute hashmap-set! <> (ellipsis-group) '()))
      (let ((returned (apply procedure args)))
        (values returned (unbox (bound-here)))))))

(define (add-name! identifier)
  ;; Add `identifier` to the name map with the current ellipsis nesting
  ;; level. If the identifier is added inside of an ellipses nesting level,
  ;; then it is also added to the `bound-here` map.
  (set-parameter! bound-here
                  (lambda (bound-here)
                    (hashmap-set! bound-here
                                  identifier
                                  '())))
  (set-parameter!
   bindings
   (lambda (map)
     (when (hashmap-contains? map identifier)
       (error "identifier bound twice" identifier))
     (hashmap-set! map
                   identifier
                   (cons (nesting-level) (ellipsis-group))))))

(define compile-pattern
  ;; Compile `stx` into a pattern matcher with `ellipsis` as the ellipsis
  ;; identifier and the set of `literals`.
  ;; 
  ;; Returns two values, the binding map and the matcher, which is a
  ;; procedure `binding-map * syntax -> (or binding-map #f)`.
  (case-lambda
    ((literals stx) (compile-pattern literals stx #f))
    ((literals stx ellipsis)
     (parameterize ((nesting-level 0)
                    (matcher-input (vector ellipsis literals))
                    (bindings (box (empty-map)))
                    (ellipsis-group-map (box (hashmap exact-integer-comparator))))
       (let ((match (compile stx)))
         (values (lambda (stx) (match (empty-map) stx))
                 (unbox (bindings))
                 (unbox (ellipsis-group-map))))))))

;;; ;;;;;;;;;;;;;;;;;;;;
;;; Helper functions
;;; ;;;;;;;;;;;;;;;;;;;;

(define (empty-map)
  (hashmap bound-identifier-comparator))

(define (merge-names oldnames newnames)
  ;; newnames is the patterns matched in an ellipsis expression. Append
  ;; each to the lists in `oldnames` associated with each name and return
  ;; the merged list.
  (define (proc key val names)
    (hashmap-update/default names key (cut cons val <>) '()))
  (hashmap-fold proc oldnames newnames))

;;; ;;;;;;;;;;;;;;
;;; Compile functions
;;; ;;;;;;;;;;;;;;

(define (compile pattern)
  (let ((pattern (unwrap-syntax pattern)))
    (cond
      ((self-syntax? pattern)
       (lambda (names stx)
         (let ((stx (unwrap-syntax stx)))
           (and (self-syntax? stx)
                (equal? stx pattern)
                names))))
      ((pair? pattern)
       (compile-pair (unwrap-syntax (car pattern))
                     (unwrap-syntax (cdr pattern))))
      ((vector? pattern)
       (compile-vector pattern))
      ((not (identifier? pattern))
       (error "not syntax" pattern))
      ((literal? pattern)
       (lambda (names stx)
         (and (identifier? stx)
              (free-identifier=? stx pattern)
              names)))
      ((actual-ellipsis? pattern)
       (error "invalid ellipsis location" pattern))
      ((free-identifier=? pattern _)
       (lambda (names stx) names))
      (else
       (add-name! pattern)
       (lambda (names stx)
         (hashmap-set names pattern stx))))))

;;; ;;;;;;;;;;;;;;;
;;; Compile pairs
;;; ;;;;;;;;;;;;;;;

(define (compile-pair patcar patcdr)
  ;; Compile a general pair. A pair pattern can either be
  ;; 
  ;;     (x <ellipsis> . y)
  ;; or
  ;;     (x)
  ;; or
  ;;     (x <not-an-ellipsis> . y)
  (if (null? patcdr)
      (compile-actual-pair patcar '())
      (let-values (((has-ellipsis? pat-next)
                    (is-ellipsis-list patcdr)))
        (if has-ellipsis?
            (compile-ellipsis patcar pat-next)
            (compile-actual-pair patcar pat-next)))))

(define (compile-ellipsis patcar patcddr)
  ;; Compile an ellipsis pattern that matches `patcar` zero or more times
  ;; and then must match `patcddr`.
  (define-values (match-patcar default-names)
    (call/nesting-level compile patcar))
  (define match-patcddr (compile patcddr))
  (lambda (names stx)
    (let match* ((names (hashmap-union names default-names))
                 (stx (unwrap-syntax stx)))
      (cond
        ((null? stx) names)
        ((not (pair? stx)) #f)
        ((match-patcar (empty-map) (car stx))
         => (lambda (newnames)
              (cond
                ((match* (merge-names names newnames) (cdr stx))
                 => values)
                (else (match-patcddr names stx)))))
        (else (match-patcddr names stx))))))

(define (compile-actual-pair patcar patcdr)
  ;; Compile a pair that is not an ellipsis pattern. I.e. match `patcar`
  ;; then match `patcdr.
  (define match-patcar (compile patcar))
  (define match-patcdr (compile patcdr))
  (lambda (names stx)
    (let ((stx (unwrap-syntax stx)))
      (cond
        ((not (pair? stx)) #f)
        ((match-patcar names (car stx))
         => (cute match-patcdr <> (cdr stx)))
        (else #f)))))

;;; ;;;;;;;;;;;;;;;;;;;
;;; Compile vectors
;;; ;;;;;;;;;;;;;;;;;;;

(define (index-in-range? vec i)
  (and (>= i 0) (< i (vector-length vec))))

(define (compile-vector vec)
  ;; Vector patterns are traversed in reverse order, which means that
  ;; 
  ;; 1. The compiler is tail-recursive (not that it will matter much).
  ;; 2. Ellipses detection requires no lookahead.
  ;; 
  ;; The internal vector procedures take an extra argument, `i`, which
  ;; is the current index into the matched vector.
  (define entry
    (let compile-index ((i (- (vector-length vec) 1))
                        (k match-end-of-vector))
      (cond
        ((< i 0) k)
        (else
         (let ((cur (vector-ref vec i)))
           (if (actual-ellipsis? cur)
               (compile-index (- i 2)
                              (compile-vector-ellipsis vec (- i 1) k))
               (compile-index (- i 1)
                              (compile-pattern-in-vector cur k))))))))
  (lambda (names stx)
    (if (vector? stx)
        (entry names stx 0)
        #f)))

(define (match-end-of-vector names vec i)
  ;; Compiled procedure to match the end of a vector. This is constant for
  ;; any vector.
  (if (= i (vector-length vec))
      names
      #f))

(define (compile-vector-ellipsis vec i match-rest)
  (when (< i 0)
    (error "... is not allowed at the start of a vector" vec))
  (let ((cur (vector-ref vec i)))
    (when (actual-ellipsis? cur)
      (error "... ... is not allowed" cur))
    (let-values (((match default-names) (call/nesting-level compile cur)))
      (lambda (names vec i)
        (let match* ((names (hashmap-union names default-names))
                     (i i))
          (cond
            ((not (index-in-range? vec i)) (match-rest names vec i))
            ((match (empty-map) (vector-ref vec i))
             => (lambda (new-names)
                  (cond
                    ((match* (merge-names names new-names)
                             (+ i 1))
                     => values)
                    (else (match-rest names vec i)))))
            (else (match-rest names vec i))))))))

(define (compile-pattern-in-vector pattern match-rest)
  (define match (compile pattern))
  (lambda (names vec i)
    (cond
      ((not (index-in-range? vec i)) #f)
      ((match names (vector-ref vec i))
       => (cute match-rest <> vec (+ i 1)))
      (else #f))))

