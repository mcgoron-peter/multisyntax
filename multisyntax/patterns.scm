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
 | Syntax pattern matcher for syntax-rules and syntax-case.
 | The matcher is a backtracking recursive tracker.
 |
 | The "compiler" turns the pattern into lambdas.
 |
 | The map will return either `#f` (failure) or a map from identifiers to
 | either
 | 
 | 1. Syntax objects, or
 | 2. Reversed lists containined repeated matched values (for ellipsis
 |    patterns).
 |
 | The pattern matcher compiler outputs the nesting level of each
 | identifier, which allows a user to differentiate between lists and
 | ellipsis pattern values.
 |
 | The matcher is implemented as a recursive procedure that accumulates
 | the result map as it continues. When it encounters an ellipsis pattern,
 | the pattern P that the ellipsis pattern repeats is called with an empty
 | map. If the pattern P returns, the returned identifiers inside of that
 | pattern are inserted into the result map as elements of a listm and then
 | P is called again. This way, multiple values from an ellipsis can be
 | collected.
 |#

#;(define (display-hashmap hashmap)
  (display
   (list
    "hashmap:"
    (map (lambda (pair)
           (cons (syntax->datum (car pair))
                 (cdr pair)))
         (hashmap->alist hashmap)))))

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

(define actual-ellipsis-container
  ;; Parameter object that contains the ellipsis identifier.
  (make-parameter #f))

(define literals-parameter
  ;; Parameter object that contains the literals.
  (make-parameter #f))

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

(define (call/nesting-level procedure . args)
  ;; Invoke (procedure args ...) with a higher nesting level and an empty
  ;; `bound-here` set.
  ;; 
  ;; Returns (values return map):
  ;; 
  ;; 1. `return` is the returned value from `(procedure args ...)`
  ;; 2. `map` is the map of identifiers at this nesting level to their
  ;;    default values (the empty list).
  (let ((old-bound-here-box (bound-here)))
    (parameterize ((nesting-level (+ (nesting-level) 1))
                   (bound-here (box (empty-map))))
      (let ((returned (apply procedure args)))
        (values returned (unbox (bound-here)))))))

(define (actual-ellipsis? identifier)
  ;; Returns `#t` if `id` is an ellipsis, and `#f` otherwise.
  (cond
    ((actual-ellipsis-container) => (cut <> identifier))
    (else #f)))

(define (add-name! identifier)
  ;; Add `identifier` to the name map with the current ellipsis nesting
  ;; level. If the identifier is added inside of an ellipses nesting level,
  ;; then it is also added to the `bound-here` map.
  (let* ((the-box (bindings))
         (old (unbox the-box)))
    (when (hashmap-contains? old identifier)
      (error "identifier bound twice" identifier))
    (when (bound-here)
      (let ((the-set (unbox (bound-here))))
        (set-box! (bound-here) (hashmap-set! the-set
                                             identifier
                                             '()))))
    (set-box! the-box (hashmap-set! old
                                    identifier
                                    (nesting-level)))))

(define (compile-pattern ellipsis literals stx)
  ;; Compile `stx` into a pattern matcher with `ellipsis` as the ellipsis
  ;; identifier and the set of `literals`.
  ;; 
  ;; Returns two values, the binding map and the matcher, which is a
  ;; procedure `binding-map * syntax -> (or binding-map #f)`.
  (parameterize ((nesting-level 0)
                 (actual-ellipsis-container
                  (if (set-contains? literals ellipsis)
                      #f
                      (lambda (stx)
                        (and (identifier? stx)
                             (bound-identifier=? stx ellipsis)))))
                 (literals-parameter literals)
                 (bindings (box (empty-map))))
    (let ((match (compile stx)))
      (values match (unbox (bindings))))))

;;; ;;;;;;;;;;;;;;;;;;;;
;;; Helper functions
;;; ;;;;;;;;;;;;;;;;;;;;

(define (empty-map)
  (hashmap bound-identifier-comparator))

(define (literal? identifier)
  (set-contains? (literals-parameter) identifier))

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
         (and (bound-identifier=? stx pattern)
              names)))
      ((actual-ellipsis? pattern)
       (error "invalid ellipsis location" pattern))
      ((bound-identifier=? pattern (empty-wrap '_))
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

(define (is-ellipsis-list patcdr)
  ;; Returns (values has-ellipsis? next). `has-ellipsis?` is true if the
  ;; pair is an ellipsis pattern, and false otherwise. `next` is the next
  ;; pattern that will be matched.
  (if (null? patcdr)
      (values #f patcdr)
      (let ((patcadr (unwrap-syntax (car patcdr))))
        (if (actual-ellipsis? patcadr)
            (values #t (cdr patcdr))
            (values #f patcdr)))))

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

(define (compile-vector vec)
  ;; Vector patterns are traversed in reverse order, which means that
  ;; 
  ;; 1. The compiler is tail-recursive (not that it will matter much).
  ;; 2. Ellipses detection requires no lookahead.
  ;; 
  ;; The internal vector procedures take an extra argument, `i`, which
  ;; is the current index into the matched vector.
  (define entry
    (let compile-index ((i 0)
                        (k match-end-of-vector))
      (cond
        ((zero? i) k)
        (else
         (let ((cur (vector-ref vec i)))
           (if (actual-ellipsis? cur)
               (compile-index (- i 2)
                              (compile-vector-ellipsis vec (- i 1) k))
               (compile-index (- i 1)
                              (compile-pattern-in-vector cur k))))))))
  (lambda (names stx)
    (entry names stx 0)))

(define (match-end-of-vector names vec i)
  ;; Compiled procedure to match the end of a vector. This is constant for
  ;; any vector.
  (if (= i (vector-length vec))
      names
      #f))

(define (compile-vector-ellipsis vec i match-rest)
  ;; Compile the pattern in `vec` at `i` as an ellipsis pattern.
  (when (= i 0)
    (error "... is not allowed at the start of a vector" vec))
  (let ((cur (vector-ref vec i)))
    (when (actual-ellipsis? cur)
      (error "... ... is not allowed" cur))
    (let-values (((match default-names) (call/nesting-level compile cur)))
      (lambda (names vec i)
        (let match* ((names (hashmap-union names default-names))
                     (i i))
          (cond
            ((= i (vector-length vec)) (match-rest names vec i))
            ((match (empty-map) (vector-ref vec i))
             => (lambda (new-names)
                  (cond
                    ((match* (merge-names names new-names)
                             (+ i 1))
                     => values)
                    (else (match-rest names vec i)))))
            (else (match-rest names vec i))))))))

(define (compile-pattern-in-vector pattern match-rest)
  ;; Compile `pattern` to be matched in a vector.
  (define match (compile pattern))
  (lambda (names vec i)
    (let ((stx (vector-ref vec i)))
      (cond
        ((match names stx) => (cute match-rest <> vec (+ i 1)))
        (else #f)))))

