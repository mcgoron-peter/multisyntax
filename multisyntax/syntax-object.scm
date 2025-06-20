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
 | Hygenic syntax transformer based on Dybvig, Hieb, and Bruggeman 1992.
 | "Syntax objects" are Scheme data that contain a set of "marks" and
 | "substitutions," also called "timestamps" and a "lexical environment."
 |
 | Marks are used to color data that is returned by syntax transformers
 | that was not part of the input of the transformer. Substitutions are
 | used to lazy convert bound identifiers into new bound identifiers.
 |
 | This library implements Chapter 3 of the Macrological Fascicle.
 | Whats missing?
 |
 | * A full implementation of `identifier-defined?`, `define-property`, and
 |   `identifier-properties`, because global scope is not stored inside of
 |   the syntax object address store.
 |   For hosted systems, use a hashmap with `bound-identifier-comparator`.
 |   A hashmap has better garbage collection properties.
 | * Macros like `quote-syntax`, because they would be difficult to write
 |   purely in `syntax-rules`.
 |#

;;; ;;;;;;;;;;
;;; Timestamps
;;; ;;;;;;;;;;

(define generate-timestamp
  ;; A timestamp is an integer.
  generate-unique-integer)

(define timestamp-comparator exact-integer-comparator)

;;; ;;;;;;;;;;;;;;;
;;; Locations and substitutions

(define-record-type <lexical-location>
  (raw-lexical-location symbol value)
  lexical-location?
  (symbol lexical-location->symbol)
  (value lexical-location->unique-id))

(define lexical-location-comparator
  (make-comparator
   lexical-location?
   (lambda (x y)
     (= (lexical-location->unique-id x)
        (lexical-location->unique-id y)))
   (lambda (x y)
     (< (lexical-location->unique-id x)
        (lexical-location->unique-id y)))
   (lambda (x) (number-hash (lexical-location->unique-id x)))))

(define (generate-lexical-location symbol)
  (raw-lexical-location symbol
                        (generate-unique-integer)))

(define (lexical-location->string ll)
  (string-append (symbol->string (lexical-location->symbol ll))
                 "."
                 (number->string (lexical-location->unique-id ll))))

(define environment-key-comparator
  ;; Comparator for keys to the environment that stores substitutions.
  ;; 
  ;; Keys are either regular Scheme symbols or unique lexical locations.
  (make-sum-comparator lexical-location-comparator
                       symbol-comparator))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Syntax objects
;;; 
;;; Some properties of the DHB expander, and how they relate to the
;;; `<wrap>` object:
;;; 
;;; 1. Marks and substitutions are constructors of the identifier set.
;;;    This means that marks made after a substitution do not affect the
;;;    substitution. In the absence of mutation, a substitution will
;;;    always fail if the marks of the object that it wraps do not match
;;;    the marks of the identifier it matches against.
;;; 2. The `resolve` function will do intermediate substitutions. This is
;;;    the reason that the `<wrap>` object contains an
;;;    `inverse-environment` object. If a substitution maps an identifier
;;;    produced by a substitution into another identifier, then that
;;;    mapping is replaced.
;;; 
;;; This implementation uses flat sets and flat maps, as opposed to a
;;; direct implementation of the algorithm, which would make both into a
;;; list.
;;; 
;;; The advantage of flat sets and flat maps is the asymtopic complexity
;;; of various set/map implementations (HAMT, AVL tree, etc.) However,
;;; the implementation must preserve the invariant that no wrapped syntax
;;; object contains another wrapped syntax object. This means that the
;;; *introduced* portion of a macro transformer must be scanned twice:
;;; once to add the timestamp of the end of the macro step, and once again
;;; to do the actual macro expansion. This still results in linear
;;; time complexity, but with a larger constant.
;;; 
;;; IDEA: Currently the entire algorithm is persistent. One could give up
;;; persistence and do eager wrap propagation using mutation. This means that
;;; the input no longer has to be scanned. This would make the additional
;;; growth linear in the size of introduced identifiers that have not been
;;; reached yet. This would help in cases where introduced identifiers are
;;; very far down the tree.
;;; 
;;; The direct-implementation has the property that no eager wrap
;;; propagation must be done. However, it requires linear behavior for the
;;; set of marks and substitutions. This would make the expander very slow
;;; if it used to implement a module system (although Scheme gets away
;;; with having such a small amount of things to import in the first
;;; place).

(define-record-type <wrap>
  ;; A `<wrap>` contains
  ;; 
  ;; * `expr`, which is Scheme data,
  ;; * `timestamps`, a set of timestamps
  ;; * `environment`, a map from locations/symbols to locations,
  ;; * `inverse-environment`, a map from locations to a list of
  ;;   locations/symbols
  (raw-wrap expr timestamps environment inverse-environment)
  wrap?
  (expr wrap->expr)
  (timestamps wrap->timestamps)
  (environment wrap->environment)
  (inverse-environment wrap->inverse-environment))

(define empty-wrap
  ;; Wrap `expr` with an empty timestamp set and environment.
  (let ((empty-timestamp-set (set timestamp-comparator))
        (empty-mapping (mapping environment-key-comparator)))
    (lambda (expr)
      (raw-wrap expr
                empty-timestamp-set
                empty-mapping
                empty-mapping))))

(define (self-syntax? obj)
  ;; A self-syntax object cannot contain an identifier.
  (or (null? obj)
      (char? obj)
      (string? obj)
      (bytevector? obj)
      (boolean? obj)
      (number? obj)))

(define (syntax? obj)
  (or (wrap? obj)
      (and (pair? obj)
           (syntax? (car obj))
           (syntax? (cdr obj)))
      (and (vector? obj)
           (vector-every syntax? obj))
      (self-syntax? obj)))

(define (identifier? obj)
  ;; An identifier is a wrapped symbol.
  (and (wrap? obj) (symbol? (wrap->expr obj))))

(define (add-timestamp stx ts)
  ;; Adds a timestamp to the syntax object `stx`. If the timestamp is
  ;; already in the wrap, the timestamp is removed instead.
  (cond
    ((pair? stx) (cons (add-timestamp (car stx) ts)
                       (add-timestamp (cdr stx) ts)))
    ((vector? stx) (vector-map (cut add-timestamp <> ts) stx))
    ((self-syntax? stx) stx)
    ((wrap? stx)
     (let* ((timestamps (wrap->timestamps stx))
            (timestamps (if (set-contains? timestamps ts)
                            (set-delete timestamps ts)
                            (set-adjoin timestamps ts))))
       (raw-wrap (wrap->expr stx)
                 timestamps
                 (wrap->environment stx)
                 (wrap->inverse-environment stx))))))

(define (resolve id)
  ;; Get the location that `id` ultimately resolves to.
  (let ((sym (wrap->expr id))
        (environment (wrap->environment id)))
    (mapping-ref/default environment sym sym)))

(define (add-timestamps/same-wrap stx id location-to)
  ;; TODO: Make more functional?
  (let ((location-from (resolve id))
        (env (wrap->environment stx))
        (invenv (wrap->inverse-environment stx)))
    ;; If there are mappings in the environment to `location-from`,
    ;; then those must be updated to `location-to`. The inverse
    ;; environment stores these mappings.
    ;; 
    ;; The inverse environment has no need to store the
    ;; intermediate substitutions after this. The new mapping
    ;; inherits the old intermediate substitution's mappings.
    (cond
      ((mapping-ref/default invenv location-from '())
       => (lambda (lst)
            (for-each (lambda (maps-to-location-from)
                        (display "a\n")
                        (set! env (mapping-set
                                   env
                                   maps-to-location-from
                                   location-to)))
                      lst)
            (set! invenv (mapping-delete invenv location-from))
            (set! invenv (mapping-set invenv location-to lst)))))
    (raw-wrap (wrap->expr stx)
              (wrap->timestamps stx)
              (mapping-set env location-from location-to)
              (mapping-update/default invenv
                                      location-to
                                      (cut cons location-from <>)
                                      '()))))

(define (add-substitution stx id location-to)
  ;; Add to the lexical environment of `stx` a mapping from the location
  ;; that `id` resolves to to the location `location-to`.
  ;; 
  ;; If the identifier does not satisfy the marks of the expression, then
  ;; it is never added.
  ;; 
  ;; Otherwise update `environment` and `inverse-environment` with the
  ;; new locations.
  (cond
    ((pair? stx) (cons (add-substitution (car stx)
                                         id
                                         location-to)
                       (add-substitution (cdr stx)
                                         id
                                         location-to)))
    ((vector? stx) (vector-map (cut add-substitution <> id location-to)
                               stx))
    ((self-syntax? stx) stx)
    (else
     (let ((timestamps (wrap->timestamps stx)))
       (if (not (set=? timestamps (wrap->timestamps id)))
           stx
           (add-timestamps/same-wrap stx id location-to))))))

(define (generate-unique-symbol)
  ;; Tries as best as possible to generate a unique symbol. Not read/write
  ;; invariant. An actual implementation of this procedure would require
  ;; implementation support.
  (string->symbol
   (string-append "gensym."
                  (number->string
                   (generate-unique-integer)))))

(define (identifier-lexically-bound? id)
  ;; Returns true if `id` was bound by some lexical construct. Returns
  ;; false for globally bound identifiers.
  (lexical-location? (resolve id)))

(define generate-identifier
  ;; Generate an identifier that is never `bound-identifier=?` to any
  ;; previous identifier.
  (case-lambda
    (() (generate-identifier (generate-unique-symbol)))
    ((symbol)
     (when (not (symbol? symbol))
       (error "generate-symbol requires symbol" symbol))
     (raw-wrap symbol
               (set timestamp-comparator (generate-unique-integer))
               (mapping environment-key-comparator)
               (mapping environment-key-comparator)))))

(define (generate-temporaries lst)
  ;; Generate a list of identifiers from `generate-identifier`.
  (let loop ((lst (unwrap-syntax lst))
             (acc '()))
    (if (null? lst)
        '()
        (loop (unwrap-syntax (cdr list)) (cons (generate-identifier) acc)))))

(define (symbolic-identifier=? id1 id2)
  ;; Returns true if the underlying symbol of each identifier is the same.
  (symbol=? (syntax->datum id1) (syntax->datum id2)))

(define (free-identifier=? id1 id2)
  ;; Returns true if, when inserted into output as free identifiers, `id1`
  ;; and `id2` would refer to the same location.
  (=? environment-key-comparator (resolve id1) (resolve id2)))

(define (bound-identifier=? id1 id2)
  ;; Returns true if binding one identifier would cause the other
  ;; identifier to be bound.
  (and (free-identifier=? id1 id2)
       (set=? (wrap->timestamps id1) (wrap->timestamps id2))))

(define bound-identifier-comparator
  (let ((bound-identifier<?
         (lambda (id1 id2)
           (comparator-if<=> environment-key-comparator
                             (resolve id1)
                             (resolve id2)
             #t
             (<? set-comparator
                 (wrap->timestamps id1)
                 (wrap->timestamps id2))
             #f)))
        (bound-identifier-hash
         (lambda (id)
           (+ (comparator-hash set-comparator
                               (wrap->timestamps id))
              (comparator-hash environment-key-comparator
                               (resolve id))))))
    (make-comparator
     identifier?
     bound-identifier=?
     bound-identifier<?
     bound-identifier-hash)))

(define (push-wrap stx expr)
  ;; Give `expr` the wrap of `stx`. This does not check that `stx` does
  ;; not contain wrapped syntax objects.
  (raw-wrap expr
            (wrap->timestamps stx)
            (wrap->environment stx)
            (wrap->inverse-environment stx)))

(define (unwrap-syntax stx)
  ;; If `stx` is a wrapped pair or vector, return a pair/vector of syntax
  ;; objects with the same wrap. Otherwise return the syntax object unchanged.
  (if (wrap? stx)
      (let ((expr (wrap->expr stx)))
        (cond
          ((pair? expr) (cons (push-wrap stx (car expr))
                              (push-wrap stx (cdr expr))))
          ((vector? expr) (vector-map (cut push-wrap stx <>) expr))
          (else stx)))
      stx))

(define (syntax->datum stx)
  ;; Remove wraps from the syntax object.
  (cond
    ((pair? stx) (cons (syntax->datum (car stx))
                       (syntax->datum (cdr stx))))
    ((vector? stx) (vector-map syntax->datum stx))
    ((wrap? stx) (syntax->datum (wrap->expr stx)))
    (else stx)))

(define (if-contains-wrap operate obj)
  ;; If `obj` does not contain a wrapped syntax object, return `#f`.
  ;; 
  ;; Otherwise, return a wrapped syntax object. This is an object that
  ;; 
  ;; 1. All wrapped syntax objects have `operate` called on them, and
  ;; 2. The maximal subsections of `obj` that do not contain wrapped syntax
  ;;    objects have `operate` called on them.
  ;; 
  ;; `operate` is either passed a wrap or an object which does not have
  ;; wrapped syntax objects inside of it.
  ;; 
  ;; A maximal subsection is a part of `obj` that is contained in another
  ;; object which has a wrapped syntax object inside of it.
  ;; 
  ;; This procedure is used to deal with unwrapped syntax objects without
  ;; violating the invariant that no wrapped syntax object can contain
  ;; another wrapped syntax object.
  (cond
    ((or (self-syntax? obj) (symbol? obj)) #f)
    ((wrap? obj) (operate (wrap->expr obj)))
    ((pair? obj)
     (let ((first (if-contains-wrap operate obj))
           (second (if-contains-wrap operate obj)))
       (cond
         ((and (not first) (not second)) #f)
         ((not first) (cons (operate first) second))
         ((not second) (cons first (operate second)))
         (else (error "internal error" obj)))))
    ((vector? obj)
     (letrec ((loop
               (lambda (i)
                 (if (= i (vector-length obj))
                     #f
                     (let ((value (if-contains-wrap operate
                                                    (vector-ref obj i))))
                       (if (not value)
                           (loop (+ i 1))
                           (create-new-vector))))))
              (create-new-vector
               (lambda ()
                 (let ((returned-vector (make-vector (vector-length obj))))
                   (do ((i 0 (+ i 1)))
                       ((= (vector-length returned-vector) i)
                        returned-vector)
                     (let* ((datum (vector-ref obj i))
                            (value (if-contains-wrap operate datum)))
                       (if (not value)
                           (vector-set! returned-vector (operate datum))
                           (vector-set! returned-vector value))))))))
       (loop 0)))
    (else (error "invalid nested obj" obj))))

(define (datum->syntax context-id datum)
  ;; Create `datum` as a syntax object with the same wrap as `context-id`.
  (define (operate obj)
    (if (wrap? obj)
        (push-wrap context-id (wrap->expr obj))
        (push-wrap context-id obj)))
  (cond
    ((self-syntax? datum) datum)
    ((if-contains-wrap operate datum) => values)
    (else (push-wrap context-id datum))))
