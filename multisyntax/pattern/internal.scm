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
 |-----------------------------------------------
 | Procedures shared by matcher and producer.
 |#

;;; ;;;;;;;;
;;; Ellipsis and literals
;;; ;;;;;;;;

(define ... (empty-wrap '...))

(define (contains-as-free-identifier set key)
  ;; Returns an identifier if `key` is `free-identifier=?` to any
  ;; identifier in `set`. Otherwise return `#f`.
  (cond
    ((member key (set->list set) free-identifier=?) => car)
    (else #f)))

(define (generate-ellipsis-procedure ellipsis literals)
  ;; Generates a procedure of one argument that returns `#t` if the
  ;; argument is an ellipsis for the purposes of the current matcher.
  ;; 
  ;; If `ellipsis` is `#f`, then the `ellipsis` is the auxillary global
  ;; `...`, and matching is done with `free-identifier=?`.
  ;; 
  ;; If `ellipsis` is some identifier, then matching is done against it
  ;; with `bound-identifier=?`.
  ;; 
  ;; If either ellipsis is in the literals, then there is no repeating
  ;; patterns and the returned procedure returns `#f`.
  ;; 
  ;; This procedure is only runs inside of a parameterization, so
  ;; `literals` must be passed explicitly and `literal?` cannot be used.
  (define always-false-case
    (lambda (identifier) #f))
  (define matches-free-identifier
    (lambda (identifier)
      (and (identifier? identifier)
           (free-identifier=? identifier ...))))
  (define matches-passed-ellipsis
    (lambda (identifier)
      (and (identifier? identifier)
           (bound-identifier=? identifier ellipsis))))
  (cond
    ((and (not ellipsis)
          (contains-as-free-identifier literals ...))
     always-false-case)
    ((not ellipsis) matches-free-identifier)
    ((set-contains? literals ellipsis) always-false-case)
    (else matches-passed-ellipsis)))

(define (convert-to-literals-set literals)
  ;; Convert some arguments into a set of literals.
  (cond
    ((set? literals) literals)
    ((null? literals) (set bound-identifier-comparator))
    ((pair? literals)
     (list->set bound-identifier-comparator literals))
    (else (error "invalid literals" literals))))

(define matcher-input
  ;; Parameter for the inputs to the pattern matcher.
  ;; 
  ;; The inputs are a vector of #(ellipsis literals), where `ellipsis`
  ;; is either #f or an identifier which binds the ellipsis, and
  ;; `literals` is a list or set of identifiers which are the literals
  ;; for the pattern matcher.
  ;; 
  ;; The values of the parameter is a vector #(ellipsis-procedure literals),
  ;; where `ellipsis-procedure` is a procedure of one argument that returns
  ;; true if the passed argument is a real ellipsis and 0 otherwise. The
  ;; value `literals` is a set of literals, disambiguated by
  ;; `bound-identifier=?`.
  (let ()
    (define (transformer inputs)
      (and inputs
           (let ((literals-set
                  (convert-to-literals-set
                   (vector-ref inputs 1))))
             (vector (generate-ellipsis-procedure
                      (cond
                        ((vector-ref inputs 0) => values)
                        (else ...))
                      literals-set)
                     literals-set))))
    (make-parameter #f transformer)))

(define (ellipsis-procedure)
  ;; Returns the predicate that will check if its first argument is the
  ;; ellipsis identifier.
  (vector-ref (matcher-input) 0))

(define (literals)
  ;; Returns the set of literals in dynamic scope.
  (vector-ref (matcher-input) 1))

(define disable-ellipsis? (make-parameter #f))

(define (actual-ellipsis? identifier)
  ;; Returns `#t` if `id` is an ellipsis, and `#f` otherwise.
  (if (disable-ellipsis?)
      #f
      ((ellipsis-procedure) identifier)))

(define (is-ellipsis-list patcdr)
  ;; Returns (values has-ellipsis? next). `has-ellipsis?` is true if the
  ;; pair is an ellipsis pattern, and false otherwise. `next` is the next
  ;; pattern that will be matched.
  (cond
    ((null? patcdr) (values #f patcdr))
    ((not (pair? patcdr)) (values #f patcdr))
    (else
     (let ((patcadr (unwrap-syntax (car patcdr))))
       (if (actual-ellipsis? patcadr)
           (values #t (cdr patcdr))
           (values #f patcdr))))))

(define (literal? identifier)
  (set-contains? (literals) identifier))

(define (empty-map)
  (hashmap bound-identifier-comparator))

