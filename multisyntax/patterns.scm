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
 | The "compiler" turns the pattern into lambdas. The lambdas are
 | `map * syntax -> (or map #f)`.
 |
 | The map will map identifiers to matched values. These are either
 | 
 | 1. Syntax objects, or
 | 2. Reversed lists containined matched values.
 |#

(define empty-map
  (hashmap bound-identifier-comparator))

(define (compile-single-list match-patcar)
  (lambda (names stx)
    (let ((stx (unwrap-syntax stx)))
      (and (pair? stx)
           (null? (unwrap-syntax (cdr stx)))
           (match-patcar names (car stx))))))

(define-record-type <matched-ellipsis>
  (make-matched-ellipsis reversed-list)
  matched-ellipsis?
  (reversed-list matched-ellipsis-reversed-list))

(define (push-to-matched-ellipsis key matched)
  (make-matched-ellipsis (cons key
                               (matched-ellipsis-reversed-list matched))))

(define (merge-names oldnames newnames)
  ;; newnames is the patterns matched in an ellipsis expression. Append
  ;; each to the lists in `oldnames`, and return that map.
  (define (proc key val names)
    (hashmap-update/default names
                            key
                            (cut push-to-matched-ellipsis key <>)
                            (make-matched-ellipsis '())))
  (hashmap-fold proc oldnames newnames))

(define (compile-ellipsis match-patcar match-patcddr)
  (letrec ((match*
            (lambda (names stx)
              (let ((stx (unwrap-syntax stx)))
                (cond
                  ((null? stx) names)
                  ((not (pair? stx)) #f)
                  ((match-patcar empty-map (car stx))
                   => (lambda (newnames)
                        (cond
                          ((match* (merge-names names newnames)
                                   (cdr stx))
                           => values)
                          (else (match-patcddr names stx)))))
                  (else (match-patcddr names stx)))))))
    match*))

(define (compile-actual-pair match-patcar match-patcdr)
  (lambda (names stx)
    (let ((stx (unwrap-syntax stx)))
      (cond
        ((not (pair? stx)) #f)
        ((match-patcar names (car stx))
         => (cute match-patcdr <> (cdr stx)))
        (else #f)))))


(define (compile-pattern-in-vector match k)
  (lambda (names vec i)
    (let ((stx (vector-ref vec i)))
      (cond
        ((match names stx) => (cute k <> vec (+ i 1)))
        (else #f)))))

(define (compile-ellipsis-in-vector match k)
  (define (match* names vec i)
    (cond
      ((= i (vector-length vec)) (k names vec i))
      ((match empty-map (vector-ref vec i))
       => (lambda (new-names)
            (cond
              ((match* (merge-names names
                                    new-names)
                       (+ i 1))
               => values)
              (else (k names vec i)))))
      (else (k names vec i))))
  match*)

(define (compile-pattern ellipsis literals pattern)
  (define names (set bound-identifier-comparator))
  ;; 
  ;; 
  (define actual-ellipsis?
    (if (set-contains? literals ellipsis)
        (lambda (x) #f)
        (lambda (stx)
          (and (identifier? stx)
               (bound-identifier=? stx ellipsis)))))
  ;; 
  ;; 
  (define (is-ellipsis-list patcdr)
    (if (null? patcdr)
        (values #f (compile patcdr))
        (let ((patcadr (unwrap-syntax (car patcdr))))
          (if (actual-ellipsis? patcadr)
              (values #t (compile (cdr patcdr)))
              (values #f (compile patcdr))))))
  ;; 
  ;; 
  (define (compile-pair patcar patcdr)
    (let ((match-patcar (compile patcar)))
      (if (null? patcdr)
          (compile-single-list match-patcar)
          (let-values (((has-ellipsis? match-next)
                        (is-ellipsis-list patcdr)))
            (if has-ellipsis?
                (compile-ellipsis match-patcar match-next)
                (compile-actual-pair match-patcar match-next))))))
  ;; 
  ;; 
  (define (compile-vector vec)
    (define final-continuation
      (lambda (names vec i)
        (if (= i (vector-length vec))
            names
            #f)))
    (define (on-ellipsis i k)
      (let ((cur (vector-ref vec (- i 1))))
        (when (actual-ellipsis? cur)
          (error "... ... is not allowed" cur))
        (compile-index (- i 2)
                       (compile-ellipsis-in-vector
                        (compile (vector-ref vec (- i 1)))
                        k))))
    (define (compile-index i k)
      (cond
        ((zero? i) k)
        (else
         (let ((cur (vector-ref vec i)))
           (if (actual-ellipsis? cur)
               (on-ellipsis i k)
               (compile-index (- i 1)
                              (compile-pattern-in-vector (compile cur)
                                                         k)))))))
    (compile-index 0 final-continuation))
  ;; 
  ;; 
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
        ((set-contains? literals pattern)
         (lambda (names stx)
           (and (bound-identifier=? stx pattern)
                names)))
        ((actual-ellipsis? pattern)
         (error "invalid ellipsis location" pattern))
        ((bound-identifier=? pattern (empty-wrap '_))
         (lambda (names stx) names))
        (else
         (when (set-contains? names pattern)
           (error "duplicated name" pattern))
         (set! names (set-adjoin! names pattern))
         (lambda (names stx)
           (hashmap-set names pattern stx))))))
  (compile pattern))
