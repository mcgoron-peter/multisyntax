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
 |#

(define examples-of-self-syntax
  (list '() 1 #\a #f "a" #u8(1 2 3 4)))

(define (test-locations)
  (test-assert (comparator-test-type location-comparator
                                       (generate-lexical-location 'test)))
  (test-assert (comparator-test-type location-comparator
                                       'test)))

(define (test-self-syntax)
  (for-each (lambda (x)
                (test-assert "example of self-syntax" (self-syntax? x)))
            examples-of-self-syntax)
  (test-assert "symbol is not self-syntax" (not (self-syntax? 'x)))
  (test-assert "cons is not self-syntax" (not (self-syntax? (cons 1 2))))
  (test-assert "vector is not self-syntax" (not (self-syntax? #(1 2)))))

(define (test-syntax-predicate)
  (for-each (lambda (x)
              (test-assert "self-syntax is syntax" (syntax? x)))
            examples-of-self-syntax)
  (for-each (lambda (x)
              (test-assert "wrap of self-syntax is syntax" (syntax? (empty-wrap x))))
            examples-of-self-syntax)
  (test-assert "list of self-syntax is syntax"
               (syntax? examples-of-self-syntax))
  (test-assert "vector of self-syntax is syntax"
               (syntax? (list->vector examples-of-self-syntax)))
  (test-assert "wrap of symbol is syntax"
               (syntax? (empty-wrap 'x))))

(define (test-identifier-predicate)
  (for-each (lambda (x)
              (test-assert "self-syntax is not identifier"
                           (not (identifier? x))))
            examples-of-self-syntax)
  (test-assert "wrap of symbol is identifier"
               (identifier? (empty-wrap 'x)))
  (test-assert "wrap of list is not identifier"
               (not (identifier? (empty-wrap (list 'x))))))

(define (test-add-timestamp)
  (test-assert "timestamps are unique"
               (not (equal? (generate-timestamp) (generate-timestamp))))
  (let ((ts (generate-timestamp))
        (id (empty-wrap 'test)))
    (test-assert "empty wrap has no timestamps"
                 (set-empty? (wrap->timestamps id)))
    (set! id (add-timestamp id ts))
    (test-equal "adding a timestamp adds that timestamp"
                (list ts)
                (set->list (wrap->timestamps id)))
    (set! id (add-timestamp id ts))
    (test-assert "adding the same timestamp removes the timestamp"
                 (set-empty? (wrap->timestamps id)))
    (let ((new-ts (generate-timestamp)))
      (set! id (add-timestamp id ts))
      (set! id (add-timestamp id new-ts))
      (test-assert "adding one timestamp makes the wrap contain that timestamp"
                   (set-contains? (wrap->timestamps id) ts))
      (test-assert "adding another timestamp makes the wrap contain that timestamp"
                   (set-contains? (wrap->timestamps id) new-ts))
      (test-equal "only two timestamps were added"
                  (set-size (wrap->timestamps id))
                  2)
      (set! id (add-timestamp id new-ts))
      (test-equal "adding one timestamp removes one and keeps the other"
                  (list ts)
                  (set->list (wrap->timestamps id))))))

(define (test-add-substitution)
  (test-group "no timestamps"
    (let* ((newloc (generate-lexical-location 'test))
           (stx (add-substitution (empty-wrap 'test)
                                  (empty-wrap 'test)
                                  newloc)))
      (test-assert (=? lexical-location-comparator
                       newloc
                       (resolve stx)))))
  (test-group "mismatched timestamps"
    (let* ((newloc (generate-lexical-location 'test))
           (stx (add-substitution (add-timestamp (empty-wrap 'test)
                                                 (generate-timestamp))
                                  (empty-wrap 'test)
                                  newloc)))
      (test-assert (=? location-comparator
                       'test
                       (resolve stx)))))
  (test-group "mismatched resolved name"
    (let* ((newloc (generate-lexical-location 'test))
           (stx (add-substitution (empty-wrap 'test)
                                  (empty-wrap 'test2)
                                  newloc)))
      (test-assert (=? location-comparator
                       'test
                       (resolve stx)))))
  (test-group "multiple names in environment"
    (let* ((loc1 (generate-lexical-location 'test1))
           (loc2 (generate-lexical-location 'test2))
           (stx (add-substitution (empty-wrap 'test)
                                  (empty-wrap 'test)
                                  loc1))
           (stx (add-substitution stx
                                  (empty-wrap 'test2)
                                  loc2)))
      (test-assert (=? location-comparator
                       (resolve stx)
                       loc1))))
  (test-group "intermediate substitutions"
    (let* ((loc1 (generate-lexical-location 'test1))
           (loc2 (generate-lexical-location 'test2))
           (stx (add-substitution (empty-wrap 'test)
                                  (empty-wrap 'test)
                                  loc1))
           (stx (add-substitution stx stx loc2)))
      (test-assert (=? location-comparator
                       (resolve stx)
                       loc2)))))

(define (test-bound-identifier-comparator)
  (define ids
    (let* ((ts (generate-timestamp))
           (id-x (empty-wrap 'x))
           (id-y (empty-wrap 'y))
           (mark-x (add-timestamp id-x ts))
           (loc1 (generate-lexical-location 'test1))
           (subst-on-x (add-substitution id-x
                                         id-x
                                         loc1))
           (subst-on-mark-x (add-substitution id-x
                                              id-x
                                              loc1)))
      (list
       id-x
       id-y
       mark-x
       (add-timestamp (empty-wrap 'x)
                      (generate-timestamp))
       (add-timestamp (empty-wrap 'y) ts)
       (empty-wrap 'z)
       subst-on-x
       subst-on-mark-x)))
  (define the-set (list->set bound-identifier-comparator ids))
  (for-each (lambda (id)
              (test-assert (bound-identifier=?
                            id
                            (set-member the-set id #f))))
            ids))

(define (test)
  (test-group "locations" (test-locations))
  (test-group "self-syntax?" (test-self-syntax))
  (test-group "syntax?" (test-syntax-predicate))
  (test-group "identifier?" (test-identifier-predicate))
  (test-group "add-timestamp" (test-add-timestamp))
  (test-group "resolve a symbol"
    (test-eq (resolve (empty-wrap 'test)) 'test))
  (test-group "add-substitution" (test-add-substitution))
  (test-group "bound-identifier-comparator"
    (test-bound-identifier-comparator)))
