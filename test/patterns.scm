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

(define ellipsis (empty-wrap '...))
(define empty-map (hashmap bound-identifier-comparator))
(define empty-set (set bound-identifier-comparator))

(define (test-single-match)
  (define matcher
    (compile-pattern ellipsis
                     empty-set
                     (empty-wrap 'x)))
  (let ((returned (matcher empty-map (empty-wrap 'y))))
    (test-assert "identifier"
                 (bound-identifier=? (hashmap-ref returned
                                                  (empty-wrap 'x))
                                     (empty-wrap 'y))))
  (let* ((returned (matcher empty-map (list
                                       (empty-wrap 'y))))
         (res (hashmap-ref returned (empty-wrap 'x))))
    (test-assert "match on list returns list" (list? res))
    (test-assert "is the same list"
                 (bound-identifier=? (list-ref res 0)
                                     (empty-wrap 'y)))))

(define (test-match-in-list)
  (define matcher
    (compile-pattern ellipsis
                     empty-set
                     (list (empty-wrap 'x))))
  (let ((returned (matcher empty-map (empty-wrap 'y))))
    (test-assert "does not match identifier"
                 (not returned)))
  (let ((returned (matcher empty-map (list (empty-wrap 'y)))))
    (test-assert "matches inside of list"
                 (bound-identifier=? (hashmap-ref returned (empty-wrap 'x))
                                     (empty-wrap 'y)))))

(define (test-multiple-matches-in-list)
  (define matcher
    (compile-pattern ellipsis
                     empty-set
                     (list (empty-wrap 'x)
                           (empty-wrap 'y))))
  (let ((returned (matcher empty-map (list 1 2))))
    (test-equal "first" 1 (hashmap-ref returned (empty-wrap 'x)))
    (test-equal "second" 2 (hashmap-ref returned (empty-wrap 'y)))))

(define (test-simple-ellipsis)
  (define matcher
    (compile-pattern ellipsis
                     empty-set
                     (list (empty-wrap 'x) ellipsis)))
  (let* ((list '(1 2 3 4 5 6 7 8))
         (returned (matcher empty-map list))
         (x-value (hashmap-ref returned (empty-wrap 'x))))
    (test-assert "returned is matched-ellipsis"
                 (matched-ellipsis? x-value))
    (test-equal "(x ...)"
                (reverse list)
                (matched-ellipsis-reversed-list x-value)))
  #;(let* ((returned (matcher empty-map '()))
           (x-value (hashmap-ref returned (empty-wrap 'x))))))

(define (test-patterns)
  (test-group "single match" (test-single-match))
  (test-group "test match in list" (test-match-in-list))
  (test-group "test multiple matches in list"
    (test-multiple-matches-in-list))
  (test-group "simple ellipsis" (test-simple-ellipsis))
  )