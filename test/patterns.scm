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
  (define-values (matcher names)
    (compile-pattern ellipsis '() (empty-wrap 'x)))
  (test-equal "nesting level of identifier"
              0
              (hashmap-ref names (empty-wrap 'x)))
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
  (define-values (matcher names)
    (compile-pattern ellipsis '() (list (empty-wrap 'x))))
  (test-equal "nesting level of identifier"
              0
              (hashmap-ref names (empty-wrap 'x)))
  (let ((returned (matcher empty-map (empty-wrap 'y))))
    (test-assert "does not match identifier"
                 (not returned)))
  (let ((returned (matcher empty-map (list (empty-wrap 'y)))))
    (test-assert "matches inside of list"
                 (bound-identifier=? (hashmap-ref returned (empty-wrap 'x))
                                     (empty-wrap 'y)))))

(define (test-multiple-matches-in-list)
  (define-values (matcher names)
    (compile-pattern ellipsis '() (list (empty-wrap 'x)
                                        (empty-wrap 'y))))
  (test-equal "nesting level of x"
              0
              (hashmap-ref names (empty-wrap 'x)))
  (test-equal "nesting level of y"
              0
              (hashmap-ref names (empty-wrap 'y)))
  (let ((returned (matcher empty-map (list 1 2))))
    (test-equal "first" 1 (hashmap-ref returned (empty-wrap 'x)))
    (test-equal "second" 2 (hashmap-ref returned (empty-wrap 'y)))))

(define (test-simple-ellipsis)
  (define-values (matcher names)
    (compile-pattern ellipsis
                     '()
                     (list (empty-wrap 'x) ellipsis)))
  (test-equal "nesting level of x"
              1
              (hashmap-ref names (empty-wrap 'x)))
  (let* ((list '(1 2 3 4 5 6 7 8))
         (returned (matcher empty-map list))
         (x-value (hashmap-ref returned (empty-wrap 'x))))
    (test-equal "(1 2 3 ...)"
                (reverse list)
                x-value))
  (let ((returned (matcher empty-map '())))
    (test-equal "()"
                '()
                (hashmap-ref returned (empty-wrap 'x))))
  (let* ((list (list (empty-wrap 'x)
                     1
                     (empty-wrap 'y)))
         (returned (matcher empty-map list))
         (values (hashmap-ref returned (empty-wrap 'x))))
    (test-group "(x 1 y)"
      (test-assert "y"
                   (bound-identifier=?
                    (list-ref values 0)
                    (empty-wrap 'y)))
      (test-equal 1 (list-ref values 1))
      (test-assert "x"
                   (bound-identifier=?
                    (list-ref values 2)
                    (empty-wrap 'x))))))

(define (test-multiple-ellipsis)
  (define-values (matcher names)
    (compile-pattern ellipsis
                     '()
                     (list (list (empty-wrap 'x) ellipsis)
                           (list (empty-wrap 'y) ellipsis))))
  (define (test-for list x y)
    (let ((returned (matcher empty-map list)))
      (test-equal "x"
                  x
                  (hashmap-ref returned (empty-wrap 'x)))
      (test-equal "y"
                  y
                  (hashmap-ref returned (empty-wrap 'y)))))
  (test-group "two lists"
    (test-for '((1 2 3 4) (5 6 7 8))
              '(4 3 2 1)
              '(8 7 6 5)))
  (test-group "one list"
    (test-for '((1 2 3 4) ())
              '(4 3 2 1)
              '())))

(define (test-compound-ellipsis)
  (define-values (matcher names)
    (compile-pattern ellipsis
                     '()
                     (list (list (empty-wrap 'name) (empty-wrap 'value))
                           ellipsis)))
  (define (test-for list x y)
    (let ((returned (matcher empty-map list)))
      (test-equal "x"
                  x
                  (hashmap-ref returned (empty-wrap 'x)))
      (test-equal "y"
                  y
                  (hashmap-ref returned (empty-wrap 'y)))))
  (test-group "pairs"
    (test-for '((1 2) (3 4) (5 6))
              '(5 3 1)
              '(6 4 2)))
  (test-group "empty"
    (test-for '() '() '())))

(define (test-nested-ellipsis)
  (define-values (matcher names)
    (compile-pattern ellipsis
                     '()
                     (list (list (list (empty-wrap 'name) ellipsis)
                                 (empty-wrap 'value))
                           ellipsis)))
  (define (test-of form names values)
    (let ((returned (matcher empty-map form)))
      (test-equal "names"
                  names
                  (hashmap-ref returned (empty-wrap 'name)))
      (test-equal "values"
                  values
                  (hashmap-ref returned (empty-wrap 'value)))))
  (test-group "let-values like form"
    (test-of '((("name1" "name2" "name3") "value1")
               (("name4" "name5" "name6") "value2"))
             '(("name6" "name5" "name4")
               ("name3" "name2" "name1"))
             '("value2" "value1")))
  (test-assert "non list fails"
               (not (matcher empty-map
                             '(("name1 value1") ("name2" "value2")))))
  (test-assert "partial non list fails"
               (not
                (matcher
                 empty-map
                 '((("name1" "name2") "value1")
                   ("name3" "value3"))))))

(define (test-single-literal)
  (define literal-list (list (empty-wrap 'literal)))
  (define-values (matcher names)
    (compile-pattern ellipsis
                     literal-list
                     (list (empty-wrap 'literal) (empty-wrap 'x))))
  (test-assert "without literal fails"
               (not
                (matcher empty-map '("literal" "value"))))
  (test-group "with literal succeeds"
    (let ((returned (matcher empty-map `(,(empty-wrap 'literal) "value"))))
      (test-equal "x"
                  "value"
                  (hashmap-ref returned (empty-wrap 'x))))))

(define (test-ignored-pattern)
  (define-values (matcher names)
    (compile-pattern ellipsis
                     '()
                     (list (empty-wrap '_) (empty-wrap 'x))))
  (test-equal "names is length 1"
              1
              (hashmap-size names))
  (test-assert "names contains x"
               (hashmap-contains? names (empty-wrap 'x)))
  (let ((returned (matcher empty-map '(1 2))))
    (test-equal "x"
                2
                (hashmap-ref returned (empty-wrap 'x)))))

(define (test-matching-a-vector)
  (define-values (matcher names)
    (compile-pattern ellipsis
                     (list (empty-wrap 'then))
                     (vector (empty-wrap 'x)
                             ellipsis
                             (empty-wrap 'then)
                             (empty-wrap 'y))))
  (let ((returned (matcher empty-map
                           (vector 1 2 3 4 5
                                   (empty-wrap 'then)
                                   6))))
    (test-assert "matched" returned)
    (test-equal "x"
                  '(5 4 3 2 1)
                  (hashmap-ref returned (empty-wrap 'x)))
    (test-equal "y"
                  6
                  (hashmap-ref returned (empty-wrap 'y)))))

(define (test-patterns)
  (test-group "single match" (test-single-match))
  (test-group "test match in list" (test-match-in-list))
  (test-group "test multiple matches in list"
    (test-multiple-matches-in-list))
  (test-group "simple ellipsis" (test-simple-ellipsis))
  (test-group "test multiple ellipsis" (test-multiple-ellipsis))
  (test-group "test nested ellipsis" (test-nested-ellipsis))
  (test-group "test single literal" (test-single-literal))
  (test-group "test ignored pattern" (test-ignored-pattern))
  (test-group "test matching a vector" (test-matching-a-vector)))