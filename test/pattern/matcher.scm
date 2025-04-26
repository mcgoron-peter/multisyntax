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
 |
 | To test: ellipsis and underscore as literals override their usual
 | behavior.
 |#

(define ellipsis (empty-wrap '...))
(define empty-map (hashmap bound-identifier-comparator))
(define empty-set (set bound-identifier-comparator))

(define (test-single-match)
  (define-values (matcher names _)
    (compile-pattern '() (empty-wrap 'x)))
  (test-equal "nesting info of identifier"
              (cons 0 #f)
              (hashmap-ref names (empty-wrap 'x)))
  (let ((returned (matcher (empty-wrap 'y))))
    (test-assert "identifier"
                 (bound-identifier=? (hashmap-ref returned
                                                  (empty-wrap 'x))
                                     (empty-wrap 'y))))
  (let* ((returned (matcher (list
                             (empty-wrap 'y))))
         (res (hashmap-ref returned (empty-wrap 'x))))
    (test-assert "match on list returns list" (list? res))
    (test-assert "is the same list"
                 (bound-identifier=? (list-ref res 0)
                                     (empty-wrap 'y)))))

(define (test-match-in-list)
  (define-values (matcher names _)
    (compile-pattern '() (list (empty-wrap 'x))))
  (test-equal "nesting info of identifier"
              (cons 0 #f)
              (hashmap-ref names (empty-wrap 'x)))
  (let ((returned (matcher (empty-wrap 'y))))
    (test-assert "does not match identifier"
                 (not returned)))
  (let ((returned (matcher (list (empty-wrap 'y)))))
    (test-assert "matches inside of list"
                 (bound-identifier=? (hashmap-ref returned (empty-wrap 'x))
                                     (empty-wrap 'y)))))

(define (test-multiple-matches-in-list)
  (define-values (matcher names _)
    (compile-pattern '() (list (empty-wrap 'x)
                               (empty-wrap 'y))))
  (test-equal "nesting info of x"
              (cons 0 #f)
              (hashmap-ref names (empty-wrap 'x)))
  (test-equal "nesting info of y"
              (cons 0 #f)
              (hashmap-ref names (empty-wrap 'y)))
  (let ((returned (matcher (list 1 2))))
    (test-equal "first" 1 (hashmap-ref returned (empty-wrap 'x)))
    (test-equal "second" 2 (hashmap-ref returned (empty-wrap 'y)))))

(define (test-simple-ellipsis)
  (define-values (matcher names levels)
    (compile-pattern '() (list (empty-wrap 'x) ellipsis)))
  (define data-x (hashmap-ref names (empty-wrap 'x)))
  (test-equal "nesting level of x"
              1
              (car data-x))
  (test-equal "nested ellipsis groups of the group of x"
              '()
              (hashmap-ref levels (cdr data-x)))
  (let* ((list '(1 2 3 4 5 6 7 8))
         (returned (matcher list))
         (x-value (hashmap-ref returned (empty-wrap 'x))))
    (test-equal "(1 2 3 ...)"
                (reverse list)
                x-value))
  (let ((returned (matcher '())))
    (test-equal "()"
                '()
                (hashmap-ref returned (empty-wrap 'x))))
  (let* ((list (list (empty-wrap 'x)
                     1
                     (empty-wrap 'y)))
         (returned (matcher list))
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
  (define-values (matcher names levels)
    (compile-pattern '()
                     (list (list (empty-wrap 'x) ellipsis)
                           (list (empty-wrap 'y) ellipsis))))
  (define data-x (hashmap-ref names (empty-wrap 'x)))
  (define data-y (hashmap-ref names (empty-wrap 'y)))
  (define (test-for list x y)
    (let ((returned (matcher list)))
      (test-equal "x"
                  x
                  (hashmap-ref returned (empty-wrap 'x)))
      (test-equal "y"
                  y
                  (hashmap-ref returned (empty-wrap 'y)))))
  (test-equal "level of x" 1 (car data-x))
  (test-equal "level of y" 1 (car data-y))
  (test-assert "groups of x and y are different"
               (not (= (cdr data-x) (cdr data-y))))
  (test-equal "ellipses subgroups of x"
              '()
              (hashmap-ref levels (cdr data-y)))
  (test-equal "ellipses subgroups of y"
              '()
              (hashmap-ref levels (cdr data-y)))
  (test-group "two lists"
    (test-for '((1 2 3 4) (5 6 7 8))
              '(4 3 2 1)
              '(8 7 6 5)))
  (test-group "one list"
    (test-for '((1 2 3 4) ())
              '(4 3 2 1)
              '())))

(define (test-compound-ellipsis)
  (define-values (matcher names levels)
    (compile-pattern '()
                     (list (list (empty-wrap 'name) (empty-wrap 'value))
                           ellipsis)))
  (define data-x (hashmap-ref names (empty-wrap 'x)))
  (define data-y (hashmap-ref names (empty-wrap 'y)))
  (define (test-for list x y)
    (let ((returned (matcher list)))
      (test-equal "x"
                  x
                  (hashmap-ref returned (empty-wrap 'x)))
      (test-equal "y"
                  y
                  (hashmap-ref returned (empty-wrap 'y)))))
  (test-equal "level of x" 1 (car data-x))
  (test-equal "level of y" 1 (car data-y))
  (test-equal "x and y are in the same group"
              (= (cdr data-x) (cdr data-y)))
  (test-group "pairs"
    (test-for '((1 2) (3 4) (5 6))
              '(5 3 1)
              '(6 4 2)))
  (test-group "empty"
    (test-for '() '() '())))

(define (test-nested-ellipsis)
  (define-values (matcher names levels)
    (compile-pattern '()
                     (list (list (list (empty-wrap 'name) ellipsis)
                                 (empty-wrap 'value))
                           ellipsis)))
  (define data-name (hashmap-ref names (empty-wrap 'name)))
  (define data-value (hashmap-ref names (empty-wrap 'value)))
  (define (test-of form names values)
    (let ((returned (matcher form)))
      (test-equal "names"
                  names
                  (hashmap-ref returned (empty-wrap 'name)))
      (test-equal "values"
                  values
                  (hashmap-ref returned (empty-wrap 'value)))))
  (test-equal "level of name" 2 (car data-name))
  (test-equal "level of value" 1 (car data-value))
  (test-assert "name and value have different groups"
               (not (= (cdr data-name) (cdr data-value))))
  (test-assert "name group is a subset of value group"
               (member (cdr data-name)
                       (hashmap-ref levels (cdr data-value))))
  (test-group "let-values like form"
    (test-of '((("name1" "name2" "name3") "value1")
               (("name4" "name5" "name6") "value2"))
             '(("name6" "name5" "name4")
               ("name3" "name2" "name1"))
             '("value2" "value1")))
  (test-assert "non list fails"
               (not (matcher '(("name1 value1") ("name2" "value2")))))
  (test-assert "partial non list fails"
               (not
                (matcher
                 '((("name1" "name2") "value1")
                   ("name3" "value3"))))))

(define (test-single-literal)
  (define literal-list (list (empty-wrap 'literal)))
  (define-values (matcher names levels)
    (compile-pattern literal-list
                     (list (empty-wrap 'literal) (empty-wrap 'x))))
  (test-assert "without literal fails"
               (not
                (matcher '("literal" "value"))))
  (test-group "with literal succeeds"
    (let ((returned (matcher `(,(empty-wrap 'literal) "value"))))
      (test-equal "x"
                  "value"
                  (hashmap-ref returned (empty-wrap 'x))))))

(define (test-ignored-pattern)
  (define-values (matcher names levels)
    (compile-pattern '()
                     (list (empty-wrap '_) (empty-wrap 'x))))
  (test-equal "names is length 1"
              1
              (hashmap-size names))
  (test-assert "names contains x"
               (hashmap-contains? names (empty-wrap 'x)))
  (let ((returned (matcher '(1 2))))
    (test-equal "x"
                2
                (hashmap-ref returned (empty-wrap 'x)))))

(define (test-matching-a-vector)
  (define-values (matcher names levels)
    (compile-pattern (list (empty-wrap 'then))
                     (vector (empty-wrap 'x)
                             ellipsis
                             (empty-wrap 'then)
                             (empty-wrap 'y))))
  (let ((returned (matcher (vector 1 2 3 4 5
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