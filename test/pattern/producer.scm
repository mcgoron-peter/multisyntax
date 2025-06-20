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

(define (idmap . values)
  (apply hashmap bound-identifier-comparator values))

(define (test-self-syntax)
  (let ((producer (compile-producer '() '() '())))
    (test-equal "()" '() (producer (idmap))))
  (let ((producer (compile-producer '() 0 '())))
    (test-equal "0" 0 (producer (idmap))))
  (let ((producer (compile-producer '() "call/cc" '())))
    (test-equal "string" "call/cc" (producer (idmap))))
  (let ((producer (compile-producer '() #u8(1 2 3 4) '())))
    (test-equal "bytevector" #u8(1 2 3 4) (producer (idmap))))
  (let ((producer (compile-producer '() #f '())))
    (test-equal "boolean" #f (producer (idmap))))
  (let ((producer (compile-producer '() #\a '())))
    (test-equal "char" #\a (producer (idmap)))))

(define (test-regular-lists)
  (let ((producer (compile-producer '() (list 1 2 3 4) '())))
    (test-equal "(1 2 3 4)" (producer (idmap))))
  (let ((producer (compile-producer '() (list (empty-wrap 'x))
                                    (idmap (empty-wrap 'x) 0))))
    (test-equal "(x)" '(0) (producer (idmap (empty-wrap 'x) 0)))))

(define (test-list-ellipses)
  (let ((producer
         (compile-producer '()
                           (list (empty-wrap 'x) (empty-wrap '...))
                           (idmap (empty-wrap 'x)
                                  1))))
    (test-equal "x ..."
                '(1 2 3 4 5)
                (producer (idmap (empty-wrap 'x)
                                 '(5 4 3 2 1)))))
  (let ((producer
         (compile-producer '()
                           (list (list (empty-wrap 'x) (empty-wrap '...))
                                 (empty-wrap '...))
                           (idmap (empty-wrap 'x)
                                  2))))
    (test-equal "(x ...) ..."
                '((1 2) (3 4) (5 6) (7 8))
                (producer (idmap (empty-wrap 'x)
                                 '((8 7) (6 5) (4 3) (2 1))))))
  (let ((producer
         (compile-producer '()
                           (list (empty-wrap 'x) (empty-wrap '...) (empty-wrap '...))
                           (idmap (empty-wrap 'x)
                                  2))))
    (test-equal "x ... ..."
                '(1 2 3 4 5 6 7 8)
                (producer (idmap (empty-wrap 'x)
                                 '((8 7) (6 5) (4 3) (2 1))))))
  (test-group "(... (x ...))"
    (let* ((producer
            (compile-producer '()
                              (list (empty-wrap '...) (list (empty-wrap 'x) (empty-wrap '...)))
                              (idmap (empty-wrap 'x)
                                     0)))
           (got (producer (idmap (empty-wrap 'x)
                                 0))))
      (test-assert "returned a list" (list? got))
      (test-eqv "returned the correct length"
                2
                (length got))
      (test-eqv "first value is 0" 0 (car got))
      (test-assert "second value" (bound-identifier=? (cadr got) (empty-wrap '...)))))
  (test-group "(let-values (((names ...) value ...) ...) body ...)"
    (let* ((producer
            (compile-producer '()
                              (list (empty-wrap 'let-values)
                                    (list (list (list (empty-wrap 'names)
                                                      (empty-wrap '...))
                                                (empty-wrap 'values))
                                          (empty-wrap '...))
                                    (empty-wrap 'body)
                                    (empty-wrap '...))
                              (idmap (empty-wrap 'let-values) 0
                                     (empty-wrap 'names) 2
                                     (empty-wrap 'values) 1
                                     (empty-wrap 'body) 1)))
           (got (producer (idmap (empty-wrap 'names)
                                 '((0 1) (2 3) (4 5) (6 7))
                                 (empty-wrap 'values)
                                 '(100 200 300 400)
                                 (empty-wrap 'body)
                                 '(600 700 800 900)
                                 (empty-wrap 'let-values)
                                 1000))))
      (test-equal '(1000 (((7 6) 400)
                          ((5 4) 300)
                          ((3 2) 200)
                          ((1 0) 100))
                         900 800 700 600)
                  got)))
  (test-group "repetition of self-syntax"
    (let ((producer (compile-producer
                     '()
                     (list (list (empty-wrap 'x) 10) (empty-wrap '...))
                     (idmap (empty-wrap 'x) 1))))
      (test-equal '((1 10) (2 10) (3 10))
                  (producer (idmap (empty-wrap 'x) '(3 2 1))))))
  (test-group "multiple uses of the same identifier"
    (let ((producer (compile-producer
                     '()
                     (list (list (empty-wrap 'x) (empty-wrap '...))
                           (list (empty-wrap 'x) (empty-wrap '...)))
                     (idmap (empty-wrap 'x) 1))))
      (test-equal '((1 2 3) (1 2 3))
                  (producer (idmap (empty-wrap 'x) '(3 2 1))))))
  (test-group "excess ellipses"
    (let ((producer (compile-producer
                     '()
                     (list (list (list (empty-wrap 'x) (empty-wrap '...))
                                 (empty-wrap 'y))
                           (empty-wrap '...))
                     (idmap (empty-wrap 'x) 1
                            (empty-wrap 'y) 1))))
      (test-equal '(((1 2 3) 10) ((1 2 3) 20) ((1 2 3) 30))
                  (producer (idmap (empty-wrap 'x) '(3 2 1)
                                   (empty-wrap 'y) '(30 20 10))))))
  (test-group "(test2 (1 2) (3 4))"
    ;; I don't know what the "correct" response for this is, but this
    ;; should emulate Chez's output.
    (let* ((pat
            (list (list (list (empty-wrap 'x)
                             (list (empty-wrap 'x) (empty-wrap '...)))
                       (empty-wrap '...))
                  (empty-wrap '...)))
           (producer (compile-producer '() pat (idmap (empty-wrap 'x) 2))))
      (test-equal '(((1 (1 2)) (2 (3 4))) ((3 (1 2)) (4 (3 4))))
                  (producer (idmap (empty-wrap 'x)
                                   '((4 3) (2 1))))))))

(define (test-producers)
  (test-group "producers"
    (test-group "self-syntax"
      (test-self-syntax))
    (test-group "list ellipses"
      (test-list-ellipses))))