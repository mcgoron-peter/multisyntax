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
      (test-assert "second value" (bound-identifier=? (cadr got) (empty-wrap '...))))))

(define (test-producers)
  (test-group "producers"
    (test-group "self-syntax"
      (test-self-syntax))
    (test-group "list ellipses"
      (test-list-ellipses))))