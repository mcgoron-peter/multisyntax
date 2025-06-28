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

(define-syntax test-alpha
  (syntax-rules ()
    ((test-alpha name (inputs ...) output)

     (let-values (((global-map expanded-list)
                   (expand initial-environment
                           (list (empty-wrap (quote inputs)) ...))))
       (test-equal name
                   (map (lambda (term)
                          (debruijnize global-map term '()))
                        expanded-list)
                   (quote output))))))

(define (test-expander)
  (test-alpha "identity"
              ((lambda x x))
              ((lambda 0)))
  (test-alpha "let-syntax of identifier"
              ((let-syntax ((λ lambda))
                 (λ x x)))
              ((lambda 0)))
  (test-alpha "lexical renaming of keywords"
              ((lambda lambda (lambda lambda)))
              ((lambda (0 0))))
  (test-alpha "simple define-syntax"
              ((define-syntax let
                 (syntax-rules ()
                   ((let ((name value)) body)
                    ((lambda name body) value))))
               (let ((x (f y))) (f x)))
              (((lambda (f 0)) (f y))))
  (test-alpha "define-syntax with ellipsis"
              ((define-syntax let
                 (syntax-rules ()
                   ((let ((name value)) body)
                    ((lambda name body) value))))
               (define-syntax or
                 (syntax-rules ()
                   ((or) false)
                   ((or x y ...)
                    (let ((tmp x))
                      (if tmp tmp (or y ...))))))
               (or a tmp b))
              (((lambda (if 0
                            0
                            ((lambda (if 0
                                         0
                                         ((lambda (if 0
                                                     0
                                                     false))
                                          b))) tmp))) a))))

(define (test-untyped-lambda-calculus)
  (test-group "untyped lambda calculus"
    (test-expander)))

