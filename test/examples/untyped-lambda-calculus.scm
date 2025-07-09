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

(define (curry-form form)
  (cond
    ((and (pair? form)
          (eq? (car form) 'lambda))
     (list 'lambda (curry-form (cadr form))))
    ((and (pair? form)
          (> (length form) 2))
     (curry-form (cons (list (list-ref form 0)
                             (list-ref form 1))
                       (list-tail form 2))))
    ((pair? form)
     (list (curry-form (list-ref form 0))
           (curry-form (list-ref form 1))))
    (else form)))

(define-syntax test-alpha
  (syntax-rules ()
    ((test-alpha name (inputs ...) output)

     (let-values (((global-map expanded-list)
                   (expand initial-environment
                           (list (empty-wrap (quote inputs)) ...))))
       (test-equal name
                   (map curry-form (quote output))
                   (map (lambda (term)
                          (debruijnize global-map term '()))
                        expanded-list))))))

(define (test-expander)
  (test-alpha "identity"
              ((lambda x x))
              ((lambda 0)))
  (test-alpha "let-syntax of identifier"
              ((let-syntax ((λ lambda))
                 (λ x x))
               (λ x x))
              ((lambda 0)
               (λ x x)))
  (test-alpha "define-syntax of identifier"
              ((define-syntax λ lambda)
               (λ x x))
              ((lambda 0)))
  (test-alpha "lexical renaming of keywords"
              ((lambda lambda (lambda lambda)))
              ((lambda (0 0))))
  (test-alpha "simple syntax-rules"
              ((define-syntax let
                 (syntax-rules ()
                   ((let ((name value)) body)
                    ((lambda name body) value))))
               (let ((x (f y))) (f x)))
              (((lambda (f 0)) (f y))))
  (test-alpha "syntax-rules with ellipsis"
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
                                          b))) tmp))) a)))
  (test-alpha "splicing-let-syntax"
              ((splicing-let-syntax ((λ lambda))
                 (define-syntax lambda
                   (syntax-rules ()
                     ((_ (name) body) (λ name body))
                     ((_ (name rest ...) body)
                      (λ name (lambda (rest ...) body)))
                     ((_ name body) (λ name body)))))
               (lambda (x y) (x y)))
              ((lambda (lambda (1 0))))))

(define-syntax test-eval-alpha
  (syntax-rules ()
    ((_ name (inputs ...) outputs)
     (let-values (((expanded-list global-map)
                   (lceval (list (empty-wrap (quote inputs)) ...)
                           initial-environment)))
       (test-equal name
                   (map curry-form (quote outputs))
                   (map (lambda (term)
                          (debruijnize global-map term '()))
                        expanded-list))))))

(define (test-eval)
  (test-eval-alpha "identity evals to itself"
                   ((lambda x x))
                   ((lambda 0)))
  (test-eval-alpha "identity applied to itself"
                   (((lambda x x) (lambda x x)))
                   ((lambda 0)))
  (test-eval-alpha "define returns nothing"
                   ((define I (lambda x x)))
                   ())
  (test-eval-alpha "global environment lookup"
                   ((define I (lambda x x))
                    I)
                   ((lambda 0)))
  (test-eval-alpha "K combinator, 1"
                   ((define K (lambda x (lambda y x)))
                    (define I (lambda x x))
                    (K I I))
                   ((lambda 0)))
  (test-eval-alpha "K combinator, 2"
                   ((define K (lambda x (lambda y x)))
                    (define I (lambda x x))
                    (K I K))
                   ((lambda 0)))
  (test-eval-alpha "K combinator, 3"
                   ((define K (lambda x (lambda y x)))
                    (define I (lambda x x))
                    (K K I))
                   ((lambda (lambda 1))))
  (test-eval-alpha "define-syntax"
                   ((define-syntax λ
                      (syntax-rules ()
                        ((λ (x) body)
                         (lambda x body))
                        ((λ (x y ...) body)
                         (lambda x (λ (y ...) body)))))
                    (define true (λ (x y) x))
                    (define false (λ (x y) y))
                    (false false true))
                   ((lambda (lambda 1))))
  (test-eval-alpha "normal order evaluation"
                   ((define ω (lambda x (x x)))
                    (define K (lambda x (lambda y x)))
                    (K K (ω ω)))
                   ((lambda (lambda 1)))))

(define (test-untyped-lambda-calculus)
  (test-group "untyped lambda calculus"
    (test-group "expander"
      (test-expander))
    (test-group "eval"
      (test-eval))))

