(cond-expand
  (chicken (import r7rs
                   (prefix (mcgoron srfi 64)
                           mcgoron-)
                   (srfi 64))
           (test-runner-factory mcgoron-factory)
           (test-runner-current (mcgoron-factory)))
  (else (import (srfi 64))))

(load "../multisyntax/utils.sld")
(load "../multisyntax/syntax-object.sld")
(load "syntax-object.sld")

(import (rename (multisyntax syntax-object test)
                (test test-syntax-object)))

#;(test-syntax-object)

(load "../multisyntax/pattern/internal.sld")
(load "../multisyntax/pattern/matcher.sld")
(load "pattern/matcher.sld")
(import (multisyntax pattern matcher test))
#;(test-patterns)

(load "../multisyntax/pattern/producer.sld")
(load "pattern/producer.sld")
(import (multisyntax pattern producer test))
#;(test-producers)

(load "../multisyntax/examples/untyped-lambda-calculus.sld")
(import (multisyntax examples untyped-lambda-calculus)
        (multisyntax syntax-object))

(let-values (((global-map expanded-list)
              (expand initial-environment (list (empty-wrap '(lambda x x))))))
  (display (alpha expanded-list)) (newline))

(let-values (((global-map expanded-list)
              (expand initial-environment
                      (list (empty-wrap '(let-syntax ((λ lambda))
                                           (λ x x)))))))
  (display (alpha expanded-list)) (newline))

(let-values (((global-map expanded-list)
              (expand initial-environment
                      (list (empty-wrap '(lambda lambda (lambda lambda)))))))
  (display (alpha expanded-list)) (newline))

(let-values (((global-map expanded-list)
              (expand initial-environment
                      (list (empty-wrap '(define I (lambda x x)))
                            (empty-wrap '(I (lambda I I)))))))
  (display (alpha expanded-list)) (newline))

(let-values (((global-map expanded-list)
              (expand initial-environment
                      (list (empty-wrap
                             '(define-syntax let
                                (syntax-rules ()
                                  ((let (name value) body)
                                   ((lambda name body) value)))))
                            (empty-wrap '(let (x (lambda x x)) (x x)))))))
  (display (alpha expanded-list)) (newline))

#;(begin
  (load "examples/untyped-lambda-calculus.sld")
  (import (multisyntax examples untyped-lambda-calculus test))
  (test-untyped-lambda-calculus))
