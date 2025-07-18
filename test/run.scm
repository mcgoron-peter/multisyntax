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

(test-syntax-object)

(load "../multisyntax/pattern/internal.sld")
(load "../multisyntax/pattern/matcher.sld")
(load "pattern/matcher.sld")
(import (multisyntax pattern matcher test))
(test-patterns)

(load "../multisyntax/pattern/producer.sld")
(load "pattern/producer.sld")
(import (multisyntax pattern producer test))
(test-producers)

(load "../multisyntax/examples/untyped-lambda-calculus.sld")
(load "examples/untyped-lambda-calculus.sld")
(import (multisyntax examples untyped-lambda-calculus test))
(test-untyped-lambda-calculus)

(import (multisyntax examples untyped-lambda-calculus))
(lcload "/home/user/Documents/code/scheme/multisyntax/multisyntax/examples/untyped-lambda-calculus-prelude.scm")
