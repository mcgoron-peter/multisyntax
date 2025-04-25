(import r7rs)

(cond-expand
  (chicken (import (prefix (mcgoron srfi 64)
                           mcgoron-)
                   (srfi 64))
           (test-runner-factory mcgoron-factory)
           (test-runner-current (mcgoron-factory))))

(load "../multisyntax/syntax-object.sld")
(load "syntax-object.sld")

(import (rename (multisyntax syntax-object test)
                (test test-syntax-object)))

(test-syntax-object)

(load "../multisyntax/pattern/matcher.sld")
(load "pattern/matcher.sld")
(import (multisyntax pattern matcher test))
(test-patterns)
