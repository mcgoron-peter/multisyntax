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

(define-library (multisyntax utils)
  (import (scheme base) (srfi 128))
  (export generate-unique-integer
          symbol-comparator exact-integer-comparator)
  (begin
    (define generate-unique-integer
      ;; Generate a unique positive integer.
      (let ((x 1))
        (lambda ()
          (set! x (+ x 1))
          x)))
    (define symbol-comparator
      (make-comparator
       symbol?
       symbol=?
       (lambda (x y) (string<? (symbol->string x) (symbol->string y)))
       symbol-hash))
    (define exact-integer-comparator
      (make-comparator exact-integer? = < number-hash))))
