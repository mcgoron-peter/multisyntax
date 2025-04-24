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
 |------------------------------------------------------------------------
 | Hygenic syntax transformer based on Dybvig, Hieb, and Bruggeman 1992.
 |#

(define-library (multisyntax syntax-object)
  (import (scheme base) (scheme case-lambda)
          (scheme write)
          (srfi 26) (srfi 113) (srfi 128) (srfi 133) (srfi 146) (srfi 228))
  (export generate-lexical-location
          lexical-location->string
          lexical-location-comparator
          environment-key-comparator
          bound-identifier-comparator
          ;; Misc. predicates
          self-syntax? syntax?
          ;; Operations on wraps
          generate-timestamp empty-wrap add-timestamp add-substitution
          wrap->timestamps resolve
          ;; Standard operations
          symbolic-identifier=? free-identifier=? bound-identifier=?
          identifier?
          generate-identifier generate-temporaries
          unwrap-syntax syntax->datum
          if-contains-wrap datum->syntax)
  (include "syntax-object.scm"))
