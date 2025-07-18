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

(define-library (multisyntax pattern matcher)
  (import (scheme base) (scheme write) (scheme case-lambda)
          (srfi 26) (srfi 111) (srfi 113) (srfi 146 hash) (srfi 197)
          (multisyntax utils) (multisyntax pattern internal)
          (only (multisyntax syntax-object)
                identifier? bound-identifier=? free-identifier=?
                unwrap-syntax syntax->datum
                ;; unportable extensions
                self-syntax?
                empty-wrap bound-identifier-comparator))
  (export compile-pattern)
  (include "matcher.scm"))