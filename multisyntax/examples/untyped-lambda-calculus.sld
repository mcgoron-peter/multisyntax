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

(define-library (multisyntax examples untyped-lambda-calculus)
  (import (scheme base) (scheme write) (scheme read) (scheme file) (scheme cxr)
          (srfi 1) (srfi 26) (srfi 111) (srfi 146 hash)
          (multisyntax syntax-object)
          (multisyntax pattern matcher)
          (multisyntax pattern producer))
  (cond-expand
    (chicken-5 (import (rename (chicken pretty-print)
                               (pp pretty))))
    (else (import (srfi 166))))
  (export expand transformer? initial-environment alpha
          debruijnize lceval current-environment lcrepl lcload)
  (include "untyped-lambda-calculus.scm"))