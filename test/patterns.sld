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

(define-library (multisyntax patterns test)
  (import (scheme base) (scheme write) (srfi 113) (srfi 146 hash)
          (multisyntax syntax-object)
          (multisyntax patterns))
  (cond-expand
    (chicken (import (srfi 64)
                     (chicken condition)))
    (else))
  (export test-patterns)
  (include "patterns.scm"))
