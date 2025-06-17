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

(define (test-producers)
  (let ((producer
         (compile-producer '()
                           (list (empty-wrap 'x) (empty-wrap '...))
                           (hashmap bound-identifier-comparator
                                    (empty-wrap 'x)
                                    1))))
    (test-equal "produces x = '(5 4 3 2 1)"
                '(1 2 3 4 5)
                (producer (hashmap bound-identifier-comparator
                                   (empty-wrap 'x)
                                   '(5 4 3 2 1))))))
