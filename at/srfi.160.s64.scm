(declare (fixnum-arithmetic))

(module (srfi 160 s64) ()
  (import (scheme))
  (import (only (chicken base)
    open-input-string include define-record-type case-lambda
    when unless let-values))
  (import (only (chicken module) export))
  (import (only (chicken memory) move-memory!))
  (import (srfi 128))
  (import (srfi 160 base))

  ;; Constructors
  (export make-s64vector s64vector
          s64vector-unfold s64vector-unfold-right
          s64vector-copy s64vector-reverse-copy
          s64vector-append s64vector-concatenate
          s64vector-append-subvectors)
  ;; Predicates
  (export s64? s64vector? s64vector-empty? s64vector=)
  ;; Selectors
  (export s64vector-ref s64vector-length)
  ;; Iteration
  (export s64vector-take s64vector-take-right
          s64vector-drop s64vector-drop-right
          s64vector-segment
          s64vector-fold s64vector-fold-right
          s64vector-map s64vector-map! s64vector-for-each
          s64vector-count s64vector-cumulate)
  ;; Searching
  (export s64vector-take-while s64vector-take-while-right
          s64vector-drop-while s64vector-drop-while-right
          s64vector-index s64vector-index-right s64vector-skip s64vector-skip-right
          s64vector-any s64vector-every s64vector-partition
          s64vector-filter s64vector-remove)
  ;; Mutators
  (export s64vector-set! s64vector-swap! s64vector-fill! s64vector-reverse!
          s64vector-copy! s64vector-reverse-copy!
          s64vector-unfold! s64vector-unfold-right!)
  ;; Conversion
  (export s64vector->list reverse-s64vector->list reverse-list->s64vector
          list->s64vector s64vector->vector vector->s64vector)
  ;; Misc
  (export make-s64vector-generator s64vector-comparator write-s64vector)

  (include "at/s64-impl.scm")

  (define (eof-object)
     (let* ((p (open-input-string "")) (e (read p)))
      (close-input-port p) e))
)
