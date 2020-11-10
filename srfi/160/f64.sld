(define-library (srfi 160 f64)
  (import (scheme base))
  (import (scheme case-lambda))
  (import (scheme cxr))
  (import (only (scheme r5rs) inexact->exact))
  (import (scheme complex))
  (import (scheme write))
  (import (srfi 128))
  (import (srfi 160 base))
  ;; Constructors 
  (export make-f64vector f64vector
          f64vector-unfold f64vector-unfold-right
          f64vector-copy f64vector-reverse-copy 
          f64vector-append f64vector-concatenate
          f64vector-append-subvectors)
  ;; Predicates 
  (export f64? f64vector? f64vector-empty? f64vector=)
  ;; Selectors
  (export f64vector-ref f64vector-length)
  ;; Iteration 
  (export f64vector-take f64vector-take-right
          f64vector-drop f64vector-drop-right
          f64vector-segment
          f64vector-fold f64vector-fold-right
          f64vector-map f64vector-map! f64vector-for-each
          f64vector-count f64vector-cumulate)
  ;; Searching 
  (export f64vector-take-while f64vector-take-while-right
          f64vector-drop-while f64vector-drop-while-right
          f64vector-index f64vector-index-right f64vector-skip f64vector-skip-right 
          f64vector-any f64vector-every f64vector-partition
          f64vector-filter f64vector-remove)
  ;; Mutators 
  (export f64vector-set! f64vector-swap! f64vector-fill! f64vector-reverse!
          f64vector-copy! f64vector-reverse-copy!
          f64vector-unfold! f64vector-unfold-right!)
  ;; Conversion 
  (export f64vector->list list->f64vector
          reverse-f64vector->list reverse-list->f64vector
          f64vector->vector vector->f64vector)
  ;; Misc
  (export make-f64vector-generator f64vector-comparator write-f64vector)

  (include "f64-impl.scm")
)
