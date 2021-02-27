;; The representation of complex vectors

(define-record-type c64vector (raw-make-c64vector bv) c64vector?
  (bv c64vector-body))

(define-record-type c128vector (raw-make-c128vector bv) c128vector?
  (bv c128vector-body))

