;; The representation of complex vectors

(define-record-type c64vector (raw-make-c64vector bv) c64vector?
  (bv c64vector-body))

(set! (record-printer 'srfi.160.base#c64vector)
  (lambda (v out)
    (display "#!c64" out)
    (display (c64vector->list v))))

(set-sharp-read-syntax! 'c64
  (lambda (p)
    (list->c64vector (read p))))

(define-record-type c128vector (raw-make-c128vector bv) c128vector?
  (bv c128vector-body))

(set! (record-printer 'srfi.160.base#c128vector)
  (lambda (v out)
    (display "#!c128" out)
    (display (c128vector->list v))))

(set-sharp-read-syntax! 'c128
  (lambda (p)
    (list->c128vector (read p))))

