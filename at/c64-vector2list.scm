;;;; Implementation of SRFI 160 base c64vector->list

(define c64vector->list
  (case-lambda
    ((vec) (%c64vector->list vec))
    ((vec start) (%c64vector->list (subc64vector vec start (c64vector-length vec))))
    ((vec start end) (%c64vector->list (subc64vector vec start end)))))


