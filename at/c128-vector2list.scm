;;;; Implementation of SRFI 160 base c128vector->list

(define c128vector->list
  (case-lambda
    ((vec) (%c128vector->list vec))
    ((vec start) (%c128vector->list (subc128vector vec start (c128vector-length vec))))
    ((vec start end) (%c128vector->list (subc128vector vec start end)))))


