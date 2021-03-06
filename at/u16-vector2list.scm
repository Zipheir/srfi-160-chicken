;;;; Implementation of SRFI 160 base u16vector->list

(define u16vector->list
  (case-lambda
    ((vec) (%u16vector->list vec))
    ((vec start) (%u16vector->list (subu16vector vec start (u16vector-length vec))))
    ((vec start end) (%u16vector->list (subu16vector vec start end)))))


