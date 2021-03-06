;;;; Implementation of SRFI 160 base u8vector->list

(define u8vector->list
  (case-lambda
    ((vec) (%u8vector->list vec))
    ((vec start) (%u8vector->list (subu8vector vec start (u8vector-length vec))))
    ((vec start end) (%u8vector->list (subu8vector vec start end)))))


