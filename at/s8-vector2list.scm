;;;; Implementation of SRFI 160 base s8vector->list

(define s8vector->list
  (case-lambda
    ((vec) (%s8vector->list vec))
    ((vec start) (%s8vector->list (subs8vector vec start (s8vector-length vec))))
    ((vec start end) (%s8vector->list (subs8vector vec start end)))))


