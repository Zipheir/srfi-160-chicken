;;;; Implementation of SRFI 160 base s16vector->list

(define s16vector->list
  (case-lambda
    ((vec) (%s16vector->list vec))
    ((vec start) (%s16vector->list (subs16vector vec start (s16vector-length vec))))
    ((vec start end) (%s16vector->list (subs16vector vec start end)))))
