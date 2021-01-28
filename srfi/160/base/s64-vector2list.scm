;;;; Implementation of SRFI 160 base s64vector->list

(define s64vector->list
  (case-lambda
    ((vec) (%s64vector->list vec))
    ((vec start) (%s64vector->list (subs64vector vec start (s64vector-length vec))))
    ((vec start end) (%s64vector->list (subs64vector vec start end)))))
