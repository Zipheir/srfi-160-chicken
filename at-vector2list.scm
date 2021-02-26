;;;; Implementation of SRFI 160 base @vector->list

(define @vector->list
  (case-lambda
    ((vec) (%@vector->list vec))
    ((vec start) (%@vector->list (sub@vector vec start (@vector-length vec))))
    ((vec start end) (%@vector->list (sub@vector vec start end)))))


