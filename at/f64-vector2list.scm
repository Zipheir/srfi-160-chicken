;;;; Implementation of SRFI 160 base f64vector->list

(define f64vector->list
  (case-lambda
    ((vec) (%f64vector->list vec))
    ((vec start) (%f64vector->list (subf64vector vec start (f64vector-length vec))))
    ((vec start end) (%f64vector->list (subf64vector vec start end)))))


