;;;; Implementation of SRFI 160 base u64vector->list

(define u64vector->list
  (case-lambda
    ((vec) (%u64vector->list vec))
    ((vec start) (%u64vector->list (subu64vector vec start (u64vector-length vec))))
    ((vec start end) (%u64vector->list (subu64vector vec start end)))))

