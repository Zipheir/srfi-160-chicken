;;;; Implementation of SRFI 160 base u32vector->list

(define u32vector->list
  (case-lambda
    ((vec) (%u32vector->list vec))
    ((vec start) (%u32vector->list (subu32vector vec start (u32vector-length vec))))
    ((vec start end) (%u32vector->list (subu32vector vec start end)))))
