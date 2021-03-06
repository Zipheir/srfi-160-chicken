;;;; Implementation of SRFI 160 base f32vector->list

(define f32vector->list
  (case-lambda
    ((vec) (%f32vector->list vec))
    ((vec start) (%f32vector->list (subf32vector vec start (f32vector-length vec))))
    ((vec start end) (%f32vector->list (subf32vector vec start end)))))


