;;;; Implementation of SRFI 160 base s32vector->list

(define s32vector->list
  (case-lambda
    ((vec) (%s32vector->list vec))
    ((vec start) (%s32vector->list (subs32vector vec start (s32vector-length vec))))
    ((vec start end) (%s32vector->list (subs32vector vec start end)))))
