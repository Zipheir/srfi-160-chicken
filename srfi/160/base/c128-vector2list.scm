;;;; Implementation of SRFI 160 base c128vector->list

(define c128vector->list
  (case-lambda
    ((vec) (c128vector->list* vec 0 (c128vector-length vec)))
    ((vec start) (c128vector->list* vec start (c128vector-length vec)))
    ((vec start end) (c128vector->list* vec start end))))

(define (c128vector->list* vec start end)
  (let loop ((i (fx- end 1))
             (list '()))
    (if (fx< i start)
      list
      (loop (fx- i 1) (cons (c128vector-ref vec i) list)))))

