;;;; Implementation of SRFI 160 base c64vectors and c128vectors

;;; Main constructor

(define (make-c64vector len . maybe-fill)
  (assert (fixnum? len) 'make-c64vector "bad argument type - not a fixnum" len)
  (define vec (raw-make-c64vector (make-f32vector (fx* len 2))))
  (if (not (null? maybe-fill))
    (c64vector-simple-fill! vec (car maybe-fill)))
  vec)

(define (make-c128vector len . maybe-fill)
  (assert (fixnum? len) 'make-c128vector "bad argument type - not a fixnum" len)
  (define vec (raw-make-c128vector (make-f64vector (fx* len 2))))
  (if (not (null? maybe-fill))
    (c128vector-simple-fill! vec (car maybe-fill)))
  vec)

;; Simple fill! (not exported)

(define (c64vector-simple-fill! vec value)
  (define len (c64vector-length vec))
  (let loop ((i 0))
    (if (fx= i len)
      vec
      (begin
        (c64vector-set! vec i value)
        (loop (fx+ i 1))))))

(define (c128vector-simple-fill! vec value)
  (define len (c128vector-length vec))
  (let loop ((i 0))
    (if (fx= i len)
      vec
      (begin
        (c128vector-set! vec i value)
        (loop (fx+ i 1))))))

;;; Variable-argument constructor

(define (c64vector . list)
  (list->c64vector list))

(define (c128vector . list)
  (list->c128vector list))

;; Predicate already defined

;; Length

(define (c64vector-length vec)
  (fx/ (f32vector-length (c64vector-body vec)) 2))

(define (c128vector-length vec)
  (fx/ (f64vector-length (c128vector-body vec)) 2))

;; Get element

(define (c64vector-ref vec i)
  (let ((fvec (c64vector-body vec))
        (j (fx* i 2)))
    (make-rectangular
      (f32vector-ref fvec j)
      (f32vector-ref fvec (fx+ j 1)))))

(define (c128vector-ref vec i)
  (let ((fvec (c128vector-body vec))
        (j (fx* i 2)))
    (make-rectangular
      (f64vector-ref fvec j)
      (f64vector-ref fvec (fx+ j 1)))))

;; Set element

(define (c64vector-set! vec i value)
  (let ((fvec (c64vector-body vec))
        (j (fx* i 2)))
    (f32vector-set! fvec j (real-part value))
    (f32vector-set! fvec (fx+ j 1) (imag-part value))))

(define (c128vector-set! vec i value)
  (let ((fvec (c128vector-body vec))
        (j (fx* i 2)))
    (f64vector-set! fvec j (real-part value))
    (f64vector-set! fvec (fx+ j 1) (imag-part value))))

;; List to vec

(define (list->c64vector list)
  (define len (length list))
  (define vec (make-c64vector len))
  (let loop ((i 0) (list list))
    (if (fx= i len)
      vec
      (begin
        (c64vector-set! vec i (car list))
        (loop (fx+ i 1) (cdr list))))))

(define (list->c128vector list)
  (define len (length list))
  (define vec (make-c128vector len))
  (let loop ((i 0) (list list))
    (if (fx= i len)
      vec
      (begin
        (c128vector-set! vec i (car list))
        (loop (fx+ i 1) (cdr list))))))

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

(define c64vector->list
  (case-lambda
    ((vec) (c64vector->list* vec 0 (c64vector-length vec)))
    ((vec start) (c64vector->list* vec start (c64vector-length vec)))
    ((vec start end) (c64vector->list* vec start end))))

(define (c64vector->list* vec start end)
  (let loop ((i (fx- end 1))
             (list '()))
    (if (fx< i start)
      list
      (loop (fx- i 1) (cons (c64vector-ref vec i) list)))))
