;;; This code is the same for all SRFI 160 vector sizes.
;;; The f64s appearing in the code are expanded to u8, s8, etc.

;; make-f64vector defined in (srfi 160 base)

;; f64vector defined in (srfi 160 base)

(define (f64vector-unfold f len seed)
  (let ((v (make-f64vector len)))
    (let loop ((i 0) (state seed))
      (unless (fx= i len)
        (let-values (((value newstate) (f i state)))
          (f64vector-set! v i value)
          (loop (fx+ i 1) newstate))))
    v))

(define (f64vector-unfold-right f len seed)
  (let ((v (make-f64vector len)))
    (let loop ((i (fx- len 1)) (state seed))
      (unless (fx= i -1)
        (let-values (((value newstate) (f i state)))
          (f64vector-set! v i value)
          (loop (fx- i 1) newstate))))
    v))

(define f64vector-copy
  (case-lambda
    ((vec) (f64vector-copy* vec 0 (f64vector-length vec)))
    ((vec start) (f64vector-copy* vec start (f64vector-length vec)))
    ((vec start end) (f64vector-copy* vec start end))))

(define (f64vector-copy* vec start end)
  (let ((v (make-f64vector (fx- end start))))
    (f64vector-copy! v 0 vec start end)
    v))

(define f64vector-copy!
  (case-lambda
    ((to at from)
     (move-memory! from to (f64vector-length from) 0 (fx* at 8)))
    ((to at from start)
     (move-memory! from to (f64vector-length from) (fx* start 8) (fx* at 8)))
    ((to at from start end)
     (move-memory! from to
                   (fx* 8 (fx- end start))
                   (fx* start 8)
                   (fx* at 8)))))

(define f64vector-reverse-copy
  (case-lambda
    ((vec) (f64vector-reverse-copy* vec 0 (f64vector-length vec)))
    ((vec start) (f64vector-reverse-copy* vec start (f64vector-length vec)))
    ((vec start end) (f64vector-reverse-copy* vec start end))))

(define (f64vector-reverse-copy* vec start end)
  (let ((v (make-f64vector (fx- end start))))
    (f64vector-reverse-copy! v 0 vec start end)
    v))

(define f64vector-reverse-copy!
  (case-lambda
    ((to at from)
     (f64vector-reverse-copy!* to at from 0 (f64vector-length from)))
    ((to at from start)
     (f64vector-reverse-copy!* to at from start (f64vector-length from)))
    ((to at from start end) (f64vector-reverse-copy!* to at from start end))))

(define (f64vector-reverse-copy!* to at from start end)
  (let loop ((at at) (i (fx- end 1)))
    (unless (fx< i start)
      (f64vector-set! to at (f64vector-ref from i))
      (loop (fx+ at 1) (fx- i 1)))))

(define (f64vector-append . vecs)
  (f64vector-concatenate vecs))

(define (f64vector-concatenate vecs)
  (let ((v (make-f64vector (len-sum vecs))))
    (let loop ((vecs vecs) (at 0))
      (unless (null? vecs)
        (let ((vec (car vecs)))
          (f64vector-copy! v at vec 0 (f64vector-length vec))
          (loop (cdr vecs) (fx+ at (f64vector-length vec)))))
    v)))

(define (len-sum vecs)
  (if (null? vecs)
    0
    (fx+ (f64vector-length (car vecs))
       (len-sum (cdr vecs)))))

(define (f64vector-append-subvectors . args)
  (let ((v (make-f64vector (len-subsum args))))
    (let loop ((args args) (at 0))
      (unless (null? args)
        (let ((vec (car args))
              (start (cadr args))
              (end (caddr args)))
          (f64vector-copy! v at vec start end)
          (loop (cdddr args) (fx+ at (fx- end start))))))
    v))

(define (len-subsum vecs)
  (if (null? vecs)
    0
    (fx+ (fx- (caddr vecs) (cadr vecs))
       (len-subsum (cdddr vecs)))))

;; f64? defined in (srfi 160 base)

;; f64vector? defined in (srfi 160 base)

(define (f64vector-empty? vec)
  (zero? (f64vector-length vec)))

(define (f64vector= . vecs)
  (f64vector=* (car vecs) (cadr vecs) (cddr vecs)))

(define (f64vector=* vec1 vec2 vecs)
  (and (f64dyadic-vecs= vec1 0 (f64vector-length vec1)
                      vec2 0 (f64vector-length vec2))
       (or (null? vecs)
           (f64vector=* vec2 (car vecs) (cdr vecs)))))

(define (f64dyadic-vecs= vec1 start1 end1 vec2 start2 end2)
  (cond
    ((not (fx= end1 end2)) #f)
    ((not (fx< start1 end1)) #t)
    ((let ((elt1 (f64vector-ref vec1 start1))
           (elt2 (f64vector-ref vec2 start2)))
      (= elt1 elt2))
     (f64dyadic-vecs= vec1 (fx+ start1 1) end1
                         vec2 (fx+ start2 1) end2))
    (else #f)))

;; f64vector-ref defined in (srfi 160 base)

;; f64vector-length defined in (srfi 160 base)

(define (f64vector-take vec n)
  (let ((v (make-f64vector n)))
    (f64vector-copy! v 0 vec 0 n)
    v))

(define (f64vector-take-right vec n)
  (let ((v (make-f64vector n))
        (len (f64vector-length vec)))
    (f64vector-copy! v 0 vec (fx- len n) len)
    v))

(define (f64vector-drop vec n)
 (let* ((len (f64vector-length vec))
        (vlen (fx- len n))
        (v (make-f64vector vlen)))
    (f64vector-copy! v 0 vec n len)
    v))

(define (f64vector-drop-right vec n)
  (let* ((len (f64vector-length vec))
         (rlen (fx- len n))
         (v (make-f64vector rlen)))
    (f64vector-copy! v 0 vec 0 rlen)
    v))

(define (f64vector-segment vec n)
  (let loop ((r '()) (i 0) (remain (f64vector-length vec)))
    (if (fx<= remain 0)
      (reverse r)
      (let ((size (min n remain)))
        (loop
          (cons (f64vector-copy vec i (fx+ i size)) r)
          (fx+ i size)
          (fx- remain size))))))

;; aux. procedure
(define (%f64vectors-ref vecs i)
  (map (lambda (v) (f64vector-ref v i)) vecs))

(define (f64vector-fold kons knil vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (f64vector-length vec)))
      (let loop ((r knil) (i 0))
        (if (fx= i len)
          r
          (loop (kons r (f64vector-ref vec i)) (fx+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map f64vector-length vecs))))
      (let loop ((r knil) (i 0))
        (if (fx= i len)
          r
          (loop (apply kons r (%f64vectors-ref vecs i))
                (fx+ i 1)))))))

(define (f64vector-fold-right kons knil vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (f64vector-length vec)))
      (let loop ((r knil) (i (fx- (f64vector-length vec) 1)))
        (if (negative? i)
          r
          (loop (kons r (f64vector-ref vec i)) (fx- i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map f64vector-length vecs))))
      (let loop ((r knil) (i (fx- len 1)))
        (if (negative? i)
          r
          (loop (apply kons r (%f64vectors-ref vecs i))
                (fx- i 1)))))))

(define (f64vector-map f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let* ((len (f64vector-length vec))
           (v (make-f64vector len)))
      (let loop ((i 0))
        (unless (fx= i len)
          (f64vector-set! v i (f (f64vector-ref vec i)))
          (loop (fx+ i 1))))
      v)
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map f64vector-length vecs)))
           (v (make-f64vector len)))
      (let loop ((i 0))
        (unless (fx= i len)
          (f64vector-set! v i (apply f (%f64vectors-ref vecs i)))
          (loop (fx+ i 1))))
      v)))


(define (f64vector-map! f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (f64vector-length vec)))
      (let loop ((i 0))
        (unless (fx= i len)
          (f64vector-set! vec i (f (f64vector-ref vec i)))
          (loop (fx+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map f64vector-length vecs))))
      (let loop ((i 0))
        (unless (fx= i len)
          (f64vector-set! vec i (apply f (%f64vectors-ref vecs i)))
          (loop (fx+ i 1)))))))

(define (f64vector-for-each f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (f64vector-length vec)))
      (let loop ((i 0))
        (unless (fx= i len)
          (f (f64vector-ref vec i))
          (loop (fx+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map f64vector-length vecs))))
      (let loop ((i 0))
        (unless (fx= i len)
          (apply f (%f64vectors-ref vecs i))
          (loop (fx+ i 1)))))))

(define (f64vector-count pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (f64vector-length vec)))
      (let loop ((i 0) (r 0))
        (cond
         ((fx= i (f64vector-length vec)) r)
         ((pred (f64vector-ref vec i)) (loop (fx+ i 1) (fx+ r 1)))
         (else (loop (fx+ i 1) r)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map f64vector-length vecs))))
      (let loop ((i 0) (r 0))
        (cond
         ((fx= i len) r)
         ((apply pred (%f64vectors-ref vecs i)) (loop (fx+ i 1) (fx+ r 1)))
         (else (loop (fx+ i 1) r)))))))

(define (f64vector-cumulate f knil vec)
  (let* ((len (f64vector-length vec))
         (v (make-f64vector len)))
    (let loop ((r knil) (i 0))
      (unless (fx= i len)
        (let ((next (f r (f64vector-ref vec i))))
          (f64vector-set! v i next)
          (loop next (fx+ i 1)))))
    v))

(define (f64vector-foreach f vec)
  (let ((len (f64vector-length vec)))
    (let loop ((i 0))
      (unless (fx= i len)
        (f (f64vector-ref vec i))
        (loop (fx+ i 1))))))

(define (f64vector-take-while pred vec)
  (let* ((len (f64vector-length vec))
         (idx (f64vector-skip pred vec))
         (idx* (if idx idx len)))
    (f64vector-copy vec 0 idx*)))

(define (f64vector-take-while-right pred vec)
  (let* ((len (f64vector-length vec))
         (idx (f64vector-skip-right pred vec))
         (idx* (if idx (fx+ idx 1) 0)))
    (f64vector-copy vec idx* len)))

(define (f64vector-drop-while pred vec)
  (let* ((len (f64vector-length vec))
         (idx (f64vector-skip pred vec))
         (idx* (if idx idx len)))
    (f64vector-copy vec idx* len)))

(define (f64vector-drop-while-right pred vec)
  (let* ((len (f64vector-length vec))
         (idx (f64vector-skip-right pred vec))
         (idx* (if idx idx -1)))
    (f64vector-copy vec 0 (fx+ 1 idx*))))

(define (f64vector-index pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (f64vector-length vec)))
      (let loop ((i 0))
        (cond
         ((fx= i len) #f)
         ((pred (f64vector-ref vec i)) i)
         (else (loop (fx+ i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map f64vector-length vecs))))
      (let loop ((i 0))
        (cond
         ((fx= i len) #f)
         ((apply pred (%f64vectors-ref vecs i)) i)
         (else (loop (fx+ i 1))))))))

(define (f64vector-index-right pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (f64vector-length vec)))
      (let loop ((i (fx- len 1)))
        (cond
         ((negative? i) #f)
         ((pred (f64vector-ref vec i)) i)
         (else (loop (fx- i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map f64vector-length vecs))))
      (let loop ((i (fx- len 1)))
        (cond
         ((negative? i) #f)
         ((apply pred (%f64vectors-ref vecs i)) i)
         (else (loop (fx- i 1))))))))

(define (f64vector-skip pred vec . vecs)
  (if (null? vecs)
    (f64vector-index (lambda (x) (not (pred x))) vec)
    (apply f64vector-index (lambda xs (not (apply pred xs))) vec vecs)))

(define (f64vector-skip-right pred vec . vecs)
  (if (null? vecs)
    (f64vector-index-right (lambda (x) (not (pred x))) vec)
    (apply f64vector-index-right (lambda xs (not (apply pred xs))) vec vecs)))

(define (f64vector-any pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (f64vector-length vec)))
      (let loop ((i 0))
        (cond
         ((fx= i len) #f)
         ((pred (f64vector-ref vec i)))  ;returns result of pred
         (else (loop (fx+ i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map f64vector-length vecs))))
      (let loop ((i 0))
        (cond
         ((fx= i len) #f)
         ((apply pred (%f64vectors-ref vecs i))) ;returns result of pred
         (else (loop (fx+ i 1))))))))

(define (f64vector-every pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (f64vector-length vec)))
      (let loop ((i 0) (last #t))
        (cond
         ((fx= i len) last)
         ((pred (f64vector-ref vec i)) => (lambda (r) (loop (fx+ i 1) r)))
         (else #f))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map f64vector-length vecs))))
      (let loop ((i 0) (last #t))
        (cond
         ((fx= i len) last)
         ((apply pred (%f64vectors-ref vecs i)) => (lambda (r) (loop (fx+ i 1) r)))
         (else #f))))))

(define (f64vector-partition pred vec)
  (let* ((len (f64vector-length vec))
         (cnt (f64vector-count pred vec))
         (r (make-f64vector len)))
    (let loop ((i 0) (yes 0) (no cnt))
      (cond
        ((fx= i len) r)
        ((pred (f64vector-ref vec i))
         (f64vector-set! r yes (f64vector-ref vec i))
         (loop (fx+ i 1) (fx+ yes 1) no))
        (else
         (f64vector-set! r no (f64vector-ref vec i))
         (loop (fx+ i 1) yes (fx+ no 1)))))))

(define (f64vector-filter pred vec)
  (let* ((len (f64vector-length vec))
         (cnt (f64vector-count pred vec))
         (r (make-f64vector cnt)))
    (let loop ((i 0) (j 0))
      (cond
        ((fx= i len) r)
        ((pred (f64vector-ref vec i))
         (f64vector-set! r j (f64vector-ref vec i))
         (loop (fx+ i 1) (fx+ j 1)))
        (else
         (loop (fx+ i 1) j))))))

(define (f64vector-remove pred vec)
  (f64vector-filter (lambda (x) (not (pred x))) vec))

;; f64vector-set! defined in (srfi 160 base)

(define (f64vector-swap! vec i j)
  (let ((ival (f64vector-ref vec i))
        (jval (f64vector-ref vec j)))
    (f64vector-set! vec i jval)
    (f64vector-set! vec j ival)))

(define f64vector-fill!
  (case-lambda
    ((vec fill) (f64vector-fill-some! vec fill 0 (f64vector-length vec)))
    ((vec fill start) (f64vector-fill-some! vec fill start (f64vector-length vec)))
    ((vec fill start end) (f64vector-fill-some! vec fill start end))))

(define (f64vector-fill-some! vec fill start end)
  (unless (fx= start end)
    (f64vector-set! vec start fill)
    (f64vector-fill-some! vec fill (fx+ start 1) end)))

(define f64vector-reverse!
  (case-lambda
    ((vec) (f64vector-reverse-some! vec 0 (f64vector-length vec)))
    ((vec start) (f64vector-reverse-some! vec start (f64vector-length vec)))
    ((vec start end) (f64vector-reverse-some! vec start end))))

(define (f64vector-reverse-some! vec start end)
  (let loop ((i start) (j (fx- end 1)))
    (when (fx< i j)
      (f64vector-swap! vec i j)
      (loop (fx+ i 1) (fx- j 1)))))

(define (f64vector-unfold! f vec start end seed)
  (let loop ((i start) (seed seed))
    (when (fx< i end)
      (let-values (((elt seed) (f seed)))
        (f64vector-set! vec i elt)
        (loop (fx+ i 1) seed)))))

(define (f64vector-unfold-right! f vec start end seed)
  (let loop ((i (fx- end 1)) (seed seed))
    (when (fx>= i start)
      (let-values (((elt seed) (f seed)))
        (f64vector-set! vec i elt)
        (loop (fx- i 1) seed)))))

(define reverse-f64vector->list
  (case-lambda
    ((vec) (reverse-f64vector->list* vec 0 (f64vector-length vec)))
    ((vec start) (reverse-f64vector->list* vec start (f64vector-length vec)))
    ((vec start end) (reverse-f64vector->list* vec start end))))

(define (reverse-f64vector->list* vec start end)
  (let loop ((i start) (r '()))
    (if (fx= i end)
      r
      (loop (fx+ 1 i) (cons (f64vector-ref vec i) r)))))

(define (reverse-list->f64vector list)
  (let* ((len (length list))
         (r (make-f64vector len)))
    (let loop ((i 0) (list list))
      (cond
        ((fx= i len) r)
        (else
          (f64vector-set! r (fx- (fx- len i) 1) (car list))
          (loop (fx+ i 1) (cdr list)))))))

(define f64vector->vector
  (case-lambda
    ((vec) (f64vector->vector* vec 0 (f64vector-length vec)))
    ((vec start) (f64vector->vector* vec start (f64vector-length vec)))
    ((vec start end) (f64vector->vector* vec start end))))

(define (f64vector->vector* vec start end)
  (let* ((len (fx- end start))
         (r (make-vector len)))
    (let loop ((i start) (o 0))
      (cond
        ((fx= i end) r)
        (else
          (vector-set! r o (f64vector-ref vec i))
          (loop (fx+ i 1) (fx+ o 1)))))))

(define vector->f64vector
  (case-lambda
    ((vec) (vector->f64vector* vec 0 (vector-length vec)))
    ((vec start) (vector->f64vector* vec start (vector-length vec)))
    ((vec start end) (vector->f64vector* vec start end))))

(define (vector->f64vector* vec start end)
  (let* ((len (fx- end start))
         (r (make-f64vector len)))
    (let loop ((i start) (o 0))
      (cond
        ((fx= i end) r)
        (else
          (f64vector-set! r o (vector-ref vec i))
          (loop (fx+ i 1) (fx+ o 1)))))))

(define make-f64vector-generator
  (case-lambda ((vec) (make-f64vector-generator vec 0 (f64vector-length vec)))
               ((vec start) (make-f64vector-generator vec start (f64vector-length vec)))
               ((vec start end)
                (lambda () (if (fx>= start end)
                             (eof-object)
                             (let ((next (f64vector-ref vec start)))
                              (set! start (fx+ start 1))
                              next))))))

(define write-f64vector
  (case-lambda
    ((vec) (write-f64vector* vec (current-output-port)))
    ((vec port) (write-f64vector* vec port))))


(define (write-f64vector* vec port)
  (display "#f64(" port)  ; f64-expansion is blind, so will expand this too
  (let ((last (fx- (f64vector-length vec) 1)))
    (let loop ((i 0))
      (cond
        ((fx= i last)
         (write (f64vector-ref vec i) port)
         (display ")" port))
        (else
          (write (f64vector-ref vec i) port)
          (display " " port)
          (loop (fx+ i 1)))))))

(define (f64vector< vec1 vec2)
  (let ((len1 (f64vector-length vec1))
        (len2 (f64vector-length vec2)))
    (cond
      ((fx< len1 len2)
       #t)
      ((fx> len1 len2)
       #f)
      (else
       (let loop ((i 0))
         (cond
           ((fx= i len1)
            #f)
           ((< (f64vector-ref vec1 i) (f64vector-ref vec2 i))
            #t)
           ((> (f64vector-ref vec1 i) (f64vector-ref vec2 i))
            #f)
           (else
             (loop (fx+ i 1)))))))))

(define (f64vector-hash vec)
  (let ((len (min 256 (f64vector-length vec))))
    (let loop ((i 0) (r 0))
      (if (fx= i len)
        (abs (floor (real-part (inexact->exact r))))
        (loop (fx+ i 1) (+ r (f64vector-ref vec i)))))))

(define f64vector-comparator
  (make-comparator f64vector? f64vector= f64vector< f64vector-hash))
