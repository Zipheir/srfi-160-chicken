;;; This code is the same for all SRFI 160 vector sizes.
;;; The f32s appearing in the code are expanded to u8, s8, etc.

;; make-f32vector defined in (srfi 160 base)

;; f32vector defined in (srfi 160 base)

(define (f32vector-unfold f len seed)
  (let ((v (make-f32vector len)))
    (let loop ((i 0) (state seed))
      (unless (fx= i len)
        (let-values (((value newstate) (f i state)))
          (f32vector-set! v i value)
          (loop (fx+ i 1) newstate))))
    v))

(define (f32vector-unfold-right f len seed)
  (let ((v (make-f32vector len)))
    (let loop ((i (fx- len 1)) (state seed))
      (unless (fx= i -1)
        (let-values (((value newstate) (f i state)))
          (f32vector-set! v i value)
          (loop (fx- i 1) newstate))))
    v))

(define f32vector-copy
  (case-lambda
    ((vec) (f32vector-copy* vec 0 (f32vector-length vec)))
    ((vec start) (f32vector-copy* vec start (f32vector-length vec)))
    ((vec start end) (f32vector-copy* vec start end))))

(define (f32vector-copy* vec start end)
  (let ((v (make-f32vector (fx- end start))))
    (f32vector-copy! v 0 vec start end)
    v))

(define f32vector-copy!
  (case-lambda
    ((to at from)
     (move-memory! from to (f32vector-length from) 0 (fx* at 4)))
    ((to at from start)
     (move-memory! from to (f32vector-length from) (fx* start 4) (fx* at 4)))
    ((to at from start end)
     (move-memory! from to
                   (fx* 4 (fx- end start))
                   (fx* start 4)
                   (fx* at 4)))))

(define f32vector-reverse-copy
  (case-lambda
    ((vec) (f32vector-reverse-copy* vec 0 (f32vector-length vec)))
    ((vec start) (f32vector-reverse-copy* vec start (f32vector-length vec)))
    ((vec start end) (f32vector-reverse-copy* vec start end))))

(define (f32vector-reverse-copy* vec start end)
  (let ((v (make-f32vector (fx- end start))))
    (f32vector-reverse-copy! v 0 vec start end)
    v))

(define f32vector-reverse-copy!
  (case-lambda
    ((to at from)
     (f32vector-reverse-copy!* to at from 0 (f32vector-length from)))
    ((to at from start)
     (f32vector-reverse-copy!* to at from start (f32vector-length from)))
    ((to at from start end) (f32vector-reverse-copy!* to at from start end))))

(define (f32vector-reverse-copy!* to at from start end)
  (let loop ((at at) (i (fx- end 1)))
    (unless (fx< i start)
      (f32vector-set! to at (f32vector-ref from i))
      (loop (fx+ at 1) (fx- i 1)))))

(define (f32vector-append . vecs)
  (f32vector-concatenate vecs))

(define (f32vector-concatenate vecs)
  (let ((v (make-f32vector (len-sum vecs))))
    (let loop ((vecs vecs) (at 0))
      (unless (null? vecs)
        (let ((vec (car vecs)))
          (f32vector-copy! v at vec 0 (f32vector-length vec))
          (loop (cdr vecs) (fx+ at (f32vector-length vec)))))
    v)))

(define (len-sum vecs)
  (if (null? vecs)
    0
    (fx+ (f32vector-length (car vecs))
       (len-sum (cdr vecs)))))

(define (f32vector-append-subvectors . args)
  (let ((v (make-f32vector (len-subsum args))))
    (let loop ((args args) (at 0))
      (unless (null? args)
        (let ((vec (car args))
              (start (cadr args))
              (end (caddr args)))
          (f32vector-copy! v at vec start end)
          (loop (cdddr args) (fx+ at (fx- end start))))))
    v))

(define (len-subsum vecs)
  (if (null? vecs)
    0
    (fx+ (fx- (caddr vecs) (cadr vecs))
       (len-subsum (cdddr vecs)))))

;; f32? defined in (srfi 160 base)

;; f32vector? defined in (srfi 160 base)

(define (f32vector-empty? vec)
  (zero? (f32vector-length vec)))

(define (f32vector= . vecs)
  (f32vector=* (car vecs) (cadr vecs) (cddr vecs)))

(define (f32vector=* vec1 vec2 vecs)
  (and (f32dyadic-vecs= vec1 0 (f32vector-length vec1)
                      vec2 0 (f32vector-length vec2))
       (or (null? vecs)
           (f32vector=* vec2 (car vecs) (cdr vecs)))))

(define (f32dyadic-vecs= vec1 start1 end1 vec2 start2 end2)
  (cond
    ((not (fx= end1 end2)) #f)
    ((not (fx< start1 end1)) #t)
    ((let ((elt1 (f32vector-ref vec1 start1))
           (elt2 (f32vector-ref vec2 start2)))
      (= elt1 elt2))
     (f32dyadic-vecs= vec1 (fx+ start1 1) end1
                         vec2 (fx+ start2 1) end2))
    (else #f)))

;; f32vector-ref defined in (srfi 160 base)

;; f32vector-length defined in (srfi 160 base)

(define (f32vector-take vec n)
  (let ((v (make-f32vector n)))
    (f32vector-copy! v 0 vec 0 n)
    v))

(define (f32vector-take-right vec n)
  (let ((v (make-f32vector n))
        (len (f32vector-length vec)))
    (f32vector-copy! v 0 vec (fx- len n) len)
    v))

(define (f32vector-drop vec n)
 (let* ((len (f32vector-length vec))
        (vlen (fx- len n))
        (v (make-f32vector vlen)))
    (f32vector-copy! v 0 vec n len)
    v))

(define (f32vector-drop-right vec n)
  (let* ((len (f32vector-length vec))
         (rlen (fx- len n))
         (v (make-f32vector rlen)))
    (f32vector-copy! v 0 vec 0 rlen)
    v))

(define (f32vector-segment vec n)
  (let loop ((r '()) (i 0) (remain (f32vector-length vec)))
    (if (fx<= remain 0)
      (reverse r)
      (let ((size (min n remain)))
        (loop
          (cons (f32vector-copy vec i (fx+ i size)) r)
          (fx+ i size)
          (fx- remain size))))))

;; aux. procedure
(define (%f32vectors-ref vecs i)
  (map (lambda (v) (f32vector-ref v i)) vecs))

(define (f32vector-fold kons knil vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (f32vector-length vec)))
      (let loop ((r knil) (i 0))
        (if (fx= i len)
          r
          (loop (kons r (f32vector-ref vec i)) (fx+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map f32vector-length vecs))))
      (let loop ((r knil) (i 0))
        (if (fx= i len)
          r
          (loop (apply kons r (%f32vectors-ref vecs i))
                (fx+ i 1)))))))

(define (f32vector-fold-right kons knil vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (f32vector-length vec)))
      (let loop ((r knil) (i (fx- (f32vector-length vec) 1)))
        (if (negative? i)
          r
          (loop (kons r (f32vector-ref vec i)) (fx- i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map f32vector-length vecs))))
      (let loop ((r knil) (i (fx- len 1)))
        (if (negative? i)
          r
          (loop (apply kons r (%f32vectors-ref vecs i))
                (fx- i 1)))))))

(define (f32vector-map f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let* ((len (f32vector-length vec))
           (v (make-f32vector len)))
      (let loop ((i 0))
        (unless (fx= i len)
          (f32vector-set! v i (f (f32vector-ref vec i)))
          (loop (fx+ i 1))))
      v)
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map f32vector-length vecs)))
           (v (make-f32vector len)))
      (let loop ((i 0))
        (unless (fx= i len)
          (f32vector-set! v i (apply f (%f32vectors-ref vecs i)))
          (loop (fx+ i 1))))
      v)))


(define (f32vector-map! f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (f32vector-length vec)))
      (let loop ((i 0))
        (unless (fx= i len)
          (f32vector-set! vec i (f (f32vector-ref vec i)))
          (loop (fx+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map f32vector-length vecs))))
      (let loop ((i 0))
        (unless (fx= i len)
          (f32vector-set! vec i (apply f (%f32vectors-ref vecs i)))
          (loop (fx+ i 1)))))))

(define (f32vector-for-each f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (f32vector-length vec)))
      (let loop ((i 0))
        (unless (fx= i len)
          (f (f32vector-ref vec i))
          (loop (fx+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map f32vector-length vecs))))
      (let loop ((i 0))
        (unless (fx= i len)
          (apply f (%f32vectors-ref vecs i))
          (loop (fx+ i 1)))))))

(define (f32vector-count pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (f32vector-length vec)))
      (let loop ((i 0) (r 0))
        (cond
         ((fx= i (f32vector-length vec)) r)
         ((pred (f32vector-ref vec i)) (loop (fx+ i 1) (fx+ r 1)))
         (else (loop (fx+ i 1) r)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map f32vector-length vecs))))
      (let loop ((i 0) (r 0))
        (cond
         ((fx= i len) r)
         ((apply pred (%f32vectors-ref vecs i)) (loop (fx+ i 1) (fx+ r 1)))
         (else (loop (fx+ i 1) r)))))))

(define (f32vector-cumulate f knil vec)
  (let* ((len (f32vector-length vec))
         (v (make-f32vector len)))
    (let loop ((r knil) (i 0))
      (unless (fx= i len)
        (let ((next (f r (f32vector-ref vec i))))
          (f32vector-set! v i next)
          (loop next (fx+ i 1)))))
    v))

(define (f32vector-foreach f vec)
  (let ((len (f32vector-length vec)))
    (let loop ((i 0))
      (unless (fx= i len)
        (f (f32vector-ref vec i))
        (loop (fx+ i 1))))))

(define (f32vector-take-while pred vec)
  (let* ((len (f32vector-length vec))
         (idx (f32vector-skip pred vec))
         (idx* (if idx idx len)))
    (f32vector-copy vec 0 idx*)))

(define (f32vector-take-while-right pred vec)
  (let* ((len (f32vector-length vec))
         (idx (f32vector-skip-right pred vec))
         (idx* (if idx (fx+ idx 1) 0)))
    (f32vector-copy vec idx* len)))

(define (f32vector-drop-while pred vec)
  (let* ((len (f32vector-length vec))
         (idx (f32vector-skip pred vec))
         (idx* (if idx idx len)))
    (f32vector-copy vec idx* len)))

(define (f32vector-drop-while-right pred vec)
  (let* ((len (f32vector-length vec))
         (idx (f32vector-skip-right pred vec))
         (idx* (if idx idx -1)))
    (f32vector-copy vec 0 (fx+ 1 idx*))))

(define (f32vector-index pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (f32vector-length vec)))
      (let loop ((i 0))
        (cond
         ((fx= i len) #f)
         ((pred (f32vector-ref vec i)) i)
         (else (loop (fx+ i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map f32vector-length vecs))))
      (let loop ((i 0))
        (cond
         ((fx= i len) #f)
         ((apply pred (%f32vectors-ref vecs i)) i)
         (else (loop (fx+ i 1))))))))

(define (f32vector-index-right pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (f32vector-length vec)))
      (let loop ((i (fx- len 1)))
        (cond
         ((negative? i) #f)
         ((pred (f32vector-ref vec i)) i)
         (else (loop (fx- i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map f32vector-length vecs))))
      (let loop ((i (fx- len 1)))
        (cond
         ((negative? i) #f)
         ((apply pred (%f32vectors-ref vecs i)) i)
         (else (loop (fx- i 1))))))))

(define (f32vector-skip pred vec . vecs)
  (if (null? vecs)
    (f32vector-index (lambda (x) (not (pred x))) vec)
    (apply f32vector-index (lambda xs (not (apply pred xs))) vec vecs)))

(define (f32vector-skip-right pred vec . vecs)
  (if (null? vecs)
    (f32vector-index-right (lambda (x) (not (pred x))) vec)
    (apply f32vector-index-right (lambda xs (not (apply pred xs))) vec vecs)))

(define (f32vector-any pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (f32vector-length vec)))
      (let loop ((i 0))
        (cond
         ((fx= i len) #f)
         ((pred (f32vector-ref vec i)))  ;returns result of pred
         (else (loop (fx+ i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map f32vector-length vecs))))
      (let loop ((i 0))
        (cond
         ((fx= i len) #f)
         ((apply pred (%f32vectors-ref vecs i))) ;returns result of pred
         (else (loop (fx+ i 1))))))))

(define (f32vector-every pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (f32vector-length vec)))
      (let loop ((i 0) (last #t))
        (cond
         ((fx= i len) last)
         ((pred (f32vector-ref vec i)) => (lambda (r) (loop (fx+ i 1) r)))
         (else #f))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map f32vector-length vecs))))
      (let loop ((i 0) (last #t))
        (cond
         ((fx= i len) last)
         ((apply pred (%f32vectors-ref vecs i)) => (lambda (r) (loop (fx+ i 1) r)))
         (else #f))))))

(define (f32vector-partition pred vec)
  (let* ((len (f32vector-length vec))
         (cnt (f32vector-count pred vec))
         (r (make-f32vector len)))
    (let loop ((i 0) (yes 0) (no cnt))
      (cond
        ((fx= i len) r)
        ((pred (f32vector-ref vec i))
         (f32vector-set! r yes (f32vector-ref vec i))
         (loop (fx+ i 1) (fx+ yes 1) no))
        (else
         (f32vector-set! r no (f32vector-ref vec i))
         (loop (fx+ i 1) yes (fx+ no 1)))))))

(define (f32vector-filter pred vec)
  (let* ((len (f32vector-length vec))
         (cnt (f32vector-count pred vec))
         (r (make-f32vector cnt)))
    (let loop ((i 0) (j 0))
      (cond
        ((fx= i len) r)
        ((pred (f32vector-ref vec i))
         (f32vector-set! r j (f32vector-ref vec i))
         (loop (fx+ i 1) (fx+ j 1)))
        (else
         (loop (fx+ i 1) j))))))

(define (f32vector-remove pred vec)
  (f32vector-filter (lambda (x) (not (pred x))) vec))

;; f32vector-set! defined in (srfi 160 base)

(define (f32vector-swap! vec i j)
  (let ((ival (f32vector-ref vec i))
        (jval (f32vector-ref vec j)))
    (f32vector-set! vec i jval)
    (f32vector-set! vec j ival)))

(define f32vector-fill!
  (case-lambda
    ((vec fill) (f32vector-fill-some! vec fill 0 (f32vector-length vec)))
    ((vec fill start) (f32vector-fill-some! vec fill start (f32vector-length vec)))
    ((vec fill start end) (f32vector-fill-some! vec fill start end))))

(define (f32vector-fill-some! vec fill start end)
  (unless (fx= start end)
    (f32vector-set! vec start fill)
    (f32vector-fill-some! vec fill (fx+ start 1) end)))

(define f32vector-reverse!
  (case-lambda
    ((vec) (f32vector-reverse-some! vec 0 (f32vector-length vec)))
    ((vec start) (f32vector-reverse-some! vec start (f32vector-length vec)))
    ((vec start end) (f32vector-reverse-some! vec start end))))

(define (f32vector-reverse-some! vec start end)
  (let loop ((i start) (j (fx- end 1)))
    (when (fx< i j)
      (f32vector-swap! vec i j)
      (loop (fx+ i 1) (fx- j 1)))))

(define (f32vector-unfold! f vec start end seed)
  (let loop ((i start) (seed seed))
    (when (fx< i end)
      (let-values (((elt seed) (f seed)))
        (f32vector-set! vec i elt)
        (loop (fx+ i 1) seed)))))

(define (f32vector-unfold-right! f vec start end seed)
  (let loop ((i (fx- end 1)) (seed seed))
    (when (fx>= i start)
      (let-values (((elt seed) (f seed)))
        (f32vector-set! vec i elt)
        (loop (fx- i 1) seed)))))

(define reverse-f32vector->list
  (case-lambda
    ((vec) (reverse-f32vector->list* vec 0 (f32vector-length vec)))
    ((vec start) (reverse-f32vector->list* vec start (f32vector-length vec)))
    ((vec start end) (reverse-f32vector->list* vec start end))))

(define (reverse-f32vector->list* vec start end)
  (let loop ((i start) (r '()))
    (if (fx= i end)
      r
      (loop (fx+ 1 i) (cons (f32vector-ref vec i) r)))))

(define (reverse-list->f32vector list)
  (let* ((len (length list))
         (r (make-f32vector len)))
    (let loop ((i 0) (list list))
      (cond
        ((fx= i len) r)
        (else
          (f32vector-set! r (fx- (fx- len i) 1) (car list))
          (loop (fx+ i 1) (cdr list)))))))

(define f32vector->vector
  (case-lambda
    ((vec) (f32vector->vector* vec 0 (f32vector-length vec)))
    ((vec start) (f32vector->vector* vec start (f32vector-length vec)))
    ((vec start end) (f32vector->vector* vec start end))))

(define (f32vector->vector* vec start end)
  (let* ((len (fx- end start))
         (r (make-vector len)))
    (let loop ((i start) (o 0))
      (cond
        ((fx= i end) r)
        (else
          (vector-set! r o (f32vector-ref vec i))
          (loop (fx+ i 1) (fx+ o 1)))))))

(define vector->f32vector
  (case-lambda
    ((vec) (vector->f32vector* vec 0 (vector-length vec)))
    ((vec start) (vector->f32vector* vec start (vector-length vec)))
    ((vec start end) (vector->f32vector* vec start end))))

(define (vector->f32vector* vec start end)
  (let* ((len (fx- end start))
         (r (make-f32vector len)))
    (let loop ((i start) (o 0))
      (cond
        ((fx= i end) r)
        (else
          (f32vector-set! r o (vector-ref vec i))
          (loop (fx+ i 1) (fx+ o 1)))))))

(define make-f32vector-generator
  (case-lambda ((vec) (make-f32vector-generator vec 0 (f32vector-length vec)))
               ((vec start) (make-f32vector-generator vec start (f32vector-length vec)))
               ((vec start end)
                (lambda () (if (fx>= start end)
                             (eof-object)
                             (let ((next (f32vector-ref vec start)))
                              (set! start (fx+ start 1))
                              next))))))

(define write-f32vector
  (case-lambda
    ((vec) (write-f32vector* vec (current-output-port)))
    ((vec port) (write-f32vector* vec port))))


(define (write-f32vector* vec port)
  (display "#f32(" port)  ; f32-expansion is blind, so will expand this too
  (let ((last (fx- (f32vector-length vec) 1)))
    (let loop ((i 0))
      (cond
        ((fx= i last)
         (write (f32vector-ref vec i) port)
         (display ")" port))
        (else
          (write (f32vector-ref vec i) port)
          (display " " port)
          (loop (fx+ i 1)))))))

(define (f32vector< vec1 vec2)
  (let ((len1 (f32vector-length vec1))
        (len2 (f32vector-length vec2)))
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
           ((< (f32vector-ref vec1 i) (f32vector-ref vec2 i))
            #t)
           ((> (f32vector-ref vec1 i) (f32vector-ref vec2 i))
            #f)
           (else
             (loop (fx+ i 1)))))))))

(define (f32vector-hash vec)
  (let ((len (min 256 (f32vector-length vec))))
    (let loop ((i 0) (r 0))
      (if (fx= i len)
        (abs (floor (real-part (inexact->exact r))))
        (loop (fx+ i 1) (+ r (f32vector-ref vec i)))))))

(define f32vector-comparator
  (make-comparator f32vector? f32vector= f32vector< f32vector-hash))
