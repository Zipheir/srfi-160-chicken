;;; This code is the same for all SRFI 160 vector sizes.
;;; The u64s appearing in the code are expanded to u8, s8, etc.

;; make-u64vector defined in (srfi 160 base)

;; u64vector defined in (srfi 160 base)

(define (u64vector-unfold f len seed)
  (let ((v (make-u64vector len)))
    (let loop ((i 0) (state seed))
      (unless (fx= i len)
        (let-values (((value newstate) (f i state)))
          (u64vector-set! v i value)
          (loop (fx+ i 1) newstate))))
    v))

(define (u64vector-unfold-right f len seed)
  (let ((v (make-u64vector len)))
    (let loop ((i (fx- len 1)) (state seed))
      (unless (fx= i -1)
        (let-values (((value newstate) (f i state)))
          (u64vector-set! v i value)
          (loop (fx- i 1) newstate))))
    v))

(define u64vector-copy
  (case-lambda
    ((vec) (u64vector-copy* vec 0 (u64vector-length vec)))
    ((vec start) (u64vector-copy* vec start (u64vector-length vec)))
    ((vec start end) (u64vector-copy* vec start end))))

(define (u64vector-copy* vec start end)
  (let ((v (make-u64vector (fx- end start))))
    (u64vector-copy! v 0 vec start end)
    v))

(define u64vector-copy!
  (case-lambda
    ((to at from)
     (move-memory! from to (u64vector-length from) 0 (fx* at 8)))
    ((to at from start)
     (move-memory! from to (u64vector-length from) (fx* start 8) (fx* at 8)))
    ((to at from start end)
     (move-memory! from to
                   (fx* 8 (fx- end start))
                   (fx* start 8)
                   (fx* at 8)))))

(define u64vector-reverse-copy
  (case-lambda
    ((vec) (u64vector-reverse-copy* vec 0 (u64vector-length vec)))
    ((vec start) (u64vector-reverse-copy* vec start (u64vector-length vec)))
    ((vec start end) (u64vector-reverse-copy* vec start end))))

(define (u64vector-reverse-copy* vec start end)
  (let ((v (make-u64vector (fx- end start))))
    (u64vector-reverse-copy! v 0 vec start end)
    v))

(define u64vector-reverse-copy!
  (case-lambda
    ((to at from)
     (u64vector-reverse-copy!* to at from 0 (u64vector-length from)))
    ((to at from start)
     (u64vector-reverse-copy!* to at from start (u64vector-length from)))
    ((to at from start end) (u64vector-reverse-copy!* to at from start end))))

(define (u64vector-reverse-copy!* to at from start end)
  (let loop ((at at) (i (fx- end 1)))
    (unless (fx< i start)
      (u64vector-set! to at (u64vector-ref from i))
      (loop (fx+ at 1) (fx- i 1)))))

(define (u64vector-append . vecs)
  (u64vector-concatenate vecs))

(define (u64vector-concatenate vecs)
  (let ((v (make-u64vector (len-sum vecs))))
    (let loop ((vecs vecs) (at 0))
      (unless (null? vecs)
        (let ((vec (car vecs)))
          (u64vector-copy! v at vec 0 (u64vector-length vec))
          (loop (cdr vecs) (fx+ at (u64vector-length vec)))))
    v)))

(define (len-sum vecs)
  (if (null? vecs)
    0
    (fx+ (u64vector-length (car vecs))
       (len-sum (cdr vecs)))))

(define (u64vector-append-subvectors . args)
  (let ((v (make-u64vector (len-subsum args))))
    (let loop ((args args) (at 0))
      (unless (null? args)
        (let ((vec (car args))
              (start (cadr args))
              (end (caddr args)))
          (u64vector-copy! v at vec start end)
          (loop (cdddr args) (fx+ at (fx- end start))))))
    v))

(define (len-subsum vecs)
  (if (null? vecs)
    0
    (fx+ (fx- (caddr vecs) (cadr vecs))
       (len-subsum (cdddr vecs)))))

;; u64? defined in (srfi 160 base)

;; u64vector? defined in (srfi 160 base)

(define (u64vector-empty? vec)
  (zero? (u64vector-length vec)))

(define (u64vector= . vecs)
  (u64vector=* (car vecs) (cadr vecs) (cddr vecs)))

(define (u64vector=* vec1 vec2 vecs)
  (and (u64dyadic-vecs= vec1 0 (u64vector-length vec1)
                      vec2 0 (u64vector-length vec2))
       (or (null? vecs)
           (u64vector=* vec2 (car vecs) (cdr vecs)))))

(define (u64dyadic-vecs= vec1 start1 end1 vec2 start2 end2)
  (cond
    ((not (fx= end1 end2)) #f)
    ((not (fx< start1 end1)) #t)
    ((let ((elt1 (u64vector-ref vec1 start1))
           (elt2 (u64vector-ref vec2 start2)))
      (= elt1 elt2))
     (u64dyadic-vecs= vec1 (fx+ start1 1) end1
                         vec2 (fx+ start2 1) end2))
    (else #f)))

;; u64vector-ref defined in (srfi 160 base)

;; u64vector-length defined in (srfi 160 base)

(define (u64vector-take vec n)
  (let ((v (make-u64vector n)))
    (u64vector-copy! v 0 vec 0 n)
    v))

(define (u64vector-take-right vec n)
  (let ((v (make-u64vector n))
        (len (u64vector-length vec)))
    (u64vector-copy! v 0 vec (fx- len n) len)
    v))

(define (u64vector-drop vec n)
 (let* ((len (u64vector-length vec))
        (vlen (fx- len n))
        (v (make-u64vector vlen)))
    (u64vector-copy! v 0 vec n len)
    v))

(define (u64vector-drop-right vec n)
  (let* ((len (u64vector-length vec))
         (rlen (fx- len n))
         (v (make-u64vector rlen)))
    (u64vector-copy! v 0 vec 0 rlen)
    v))

(define (u64vector-segment vec n)
  (let loop ((r '()) (i 0) (remain (u64vector-length vec)))
    (if (fx<= remain 0)
      (reverse r)
      (let ((size (min n remain)))
        (loop
          (cons (u64vector-copy vec i (fx+ i size)) r)
          (fx+ i size)
          (fx- remain size))))))

;; aux. procedure
(define (%u64vectors-ref vecs i)
  (map (lambda (v) (u64vector-ref v i)) vecs))

(define (u64vector-fold kons knil vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (u64vector-length vec)))
      (let loop ((r knil) (i 0))
        (if (fx= i len)
          r
          (loop (kons r (u64vector-ref vec i)) (fx+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map u64vector-length vecs))))
      (let loop ((r knil) (i 0))
        (if (fx= i len)
          r
          (loop (apply kons r (%u64vectors-ref vecs i))
                (fx+ i 1)))))))

(define (u64vector-fold-right kons knil vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (u64vector-length vec)))
      (let loop ((r knil) (i (fx- (u64vector-length vec) 1)))
        (if (negative? i)
          r
          (loop (kons r (u64vector-ref vec i)) (fx- i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map u64vector-length vecs))))
      (let loop ((r knil) (i (fx- len 1)))
        (if (negative? i)
          r
          (loop (apply kons r (%u64vectors-ref vecs i))
                (fx- i 1)))))))

(define (u64vector-map f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let* ((len (u64vector-length vec))
           (v (make-u64vector len)))
      (let loop ((i 0))
        (unless (fx= i len)
          (u64vector-set! v i (f (u64vector-ref vec i)))
          (loop (fx+ i 1))))
      v)
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map u64vector-length vecs)))
           (v (make-u64vector len)))
      (let loop ((i 0))
        (unless (fx= i len)
          (u64vector-set! v i (apply f (%u64vectors-ref vecs i)))
          (loop (fx+ i 1))))
      v)))


(define (u64vector-map! f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (u64vector-length vec)))
      (let loop ((i 0))
        (unless (fx= i len)
          (u64vector-set! vec i (f (u64vector-ref vec i)))
          (loop (fx+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map u64vector-length vecs))))
      (let loop ((i 0))
        (unless (fx= i len)
          (u64vector-set! vec i (apply f (%u64vectors-ref vecs i)))
          (loop (fx+ i 1)))))))

(define (u64vector-for-each f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (u64vector-length vec)))
      (let loop ((i 0))
        (unless (fx= i len)
          (f (u64vector-ref vec i))
          (loop (fx+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map u64vector-length vecs))))
      (let loop ((i 0))
        (unless (fx= i len)
          (apply f (%u64vectors-ref vecs i))
          (loop (fx+ i 1)))))))

(define (u64vector-count pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (u64vector-length vec)))
      (let loop ((i 0) (r 0))
        (cond
         ((fx= i (u64vector-length vec)) r)
         ((pred (u64vector-ref vec i)) (loop (fx+ i 1) (fx+ r 1)))
         (else (loop (fx+ i 1) r)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map u64vector-length vecs))))
      (let loop ((i 0) (r 0))
        (cond
         ((fx= i len) r)
         ((apply pred (%u64vectors-ref vecs i)) (loop (fx+ i 1) (fx+ r 1)))
         (else (loop (fx+ i 1) r)))))))

(define (u64vector-cumulate f knil vec)
  (let* ((len (u64vector-length vec))
         (v (make-u64vector len)))
    (let loop ((r knil) (i 0))
      (unless (fx= i len)
        (let ((next (f r (u64vector-ref vec i))))
          (u64vector-set! v i next)
          (loop next (fx+ i 1)))))
    v))

(define (u64vector-foreach f vec)
  (let ((len (u64vector-length vec)))
    (let loop ((i 0))
      (unless (fx= i len)
        (f (u64vector-ref vec i))
        (loop (fx+ i 1))))))

(define (u64vector-take-while pred vec)
  (let* ((len (u64vector-length vec))
         (idx (u64vector-skip pred vec))
         (idx* (if idx idx len)))
    (u64vector-copy vec 0 idx*)))

(define (u64vector-take-while-right pred vec)
  (let* ((len (u64vector-length vec))
         (idx (u64vector-skip-right pred vec))
         (idx* (if idx (fx+ idx 1) 0)))
    (u64vector-copy vec idx* len)))

(define (u64vector-drop-while pred vec)
  (let* ((len (u64vector-length vec))
         (idx (u64vector-skip pred vec))
         (idx* (if idx idx len)))
    (u64vector-copy vec idx* len)))

(define (u64vector-drop-while-right pred vec)
  (let* ((len (u64vector-length vec))
         (idx (u64vector-skip-right pred vec))
         (idx* (if idx idx -1)))
    (u64vector-copy vec 0 (fx+ 1 idx*))))

(define (u64vector-index pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (u64vector-length vec)))
      (let loop ((i 0))
        (cond
         ((fx= i len) #f)
         ((pred (u64vector-ref vec i)) i)
         (else (loop (fx+ i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map u64vector-length vecs))))
      (let loop ((i 0))
        (cond
         ((fx= i len) #f)
         ((apply pred (%u64vectors-ref vecs i)) i)
         (else (loop (fx+ i 1))))))))

(define (u64vector-index-right pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (u64vector-length vec)))
      (let loop ((i (fx- len 1)))
        (cond
         ((negative? i) #f)
         ((pred (u64vector-ref vec i)) i)
         (else (loop (fx- i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map u64vector-length vecs))))
      (let loop ((i (fx- len 1)))
        (cond
         ((negative? i) #f)
         ((apply pred (%u64vectors-ref vecs i)) i)
         (else (loop (fx- i 1))))))))

(define (u64vector-skip pred vec . vecs)
  (if (null? vecs)
    (u64vector-index (lambda (x) (not (pred x))) vec)
    (apply u64vector-index (lambda xs (not (apply pred xs))) vec vecs)))

(define (u64vector-skip-right pred vec . vecs)
  (if (null? vecs)
    (u64vector-index-right (lambda (x) (not (pred x))) vec)
    (apply u64vector-index-right (lambda xs (not (apply pred xs))) vec vecs)))

(define (u64vector-any pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (u64vector-length vec)))
      (let loop ((i 0))
        (cond
         ((fx= i len) #f)
         ((pred (u64vector-ref vec i)))  ;returns result of pred
         (else (loop (fx+ i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map u64vector-length vecs))))
      (let loop ((i 0))
        (cond
         ((fx= i len) #f)
         ((apply pred (%u64vectors-ref vecs i))) ;returns result of pred
         (else (loop (fx+ i 1))))))))

(define (u64vector-every pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (u64vector-length vec)))
      (let loop ((i 0) (last #t))
        (cond
         ((fx= i len) last)
         ((pred (u64vector-ref vec i)) => (lambda (r) (loop (fx+ i 1) r)))
         (else #f))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map u64vector-length vecs))))
      (let loop ((i 0) (last #t))
        (cond
         ((fx= i len) last)
         ((apply pred (%u64vectors-ref vecs i)) => (lambda (r) (loop (fx+ i 1) r)))
         (else #f))))))

(define (u64vector-partition pred vec)
  (let* ((len (u64vector-length vec))
         (cnt (u64vector-count pred vec))
         (r (make-u64vector len)))
    (let loop ((i 0) (yes 0) (no cnt))
      (cond
        ((fx= i len) r)
        ((pred (u64vector-ref vec i))
         (u64vector-set! r yes (u64vector-ref vec i))
         (loop (fx+ i 1) (fx+ yes 1) no))
        (else
         (u64vector-set! r no (u64vector-ref vec i))
         (loop (fx+ i 1) yes (fx+ no 1)))))))

(define (u64vector-filter pred vec)
  (let* ((len (u64vector-length vec))
         (cnt (u64vector-count pred vec))
         (r (make-u64vector cnt)))
    (let loop ((i 0) (j 0))
      (cond
        ((fx= i len) r)
        ((pred (u64vector-ref vec i))
         (u64vector-set! r j (u64vector-ref vec i))
         (loop (fx+ i 1) (fx+ j 1)))
        (else
         (loop (fx+ i 1) j))))))

(define (u64vector-remove pred vec)
  (u64vector-filter (lambda (x) (not (pred x))) vec))

;; u64vector-set! defined in (srfi 160 base)

(define (u64vector-swap! vec i j)
  (let ((ival (u64vector-ref vec i))
        (jval (u64vector-ref vec j)))
    (u64vector-set! vec i jval)
    (u64vector-set! vec j ival)))

(define u64vector-fill!
  (case-lambda
    ((vec fill) (u64vector-fill-some! vec fill 0 (u64vector-length vec)))
    ((vec fill start) (u64vector-fill-some! vec fill start (u64vector-length vec)))
    ((vec fill start end) (u64vector-fill-some! vec fill start end))))

(define (u64vector-fill-some! vec fill start end)
  (unless (fx= start end)
    (u64vector-set! vec start fill)
    (u64vector-fill-some! vec fill (fx+ start 1) end)))

(define u64vector-reverse!
  (case-lambda
    ((vec) (u64vector-reverse-some! vec 0 (u64vector-length vec)))
    ((vec start) (u64vector-reverse-some! vec start (u64vector-length vec)))
    ((vec start end) (u64vector-reverse-some! vec start end))))

(define (u64vector-reverse-some! vec start end)
  (let loop ((i start) (j (fx- end 1)))
    (when (fx< i j)
      (u64vector-swap! vec i j)
      (loop (fx+ i 1) (fx- j 1)))))

(define (u64vector-unfold! f vec start end seed)
  (let loop ((i start) (seed seed))
    (when (fx< i end)
      (let-values (((elt seed) (f seed)))
        (u64vector-set! vec i elt)
        (loop (fx+ i 1) seed)))))

(define (u64vector-unfold-right! f vec start end seed)
  (let loop ((i (fx- end 1)) (seed seed))
    (when (fx>= i start)
      (let-values (((elt seed) (f seed)))
        (u64vector-set! vec i elt)
        (loop (fx- i 1) seed)))))

(define reverse-u64vector->list
  (case-lambda
    ((vec) (reverse-u64vector->list* vec 0 (u64vector-length vec)))
    ((vec start) (reverse-u64vector->list* vec start (u64vector-length vec)))
    ((vec start end) (reverse-u64vector->list* vec start end))))

(define (reverse-u64vector->list* vec start end)
  (let loop ((i start) (r '()))
    (if (fx= i end)
      r
      (loop (fx+ 1 i) (cons (u64vector-ref vec i) r)))))

(define (reverse-list->u64vector list)
  (let* ((len (length list))
         (r (make-u64vector len)))
    (let loop ((i 0) (list list))
      (cond
        ((fx= i len) r)
        (else
          (u64vector-set! r (fx- (fx- len i) 1) (car list))
          (loop (fx+ i 1) (cdr list)))))))

(define u64vector->vector
  (case-lambda
    ((vec) (u64vector->vector* vec 0 (u64vector-length vec)))
    ((vec start) (u64vector->vector* vec start (u64vector-length vec)))
    ((vec start end) (u64vector->vector* vec start end))))

(define (u64vector->vector* vec start end)
  (let* ((len (fx- end start))
         (r (make-vector len)))
    (let loop ((i start) (o 0))
      (cond
        ((fx= i end) r)
        (else
          (vector-set! r o (u64vector-ref vec i))
          (loop (fx+ i 1) (fx+ o 1)))))))

(define vector->u64vector
  (case-lambda
    ((vec) (vector->u64vector* vec 0 (vector-length vec)))
    ((vec start) (vector->u64vector* vec start (vector-length vec)))
    ((vec start end) (vector->u64vector* vec start end))))

(define (vector->u64vector* vec start end)
  (let* ((len (fx- end start))
         (r (make-u64vector len)))
    (let loop ((i start) (o 0))
      (cond
        ((fx= i end) r)
        (else
          (u64vector-set! r o (vector-ref vec i))
          (loop (fx+ i 1) (fx+ o 1)))))))

(define make-u64vector-generator
  (case-lambda ((vec) (make-u64vector-generator vec 0 (u64vector-length vec)))
               ((vec start) (make-u64vector-generator vec start (u64vector-length vec)))
               ((vec start end)
                (lambda () (if (fx>= start end)
                             (eof-object)
                             (let ((next (u64vector-ref vec start)))
                              (set! start (fx+ start 1))
                              next))))))

(define write-u64vector
  (case-lambda
    ((vec) (write-u64vector* vec (current-output-port)))
    ((vec port) (write-u64vector* vec port))))


(define (write-u64vector* vec port)
  (display "#u64(" port)  ; u64-expansion is blind, so will expand this too
  (let ((last (fx- (u64vector-length vec) 1)))
    (let loop ((i 0))
      (cond
        ((fx= i last)
         (write (u64vector-ref vec i) port)
         (display ")" port))
        (else
          (write (u64vector-ref vec i) port)
          (display " " port)
          (loop (fx+ i 1)))))))

(define (u64vector< vec1 vec2)
  (let ((len1 (u64vector-length vec1))
        (len2 (u64vector-length vec2)))
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
           ((< (u64vector-ref vec1 i) (u64vector-ref vec2 i))
            #t)
           ((> (u64vector-ref vec1 i) (u64vector-ref vec2 i))
            #f)
           (else
             (loop (fx+ i 1)))))))))

(define (u64vector-hash vec)
  (let ((len (min 256 (u64vector-length vec))))
    (let loop ((i 0) (r 0))
      (if (fx= i len)
        (abs (floor (real-part (inexact->exact r))))
        (loop (fx+ i 1) (+ r (u64vector-ref vec i)))))))

(define u64vector-comparator
  (make-comparator u64vector? u64vector= u64vector< u64vector-hash))
