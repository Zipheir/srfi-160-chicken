;;; This code is the same for all SRFI 160 vector sizes.
;;; The u32s appearing in the code are expanded to u8, s8, etc.

;; make-u32vector defined in (srfi 160 base)

;; u32vector defined in (srfi 160 base)

(define (u32vector-unfold f len seed)
  (let ((v (make-u32vector len)))
    (let loop ((i 0) (state seed))
      (unless (fx= i len)
        (let-values (((value newstate) (f i state)))
          (u32vector-set! v i value)
          (loop (fx+ i 1) newstate))))
    v))

(define (u32vector-unfold-right f len seed)
  (let ((v (make-u32vector len)))
    (let loop ((i (fx- len 1)) (state seed))
      (unless (fx= i -1)
        (let-values (((value newstate) (f i state)))
          (u32vector-set! v i value)
          (loop (fx- i 1) newstate))))
    v))

(define u32vector-copy
  (case-lambda
    ((vec) (u32vector-copy* vec 0 (u32vector-length vec)))
    ((vec start) (u32vector-copy* vec start (u32vector-length vec)))
    ((vec start end) (u32vector-copy* vec start end))))

(define (u32vector-copy* vec start end)
  (let ((v (make-u32vector (fx- end start))))
    (u32vector-copy! v 0 vec start end)
    v))

(define u32vector-copy!
  (case-lambda
    ((to at from)
     (move-memory! from to (u32vector-length from) 0 (fx* at 4)))
    ((to at from start)
     (move-memory! from to (u32vector-length from) (fx* start 4) (fx* at 4)))
    ((to at from start end)
     (move-memory! from to
                   (fx* 4 (fx- end start))
                   (fx* start 4)
                   (fx* at 4)))))

(define u32vector-reverse-copy
  (case-lambda
    ((vec) (u32vector-reverse-copy* vec 0 (u32vector-length vec)))
    ((vec start) (u32vector-reverse-copy* vec start (u32vector-length vec)))
    ((vec start end) (u32vector-reverse-copy* vec start end))))

(define (u32vector-reverse-copy* vec start end)
  (let ((v (make-u32vector (fx- end start))))
    (u32vector-reverse-copy! v 0 vec start end)
    v))

(define u32vector-reverse-copy!
  (case-lambda
    ((to at from)
     (u32vector-reverse-copy!* to at from 0 (u32vector-length from)))
    ((to at from start)
     (u32vector-reverse-copy!* to at from start (u32vector-length from)))
    ((to at from start end) (u32vector-reverse-copy!* to at from start end))))

(define (u32vector-reverse-copy!* to at from start end)
  (let loop ((at at) (i (fx- end 1)))
    (unless (fx< i start)
      (u32vector-set! to at (u32vector-ref from i))
      (loop (fx+ at 1) (fx- i 1)))))

(define (u32vector-append . vecs)
  (u32vector-concatenate vecs))

(define (u32vector-concatenate vecs)
  (let ((v (make-u32vector (len-sum vecs))))
    (let loop ((vecs vecs) (at 0))
      (unless (null? vecs)
        (let ((vec (car vecs)))
          (u32vector-copy! v at vec 0 (u32vector-length vec))
          (loop (cdr vecs) (fx+ at (u32vector-length vec)))))
    v)))

(define (len-sum vecs)
  (if (null? vecs)
    0
    (fx+ (u32vector-length (car vecs))
       (len-sum (cdr vecs)))))

(define (u32vector-append-subvectors . args)
  (let ((v (make-u32vector (len-subsum args))))
    (let loop ((args args) (at 0))
      (unless (null? args)
        (let ((vec (car args))
              (start (cadr args))
              (end (caddr args)))
          (u32vector-copy! v at vec start end)
          (loop (cdddr args) (fx+ at (fx- end start))))))
    v))

(define (len-subsum vecs)
  (if (null? vecs)
    0
    (fx+ (fx- (caddr vecs) (cadr vecs))
       (len-subsum (cdddr vecs)))))

;; u32? defined in (srfi 160 base)

;; u32vector? defined in (srfi 160 base)

(define (u32vector-empty? vec)
  (zero? (u32vector-length vec)))

(define (u32vector= . vecs)
  (u32vector=* (car vecs) (cadr vecs) (cddr vecs)))

(define (u32vector=* vec1 vec2 vecs)
  (and (u32dyadic-vecs= vec1 0 (u32vector-length vec1)
                      vec2 0 (u32vector-length vec2))
       (or (null? vecs)
           (u32vector=* vec2 (car vecs) (cdr vecs)))))

(define (u32dyadic-vecs= vec1 start1 end1 vec2 start2 end2)
  (cond
    ((not (fx= end1 end2)) #f)
    ((not (fx< start1 end1)) #t)
    ((let ((elt1 (u32vector-ref vec1 start1))
           (elt2 (u32vector-ref vec2 start2)))
      (= elt1 elt2))
     (u32dyadic-vecs= vec1 (fx+ start1 1) end1
                         vec2 (fx+ start2 1) end2))
    (else #f)))

;; u32vector-ref defined in (srfi 160 base)

;; u32vector-length defined in (srfi 160 base)

(define (u32vector-take vec n)
  (let ((v (make-u32vector n)))
    (u32vector-copy! v 0 vec 0 n)
    v))

(define (u32vector-take-right vec n)
  (let ((v (make-u32vector n))
        (len (u32vector-length vec)))
    (u32vector-copy! v 0 vec (fx- len n) len)
    v))

(define (u32vector-drop vec n)
 (let* ((len (u32vector-length vec))
        (vlen (fx- len n))
        (v (make-u32vector vlen)))
    (u32vector-copy! v 0 vec n len)
    v))

(define (u32vector-drop-right vec n)
  (let* ((len (u32vector-length vec))
         (rlen (fx- len n))
         (v (make-u32vector rlen)))
    (u32vector-copy! v 0 vec 0 rlen)
    v))

(define (u32vector-segment vec n)
  (let loop ((r '()) (i 0) (remain (u32vector-length vec)))
    (if (fx<= remain 0)
      (reverse r)
      (let ((size (min n remain)))
        (loop
          (cons (u32vector-copy vec i (fx+ i size)) r)
          (fx+ i size)
          (fx- remain size))))))

;; aux. procedure
(define (%u32vectors-ref vecs i)
  (map (lambda (v) (u32vector-ref v i)) vecs))

(define (u32vector-fold kons knil vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (u32vector-length vec)))
      (let loop ((r knil) (i 0))
        (if (fx= i len)
          r
          (loop (kons r (u32vector-ref vec i)) (fx+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map u32vector-length vecs))))
      (let loop ((r knil) (i 0))
        (if (fx= i len)
          r
          (loop (apply kons r (%u32vectors-ref vecs i))
                (fx+ i 1)))))))

(define (u32vector-fold-right kons knil vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (u32vector-length vec)))
      (let loop ((r knil) (i (fx- (u32vector-length vec) 1)))
        (if (negative? i)
          r
          (loop (kons r (u32vector-ref vec i)) (fx- i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map u32vector-length vecs))))
      (let loop ((r knil) (i (fx- len 1)))
        (if (negative? i)
          r
          (loop (apply kons r (%u32vectors-ref vecs i))
                (fx- i 1)))))))

(define (u32vector-map f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let* ((len (u32vector-length vec))
           (v (make-u32vector len)))
      (let loop ((i 0))
        (unless (fx= i len)
          (u32vector-set! v i (f (u32vector-ref vec i)))
          (loop (fx+ i 1))))
      v)
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map u32vector-length vecs)))
           (v (make-u32vector len)))
      (let loop ((i 0))
        (unless (fx= i len)
          (u32vector-set! v i (apply f (%u32vectors-ref vecs i)))
          (loop (fx+ i 1))))
      v)))


(define (u32vector-map! f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (u32vector-length vec)))
      (let loop ((i 0))
        (unless (fx= i len)
          (u32vector-set! vec i (f (u32vector-ref vec i)))
          (loop (fx+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map u32vector-length vecs))))
      (let loop ((i 0))
        (unless (fx= i len)
          (u32vector-set! vec i (apply f (%u32vectors-ref vecs i)))
          (loop (fx+ i 1)))))))

(define (u32vector-for-each f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (u32vector-length vec)))
      (let loop ((i 0))
        (unless (fx= i len)
          (f (u32vector-ref vec i))
          (loop (fx+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map u32vector-length vecs))))
      (let loop ((i 0))
        (unless (fx= i len)
          (apply f (%u32vectors-ref vecs i))
          (loop (fx+ i 1)))))))

(define (u32vector-count pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (u32vector-length vec)))
      (let loop ((i 0) (r 0))
        (cond
         ((fx= i (u32vector-length vec)) r)
         ((pred (u32vector-ref vec i)) (loop (fx+ i 1) (fx+ r 1)))
         (else (loop (fx+ i 1) r)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map u32vector-length vecs))))
      (let loop ((i 0) (r 0))
        (cond
         ((fx= i len) r)
         ((apply pred (%u32vectors-ref vecs i)) (loop (fx+ i 1) (fx+ r 1)))
         (else (loop (fx+ i 1) r)))))))

(define (u32vector-cumulate f knil vec)
  (let* ((len (u32vector-length vec))
         (v (make-u32vector len)))
    (let loop ((r knil) (i 0))
      (unless (fx= i len)
        (let ((next (f r (u32vector-ref vec i))))
          (u32vector-set! v i next)
          (loop next (fx+ i 1)))))
    v))

(define (u32vector-foreach f vec)
  (let ((len (u32vector-length vec)))
    (let loop ((i 0))
      (unless (fx= i len)
        (f (u32vector-ref vec i))
        (loop (fx+ i 1))))))

(define (u32vector-take-while pred vec)
  (let* ((len (u32vector-length vec))
         (idx (u32vector-skip pred vec))
         (idx* (if idx idx len)))
    (u32vector-copy vec 0 idx*)))

(define (u32vector-take-while-right pred vec)
  (let* ((len (u32vector-length vec))
         (idx (u32vector-skip-right pred vec))
         (idx* (if idx (fx+ idx 1) 0)))
    (u32vector-copy vec idx* len)))

(define (u32vector-drop-while pred vec)
  (let* ((len (u32vector-length vec))
         (idx (u32vector-skip pred vec))
         (idx* (if idx idx len)))
    (u32vector-copy vec idx* len)))

(define (u32vector-drop-while-right pred vec)
  (let* ((len (u32vector-length vec))
         (idx (u32vector-skip-right pred vec))
         (idx* (if idx idx -1)))
    (u32vector-copy vec 0 (fx+ 1 idx*))))

(define (u32vector-index pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (u32vector-length vec)))
      (let loop ((i 0))
        (cond
         ((fx= i len) #f)
         ((pred (u32vector-ref vec i)) i)
         (else (loop (fx+ i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map u32vector-length vecs))))
      (let loop ((i 0))
        (cond
         ((fx= i len) #f)
         ((apply pred (%u32vectors-ref vecs i)) i)
         (else (loop (fx+ i 1))))))))

(define (u32vector-index-right pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (u32vector-length vec)))
      (let loop ((i (fx- len 1)))
        (cond
         ((negative? i) #f)
         ((pred (u32vector-ref vec i)) i)
         (else (loop (fx- i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map u32vector-length vecs))))
      (let loop ((i (fx- len 1)))
        (cond
         ((negative? i) #f)
         ((apply pred (%u32vectors-ref vecs i)) i)
         (else (loop (fx- i 1))))))))

(define (u32vector-skip pred vec . vecs)
  (if (null? vecs)
    (u32vector-index (lambda (x) (not (pred x))) vec)
    (apply u32vector-index (lambda xs (not (apply pred xs))) vec vecs)))

(define (u32vector-skip-right pred vec . vecs)
  (if (null? vecs)
    (u32vector-index-right (lambda (x) (not (pred x))) vec)
    (apply u32vector-index-right (lambda xs (not (apply pred xs))) vec vecs)))

(define (u32vector-any pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (u32vector-length vec)))
      (let loop ((i 0))
        (cond
         ((fx= i len) #f)
         ((pred (u32vector-ref vec i)))  ;returns result of pred
         (else (loop (fx+ i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map u32vector-length vecs))))
      (let loop ((i 0))
        (cond
         ((fx= i len) #f)
         ((apply pred (%u32vectors-ref vecs i))) ;returns result of pred
         (else (loop (fx+ i 1))))))))

(define (u32vector-every pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (u32vector-length vec)))
      (let loop ((i 0) (last #t))
        (cond
         ((fx= i len) last)
         ((pred (u32vector-ref vec i)) => (lambda (r) (loop (fx+ i 1) r)))
         (else #f))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map u32vector-length vecs))))
      (let loop ((i 0) (last #t))
        (cond
         ((fx= i len) last)
         ((apply pred (%u32vectors-ref vecs i)) => (lambda (r) (loop (fx+ i 1) r)))
         (else #f))))))

(define (u32vector-partition pred vec)
  (let* ((len (u32vector-length vec))
         (cnt (u32vector-count pred vec))
         (r (make-u32vector len)))
    (let loop ((i 0) (yes 0) (no cnt))
      (cond
        ((fx= i len) r)
        ((pred (u32vector-ref vec i))
         (u32vector-set! r yes (u32vector-ref vec i))
         (loop (fx+ i 1) (fx+ yes 1) no))
        (else
         (u32vector-set! r no (u32vector-ref vec i))
         (loop (fx+ i 1) yes (fx+ no 1)))))))

(define (u32vector-filter pred vec)
  (let* ((len (u32vector-length vec))
         (cnt (u32vector-count pred vec))
         (r (make-u32vector cnt)))
    (let loop ((i 0) (j 0))
      (cond
        ((fx= i len) r)
        ((pred (u32vector-ref vec i))
         (u32vector-set! r j (u32vector-ref vec i))
         (loop (fx+ i 1) (fx+ j 1)))
        (else
         (loop (fx+ i 1) j))))))

(define (u32vector-remove pred vec)
  (u32vector-filter (lambda (x) (not (pred x))) vec))

;; u32vector-set! defined in (srfi 160 base)

(define (u32vector-swap! vec i j)
  (let ((ival (u32vector-ref vec i))
        (jval (u32vector-ref vec j)))
    (u32vector-set! vec i jval)
    (u32vector-set! vec j ival)))

(define u32vector-fill!
  (case-lambda
    ((vec fill) (u32vector-fill-some! vec fill 0 (u32vector-length vec)))
    ((vec fill start) (u32vector-fill-some! vec fill start (u32vector-length vec)))
    ((vec fill start end) (u32vector-fill-some! vec fill start end))))

(define (u32vector-fill-some! vec fill start end)
  (unless (fx= start end)
    (u32vector-set! vec start fill)
    (u32vector-fill-some! vec fill (fx+ start 1) end)))

(define u32vector-reverse!
  (case-lambda
    ((vec) (u32vector-reverse-some! vec 0 (u32vector-length vec)))
    ((vec start) (u32vector-reverse-some! vec start (u32vector-length vec)))
    ((vec start end) (u32vector-reverse-some! vec start end))))

(define (u32vector-reverse-some! vec start end)
  (let loop ((i start) (j (fx- end 1)))
    (when (fx< i j)
      (u32vector-swap! vec i j)
      (loop (fx+ i 1) (fx- j 1)))))

(define (u32vector-unfold! f vec start end seed)
  (let loop ((i start) (seed seed))
    (when (fx< i end)
      (let-values (((elt seed) (f seed)))
        (u32vector-set! vec i elt)
        (loop (fx+ i 1) seed)))))

(define (u32vector-unfold-right! f vec start end seed)
  (let loop ((i (fx- end 1)) (seed seed))
    (when (fx>= i start)
      (let-values (((elt seed) (f seed)))
        (u32vector-set! vec i elt)
        (loop (fx- i 1) seed)))))

(define reverse-u32vector->list
  (case-lambda
    ((vec) (reverse-u32vector->list* vec 0 (u32vector-length vec)))
    ((vec start) (reverse-u32vector->list* vec start (u32vector-length vec)))
    ((vec start end) (reverse-u32vector->list* vec start end))))

(define (reverse-u32vector->list* vec start end)
  (let loop ((i start) (r '()))
    (if (fx= i end)
      r
      (loop (fx+ 1 i) (cons (u32vector-ref vec i) r)))))

(define (reverse-list->u32vector list)
  (let* ((len (length list))
         (r (make-u32vector len)))
    (let loop ((i 0) (list list))
      (cond
        ((fx= i len) r)
        (else
          (u32vector-set! r (fx- (fx- len i) 1) (car list))
          (loop (fx+ i 1) (cdr list)))))))

(define u32vector->vector
  (case-lambda
    ((vec) (u32vector->vector* vec 0 (u32vector-length vec)))
    ((vec start) (u32vector->vector* vec start (u32vector-length vec)))
    ((vec start end) (u32vector->vector* vec start end))))

(define (u32vector->vector* vec start end)
  (let* ((len (fx- end start))
         (r (make-vector len)))
    (let loop ((i start) (o 0))
      (cond
        ((fx= i end) r)
        (else
          (vector-set! r o (u32vector-ref vec i))
          (loop (fx+ i 1) (fx+ o 1)))))))

(define vector->u32vector
  (case-lambda
    ((vec) (vector->u32vector* vec 0 (vector-length vec)))
    ((vec start) (vector->u32vector* vec start (vector-length vec)))
    ((vec start end) (vector->u32vector* vec start end))))

(define (vector->u32vector* vec start end)
  (let* ((len (fx- end start))
         (r (make-u32vector len)))
    (let loop ((i start) (o 0))
      (cond
        ((fx= i end) r)
        (else
          (u32vector-set! r o (vector-ref vec i))
          (loop (fx+ i 1) (fx+ o 1)))))))

(define make-u32vector-generator
  (case-lambda ((vec) (make-u32vector-generator vec 0 (u32vector-length vec)))
               ((vec start) (make-u32vector-generator vec start (u32vector-length vec)))
               ((vec start end)
                (lambda () (if (fx>= start end)
                             (eof-object)
                             (let ((next (u32vector-ref vec start)))
                              (set! start (fx+ start 1))
                              next))))))

(define write-u32vector
  (case-lambda
    ((vec) (write-u32vector* vec (current-output-port)))
    ((vec port) (write-u32vector* vec port))))


(define (write-u32vector* vec port)
  (display "#u32(" port)  ; u32-expansion is blind, so will expand this too
  (let ((last (fx- (u32vector-length vec) 1)))
    (let loop ((i 0))
      (cond
        ((fx= i last)
         (write (u32vector-ref vec i) port)
         (display ")" port))
        (else
          (write (u32vector-ref vec i) port)
          (display " " port)
          (loop (fx+ i 1)))))))

(define (u32vector< vec1 vec2)
  (let ((len1 (u32vector-length vec1))
        (len2 (u32vector-length vec2)))
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
           ((< (u32vector-ref vec1 i) (u32vector-ref vec2 i))
            #t)
           ((> (u32vector-ref vec1 i) (u32vector-ref vec2 i))
            #f)
           (else
             (loop (fx+ i 1)))))))))

(define (u32vector-hash vec)
  (let ((len (min 256 (u32vector-length vec))))
    (let loop ((i 0) (r 0))
      (if (fx= i len)
        (abs (floor (real-part (inexact->exact r))))
        (loop (fx+ i 1) (+ r (u32vector-ref vec i)))))))

(define u32vector-comparator
  (make-comparator u32vector? u32vector= u32vector< u32vector-hash))
