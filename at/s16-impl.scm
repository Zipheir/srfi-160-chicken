;;; This code is the same for all SRFI 160 vector sizes.
;;; The s16s appearing in the code are expanded to u8, s8, etc.

;; make-s16vector defined in (srfi 160 base)

;; s16vector defined in (srfi 160 base)

(define (s16vector-unfold f len seed)
  (let ((v (make-s16vector len)))
    (let loop ((i 0) (state seed))
      (unless (fx= i len)
        (let-values (((value newstate) (f i state)))
          (s16vector-set! v i value)
          (loop (fx+ i 1) newstate))))
    v))

(define (s16vector-unfold-right f len seed)
  (let ((v (make-s16vector len)))
    (let loop ((i (fx- len 1)) (state seed))
      (unless (fx= i -1)
        (let-values (((value newstate) (f i state)))
          (s16vector-set! v i value)
          (loop (fx- i 1) newstate))))
    v))

(define s16vector-copy
  (case-lambda
    ((vec) (s16vector-copy* vec 0 (s16vector-length vec)))
    ((vec start) (s16vector-copy* vec start (s16vector-length vec)))
    ((vec start end) (s16vector-copy* vec start end))))

(define (s16vector-copy* vec start end)
  (let ((v (make-s16vector (fx- end start))))
    (s16vector-copy! v 0 vec start end)
    v))

(define s16vector-copy!
  (case-lambda
    ((to at from)
     (move-memory! from to (s16vector-length from) 0 (fx* at 2)))
    ((to at from start)
     (move-memory! from to (s16vector-length from) (fx* start 2) (fx* at 2)))
    ((to at from start end)
     (move-memory! from to
                   (fx* 2 (fx- end start))
                   (fx* start 2)
                   (fx* at 2)))))

(define s16vector-reverse-copy
  (case-lambda
    ((vec) (s16vector-reverse-copy* vec 0 (s16vector-length vec)))
    ((vec start) (s16vector-reverse-copy* vec start (s16vector-length vec)))
    ((vec start end) (s16vector-reverse-copy* vec start end))))

(define (s16vector-reverse-copy* vec start end)
  (let ((v (make-s16vector (fx- end start))))
    (s16vector-reverse-copy! v 0 vec start end)
    v))

(define s16vector-reverse-copy!
  (case-lambda
    ((to at from)
     (s16vector-reverse-copy!* to at from 0 (s16vector-length from)))
    ((to at from start)
     (s16vector-reverse-copy!* to at from start (s16vector-length from)))
    ((to at from start end) (s16vector-reverse-copy!* to at from start end))))

(define (s16vector-reverse-copy!* to at from start end)
  (let loop ((at at) (i (fx- end 1)))
    (unless (fx< i start)
      (s16vector-set! to at (s16vector-ref from i))
      (loop (fx+ at 1) (fx- i 1)))))

(define (s16vector-append . vecs)
  (s16vector-concatenate vecs))

(define (s16vector-concatenate vecs)
  (let ((v (make-s16vector (len-sum vecs))))
    (let loop ((vecs vecs) (at 0))
      (unless (null? vecs)
        (let ((vec (car vecs)))
          (s16vector-copy! v at vec 0 (s16vector-length vec))
          (loop (cdr vecs) (fx+ at (s16vector-length vec)))))
    v)))

(define (len-sum vecs)
  (if (null? vecs)
    0
    (fx+ (s16vector-length (car vecs))
       (len-sum (cdr vecs)))))

(define (s16vector-append-subvectors . args)
  (let ((v (make-s16vector (len-subsum args))))
    (let loop ((args args) (at 0))
      (unless (null? args)
        (let ((vec (car args))
              (start (cadr args))
              (end (caddr args)))
          (s16vector-copy! v at vec start end)
          (loop (cdddr args) (fx+ at (fx- end start))))))
    v))

(define (len-subsum vecs)
  (if (null? vecs)
    0
    (fx+ (fx- (caddr vecs) (cadr vecs))
       (len-subsum (cdddr vecs)))))

;; s16? defined in (srfi 160 base)

;; s16vector? defined in (srfi 160 base)

(define (s16vector-empty? vec)
  (zero? (s16vector-length vec)))

(define (s16vector= . vecs)
  (s16vector=* (car vecs) (cadr vecs) (cddr vecs)))

(define (s16vector=* vec1 vec2 vecs)
  (and (s16dyadic-vecs= vec1 0 (s16vector-length vec1)
                      vec2 0 (s16vector-length vec2))
       (or (null? vecs)
           (s16vector=* vec2 (car vecs) (cdr vecs)))))

(define (s16dyadic-vecs= vec1 start1 end1 vec2 start2 end2)
  (cond
    ((not (fx= end1 end2)) #f)
    ((not (fx< start1 end1)) #t)
    ((let ((elt1 (s16vector-ref vec1 start1))
           (elt2 (s16vector-ref vec2 start2)))
      (= elt1 elt2))
     (s16dyadic-vecs= vec1 (fx+ start1 1) end1
                         vec2 (fx+ start2 1) end2))
    (else #f)))

;; s16vector-ref defined in (srfi 160 base)

;; s16vector-length defined in (srfi 160 base)

(define (s16vector-take vec n)
  (let ((v (make-s16vector n)))
    (s16vector-copy! v 0 vec 0 n)
    v))

(define (s16vector-take-right vec n)
  (let ((v (make-s16vector n))
        (len (s16vector-length vec)))
    (s16vector-copy! v 0 vec (fx- len n) len)
    v))

(define (s16vector-drop vec n)
 (let* ((len (s16vector-length vec))
        (vlen (fx- len n))
        (v (make-s16vector vlen)))
    (s16vector-copy! v 0 vec n len)
    v))

(define (s16vector-drop-right vec n)
  (let* ((len (s16vector-length vec))
         (rlen (fx- len n))
         (v (make-s16vector rlen)))
    (s16vector-copy! v 0 vec 0 rlen)
    v))

(define (s16vector-segment vec n)
  (let loop ((r '()) (i 0) (remain (s16vector-length vec)))
    (if (fx<= remain 0)
      (reverse r)
      (let ((size (min n remain)))
        (loop
          (cons (s16vector-copy vec i (fx+ i size)) r)
          (fx+ i size)
          (fx- remain size))))))

;; aux. procedure
(define (%s16vectors-ref vecs i)
  (map (lambda (v) (s16vector-ref v i)) vecs))

(define (s16vector-fold kons knil vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (s16vector-length vec)))
      (let loop ((r knil) (i 0))
        (if (fx= i len)
          r
          (loop (kons r (s16vector-ref vec i)) (fx+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s16vector-length vecs))))
      (let loop ((r knil) (i 0))
        (if (fx= i len)
          r
          (loop (apply kons r (%s16vectors-ref vecs i))
                (fx+ i 1)))))))

(define (s16vector-fold-right kons knil vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (s16vector-length vec)))
      (let loop ((r knil) (i (fx- (s16vector-length vec) 1)))
        (if (negative? i)
          r
          (loop (kons r (s16vector-ref vec i)) (fx- i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s16vector-length vecs))))
      (let loop ((r knil) (i (fx- len 1)))
        (if (negative? i)
          r
          (loop (apply kons r (%s16vectors-ref vecs i))
                (fx- i 1)))))))

(define (s16vector-map f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let* ((len (s16vector-length vec))
           (v (make-s16vector len)))
      (let loop ((i 0))
        (unless (fx= i len)
          (s16vector-set! v i (f (s16vector-ref vec i)))
          (loop (fx+ i 1))))
      v)
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s16vector-length vecs)))
           (v (make-s16vector len)))
      (let loop ((i 0))
        (unless (fx= i len)
          (s16vector-set! v i (apply f (%s16vectors-ref vecs i)))
          (loop (fx+ i 1))))
      v)))


(define (s16vector-map! f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (s16vector-length vec)))
      (let loop ((i 0))
        (unless (fx= i len)
          (s16vector-set! vec i (f (s16vector-ref vec i)))
          (loop (fx+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s16vector-length vecs))))
      (let loop ((i 0))
        (unless (fx= i len)
          (s16vector-set! vec i (apply f (%s16vectors-ref vecs i)))
          (loop (fx+ i 1)))))))

(define (s16vector-for-each f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (s16vector-length vec)))
      (let loop ((i 0))
        (unless (fx= i len)
          (f (s16vector-ref vec i))
          (loop (fx+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s16vector-length vecs))))
      (let loop ((i 0))
        (unless (fx= i len)
          (apply f (%s16vectors-ref vecs i))
          (loop (fx+ i 1)))))))

(define (s16vector-count pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (s16vector-length vec)))
      (let loop ((i 0) (r 0))
        (cond
         ((fx= i (s16vector-length vec)) r)
         ((pred (s16vector-ref vec i)) (loop (fx+ i 1) (fx+ r 1)))
         (else (loop (fx+ i 1) r)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s16vector-length vecs))))
      (let loop ((i 0) (r 0))
        (cond
         ((fx= i len) r)
         ((apply pred (%s16vectors-ref vecs i)) (loop (fx+ i 1) (fx+ r 1)))
         (else (loop (fx+ i 1) r)))))))

(define (s16vector-cumulate f knil vec)
  (let* ((len (s16vector-length vec))
         (v (make-s16vector len)))
    (let loop ((r knil) (i 0))
      (unless (fx= i len)
        (let ((next (f r (s16vector-ref vec i))))
          (s16vector-set! v i next)
          (loop next (fx+ i 1)))))
    v))

(define (s16vector-foreach f vec)
  (let ((len (s16vector-length vec)))
    (let loop ((i 0))
      (unless (fx= i len)
        (f (s16vector-ref vec i))
        (loop (fx+ i 1))))))

(define (s16vector-take-while pred vec)
  (let* ((len (s16vector-length vec))
         (idx (s16vector-skip pred vec))
         (idx* (if idx idx len)))
    (s16vector-copy vec 0 idx*)))

(define (s16vector-take-while-right pred vec)
  (let* ((len (s16vector-length vec))
         (idx (s16vector-skip-right pred vec))
         (idx* (if idx (fx+ idx 1) 0)))
    (s16vector-copy vec idx* len)))

(define (s16vector-drop-while pred vec)
  (let* ((len (s16vector-length vec))
         (idx (s16vector-skip pred vec))
         (idx* (if idx idx len)))
    (s16vector-copy vec idx* len)))

(define (s16vector-drop-while-right pred vec)
  (let* ((len (s16vector-length vec))
         (idx (s16vector-skip-right pred vec))
         (idx* (if idx idx -1)))
    (s16vector-copy vec 0 (fx+ 1 idx*))))

(define (s16vector-index pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (s16vector-length vec)))
      (let loop ((i 0))
        (cond
         ((fx= i len) #f)
         ((pred (s16vector-ref vec i)) i)
         (else (loop (fx+ i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s16vector-length vecs))))
      (let loop ((i 0))
        (cond
         ((fx= i len) #f)
         ((apply pred (%s16vectors-ref vecs i)) i)
         (else (loop (fx+ i 1))))))))

(define (s16vector-index-right pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (s16vector-length vec)))
      (let loop ((i (fx- len 1)))
        (cond
         ((negative? i) #f)
         ((pred (s16vector-ref vec i)) i)
         (else (loop (fx- i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s16vector-length vecs))))
      (let loop ((i (fx- len 1)))
        (cond
         ((negative? i) #f)
         ((apply pred (%s16vectors-ref vecs i)) i)
         (else (loop (fx- i 1))))))))

(define (s16vector-skip pred vec . vecs)
  (if (null? vecs)
    (s16vector-index (lambda (x) (not (pred x))) vec)
    (apply s16vector-index (lambda xs (not (apply pred xs))) vec vecs)))

(define (s16vector-skip-right pred vec . vecs)
  (if (null? vecs)
    (s16vector-index-right (lambda (x) (not (pred x))) vec)
    (apply s16vector-index-right (lambda xs (not (apply pred xs))) vec vecs)))

(define (s16vector-any pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (s16vector-length vec)))
      (let loop ((i 0))
        (cond
         ((fx= i len) #f)
         ((pred (s16vector-ref vec i)))  ;returns result of pred
         (else (loop (fx+ i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s16vector-length vecs))))
      (let loop ((i 0))
        (cond
         ((fx= i len) #f)
         ((apply pred (%s16vectors-ref vecs i))) ;returns result of pred
         (else (loop (fx+ i 1))))))))

(define (s16vector-every pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (s16vector-length vec)))
      (let loop ((i 0) (last #t))
        (cond
         ((fx= i len) last)
         ((pred (s16vector-ref vec i)) => (lambda (r) (loop (fx+ i 1) r)))
         (else #f))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s16vector-length vecs))))
      (let loop ((i 0) (last #t))
        (cond
         ((fx= i len) last)
         ((apply pred (%s16vectors-ref vecs i)) => (lambda (r) (loop (fx+ i 1) r)))
         (else #f))))))

(define (s16vector-partition pred vec)
  (let* ((len (s16vector-length vec))
         (cnt (s16vector-count pred vec))
         (r (make-s16vector len)))
    (let loop ((i 0) (yes 0) (no cnt))
      (cond
        ((fx= i len) r)
        ((pred (s16vector-ref vec i))
         (s16vector-set! r yes (s16vector-ref vec i))
         (loop (fx+ i 1) (fx+ yes 1) no))
        (else
         (s16vector-set! r no (s16vector-ref vec i))
         (loop (fx+ i 1) yes (fx+ no 1)))))))

(define (s16vector-filter pred vec)
  (let* ((len (s16vector-length vec))
         (cnt (s16vector-count pred vec))
         (r (make-s16vector cnt)))
    (let loop ((i 0) (j 0))
      (cond
        ((fx= i len) r)
        ((pred (s16vector-ref vec i))
         (s16vector-set! r j (s16vector-ref vec i))
         (loop (fx+ i 1) (fx+ j 1)))
        (else
         (loop (fx+ i 1) j))))))

(define (s16vector-remove pred vec)
  (s16vector-filter (lambda (x) (not (pred x))) vec))

;; s16vector-set! defined in (srfi 160 base)

(define (s16vector-swap! vec i j)
  (let ((ival (s16vector-ref vec i))
        (jval (s16vector-ref vec j)))
    (s16vector-set! vec i jval)
    (s16vector-set! vec j ival)))

(define s16vector-fill!
  (case-lambda
    ((vec fill) (s16vector-fill-some! vec fill 0 (s16vector-length vec)))
    ((vec fill start) (s16vector-fill-some! vec fill start (s16vector-length vec)))
    ((vec fill start end) (s16vector-fill-some! vec fill start end))))

(define (s16vector-fill-some! vec fill start end)
  (unless (fx= start end)
    (s16vector-set! vec start fill)
    (s16vector-fill-some! vec fill (fx+ start 1) end)))

(define s16vector-reverse!
  (case-lambda
    ((vec) (s16vector-reverse-some! vec 0 (s16vector-length vec)))
    ((vec start) (s16vector-reverse-some! vec start (s16vector-length vec)))
    ((vec start end) (s16vector-reverse-some! vec start end))))

(define (s16vector-reverse-some! vec start end)
  (let loop ((i start) (j (fx- end 1)))
    (when (fx< i j)
      (s16vector-swap! vec i j)
      (loop (fx+ i 1) (fx- j 1)))))

(define (s16vector-unfold! f vec start end seed)
  (let loop ((i start) (seed seed))
    (when (fx< i end)
      (let-values (((elt seed) (f seed)))
        (s16vector-set! vec i elt)
        (loop (fx+ i 1) seed)))))

(define (s16vector-unfold-right! f vec start end seed)
  (let loop ((i (fx- end 1)) (seed seed))
    (when (fx>= i start)
      (let-values (((elt seed) (f seed)))
        (s16vector-set! vec i elt)
        (loop (fx- i 1) seed)))))

(define reverse-s16vector->list
  (case-lambda
    ((vec) (reverse-s16vector->list* vec 0 (s16vector-length vec)))
    ((vec start) (reverse-s16vector->list* vec start (s16vector-length vec)))
    ((vec start end) (reverse-s16vector->list* vec start end))))

(define (reverse-s16vector->list* vec start end)
  (let loop ((i start) (r '()))
    (if (fx= i end)
      r
      (loop (fx+ 1 i) (cons (s16vector-ref vec i) r)))))

(define (reverse-list->s16vector list)
  (let* ((len (length list))
         (r (make-s16vector len)))
    (let loop ((i 0) (list list))
      (cond
        ((fx= i len) r)
        (else
          (s16vector-set! r (fx- (fx- len i) 1) (car list))
          (loop (fx+ i 1) (cdr list)))))))

(define s16vector->vector
  (case-lambda
    ((vec) (s16vector->vector* vec 0 (s16vector-length vec)))
    ((vec start) (s16vector->vector* vec start (s16vector-length vec)))
    ((vec start end) (s16vector->vector* vec start end))))

(define (s16vector->vector* vec start end)
  (let* ((len (fx- end start))
         (r (make-vector len)))
    (let loop ((i start) (o 0))
      (cond
        ((fx= i end) r)
        (else
          (vector-set! r o (s16vector-ref vec i))
          (loop (fx+ i 1) (fx+ o 1)))))))

(define vector->s16vector
  (case-lambda
    ((vec) (vector->s16vector* vec 0 (vector-length vec)))
    ((vec start) (vector->s16vector* vec start (vector-length vec)))
    ((vec start end) (vector->s16vector* vec start end))))

(define (vector->s16vector* vec start end)
  (let* ((len (fx- end start))
         (r (make-s16vector len)))
    (let loop ((i start) (o 0))
      (cond
        ((fx= i end) r)
        (else
          (s16vector-set! r o (vector-ref vec i))
          (loop (fx+ i 1) (fx+ o 1)))))))

(define make-s16vector-generator
  (case-lambda ((vec) (make-s16vector-generator vec 0 (s16vector-length vec)))
               ((vec start) (make-s16vector-generator vec start (s16vector-length vec)))
               ((vec start end)
                (lambda () (if (fx>= start end)
                             (eof-object)
                             (let ((next (s16vector-ref vec start)))
                              (set! start (fx+ start 1))
                              next))))))

(define write-s16vector
  (case-lambda
    ((vec) (write-s16vector* vec (current-output-port)))
    ((vec port) (write-s16vector* vec port))))


(define (write-s16vector* vec port)
  (display "#s16(" port)  ; s16-expansion is blind, so will expand this too
  (let ((last (fx- (s16vector-length vec) 1)))
    (let loop ((i 0))
      (cond
        ((fx= i last)
         (write (s16vector-ref vec i) port)
         (display ")" port))
        (else
          (write (s16vector-ref vec i) port)
          (display " " port)
          (loop (fx+ i 1)))))))

(define (s16vector< vec1 vec2)
  (let ((len1 (s16vector-length vec1))
        (len2 (s16vector-length vec2)))
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
           ((< (s16vector-ref vec1 i) (s16vector-ref vec2 i))
            #t)
           ((> (s16vector-ref vec1 i) (s16vector-ref vec2 i))
            #f)
           (else
             (loop (fx+ i 1)))))))))

(define (s16vector-hash vec)
  (let ((len (min 256 (s16vector-length vec))))
    (let loop ((i 0) (r 0))
      (if (fx= i len)
        (abs (floor (real-part (inexact->exact r))))
        (loop (fx+ i 1) (+ r (s16vector-ref vec i)))))))

(define s16vector-comparator
  (make-comparator s16vector? s16vector= s16vector< s16vector-hash))
