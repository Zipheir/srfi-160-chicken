;;; This code is the same for all SRFI 160 vector sizes.
;;; The c128s appearing in the code are expanded to u8, s8, etc.

;; make-c128vector defined in (srfi 160 base)

;; c128vector defined in (srfi 160 base)

(define (c128vector-unfold f len seed)
  (let ((v (make-c128vector len)))
    (let loop ((i 0) (state seed))
      (unless (fx= i len)
        (let-values (((value newstate) (f i state)))
          (c128vector-set! v i value)
          (loop (fx+ i 1) newstate))))
    v))

(define (c128vector-unfold-right f len seed)
  (let ((v (make-c128vector len)))
    (let loop ((i (fx- len 1)) (state seed))
      (unless (fx= i -1)
        (let-values (((value newstate) (f i state)))
          (c128vector-set! v i value)
          (loop (fx- i 1) newstate))))
    v))

(define c128vector-copy
  (case-lambda
    ((vec) (c128vector-copy* vec 0 (c128vector-length vec)))
    ((vec start) (c128vector-copy* vec start (c128vector-length vec)))
    ((vec start end) (c128vector-copy* vec start end))))

(define (c128vector-copy* vec start end)
  (let ((v (make-c128vector (fx- end start))))
    (c128vector-copy! v 0 vec start end)
    v))

(define c128vector-copy!
  (case-lambda
    ((to at from)
     (move-memory! (c128vector-body from) (c128vector-body to) (c128vector-length (c128vector-body from)) 0 (fx* at 16)))
    ((to at from start)
     (move-memory! (c128vector-body from) (c128vector-body to) (c128vector-length (c128vector-body from)) (fx* start 16) (fx* at 16)))
    ((to at from start end)
     (move-memory! (c128vector-body from) (c128vector-body to)
                   (fx* 16 (fx- end start))
                   (fx* start 16)
                   (fx* at 16)))))

(define c128vector-reverse-copy
  (case-lambda
    ((vec) (c128vector-reverse-copy* vec 0 (c128vector-length vec)))
    ((vec start) (c128vector-reverse-copy* vec start (c128vector-length vec)))
    ((vec start end) (c128vector-reverse-copy* vec start end))))

(define (c128vector-reverse-copy* vec start end)
  (let ((v (make-c128vector (fx- end start))))
    (c128vector-reverse-copy! v 0 vec start end)
    v))

(define c128vector-reverse-copy!
  (case-lambda
    ((to at from)
     (c128vector-reverse-copy!* to at from 0 (c128vector-length from)))
    ((to at from start)
     (c128vector-reverse-copy!* to at from start (c128vector-length from)))
    ((to at from start end) (c128vector-reverse-copy!* to at from start end))))

(define (c128vector-reverse-copy!* to at from start end)
  (let loop ((at at) (i (fx- end 1)))
    (unless (fx< i start)
      (c128vector-set! to at (c128vector-ref from i))
      (loop (fx+ at 1) (fx- i 1)))))

(define (c128vector-append . vecs)
  (c128vector-concatenate vecs))

(define (c128vector-concatenate vecs)
  (let ((v (make-c128vector (len-sum vecs))))
    (let loop ((vecs vecs) (at 0))
      (unless (null? vecs)
        (let ((vec (car vecs)))
          (c128vector-copy! v at vec 0 (c128vector-length vec))
          (loop (cdr vecs) (fx+ at (c128vector-length vec)))))
    v)))

(define (len-sum vecs)
  (if (null? vecs)
    0
    (fx+ (c128vector-length (car vecs))
       (len-sum (cdr vecs)))))

(define (c128vector-append-subvectors . args)
  (let ((v (make-c128vector (len-subsum args))))
    (let loop ((args args) (at 0))
      (unless (null? args)
        (let ((vec (car args))
              (start (cadr args))
              (end (caddr args)))
          (c128vector-copy! v at vec start end)
          (loop (cdddr args) (fx+ at (fx- end start))))))
    v))

(define (len-subsum vecs)
  (if (null? vecs)
    0
    (fx+ (fx- (caddr vecs) (cadr vecs))
       (len-subsum (cdddr vecs)))))

;; c128? defined in (srfi 160 base)

;; c128vector? defined in (srfi 160 base)

(define (c128vector-empty? vec)
  (zero? (c128vector-length vec)))

(define (c128vector= . vecs)
  (c128vector=* (car vecs) (cadr vecs) (cddr vecs)))

(define (c128vector=* vec1 vec2 vecs)
  (and (c128dyadic-vecs= vec1 0 (c128vector-length vec1)
                      vec2 0 (c128vector-length vec2))
       (or (null? vecs)
           (c128vector=* vec2 (car vecs) (cdr vecs)))))

(define (c128dyadic-vecs= vec1 start1 end1 vec2 start2 end2)
  (cond
    ((not (fx= end1 end2)) #f)
    ((not (fx< start1 end1)) #t)
    ((let ((elt1 (c128vector-ref vec1 start1))
           (elt2 (c128vector-ref vec2 start2)))
      (= elt1 elt2))
     (c128dyadic-vecs= vec1 (fx+ start1 1) end1
                         vec2 (fx+ start2 1) end2))
    (else #f)))

;; c128vector-ref defined in (srfi 160 base)

;; c128vector-length defined in (srfi 160 base)

(define (c128vector-take vec n)
  (let ((v (make-c128vector n)))
    (c128vector-copy! v 0 vec 0 n)
    v))

(define (c128vector-take-right vec n)
  (let ((v (make-c128vector n))
        (len (c128vector-length vec)))
    (c128vector-copy! v 0 vec (fx- len n) len)
    v))

(define (c128vector-drop vec n)
 (let* ((len (c128vector-length vec))
        (vlen (fx- len n))
        (v (make-c128vector vlen)))
    (c128vector-copy! v 0 vec n len)
    v))

(define (c128vector-drop-right vec n)
  (let* ((len (c128vector-length vec))
         (rlen (fx- len n))
         (v (make-c128vector rlen)))
    (c128vector-copy! v 0 vec 0 rlen)
    v))

(define (c128vector-segment vec n)
  (let loop ((r '()) (i 0) (remain (c128vector-length vec)))
    (if (fx<= remain 0)
      (reverse r)
      (let ((size (min n remain)))
        (loop
          (cons (c128vector-copy vec i (fx+ i size)) r)
          (fx+ i size)
          (fx- remain size))))))

;; aux. procedure
(define (%c128vectors-ref vecs i)
  (map (lambda (v) (c128vector-ref v i)) vecs))

(define (c128vector-fold kons knil vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (c128vector-length vec)))
      (let loop ((r knil) (i 0))
        (if (fx= i len)
          r
          (loop (kons r (c128vector-ref vec i)) (fx+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map c128vector-length vecs))))
      (let loop ((r knil) (i 0))
        (if (fx= i len)
          r
          (loop (apply kons r (%c128vectors-ref vecs i))
                (fx+ i 1)))))))

(define (c128vector-fold-right kons knil vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (c128vector-length vec)))
      (let loop ((r knil) (i (fx- (c128vector-length vec) 1)))
        (if (negative? i)
          r
          (loop (kons r (c128vector-ref vec i)) (fx- i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map c128vector-length vecs))))
      (let loop ((r knil) (i (fx- len 1)))
        (if (negative? i)
          r
          (loop (apply kons r (%c128vectors-ref vecs i))
                (fx- i 1)))))))

(define (c128vector-map f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let* ((len (c128vector-length vec))
           (v (make-c128vector len)))
      (let loop ((i 0))
        (unless (fx= i len)
          (c128vector-set! v i (f (c128vector-ref vec i)))
          (loop (fx+ i 1))))
      v)
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map c128vector-length vecs)))
           (v (make-c128vector len)))
      (let loop ((i 0))
        (unless (fx= i len)
          (c128vector-set! v i (apply f (%c128vectors-ref vecs i)))
          (loop (fx+ i 1))))
      v)))


(define (c128vector-map! f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (c128vector-length vec)))
      (let loop ((i 0))
        (unless (fx= i len)
          (c128vector-set! vec i (f (c128vector-ref vec i)))
          (loop (fx+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map c128vector-length vecs))))
      (let loop ((i 0))
        (unless (fx= i len)
          (c128vector-set! vec i (apply f (%c128vectors-ref vecs i)))
          (loop (fx+ i 1)))))))

(define (c128vector-for-each f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (c128vector-length vec)))
      (let loop ((i 0))
        (unless (fx= i len)
          (f (c128vector-ref vec i))
          (loop (fx+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map c128vector-length vecs))))
      (let loop ((i 0))
        (unless (fx= i len)
          (apply f (%c128vectors-ref vecs i))
          (loop (fx+ i 1)))))))

(define (c128vector-count pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (c128vector-length vec)))
      (let loop ((i 0) (r 0))
        (cond
         ((fx= i (c128vector-length vec)) r)
         ((pred (c128vector-ref vec i)) (loop (fx+ i 1) (fx+ r 1)))
         (else (loop (fx+ i 1) r)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map c128vector-length vecs))))
      (let loop ((i 0) (r 0))
        (cond
         ((fx= i len) r)
         ((apply pred (%c128vectors-ref vecs i)) (loop (fx+ i 1) (fx+ r 1)))
         (else (loop (fx+ i 1) r)))))))

(define (c128vector-cumulate f knil vec)
  (let* ((len (c128vector-length vec))
         (v (make-c128vector len)))
    (let loop ((r knil) (i 0))
      (unless (fx= i len)
        (let ((next (f r (c128vector-ref vec i))))
          (c128vector-set! v i next)
          (loop next (fx+ i 1)))))
    v))

(define (c128vector-foreach f vec)
  (let ((len (c128vector-length vec)))
    (let loop ((i 0))
      (unless (fx= i len)
        (f (c128vector-ref vec i))
        (loop (fx+ i 1))))))

(define (c128vector-take-while pred vec)
  (let* ((len (c128vector-length vec))
         (idx (c128vector-skip pred vec))
         (idx* (if idx idx len)))
    (c128vector-copy vec 0 idx*)))

(define (c128vector-take-while-right pred vec)
  (let* ((len (c128vector-length vec))
         (idx (c128vector-skip-right pred vec))
         (idx* (if idx (fx+ idx 1) 0)))
    (c128vector-copy vec idx* len)))

(define (c128vector-drop-while pred vec)
  (let* ((len (c128vector-length vec))
         (idx (c128vector-skip pred vec))
         (idx* (if idx idx len)))
    (c128vector-copy vec idx* len)))

(define (c128vector-drop-while-right pred vec)
  (let* ((len (c128vector-length vec))
         (idx (c128vector-skip-right pred vec))
         (idx* (if idx idx -1)))
    (c128vector-copy vec 0 (fx+ 1 idx*))))

(define (c128vector-index pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (c128vector-length vec)))
      (let loop ((i 0))
        (cond
         ((fx= i len) #f)
         ((pred (c128vector-ref vec i)) i)
         (else (loop (fx+ i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map c128vector-length vecs))))
      (let loop ((i 0))
        (cond
         ((fx= i len) #f)
         ((apply pred (%c128vectors-ref vecs i)) i)
         (else (loop (fx+ i 1))))))))

(define (c128vector-index-right pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (c128vector-length vec)))
      (let loop ((i (fx- len 1)))
        (cond
         ((negative? i) #f)
         ((pred (c128vector-ref vec i)) i)
         (else (loop (fx- i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map c128vector-length vecs))))
      (let loop ((i (fx- len 1)))
        (cond
         ((negative? i) #f)
         ((apply pred (%c128vectors-ref vecs i)) i)
         (else (loop (fx- i 1))))))))

(define (c128vector-skip pred vec . vecs)
  (if (null? vecs)
    (c128vector-index (lambda (x) (not (pred x))) vec)
    (apply c128vector-index (lambda xs (not (apply pred xs))) vec vecs)))

(define (c128vector-skip-right pred vec . vecs)
  (if (null? vecs)
    (c128vector-index-right (lambda (x) (not (pred x))) vec)
    (apply c128vector-index-right (lambda xs (not (apply pred xs))) vec vecs)))

(define (c128vector-any pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (c128vector-length vec)))
      (let loop ((i 0))
        (cond
         ((fx= i len) #f)
         ((pred (c128vector-ref vec i)))  ;returns result of pred
         (else (loop (fx+ i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map c128vector-length vecs))))
      (let loop ((i 0))
        (cond
         ((fx= i len) #f)
         ((apply pred (%c128vectors-ref vecs i))) ;returns result of pred
         (else (loop (fx+ i 1))))))))

(define (c128vector-every pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (c128vector-length vec)))
      (let loop ((i 0) (last #t))
        (cond
         ((fx= i len) last)
         ((pred (c128vector-ref vec i)) => (lambda (r) (loop (fx+ i 1) r)))
         (else #f))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map c128vector-length vecs))))
      (let loop ((i 0) (last #t))
        (cond
         ((fx= i len) last)
         ((apply pred (%c128vectors-ref vecs i)) => (lambda (r) (loop (fx+ i 1) r)))
         (else #f))))))

(define (c128vector-partition pred vec)
  (let* ((len (c128vector-length vec))
         (cnt (c128vector-count pred vec))
         (r (make-c128vector len)))
    (let loop ((i 0) (yes 0) (no cnt))
      (cond
        ((fx= i len) r)
        ((pred (c128vector-ref vec i))
         (c128vector-set! r yes (c128vector-ref vec i))
         (loop (fx+ i 1) (fx+ yes 1) no))
        (else
         (c128vector-set! r no (c128vector-ref vec i))
         (loop (fx+ i 1) yes (fx+ no 1)))))))

(define (c128vector-filter pred vec)
  (let* ((len (c128vector-length vec))
         (cnt (c128vector-count pred vec))
         (r (make-c128vector cnt)))
    (let loop ((i 0) (j 0))
      (cond
        ((fx= i len) r)
        ((pred (c128vector-ref vec i))
         (c128vector-set! r j (c128vector-ref vec i))
         (loop (fx+ i 1) (fx+ j 1)))
        (else
         (loop (fx+ i 1) j))))))

(define (c128vector-remove pred vec)
  (c128vector-filter (lambda (x) (not (pred x))) vec))

;; c128vector-set! defined in (srfi 160 base)

(define (c128vector-swap! vec i j)
  (let ((ival (c128vector-ref vec i))
        (jval (c128vector-ref vec j)))
    (c128vector-set! vec i jval)
    (c128vector-set! vec j ival)))

(define c128vector-fill!
  (case-lambda
    ((vec fill) (c128vector-fill-some! vec fill 0 (c128vector-length vec)))
    ((vec fill start) (c128vector-fill-some! vec fill start (c128vector-length vec)))
    ((vec fill start end) (c128vector-fill-some! vec fill start end))))

(define (c128vector-fill-some! vec fill start end)
  (unless (fx= start end)
    (c128vector-set! vec start fill)
    (c128vector-fill-some! vec fill (fx+ start 1) end)))

(define c128vector-reverse!
  (case-lambda
    ((vec) (c128vector-reverse-some! vec 0 (c128vector-length vec)))
    ((vec start) (c128vector-reverse-some! vec start (c128vector-length vec)))
    ((vec start end) (c128vector-reverse-some! vec start end))))

(define (c128vector-reverse-some! vec start end)
  (let loop ((i start) (j (fx- end 1)))
    (when (fx< i j)
      (c128vector-swap! vec i j)
      (loop (fx+ i 1) (fx- j 1)))))

(define (c128vector-unfold! f vec start end seed)
  (let loop ((i start) (seed seed))
    (when (fx< i end)
      (let-values (((elt seed) (f seed)))
        (c128vector-set! vec i elt)
        (loop (fx+ i 1) seed)))))

(define (c128vector-unfold-right! f vec start end seed)
  (let loop ((i (fx- end 1)) (seed seed))
    (when (fx>= i start)
      (let-values (((elt seed) (f seed)))
        (c128vector-set! vec i elt)
        (loop (fx- i 1) seed)))))

(define reverse-c128vector->list
  (case-lambda
    ((vec) (reverse-c128vector->list* vec 0 (c128vector-length vec)))
    ((vec start) (reverse-c128vector->list* vec start (c128vector-length vec)))
    ((vec start end) (reverse-c128vector->list* vec start end))))

(define (reverse-c128vector->list* vec start end)
  (let loop ((i start) (r '()))
    (if (fx= i end)
      r
      (loop (fx+ 1 i) (cons (c128vector-ref vec i) r)))))

(define (reverse-list->c128vector list)
  (let* ((len (length list))
         (r (make-c128vector len)))
    (let loop ((i 0) (list list))
      (cond
        ((fx= i len) r)
        (else
          (c128vector-set! r (fx- (fx- len i) 1) (car list))
          (loop (fx+ i 1) (cdr list)))))))

(define c128vector->vector
  (case-lambda
    ((vec) (c128vector->vector* vec 0 (c128vector-length vec)))
    ((vec start) (c128vector->vector* vec start (c128vector-length vec)))
    ((vec start end) (c128vector->vector* vec start end))))

(define (c128vector->vector* vec start end)
  (let* ((len (fx- end start))
         (r (make-vector len)))
    (let loop ((i start) (o 0))
      (cond
        ((fx= i end) r)
        (else
          (vector-set! r o (c128vector-ref vec i))
          (loop (fx+ i 1) (fx+ o 1)))))))

(define vector->c128vector
  (case-lambda
    ((vec) (vector->c128vector* vec 0 (vector-length vec)))
    ((vec start) (vector->c128vector* vec start (vector-length vec)))
    ((vec start end) (vector->c128vector* vec start end))))

(define (vector->c128vector* vec start end)
  (let* ((len (fx- end start))
         (r (make-c128vector len)))
    (let loop ((i start) (o 0))
      (cond
        ((fx= i end) r)
        (else
          (c128vector-set! r o (vector-ref vec i))
          (loop (fx+ i 1) (fx+ o 1)))))))

(define make-c128vector-generator
  (case-lambda ((vec) (make-c128vector-generator vec 0 (c128vector-length vec)))
               ((vec start) (make-c128vector-generator vec start (c128vector-length vec)))
               ((vec start end)
                (lambda () (if (fx>= start end)
                             (eof-object)
                             (let ((next (c128vector-ref vec start)))
                              (set! start (fx+ start 1))
                              next))))))

(define write-c128vector
  (case-lambda
    ((vec) (write-c128vector* vec (current-output-port)))
    ((vec port) (write-c128vector* vec port))))


(define (write-c128vector* vec port)
  (display "#c128(" port)  ; c128-expansion is blind, so will expand this too
  (let ((last (fx- (c128vector-length vec) 1)))
    (let loop ((i 0))
      (cond
        ((fx= i last)
         (write (c128vector-ref vec i) port)
         (display ")" port))
        (else
          (write (c128vector-ref vec i) port)
          (display " " port)
          (loop (fx+ i 1)))))))

(define (c128vector< vec1 vec2)
  (let ((len1 (c128vector-length vec1))
        (len2 (c128vector-length vec2)))
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
           ((< (c128vector-ref vec1 i) (c128vector-ref vec2 i))
            #t)
           ((> (c128vector-ref vec1 i) (c128vector-ref vec2 i))
            #f)
           (else
             (loop (fx+ i 1)))))))))

(define (c128vector-hash vec)
  (let ((len (min 256 (c128vector-length vec))))
    (let loop ((i 0) (r 0))
      (if (fx= i len)
        (abs (floor (real-part (inexact->exact r))))
        (loop (fx+ i 1) (+ r (c128vector-ref vec i)))))))

(define c128vector-comparator
  (make-comparator c128vector? c128vector= c128vector< c128vector-hash))
