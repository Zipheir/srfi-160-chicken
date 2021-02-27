;;; This code is the same for all SRFI 160 vector sizes.
;;; The c64s appearing in the code are expanded to u8, s8, etc.

;; make-c64vector defined in (srfi 160 base)

;; c64vector defined in (srfi 160 base)

(define (c64vector-unfold f len seed)
  (let ((v (make-c64vector len)))
    (let loop ((i 0) (state seed))
      (unless (fx= i len)
        (let-values (((value newstate) (f i state)))
          (c64vector-set! v i value)
          (loop (fx+ i 1) newstate))))
    v))

(define (c64vector-unfold-right f len seed)
  (let ((v (make-c64vector len)))
    (let loop ((i (fx- len 1)) (state seed))
      (unless (fx= i -1)
        (let-values (((value newstate) (f i state)))
          (c64vector-set! v i value)
          (loop (fx- i 1) newstate))))
    v))

(define c64vector-copy
  (case-lambda
    ((vec) (c64vector-copy* vec 0 (c64vector-length vec)))
    ((vec start) (c64vector-copy* vec start (c64vector-length vec)))
    ((vec start end) (c64vector-copy* vec start end))))

(define (c64vector-copy* vec start end)
  (let ((v (make-c64vector (fx- end start))))
    (c64vector-copy! v 0 vec start end)
    v))

(define c64vector-copy!
  (case-lambda
    ((to at from)
     (move-memory! (c64vector-body from) (c64vector-body to) (c64vector-length (c64vector-body from)) 0 (fx* at 8)))
    ((to at from start)
     (move-memory! (c64vector-body from) (c64vector-body to) (c64vector-length (c64vector-body from)) (fx* start 8) (fx* at 8)))
    ((to at from start end)
     (move-memory! (c64vector-body from) (c64vector-body to)
                   (fx* 8 (fx- end start))
                   (fx* start 8)
                   (fx* at 8)))))

(define c64vector-reverse-copy
  (case-lambda
    ((vec) (c64vector-reverse-copy* vec 0 (c64vector-length vec)))
    ((vec start) (c64vector-reverse-copy* vec start (c64vector-length vec)))
    ((vec start end) (c64vector-reverse-copy* vec start end))))

(define (c64vector-reverse-copy* vec start end)
  (let ((v (make-c64vector (fx- end start))))
    (c64vector-reverse-copy! v 0 vec start end)
    v))

(define c64vector-reverse-copy!
  (case-lambda
    ((to at from)
     (c64vector-reverse-copy!* to at from 0 (c64vector-length from)))
    ((to at from start)
     (c64vector-reverse-copy!* to at from start (c64vector-length from)))
    ((to at from start end) (c64vector-reverse-copy!* to at from start end))))

(define (c64vector-reverse-copy!* to at from start end)
  (let loop ((at at) (i (fx- end 1)))
    (unless (fx< i start)
      (c64vector-set! to at (c64vector-ref from i))
      (loop (fx+ at 1) (fx- i 1)))))

(define (c64vector-append . vecs)
  (c64vector-concatenate vecs))

(define (c64vector-concatenate vecs)
  (let ((v (make-c64vector (len-sum vecs))))
    (let loop ((vecs vecs) (at 0))
      (unless (null? vecs)
        (let ((vec (car vecs)))
          (c64vector-copy! v at vec 0 (c64vector-length vec))
          (loop (cdr vecs) (fx+ at (c64vector-length vec)))))
    v)))

(define (len-sum vecs)
  (if (null? vecs)
    0
    (fx+ (c64vector-length (car vecs))
       (len-sum (cdr vecs)))))

(define (c64vector-append-subvectors . args)
  (let ((v (make-c64vector (len-subsum args))))
    (let loop ((args args) (at 0))
      (unless (null? args)
        (let ((vec (car args))
              (start (cadr args))
              (end (caddr args)))
          (c64vector-copy! v at vec start end)
          (loop (cdddr args) (fx+ at (fx- end start))))))
    v))

(define (len-subsum vecs)
  (if (null? vecs)
    0
    (fx+ (fx- (caddr vecs) (cadr vecs))
       (len-subsum (cdddr vecs)))))

;; c64? defined in (srfi 160 base)

;; c64vector? defined in (srfi 160 base)

(define (c64vector-empty? vec)
  (zero? (c64vector-length vec)))

(define (c64vector= . vecs)
  (c64vector=* (car vecs) (cadr vecs) (cddr vecs)))

(define (c64vector=* vec1 vec2 vecs)
  (and (c64dyadic-vecs= vec1 0 (c64vector-length vec1)
                      vec2 0 (c64vector-length vec2))
       (or (null? vecs)
           (c64vector=* vec2 (car vecs) (cdr vecs)))))

(define (c64dyadic-vecs= vec1 start1 end1 vec2 start2 end2)
  (cond
    ((not (fx= end1 end2)) #f)
    ((not (fx< start1 end1)) #t)
    ((let ((elt1 (c64vector-ref vec1 start1))
           (elt2 (c64vector-ref vec2 start2)))
      (= elt1 elt2))
     (c64dyadic-vecs= vec1 (fx+ start1 1) end1
                         vec2 (fx+ start2 1) end2))
    (else #f)))

;; c64vector-ref defined in (srfi 160 base)

;; c64vector-length defined in (srfi 160 base)

(define (c64vector-take vec n)
  (let ((v (make-c64vector n)))
    (c64vector-copy! v 0 vec 0 n)
    v))

(define (c64vector-take-right vec n)
  (let ((v (make-c64vector n))
        (len (c64vector-length vec)))
    (c64vector-copy! v 0 vec (fx- len n) len)
    v))

(define (c64vector-drop vec n)
 (let* ((len (c64vector-length vec))
        (vlen (fx- len n))
        (v (make-c64vector vlen)))
    (c64vector-copy! v 0 vec n len)
    v))

(define (c64vector-drop-right vec n)
  (let* ((len (c64vector-length vec))
         (rlen (fx- len n))
         (v (make-c64vector rlen)))
    (c64vector-copy! v 0 vec 0 rlen)
    v))

(define (c64vector-segment vec n)
  (let loop ((r '()) (i 0) (remain (c64vector-length vec)))
    (if (fx<= remain 0)
      (reverse r)
      (let ((size (min n remain)))
        (loop
          (cons (c64vector-copy vec i (fx+ i size)) r)
          (fx+ i size)
          (fx- remain size))))))

;; aux. procedure
(define (%c64vectors-ref vecs i)
  (map (lambda (v) (c64vector-ref v i)) vecs))

(define (c64vector-fold kons knil vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (c64vector-length vec)))
      (let loop ((r knil) (i 0))
        (if (fx= i len)
          r
          (loop (kons r (c64vector-ref vec i)) (fx+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map c64vector-length vecs))))
      (let loop ((r knil) (i 0))
        (if (fx= i len)
          r
          (loop (apply kons r (%c64vectors-ref vecs i))
                (fx+ i 1)))))))

(define (c64vector-fold-right kons knil vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (c64vector-length vec)))
      (let loop ((r knil) (i (fx- (c64vector-length vec) 1)))
        (if (negative? i)
          r
          (loop (kons r (c64vector-ref vec i)) (fx- i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map c64vector-length vecs))))
      (let loop ((r knil) (i (fx- len 1)))
        (if (negative? i)
          r
          (loop (apply kons r (%c64vectors-ref vecs i))
                (fx- i 1)))))))

(define (c64vector-map f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let* ((len (c64vector-length vec))
           (v (make-c64vector len)))
      (let loop ((i 0))
        (unless (fx= i len)
          (c64vector-set! v i (f (c64vector-ref vec i)))
          (loop (fx+ i 1))))
      v)
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map c64vector-length vecs)))
           (v (make-c64vector len)))
      (let loop ((i 0))
        (unless (fx= i len)
          (c64vector-set! v i (apply f (%c64vectors-ref vecs i)))
          (loop (fx+ i 1))))
      v)))


(define (c64vector-map! f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (c64vector-length vec)))
      (let loop ((i 0))
        (unless (fx= i len)
          (c64vector-set! vec i (f (c64vector-ref vec i)))
          (loop (fx+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map c64vector-length vecs))))
      (let loop ((i 0))
        (unless (fx= i len)
          (c64vector-set! vec i (apply f (%c64vectors-ref vecs i)))
          (loop (fx+ i 1)))))))

(define (c64vector-for-each f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (c64vector-length vec)))
      (let loop ((i 0))
        (unless (fx= i len)
          (f (c64vector-ref vec i))
          (loop (fx+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map c64vector-length vecs))))
      (let loop ((i 0))
        (unless (fx= i len)
          (apply f (%c64vectors-ref vecs i))
          (loop (fx+ i 1)))))))

(define (c64vector-count pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (c64vector-length vec)))
      (let loop ((i 0) (r 0))
        (cond
         ((fx= i (c64vector-length vec)) r)
         ((pred (c64vector-ref vec i)) (loop (fx+ i 1) (fx+ r 1)))
         (else (loop (fx+ i 1) r)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map c64vector-length vecs))))
      (let loop ((i 0) (r 0))
        (cond
         ((fx= i len) r)
         ((apply pred (%c64vectors-ref vecs i)) (loop (fx+ i 1) (fx+ r 1)))
         (else (loop (fx+ i 1) r)))))))

(define (c64vector-cumulate f knil vec)
  (let* ((len (c64vector-length vec))
         (v (make-c64vector len)))
    (let loop ((r knil) (i 0))
      (unless (fx= i len)
        (let ((next (f r (c64vector-ref vec i))))
          (c64vector-set! v i next)
          (loop next (fx+ i 1)))))
    v))

(define (c64vector-foreach f vec)
  (let ((len (c64vector-length vec)))
    (let loop ((i 0))
      (unless (fx= i len)
        (f (c64vector-ref vec i))
        (loop (fx+ i 1))))))

(define (c64vector-take-while pred vec)
  (let* ((len (c64vector-length vec))
         (idx (c64vector-skip pred vec))
         (idx* (if idx idx len)))
    (c64vector-copy vec 0 idx*)))

(define (c64vector-take-while-right pred vec)
  (let* ((len (c64vector-length vec))
         (idx (c64vector-skip-right pred vec))
         (idx* (if idx (fx+ idx 1) 0)))
    (c64vector-copy vec idx* len)))

(define (c64vector-drop-while pred vec)
  (let* ((len (c64vector-length vec))
         (idx (c64vector-skip pred vec))
         (idx* (if idx idx len)))
    (c64vector-copy vec idx* len)))

(define (c64vector-drop-while-right pred vec)
  (let* ((len (c64vector-length vec))
         (idx (c64vector-skip-right pred vec))
         (idx* (if idx idx -1)))
    (c64vector-copy vec 0 (fx+ 1 idx*))))

(define (c64vector-index pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (c64vector-length vec)))
      (let loop ((i 0))
        (cond
         ((fx= i len) #f)
         ((pred (c64vector-ref vec i)) i)
         (else (loop (fx+ i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map c64vector-length vecs))))
      (let loop ((i 0))
        (cond
         ((fx= i len) #f)
         ((apply pred (%c64vectors-ref vecs i)) i)
         (else (loop (fx+ i 1))))))))

(define (c64vector-index-right pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (c64vector-length vec)))
      (let loop ((i (fx- len 1)))
        (cond
         ((negative? i) #f)
         ((pred (c64vector-ref vec i)) i)
         (else (loop (fx- i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map c64vector-length vecs))))
      (let loop ((i (fx- len 1)))
        (cond
         ((negative? i) #f)
         ((apply pred (%c64vectors-ref vecs i)) i)
         (else (loop (fx- i 1))))))))

(define (c64vector-skip pred vec . vecs)
  (if (null? vecs)
    (c64vector-index (lambda (x) (not (pred x))) vec)
    (apply c64vector-index (lambda xs (not (apply pred xs))) vec vecs)))

(define (c64vector-skip-right pred vec . vecs)
  (if (null? vecs)
    (c64vector-index-right (lambda (x) (not (pred x))) vec)
    (apply c64vector-index-right (lambda xs (not (apply pred xs))) vec vecs)))

(define (c64vector-any pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (c64vector-length vec)))
      (let loop ((i 0))
        (cond
         ((fx= i len) #f)
         ((pred (c64vector-ref vec i)))  ;returns result of pred
         (else (loop (fx+ i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map c64vector-length vecs))))
      (let loop ((i 0))
        (cond
         ((fx= i len) #f)
         ((apply pred (%c64vectors-ref vecs i))) ;returns result of pred
         (else (loop (fx+ i 1))))))))

(define (c64vector-every pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (c64vector-length vec)))
      (let loop ((i 0) (last #t))
        (cond
         ((fx= i len) last)
         ((pred (c64vector-ref vec i)) => (lambda (r) (loop (fx+ i 1) r)))
         (else #f))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map c64vector-length vecs))))
      (let loop ((i 0) (last #t))
        (cond
         ((fx= i len) last)
         ((apply pred (%c64vectors-ref vecs i)) => (lambda (r) (loop (fx+ i 1) r)))
         (else #f))))))

(define (c64vector-partition pred vec)
  (let* ((len (c64vector-length vec))
         (cnt (c64vector-count pred vec))
         (r (make-c64vector len)))
    (let loop ((i 0) (yes 0) (no cnt))
      (cond
        ((fx= i len) r)
        ((pred (c64vector-ref vec i))
         (c64vector-set! r yes (c64vector-ref vec i))
         (loop (fx+ i 1) (fx+ yes 1) no))
        (else
         (c64vector-set! r no (c64vector-ref vec i))
         (loop (fx+ i 1) yes (fx+ no 1)))))))

(define (c64vector-filter pred vec)
  (let* ((len (c64vector-length vec))
         (cnt (c64vector-count pred vec))
         (r (make-c64vector cnt)))
    (let loop ((i 0) (j 0))
      (cond
        ((fx= i len) r)
        ((pred (c64vector-ref vec i))
         (c64vector-set! r j (c64vector-ref vec i))
         (loop (fx+ i 1) (fx+ j 1)))
        (else
         (loop (fx+ i 1) j))))))

(define (c64vector-remove pred vec)
  (c64vector-filter (lambda (x) (not (pred x))) vec))

;; c64vector-set! defined in (srfi 160 base)

(define (c64vector-swap! vec i j)
  (let ((ival (c64vector-ref vec i))
        (jval (c64vector-ref vec j)))
    (c64vector-set! vec i jval)
    (c64vector-set! vec j ival)))

(define c64vector-fill!
  (case-lambda
    ((vec fill) (c64vector-fill-some! vec fill 0 (c64vector-length vec)))
    ((vec fill start) (c64vector-fill-some! vec fill start (c64vector-length vec)))
    ((vec fill start end) (c64vector-fill-some! vec fill start end))))

(define (c64vector-fill-some! vec fill start end)
  (unless (fx= start end)
    (c64vector-set! vec start fill)
    (c64vector-fill-some! vec fill (fx+ start 1) end)))

(define c64vector-reverse!
  (case-lambda
    ((vec) (c64vector-reverse-some! vec 0 (c64vector-length vec)))
    ((vec start) (c64vector-reverse-some! vec start (c64vector-length vec)))
    ((vec start end) (c64vector-reverse-some! vec start end))))

(define (c64vector-reverse-some! vec start end)
  (let loop ((i start) (j (fx- end 1)))
    (when (fx< i j)
      (c64vector-swap! vec i j)
      (loop (fx+ i 1) (fx- j 1)))))

(define (c64vector-unfold! f vec start end seed)
  (let loop ((i start) (seed seed))
    (when (fx< i end)
      (let-values (((elt seed) (f seed)))
        (c64vector-set! vec i elt)
        (loop (fx+ i 1) seed)))))

(define (c64vector-unfold-right! f vec start end seed)
  (let loop ((i (fx- end 1)) (seed seed))
    (when (fx>= i start)
      (let-values (((elt seed) (f seed)))
        (c64vector-set! vec i elt)
        (loop (fx- i 1) seed)))))

(define reverse-c64vector->list
  (case-lambda
    ((vec) (reverse-c64vector->list* vec 0 (c64vector-length vec)))
    ((vec start) (reverse-c64vector->list* vec start (c64vector-length vec)))
    ((vec start end) (reverse-c64vector->list* vec start end))))

(define (reverse-c64vector->list* vec start end)
  (let loop ((i start) (r '()))
    (if (fx= i end)
      r
      (loop (fx+ 1 i) (cons (c64vector-ref vec i) r)))))

(define (reverse-list->c64vector list)
  (let* ((len (length list))
         (r (make-c64vector len)))
    (let loop ((i 0) (list list))
      (cond
        ((fx= i len) r)
        (else
          (c64vector-set! r (fx- (fx- len i) 1) (car list))
          (loop (fx+ i 1) (cdr list)))))))

(define c64vector->vector
  (case-lambda
    ((vec) (c64vector->vector* vec 0 (c64vector-length vec)))
    ((vec start) (c64vector->vector* vec start (c64vector-length vec)))
    ((vec start end) (c64vector->vector* vec start end))))

(define (c64vector->vector* vec start end)
  (let* ((len (fx- end start))
         (r (make-vector len)))
    (let loop ((i start) (o 0))
      (cond
        ((fx= i end) r)
        (else
          (vector-set! r o (c64vector-ref vec i))
          (loop (fx+ i 1) (fx+ o 1)))))))

(define vector->c64vector
  (case-lambda
    ((vec) (vector->c64vector* vec 0 (vector-length vec)))
    ((vec start) (vector->c64vector* vec start (vector-length vec)))
    ((vec start end) (vector->c64vector* vec start end))))

(define (vector->c64vector* vec start end)
  (let* ((len (fx- end start))
         (r (make-c64vector len)))
    (let loop ((i start) (o 0))
      (cond
        ((fx= i end) r)
        (else
          (c64vector-set! r o (vector-ref vec i))
          (loop (fx+ i 1) (fx+ o 1)))))))

(define make-c64vector-generator
  (case-lambda ((vec) (make-c64vector-generator vec 0 (c64vector-length vec)))
               ((vec start) (make-c64vector-generator vec start (c64vector-length vec)))
               ((vec start end)
                (lambda () (if (fx>= start end)
                             (eof-object)
                             (let ((next (c64vector-ref vec start)))
                              (set! start (fx+ start 1))
                              next))))))

(define write-c64vector
  (case-lambda
    ((vec) (write-c64vector* vec (current-output-port)))
    ((vec port) (write-c64vector* vec port))))


(define (write-c64vector* vec port)
  (display "#c64(" port)  ; c64-expansion is blind, so will expand this too
  (let ((last (fx- (c64vector-length vec) 1)))
    (let loop ((i 0))
      (cond
        ((fx= i last)
         (write (c64vector-ref vec i) port)
         (display ")" port))
        (else
          (write (c64vector-ref vec i) port)
          (display " " port)
          (loop (fx+ i 1)))))))

(define (c64vector< vec1 vec2)
  (let ((len1 (c64vector-length vec1))
        (len2 (c64vector-length vec2)))
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
           ((< (c64vector-ref vec1 i) (c64vector-ref vec2 i))
            #t)
           ((> (c64vector-ref vec1 i) (c64vector-ref vec2 i))
            #f)
           (else
             (loop (fx+ i 1)))))))))

(define (c64vector-hash vec)
  (let ((len (min 256 (c64vector-length vec))))
    (let loop ((i 0) (r 0))
      (if (fx= i len)
        (abs (floor (real-part (inexact->exact r))))
        (loop (fx+ i 1) (+ r (c64vector-ref vec i)))))))

(define c64vector-comparator
  (make-comparator c64vector? c64vector= c64vector< c64vector-hash))
