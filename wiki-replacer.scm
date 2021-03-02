#!/usr/bin/csi -ss

(import chicken.irregex
        chicken.io
        srfi-13)

(define (sub-types line)
  (let loop ((types '(u8 s8 u16 s16 u32 s32 u64 s64 f32 f64 c64 c128)))
    (unless (null? types)
      (cons
       (irregex-replace "@" line (symbol->string (car types)))
       (loop (cdr types))))))

(define (main args)
  (let ((file (car args)))
    (call-with-input-file file
      (lambda (p)
       (do ((line (read-line p) (read-line p)))
          ((eof-object? line))
         (if (and (irregex-search "^<[^>]+>" line)
                  (irregex-search "@" line))
            (for-each
             (lambda (l) (display l) (newline))
             (sub-types line))
            (begin
              (display line)
              (newline))))))))
