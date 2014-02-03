;; Some generally useful functions, IMHO.  YMMV.
#lang racket/base

(provide recur-write-proc dbg assert)

(define (recur-write-proc mode)
  (case mode
    [(#t) write]
    [(#f) display]
    [else (lambda (p port) (print p port mode))]))

(define (assert test . args)
  (if (not test)
      (apply error args)
      (void)))

(define *dbg-tags* '())

(define (dbg . rest) (void))
;  (lambda (tag format-string . format-args)
;    (if (memq tag *dbg-tags*)
;	(let ([out (λ (str . args) (display (apply format str args)))])
;	  (out "[dbg:~a:" tag)
;	  (apply out format-string format-args)
;	  (out ":~a:dbg]~%" tag))
;        (void))))

;; eof
