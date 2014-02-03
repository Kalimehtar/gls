;; runtime-library.scm
#lang racket/base
(require "types.rkt" "utils.rkt" "callable-macros.rkt")
(provide <null> <complex> <real> <rational> <integer> <int> <callable>)
;; "nonessential" functions for GLOS runtime


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BUILT IN TYPES
;;
;; ***** !!!!! ***** !!!!! *****
;; It better be the case that if I declare type <x> to be
;; (and? <y> P), then the all values that satisfy P better be (isa? <y>)

;; define the following because some of the types (e.g. <complex>) are
;; subtypes (by being and-types).  Thus the programmer knows that if s/he
;; uses the <typename> types, the expected subtyping rules will apply.

(define <boolean> boolean?)
(define <pair> pair?)
(define <symbol> symbol?)
(define <number> number?)
(define <char> char?)
(define <string> string?)
(define <vector> vector?)
(define <port> port?)
(define <procedure> procedure?)

;; TO DO: more/better order to list/pair/null hierarchy
;; Why <list> \not\le <pair>: '() is a list but not a pair.
(define <list> list?)
;; Why <null> \not\le <list>: car and cdr are defined on lists but not '().
;; However, the fact that (list? '()) => #t seems to take precedence.
;;  (and the fact that traditionally lists are sum types of nonempty and empty)
(define <null> (and? <list> null?))

;; TO DO: more/better order to number hierarchy
(define <complex> (and? <number> complex?))
(define <real> (and? <complex> real?))
(define <rational> (and? <real> rational?))
(define <integer> (and? <rational> integer?))

(define <int> <integer>)

;(define <cell> cell?)

;; CALLABLES

(define <callable> (or? generic? method? procedure?))

;; method-signature is a convenience function for those methods
;; that have signature-type method-types.  The only methods for which we know 
;; this to be the case are record-subtype constructor methods and slot 
;; accessor methods.
(define (method-signature m)
  (if (signature-type? (method-args-type m))
      (method-args-type m)
      (error "Method ~a does not have a signature-type as its method-args-type"
	     m)))

; eof
