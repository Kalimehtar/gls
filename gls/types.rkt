;; types.scm

#lang racket/base
(require (for-syntax racket/base))
(require (only-in srfi/1 any every list=) "utils.rkt" racket/function (prefix-in c: racket/class))

(provide (struct-out generic) (except-out (struct-out method) method) (struct-out call-context)
         (struct-out signature-type) make-signature-type
         subtype? *call-context* isa? type-equal? and? or?
         <top> ==)

;; TO DO:
;; * limited list typecheck caching:
;;   Want to cache most specific derived list type informatio about lists.
;;   When a list value passes a list-of or list-with predicate, that fact
;;   should be recorded.  Later, when cons, set-car, set-cdr! is done, 
;;   the type of the resulting list can be calculated by doing a lub with types
;;   of new value(s).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  TOP TYPE
;;
(struct top-type ()
  #:property prop:procedure
  (lambda (type x) #t))
(define <top> (top-type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  EQ TYPES
;;
(struct eq-type (val)
  #:property prop:procedure
  (lambda (type x)
    (eq? x (eq-type-val type)))
  #:methods gen:custom-write
  [(define (write-proc v port mode)
     ((recur-write-proc mode) `(eq-type ,(eq-type-val v)) port))])

(define == eq-type)

(define <eq-type> eq-type?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  AND TYPES
;;
(struct and-type (types) 
  #:constructor-name really-make-and-type
  #:property prop:procedure
  (lambda (type x)
    (for/and ([t (in-list (and-type-types type))])
      (isa? x t)))
  #:methods gen:custom-write
  [(define (write-proc v port mode)
     ((recur-write-proc mode) `(and-type ,@(and-type-types v)) port))])
  
(define <and-type> and-type?)

;; simple normalization of and-types:
;;   (1) (and x (and y z)) => (and x y z)
;;   (2) (and x y z), (subtype? x y) => (and x z)
;;   (3) (and x) => x
(define (make-and-type . types)
  ;; first get flat list of conjuncts (in reverse order)
  (let ((types1 (foldl (lambda (type out)
			(if (and-type? type)
			    (append (reverse (and-type-types type)) out)
			    (cons type out)))
		      '() types)))
    (dbg 'types "make-and-type, types1 = ~a" types1)
    ;; next see if can merge any conjuncts
    (let ((types2 (foldl (lambda (type1 out)
			  ;; if any other types are subtypes of type, ignore type
			  (if (any (lambda (type2)
				     (and (not (eq? type1 type2))
					  (subtype? type2 type1)))
				   types1)
			      out
			      (cons type1 out)))
			'() types1)))
      (dbg 'types "make-and-type, types2 = ~a" types2)
      (if (= 1 (length types2))
	  (car types2)
	  (really-make-and-type types2)))))

(define and? make-and-type)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  OR TYPES
;;

(struct or-type (types) 
  #:constructor-name really-make-or-type
  #:property prop:procedure
  (lambda (type x)
    (for/or ([t (in-list (and-type-types type))])
      (isa? x t)))
  #:methods gen:custom-write
  [(define (write-proc v port mode)
     ((recur-write-proc mode) `(or-type ,@(or-type-types v)) port))])
;(define-record-type or-type :or-type
;  (really-make-or-type disjuncts)
;  or-type?
;  (disjuncts or-type-types))

;(define-record-discloser :or-type
;  (lambda (v) `(or-type ,@(or-type-types v))))

(define <or-type> or-type?)

;; simple normalization of or-types:
;; (1) (or x (or y z)) => (or x y z)
;; (2) (or x y z), (subtype? x y) => (or y z)
;; (3) (or x) => x
(define (make-or-type . types)
  ;; first get flat list of disjuncts
  (let ((types1 (foldl (lambda (type out)
			(if (or-type? type)
			    (append (reverse (or-type-types type)) out)
			    (cons type out)))
		      '() types)))
    (dbg 'types "make-or-type, types1 = ~a" types1)
    ;; next see if can merge any disjuncts
    (let ((types2 (foldl (lambda (type1 out)
			  ;; if this is a subtype of any other type, ignore this
			  (if (any (lambda (type2)
				     (and (not (eq? type1 type2))
					  (subtype? type1 type2)))
				   types1)
			      out
			      (cons type1 out)))
			'() types1)))
      (dbg 'types "make-or-type, types2 = ~a" types2)
      (if (= 1 (length types2))
	  (car types2)
	  (really-make-or-type types2)))))

(define or? make-or-type)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SIGNATURE-TYPES  
;;
;; rest-type/f is element type of each remaining element.
;;   - if rest-type/f is #f, must have exactly (length types) elt.s
(struct signature-type (types rest-type/f)
  #:constructor-name really-make-signature-type
  #:property prop:procedure
  (lambda (type val)
    (cond [(list? val)
           (vals-match-signature? val type)]
          [(vector? val)
           (vals-match-signature? (vector->list val) type)]
          [else #f]))
  #:methods gen:custom-write
  [(define (write-proc v port mode)
     ((recur-write-proc mode) `(signature-type ,(signature-type-types v) ':
                                               ,(signature-type-rest-type/f v))
                               port))])

(define (make-signature-type rest-type/f . types)
  (really-make-signature-type types rest-type/f))

(define <signature-type> signature-type?)

;; *covariant*
;; true if any (list) value satisfying sig1 will also satisfy sig2
(define (signature-subtype? sig1 sig2)
  ;; look at arg-types first
  (let ((rest1 (signature-type-rest-type/f sig1))
	(rest2 (signature-type-rest-type/f sig2)))
    (let loop ((arg-types1 (signature-type-types sig1))
	       (arg-types2 (signature-type-types sig2)))
      (cond
       ((null? arg-types1)		; done with arg-types1
	(if (null? arg-types2)	
	    ;; same # of arg-types
	    (if rest1
		(if rest2
		    (subtype? rest1 rest2)
		    #f)			; sig1 has rest, sig2 doesn't
		#t)			; sig1 has no rest.
	    ;; more arg-types2 (but done with arg-types1)
	    (if rest1
		(let loop2 ((arg-types2 arg-types2))
		  (if (null? arg-types2)
		      (if rest2
			  (subtype? rest1 rest2)
			  #f)		; sig1 has rest, sig2 doesn't
		      (if (subtype? rest1 (car arg-types2))
			  (loop2 (cdr arg-types2))
			  #f)))
		#f)))			; more arg-types2 but no more rest1
       ((null? arg-types2)		; more arg1's than arg2's
	(if rest2
	    (let loop2 ((arg-types1 arg-types1))
	      (if (null? arg-types1)
		  (if rest1
		      (subtype? rest1 rest2)
		      #t)
		  (if (subtype? (car arg-types1) rest2)
		      (loop2 (cdr arg-types1))
		      #f)))
	    #f))			; more arg-types1 but no rest2
       (else				; more of both
	(if (subtype? (car arg-types1) (car arg-types2))
	    (loop (cdr arg-types1) (cdr arg-types2))
	    #f))))))
	
(define (vals-match-signature? orig-vals sig)
  (let ((arg-types (signature-type-types sig))
	(rest-type/f (signature-type-rest-type/f sig)))
    ;; quick check 1st:
    (if (or (< (length orig-vals) (length arg-types)) ; not enuf
	    (and (not rest-type/f)
		 (> (length orig-vals) (length arg-types)))) ; too many
	#f
	;; check types
	(let loop ((vals orig-vals)
		   (types arg-types)
		   (in-rest? (null? arg-types)))
	  (cond
	   ((null? vals)		; we already know there are the right #
	    #t)
	   (in-rest?
	    (and (isa? (car vals) rest-type/f)
		 (loop (cdr vals) '() in-rest?)))
	   ((isa? (car vals) (car types))
	    (loop (cdr vals) (cdr types)
		  (null? (cdr types))))
	   (else
	    #f))))))

;; TO DO: make signature-equal? a method
(define (signature-equal? s1 s2)
  ;; (method ((s1 <signature>) (s2 <signature>)) => <boolean>
  (list= eq?
      (cons (signature-type-rest-type/f s1)
            (signature-type-types s1))
      (cons (signature-type-rest-type/f s2)
            (signature-type-types s2))))

;; throw an exception if check fails
(define (check-applicable! type args)
  (if (isa? args type)
      #t
      (error (format "ck-app!: args ~a don't satisfy method type ~a" args type))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; METHOD TYPES
;; 
(struct method-type (args-type result-type) 
  #:constructor-name really-make-method-type
  #:property prop:procedure
  (lambda (type val)
    (cond [(generic? val)
           (for/and ([v (in-list (generic-methods val))])
             (isa? v type))]
          [(method? val)
           (and
            ;; use contravariant comparison of arg-types:
            (subtype? (method-type-args-type type)
                      (method-args-type val))
            ;; and covariant comparison of result-types:
            (subtype? (method-result-type val)
                      (method-type-result-type type)))]
          [else #f]))
  #:methods gen:custom-write
  [(define (write-proc v port mode)
     ((recur-write-proc mode) `(method-type ,(method-type-args-type v) ->
                                            ,(method-type-result-type v))
                               port))])

;; common case
(define (make-method-type rest-type/f result-type . arg-types)
  (really-make-method-type
   (really-make-signature-type arg-types rest-type/f)
   result-type))

(define <method-type> method-type?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ISA?
;;
(define-syntax (make-selector stx)
  (syntax-case stx ()
    [(_ selector action mora-actions ...)
     (with-syntax ([val (datum->syntax stx 'val)]
                   [type (datum->syntax stx 'type)])
       #'(cons selector (λ (val type) action mora-actions ...)))]))

(define (isa? val type)
  (if (c:class? type) (c:is-a? val type) (type val)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  SUBTYPE?
;;
;; true if any instance of t1 can be used in a context requiring a t2
;; that is, true if for every value v, (isa? v t1) => (isa? v t2)
;; - note that predicate types are not comparable.
(define (subtype? t1 t2)
  (cond
   [(eq? t1 t2) #t]
   [(eq? t2 <top>) #t]
   [(eq? t1 <top>) #f]
   ;; (and t1_1 ... t1_n) <= t2  iff 
   ;;   1. if t2 = (and t2_1 ... t2_m), t1 <= t2_i for all t2_i.
   ;;   2. else some t1_i <= t2.
   [(and-type? t1)
    (if (and-type? t2)
	(every (curry subtype? t1) (and-type-types t2))
	(any (curryr subtype? t2) (and-type-types t1)))]
   [(or-type? t1);; (or t1_1 ... t1_n) <= t2  iff all t1_i <= t2
    (every (curryr subtype? t2) (or-type-types t1))]
   [(eq-type? t1);; (eq v) <= t2  iff  v : t2 or t2 : eq(v)
    (or (isa? (eq-type-val t1) t2)
	(and (eq-type? t2) (equal? (eq-type-val t1) (eq-type-val t2))))]
   [(and-type? t2);; t1 <= (and t2_1 ... t2_n) iff t1 <= t2_i for all i
    (every (curry subtype? t1) (and-type-types t2))]
   [(or-type? t2);; t1 <= (or t2_1 ... t2_n) iff t1 <= t2_i for some i
    (any (curry subtype? t1) (or-type-types t2))]
   [(c:class? t1) (c:subclass? t1 t2)]
   [(signature-type? t1)
    (and (signature-type? t2)
	 (signature-subtype? t1 t2))]	; covariant
   [(method-type? t1)
    (and (method-type? t2)
	 ;; contravariant in the arg types
	 (subtype? (method-type-args-type t2) (method-type-args-type t1))
	 ;; covariant in result types
	 (subtype? (method-type-result-type t1)
		   (method-type-result-type t2)))]
   [else #f]))

(define <type>
  (make-or-type <eq-type> <and-type> <or-type>
		<signature-type> <method-type>
		(lambda (v) (eq? v <top>))))

;		predicate-subtype?))
		     
;; TO DO: throw an exception on error
;; returns val on success
(define (check-type! val type)
  (if (isa? val type)
      val
      (error 'check-type "check-type! failed: ~a ~a" val type)))

(define <false> (== #f))

(define (false-or t)
  (make-or-type <false> t))

(define (type-equal? t1 t2)
  (or (eq? t1 t2)
      (cond
       ((eq? <top> t1) (eq? <top> t2))
       ((and-type? t1) 
	(and (and-type? t2)
	     (= (length (and-type-types t1)) (length (and-type-types t2)))
	     (every type-equal? (and-type-types t1) (and-type-types t2))))
       ((or-type? t1)			; disjunction
	(and (or-type? t2)
	     (= (length (or-type-types t1)) (length (or-type-types t2)))
	     (every type-equal? (or-type-types t1) (or-type-types t2))))
       ((eq-type? t1)
	(and (eq-type? t2)
	     (equal? (eq-type-val t1) (eq-type-val t2))))
       ((signature-type? t1)
	(and (signature-type? t2)
	     (= (length (signature-type-types t1)) (length (signature-type-types t2)))
	     (every type-equal? (signature-type-types t1) (signature-type-types t2))
	     ;; rest-types are either both #f or both equal types
	     (or (and (eq? #f (signature-type-rest-type/f t1))
		      (eq? #f (signature-type-rest-type/f t2)))
		 (type-equal? (signature-type-rest-type/f t1)
			      (signature-type-rest-type/f t2)))))
       ((method-type? t1)
	(and (method-type? t2)
	     (type-equal? (method-type-args-type t1) (method-type-args-type t2))
	     (type-equal? (method-type-result-type t1) (method-type-result-type t2))))
       (else
	#f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   GENERIC FUNCTIONS

;(struct call-context (generic chain next callable argvals executor))

(struct generic ([name #:mutable] [methods #:mutable] [composer #:mutable] add-method-check/f)
  #:constructor-name really-make-generic
  #:property prop:procedure
  (lambda (real-gf . args)
    ;; (1) find applicable methods
    (let ((app-ms
           (filter (curryr standard-method-applicable? args)
                   (generic-methods real-gf))))
      ;; (2) compose applicable methods 
      (let ([context
             ((generic-composer real-gf) real-gf app-ms args)])
        ;; (3) initiate new call context
        (parameterize ([*call-context* context])
          ((call-context-executor context))))))
  #:methods gen:custom-write
  [(define (write-proc v port mode)
     ((recur-write-proc mode) `(generic ,(generic-name v)) port))])

(struct method ([name #:mutable] args-type result-type callable [generic/f #:mutable])
  #:constructor-name make-method
  #:property prop:procedure
  (lambda (real-m . args)
    (dbg 'calls "  call-method - checking sig...")
    ;; TO DO: many times, this check is redundant -- if the method was put
    ;; into an effective fn., it has already shown its applicability!
    ;(check-applicable! (method-args-type real-m) args) ; throws error if fails
    (dbg 'calls "    sig. checked - doing apply")
    (set-call-context-callable! (*call-context*) real-m)
    (if (and (method-result-type real-m)
             (not (top-type? (method-result-type real-m))))
        (check-type! (apply (method-callable real-m) args)
                     (method-result-type real-m))
        (apply (method-callable real-m) args)))
  #:methods gen:custom-write
  [(define (write-proc v port mode)
     ((recur-write-proc mode) `(method ,(method-name v) ,(method-args-type v)
                                       => ,(method-result-type v))
                              port))])

(define (standard-method-applicable? m vals)
  (isa? vals (method-args-type m)))

(struct call-context (generic 
                      [chain #:mutable]
                      [next #:mutable]
                      [callable #:mutable]
                      argvals
                      [executor #:mutable]))

(define *call-context* (make-parameter (call-context #f #f #f #f #f #f)))
