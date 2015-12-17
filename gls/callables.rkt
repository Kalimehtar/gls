#lang racket/base

(require (only-in srfi/1 any find) "utils.rkt" "types.rkt" racket/function)
(provide make-generic make-named-generic add-method replace-method call-next-method
         add-around-method add-before-method add-after-method)
;; callables.scm
;; Definitions of methods, generics, signatures, and the functions that call them.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   GENERIC FUNCTIONS


(define (make-generic .  methods)
  (apply make-named-generic "anon" methods))

(define (make-named-generic name . methods)
  (let ((gf (really-make-generic name methods
				 primary-composer standard-add-method-check)))
    (for-each (lambda (m)
		(set-method-generic/f! m gf))
	      methods)
    gf))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CALL CONTEXT (reflective interface to dynamic state)
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   METHOD COMPOSITION
;;;

;; Choose a single most applicable method, and leave the rest for next-ms
;; Throw exception if not a single most applicable method.
(define (primary-composer generic app-ms vals)
  (if (null? app-ms)
      (error 'primary-composer "No applicable method, generic=~a, vals=~a" generic vals)
      (let ((mams (standard-method-selector app-ms vals)))
	(when (> (length mams) 1)
	    (error 'primary-composer "Ambiguous: ~a on values ~a" mams vals))
	;; here we know mams is a list of length 1
	(call-context
	 generic
         mams			; generic, chain
	 (remove (car mams) app-ms) ; next
	 #f
         vals			; callable, argvals
	 (lambda () (apply (car mams) vals)))))) ; executor
	
;; chain in increasing order
(define (before-composer generic app-ms vals)
  (let ((sorted-ms
	 (reverse			; want general to specific
	  (sort
	   app-ms
	   (lambda (m1 m2)
	     (subtype? (method-args-type m1)
		       (method-args-type m2)))))))
    (chain-composer generic sorted-ms vals)))

;; chain in decreasing order
(define (after-composer generic app-ms vals)
  (let ((sorted-ms
	 (sort
	  app-ms
	  (lambda (m1 m2)
	    (subtype? (method-args-type m1)
		      (method-args-type m2))))))
    (chain-composer generic sorted-ms vals)))

;; Composer for a generic with before, after, around, and primary 
;; generic functions.
;; Note that currently, this is identical to after-composer (!)
;; but needs to be a separate object because it's used as the tag
;; to decide if a generic has been "two-leveled".
(define (method-combination-composer . args)
  (apply after-composer args))
	     
; TO DO: *** Concern: next-method from last before method should go to primary ***

(define *return-value* (make-parameter #f))

;; simply execute the applicable methods in order
;;  Note that the fluid variable *return-value*
;;    is bound during execution of an <after> method, if any.
(define (chain-composer generic app-ms vals)
  (call-context
   generic app-ms			; generic, chain
   '() #f vals				; next, callable, args
   (lambda ()				; executor
     (foldl (lambda (m-todo return-val)
	     (if (eq? <after> (method-args-type m-todo))
		 (parameterize ([*return-value* return-val])
                   (apply m-todo vals)
                   return-val)
		 (apply m-todo vals)))
	   #f
	   app-ms))))

;; TO DO: call-next-method is too expensive.
;; keeps the current call context, but mutates chain and next
(define (call-next-method)
  (let ((the-context (*call-context*)))
    (cond
     ;; if we're in the midst of a chain, call next method in chain
     ;; ** for now, that means recompose to a new effective fn. **
     ((memq (call-context-callable the-context)
	    (call-context-chain the-context))
      => (lambda (chain-rest)
	   ;; recompose effective function
	   (let ((new-context
		  ((generic-composer (call-context-generic the-context))
		   (call-context-generic the-context)
		   ;; using rest of chain and all next methods
		   (append (cdr chain-rest)
			   (call-context-next the-context))
		   (call-context-argvals the-context))))
	     ;; keep the current context -- hack it             
	     (set-call-context-chain! the-context (call-context-chain new-context))
	     (set-call-context-next! the-context (call-context-next new-context))
	     ;; callable is set by the individual method (in call-method)
	     ;; argvals stay the same
	     (set-call-context-executor! the-context (call-context-executor new-context))
	     ;; now go
	     ((call-context-executor new-context)))))
     (else
      (error "call-next-method called while not in a chain")))))

;; Returns most applicable methods (more than one if ambiguous).
(define (standard-method-selector app-meths vals)
  ;; Find most applicable (leaving all others unsorted).
  ;; -- uses only the methods' signatures -- not the actual arg.s.
  (foldl
   ;; mams holds candidates for mam (all elts mutually ambiguous)   
   (lambda (m mams)
     (let ((m-type (method-args-type m)))
       ;; Cannot have situation where both m is <= some method m' in mams
       ;; AND m is >= some other function m'' in mams -- that would imply that
       ;; m' and m'' are comparable and therefore not mutually ambiguous. 
       ;; Also, better not have case that arg-types(m) = arg-types(m') as that would
       ;; be duplicate methods problem.
       (cond
	;; if m >= any m' in mams, m is not a mam candidate
	((any (curryr subtype? m-type)
	      (map method-args-type mams))
	 mams)
	;; if m < any m' in mams, replace all such m' with m
	((any (curry subtype? m-type)
	      (map method-args-type mams))
	 (cons m
	       (filter
		(lambda (m1) 
		  (not (subtype?
			m-type (method-args-type m1))))
		mams)))
	;; otherwise, must be incomparable with all elts of mam, so add m
	(else
	 (cons m mams)))))
   (list (car app-meths))
   (cdr app-meths)))

(define (standard-method-applicable? m vals)
  (isa? vals (method-args-type m)))

;; errs if duplicate
(define (standard-add-method-check m gf)
  (if (find (lambda (m1) 
	      (type-equal? (method-args-type m1)
			   (method-args-type m)))
	    (generic-methods gf))
      (error "Adding duplicate method - use replace-method. ~a, ~a"
	     gf m)
      (void)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;          METHOD COMBINATION
;;

;;  before < primary < after
;;
(define <after> (const #t))
(define <primary> (and? <after> (const #t)))
(define <before> (and? <primary> (const #t)))

;; take plain old generic, with only primary methods, and 
;; replace generic-methods with 3 new "methods", before-method, after-method, 
;; and primary-method.  Each new method has a generic as its callable, and
;; each of these generics has a different composer fn.
;; change composer for original generic to method-combination-composer
(define (make-generic-two-level! gf)
  ;; does this generic need to be abstracted to a two-level generic?
  (when (not (or (eq? (generic-composer gf) method-combination-composer)
	       (eq? (generic-composer gf) around-composer)))
      	;(format #t "making generic ~a two-level~%" gf)
	(let* ((primary-generic
		;; updates method-generic/f ptr.s to new gf
		(apply make-named-generic
		       (format "primary generic for ~a" (generic-name gf))
		       (generic-methods gf)))
	       (primary-method
		(make-method (format "primary method for ~a"
				     (generic-name gf))
			     <primary> <top> ; args-type, result-type
			     primary-generic gf)) ; callable, generic/f
	       (before-generic
		;; no methods yet
		(really-make-generic
		 (format "before generic for ~a" (generic-name gf))
		 '() before-composer	; methods, composer
		 (const #t)))		; add-method: allow dupes
	       (before-method
		(make-method (format "before method for ~a"
				     (generic-name gf))
			     <before> <top> ; args-type, result-type
			     before-generic gf)) ; callable, generic/f
	       (after-generic
		;; no methods yet
		(really-make-generic
		 (format "after generic for ~a" (generic-name gf))
		 '() after-composer	; methods, composer
		 (const #t)))		; add-method: allow dupes
	       (after-method
		(make-method (format "after method for ~a"
				     (generic-name gf))
			     <after> <top> ; args-type, result-type
			     after-generic gf))) ; callable, generic/f
	  (set-generic-methods! gf (list before-method primary-method after-method))
	  (set-generic-composer! gf method-combination-composer))))

(define (find-hidden-generic gf ref-fn label)
  (cond
   ((eq? (generic-composer gf) around-composer)
    (cond ((find (lambda (m) (eq? (method-args-type m) <default-around-type>))
		 (generic-methods gf))
	   => (lambda (m) (find-hidden-generic (method-callable m) ref-fn label)))
	  (else
	   (error 'find-hidden-generic "could not find default around method, finding ~a generic"
		  label))))
   ((eq? (generic-composer gf) method-combination-composer)
    (method-callable (ref-fn (generic-methods gf))))
   (else
    (if (eq? label 'primary)
	gf
	(error 'find-hidden-generic "no ~a - generic not abstracted" label)))))

(define (before-generic gf) (find-hidden-generic gf car 'before))
(define (primary-generic gf) (find-hidden-generic gf cadr 'primary))
(define (after-generic gf) (find-hidden-generic gf caddr 'after))

(define (add-primary-method gf m)
  (let ((primary-gf (primary-generic gf)))
    (when (generic-add-method-check/f primary-gf)
	((generic-add-method-check/f primary-gf) m gf))
    (set-method-generic/f! m primary-gf)
    (set-generic-methods!
     primary-gf (cons m (generic-methods primary-gf)))))

;; add-method is synonym for add-primary-method
(define add-method add-primary-method)

(define (add-method* gf . ms)
  (for-each (curry add-primary-method gf)
	    ms))

(define (remove-primary-method gf sig)
  (let ((primary-gf (primary-generic gf)))
    (cond
     ((find (lambda (m1) 
	      (type-equal? sig (method-args-type m1)))
	      (generic-methods primary-gf))
      => (lambda (m)
	   (set-generic-methods!
	    primary-gf
            (remove m (generic-methods primary-gf)))))
     (else
      (error 'remove-primary-method "Could not find method matching ~a for generic ~a" sig gf)))))

(define (replace-primary-method gf sig m)
  (remove-primary-method gf sig)
  (add-primary-method gf m))
  
(define replace-method replace-primary-method)

;; remove-method is synonym for remove-primary-method
(define remove-method remove-primary-method)

(define (add-before-method gf m)
  (make-generic-two-level! gf)
  (add-primary-method (before-generic gf)
		      m))

(define (remove-before-method gf sig)
  (remove-primary-method (before-generic gf) sig))

(define (add-after-method gf m)
  (make-generic-two-level! gf)
  (add-primary-method (after-generic gf) m))

(define (remove-after-method gf sig)
  (remove-primary-method (after-generic gf) sig))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   AROUND METHODS   (are special)
;;

;; Choose a single most applicable method, and leave the rest for next-ms
;; Throw exception if not a single most applicable method.
;; around-composer is same as primary-composer, but needs separate identity. 
(define (around-composer generic app-ms vals)
  (if (null? app-ms)
      (error 'around-composer "No applicable around method, generic=~a, vals=~a" generic vals)
      (let ((mams (standard-method-selector app-ms vals)))
	(when (> (length mams) 1)
	    (error 'around-composer "Ambiguous: ~a on values ~a" mams vals))
	;; here we know mams is a list of length 1
	(call-context
	 generic mams			; generic, chain
	 (remove (car mams) app-ms) ; next
	 #f vals			; callable, argvals
	 (lambda () (apply (car mams) vals)))))) ; executor

(define <default-around-type> (or? <top> (const #t))) ; will be super of just <top>

;; take a "two-level" generic and turn it into a three-level generic.
(define (make-generic-arounded! gf)
  (make-generic-two-level! gf)
  (when (not (eq? (generic-composer gf) around-composer))
    ;(displayln "making generic aroundable.")
    ;; default-around will call before-primary-around methods as usual
    (let* ((default-generic		; callable of default around method
             (really-make-generic
              (format "default around generic for ~a" (generic-name gf))
              (generic-methods gf)
              method-combination-composer
              standard-add-method-check))
           (default-around-method
             (make-method
              (format "default around method for ~a" (generic-name gf))
              <default-around-type> <top> ; args-type, result-type
              default-generic gf)))	; callable, generic/f
      (set-generic-methods! gf (list default-around-method))
      (set-generic-composer! gf around-composer))))
	     
(define (add-around-method gf m)
  (make-generic-arounded! gf)
  (set-method-generic/f! m gf)
  (set-generic-methods! gf (cons m (generic-methods gf))))

;; eof
