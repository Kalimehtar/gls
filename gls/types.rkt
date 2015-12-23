;; types.scm

#lang racket/base
;(require (for-syntax racket/base))
(require (only-in srfi/1 any every list=)
         "utils.rkt"
         racket/function
         (prefix-in c: racket/class))

(provide (struct-out generic)
         (except-out (struct-out method) method)
         (struct-out call-context)
         (struct-out signature-type)
         make-signature-type
         subtype?
         subtype!
         *call-context*
         isa?
         type-equal?
         and?
         or?
         compose?
         ==)

(module+ test
  (require rackunit racket/function))

;; TO DO:
;; * limited list typecheck caching:
;;   Want to cache most specific derived list type informatio about lists.
;;   When a list value passes a list-of or list-with predicate, that fact
;;   should be recorded.  Later, when cons, set-car, set-cdr! is done, 
;;   the type of the resulting list can be calculated by doing a lub with types
;;   of new value(s).

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

;(define <eq-type> eq-type?)

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
  
;; simple normalization of and-types:
;;   (1) (and x (and y z)) => (and x y z)
;;   (2) (and x y z), (subtype? x y) => (and x z)
;;   (3) (and x) => x
(define (make-and-type . types)
  ;; first get flat list of conjuncts (in reverse order)
  (define types1 (for/fold ([out null])
                           ([type (in-list types)])                    
                    (if (and-type? type)
                        (append (reverse (and-type-types type)) out)
                        (cons type out))))
  ;; next see if can merge any conjuncts
  (define types2 (for/fold ([out null])
                           ([type (in-list types1)])
                   (if (or (memq type out)
                           (for/or ([type1 types1])
                                   (and (not (eq? type type1))
                                        (subtype? type1 type))))
                       out
                       (cons type out))))
  (if (null? (cdr types2))
      (car types2)
      (really-make-and-type types2)))

(define and? make-and-type)

(module+ test
  (check-equal? (and? byte? byte?) byte?)
  (check-equal? (and? (or? byte? integer?) byte?) byte?))

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

;; simple normalization of or-types:
;; (1) (or x (or y z)) => (or x y z)
;; (2) (or x y z), (subtype? x y) => (or y z)
;; (3) (or x) => x
(define (or? . types)
  ;; first get flat list of disjuncts
  (define types1 (for/fold ([out null])
                           ([type (in-list types)])                    
                    (if (or-type? type)
                        (append (reverse (or-type-types type)) out)
                        (cons type out))))
  ;; next see if can merge any disjuncts
  (define types2 (for/fold ([out null])
                           ([type (in-list types1)])
                   (if (or (memq type out)
                           (for/or ([type1 types1])
                                   (and (not (eq? type type1))
                                        (subtype? type type1))))
                       out
                       (cons type out))))
  (if (null? (cdr types2))
      (car types2)
      (really-make-or-type types2)))

(module+ test
  (check-equal? (or? byte? byte?) byte?)
  (check-equal? (or? (and? byte? integer?) byte?) byte?))
;(define or? make-or-type)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  COMPOSE TYPES
;;

(struct compose-type (types) 
  #:constructor-name compose?
  #:property prop:procedure
  (lambda (type x)
    ((apply compose (compose-type-types type)) x))
  #:methods gen:custom-write
  [(define (write-proc v port mode)
     ((recur-write-proc mode) `(compose-type ,@(compose-type-types v)) port))])

(define (compose-subtype? type1 type2)
  (define types1 (compose-type-types type1))
  (define types2 (compose-type-types type2))
  (and (equal? (cdr types1) (cdr types1))
       (subtype? (car types1) (car types1))))


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

;(define <signature-type> signature-type?)

;; *covariant*
;; true if any (list) value satisfying sig1 will also satisfy sig2
(define (signature-subtype? sig1 sig2)
  ;; look at arg-types first
  (define rest1 (signature-type-rest-type/f sig1))
  (define rest2 (signature-type-rest-type/f sig2))
  (let loop ([arg-types1 (signature-type-types sig1)]
             [arg-types2 (signature-type-types sig2)])
    (cond
      [(null? arg-types1)		; done with arg-types1
       (and (subtype? rest1 rest2)
            (or (null? arg-types2)
                ;; more arg-types2 (but done with arg-types1)
                (and rest1
                     (for/and ([type arg-types2])
                       (subtype? rest1 type)))))]
      [(null? arg-types2)		; more arg1's than arg2's
       (and rest2
            (for/and ([type arg-types1])
              (subtype? type rest2)))]
      [else				; more of both
       (and (subtype? (car arg-types1) (car arg-types2))
            (loop (cdr arg-types1) (cdr arg-types2)))])))

(define (vals-match-signature? orig-vals sig)
  (define arg-types (signature-type-types sig))
  (define rest-type/f (signature-type-rest-type/f sig))
  (define len-vals (length orig-vals))
  (define len-args (length arg-types))
  ;; quick check 1st:
  (if (or (< len-vals len-args)
          (and (not rest-type/f)
               (> len-vals len-args)))
      #f
      ;; check types
      (let loop ([vals orig-vals]
                 [types arg-types])
        (define in-rest? (null? types))
        (cond
          [(null? vals) #t]
          [(null? types)
           (for/and ([val vals])
             (isa? val rest-type/f))]
          [(isa? (car vals) (car types))
           (loop (cdr vals) (cdr types))]
          [else #f]))))

;; TO DO: make signature-equal? a method
(define (signature-equal? s1 s2)
  ;; (method ((s1 <signature>) (s2 <signature>)) => <boolean>
  (and (eq? (signature-type-rest-type/f s1)
            (signature-type-rest-type/f s2))
       (for/and ([type1 (signature-type-types s1)]
                 [type2 (signature-type-types s2)])
         (eq? type1 type2))))

;; throw an exception if check fails
;(define (check-applicable! type args)
;  (if (isa? args type)
;      #t
;      (error (format "ck-app!: args ~a don't satisfy method type ~a" args type))))

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
;(define-syntax (make-selector stx)
;  (syntax-case stx ()
;    [(_ selector action mora-actions ...)
;     (with-syntax ([val (datum->syntax stx 'val)]
;                   [type (datum->syntax stx 'type)])
;       #'(cons selector (λ (val type) action mora-actions ...)))]))

(define (isa? val type)
  (cond
    [(c:class? type) (c:is-a? val type)]
    [(boolean? type) type]
    [else (type val)]))


(define parents (make-hasheq)) ; type -> listOf type

(define (subtype! t1 t2)
  (hash-set! parents
             t1
             (cons t2 (hash-ref parents t1 null))))

(define (parents-subtype? t1 t2)
  (define parent-list (hash-ref parents t1 null))
  (or (if (memq t2 parent-list) #t #f)
      (for/or ([parent (in-list parent-list)])
        (parents-subtype? parent t2))))
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
    [(boolean? t2) t2] ;; any is subtype of #t, none is subtype of #f
    [(eq? t1 #f) #t]
    ;; (and t1_1 ... t1_n) <= t2  iff 
    ;;   1. if t2 = (and t2_1 ... t2_m), t1 <= t2_i for all t2_i.
    ;;   2. else some t1_i <= t2.
    [(and-type? t1)
     (if (and-type? t2)
         (for/and ([type (in-list (and-type-types t2))])
           (subtype? t1 type))
         (for/or ([type (in-list (and-type-types t1))])
           (subtype? type t2)))]
    [(or-type? t1);; (or t1_1 ... t1_n) <= t2  iff all t1_i <= t2
     (for/and ([type (in-list (or-type-types t1))])
       (subtype? type t2))]
    [(eq-type? t1);; (eq v) <= t2  iff  v : t2 or t2 : eq(v)
     (or (isa? (eq-type-val t1) t2)
         (and (eq-type? t2) (equal? (eq-type-val t1) (eq-type-val t2))))]
    [(and-type? t2);; t1 <= (and t2_1 ... t2_n) iff t1 <= t2_i for all i
     (for/and ([type (in-list (and-type-types t2))])
       (subtype? t1 type))]
    [(or-type? t2);; t1 <= (or t2_1 ... t2_n) iff t1 <= t2_i for some i
     (for/or ([type (in-list (or-type-types t2))])
       (subtype? t1 type))]
    [(compose-type? t1)
     (and (compose-type? t2)
          (compose-subtype? t1 t2))]    
    [(c:class? t2) (c:subclass? t1 t2)]
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
    [(parents-subtype? t1 t2)]
    [else #f]))

;(define <type>
;  (or? <eq-type> <and-type> <or-type>
;       <signature-type> <method-type>
;       (lambda (v) (eq? v <top>))))

;		predicate-subtype?))
		     
;; TO DO: throw an exception on error
;; returns val on success
(define (check-type! val type)
  (cond
    [(eq? type #t) val]
    [(isa? val type) val]
    [else (error 'check-type "check-type! failed: ~a ~a" val type)]))

(define <false> (== #f))

(define (false-or t)
  (or? <false> t))

(define (type-equal? t1 t2)
  (define (type-list-equal? types1 types2)
    (and
     (= (length types1) (length types2))
     (for/and ([type1 types1])
              (for/or ([type2 types2])
                      (type-equal? type1 type2)))
     (for/and ([type2 types1])
              (for/or ([type1 types2])
                      (type-equal? type1 type2)))))
  (or (eq? t1 t2)
      (and (and-type? t1) 
           (and-type? t2)
           (type-list-equal? (and-type-types t1) (and-type-types t2)))
      (and (or-type? t1)			; disjunction
           (or-type? t2)
           (type-list-equal? (or-type-types t1) (or-type-types t2)))
      (and (eq-type? t1)
           (eq-type? t2)
           (equal? (eq-type-val t1) (eq-type-val t2)))
      (and (signature-type? t1)
           (signature-type? t2)
           (= (length (signature-type-types t1)) (length (signature-type-types t2)))
           (every type-equal? (signature-type-types t1) (signature-type-types t2))
           (type-equal? (signature-type-rest-type/f t1)
                        (signature-type-rest-type/f t2)))
      (and (method-type? t1)
           (method-type? t2)
           (type-equal? (method-type-args-type t1) (method-type-args-type t2))
           (type-equal? (method-type-result-type t1) (method-type-result-type t2)))))

(module+ test
  (check type-equal? (and? integer? boolean?) (and? integer? boolean?))
  (check type-equal? (and? integer? boolean?) (and? boolean? integer?))
  (check (negate type-equal?) (and? boolean? integer?) (and? boolean?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   GENERIC FUNCTIONS

(struct generic (name [methods #:mutable] [composer #:mutable] add-method-check/f)
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
    (check-type! (apply (method-callable real-m) args)
                 (method-result-type real-m)))
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
