#lang racket/base
;; callable-macros.scm
(require "types.rkt" "callables.rkt" "util-macros.rkt")
(provide defgeneric defmethod method gfmethod)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CALLABLE-RECORDS 
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   GENERIC
;;;
(define-syntax defgeneric
  (syntax-rules ()
    ((defgeneric ?name ?method ...)
     (begin
       (define ?name (make-named-generic '?name ?method ...))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   METHOD

(define-syntax method
  (syntax-rules (=>)
    ;; handle result spec., if any
    ((method ?argspec => ?resspec ?body ...) ; handle result spec
     (method-getargs ?argspec (?body ...) ?resspec #f () ()))
    ((method ?argspec ?body ...)	; default result spec is #t
     (method-getargs ?argspec (?body ...) #t #f () ()))))

;; not for export
;; (method-getargs argspecs body result restspec args types)
;; collect args and types
(define-syntax method-getargs
  (syntax-rules (:rest)
    ;; handle rest spec, if any
    ((method-getargs (:rest (?rest-var ?rest-type))
		     ?body ?result ?rest ?args ?types)
     (revlstcps ?types ()
		method-finish ?body ?result (?rest-var ?rest-type) ?args))
    ;; rest var. with no specializer:
    ((method-getargs (:rest ?rest-var) ?body ?result ?rest ?args ?types)
     (revlstcps ?types ()
		method-finish ?body ?result (?rest-var #t) ?args))
    ;; arg. with a specializer:
    ((method-getargs ((?var1 ?type1) ?var2 ...) ?body ?result
		     ?rest (?arg ...) (?type ...))
     (method-getargs (?var2 ...) ?body ?result ?rest (?var1 ?arg ...)
		     (?type1 ?type ...)))
    ;; arg with no specializer - defaults to #t
    ((method-getargs (?var1 ?var2 ...) ?body ?result ?rest (?arg ...) (?type ...))
     (method-getargs (?var2 ...) ?body ?result ?rest (?var1 ?arg ...)
		     (#t ?type ...)))
    ;; done with arg.s, no rest
    ((method-getargs () ?body ?result ?rest ?args ?types)
     (revlstcps ?types ()
		method-finish ?body ?result ?rest ?args))
    ))

;; not exported
;; (method-finish body result rest/f args types)
(define-syntax method-finish
  (syntax-rules ()
    ;; no rest
    ((method-finish (?type ...) ?body ?result #f ?args)
     (make-method "anon"
		  (make-signature-type #f ?type ...)
		  ?result
		  (gen-method-lambda ?args () ?body)
		  #f))
    ;; types rest
    ((method-finish (?type ...) ?body ?result (?rest-var ?rest-type) ?args)
     (make-method "anon"
		  (make-signature-type ?rest-type ?type ...)
		  ?result
		  (gen-method-lambda ?args ?rest-var ?body)
		  #f))))

;; not exported
;; reverse the argument list and add rest var, if any
;; (gen-method-lambda args list body)
(define-syntax gen-method-lambda
  (syntax-rules ()
    ((gen-method-lambda (arg1 arg2 ...) l body)
     (gen-method-lambda (arg2 ...) (arg1 . l) body))
    ((gen-method-lambda () l (body ...))
     (lambda l body ...))))

(define-syntax defmethod
  (syntax-rules (=>)
    ((defmethod (?name . ?argspec) ?body ...)
     (begin
       (define ?name (method ?argspec ?body ...))
       (set-method-name! ?name '?name)))
    ((defmethod (?name . ?argspec) => ?result-spec ?body ...)
     (begin
       (define ?name (method ?argspec => ?result-spec ?body ...))
       (set-method-name! ?name '?name)))))

(define-syntax gfmethod
  (syntax-rules (=>)
    ((gfmethod (?name . ?argspec) . ?body)
     (let ((temp-method (method ?argspec . ?body)))
       (set-method-name! temp-method
			 (string->symbol (format "method-of-~a" '?name)))
       (add-method ?name temp-method)))
    ((gfmethod (?name . ?argspec) => ?result-spec . ?body)
     (let ((temp-method (method ?argspec => ?result-spec . ?body)))
       (set-method-name! temp-method
			 (string->symbol (format "method-of-~a" '?name)))
       (add-method ?name temp-method)))))

;; eof
