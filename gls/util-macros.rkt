;; util-macros.scm
#lang racket/base
(provide revlstcps)
;; reverse list continuation passing style
(define-syntax revlstcps
  (syntax-rules ()
    ((revlstcps (?car . ?cdr) ?out ?next . ?more)
     (revlstcps ?cdr (?car . ?out) ?next . ?more))
    ((revlstcps () ?out ?next . ?more)
     (?next ?out . ?more))))

;; and convert to a Scheme list (rather than list of tokens)
(define-syntax revlstlstcps
  (syntax-rules ()
    ((revlstlstcps (?car . ?cdr) ?out ?next . ?more)
     (revlstlstcps ?cdr (?car . ?out) ?next . ?more))
    ((revlstlstcps () (?out ...) ?next . ?more)
     (?next (list ?out ...) . ?more))))

;; eof
