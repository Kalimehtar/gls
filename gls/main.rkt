#lang racket/base
(require "types.rkt" "callable-macros.rkt" "callables.rkt")

(provide add-method
         replace-method
         call-next-method
         add-around-method
         add-before-method
         add-after-method
         defgeneric
         defmethod
         method
         gfmethod
         make-signature-type
         and?
         or?
         compose?
         negate?
         ==?
         *return-value*)

(subtype! complex? number?)
(subtype! real? complex?)
(subtype! rational? real?)
(subtype! integer? rational?)
(subtype! byte? integer?)
(subtype! null? list?)