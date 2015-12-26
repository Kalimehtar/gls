#lang scribble/manual

@(require (for-label racket gls) gls (prefix-in gls: gls/types) scribble/eval)

@title{GLS: Generic Little System}
@author{@(author+email "Roman Klochkov" "kalimehtar@mail.ru")}

@defmodule[gls]

@section{Introduction. Purpose of the library}

This library allows to make generics on arbitrary predicates. Generic is a function,
that called with different dunction bodies, depending upon its argument types, and these
bodies may be changed at any time. Classic implementation is
@(hyperlink "https://wikipedia.org/wiki/CLOS" "CLOS").

@racket[gls] differs from CLOS (and Swindle): it allows not only classes as dispatching condititons, but
any possible predicates. For example, classic
@(hyperlink "https://en.wikipedia.org/wiki/Circle-ellipse_problem" "circle-ellipse problem")
with @racket[gls] may be solved as

@(interaction
  (require gls)
  (struct ellipse (h v) #:mutable)
  (define circle? (and? ellipse? (λ (e) (= (ellipse-h e) (ellipse-v e)))))
  (define (circle r) (ellipse r r))
  (defgeneric circle-radius
    (method ([c circle?])
       (ellipse-h c)))
  (defgeneric name
    (method ([c circle?])
            "Circle")
    (method ([c ellipse?])
            "Ellipse"))
  (define c (circle 10))
  (name c)
  (circle-radius c)
  (set-ellipse-v! c 20)
  (name c)
  (circle-radius c))

So @racket[_c] is a circle only when both axis are equal.

@section{Base syntax}

Type may be either predicate (function with one argument, returning @racket[boolean?]), @racket[class?] or
@racket[boolean?]. Type #t means `any type`. Type #f means `type without values`. For @racket[class?] --
type values -- instances of the class. And for a predicate type values --- all values, on which predicate
returns #t.

If you use predicates, that defines subtypes, you should explicitly set one type to by
subtype of another type.

@defproc[(subtype! [subtype gls:predicate?] [supertype gls:predicate?]) void?]{
Sets @racket[_subtype] to be subtype of @racket[_supertype] for dispatching.
 So, if a generic has a method with @racket[_subtype] argument and
 a method with @racket[_supertype] argument and both a acceptable for some values,
 then the method with @racket[_subtype] argument will be executed.

Beware, that @racket[subtype!] sets subtypes on values of predicates, not predicate bodies.
 So don't put (lambda ...) in it. @racket[lambda] on each call make new procedure
 even when calles with the same body.}

@defform*[((method (arg ...) body ...+)
           (method (arg ...) => result body ...+))
          #:grammar ([arg
                      arg-name
                      (arg-name arg-type)])
          #:contracts ([arg-type gls:type?]
                       [result gls:type?])]{
Produces a method for GLS. Method may be used as a procedure, in that case no typecheck is performed.
When used in generic, type of the arguments is used to select correct method. Result type is not used
during dispatching, but is checked on the generic result.}

@defform[(defgeneric name method ...)]
{Defines generic with given name and methods.}

@(interaction
  (require gls)
  (define =1 (λ (x) (equal? x 1)))
  (subtype! =1 integer?)
  (define default (method ([n =1]) 1))  
  (defgeneric fact
    default
    (method ([n integer?])
            (* (fact (- n 1)) n)))

  (fact 5))

@section{Dynamic change methods}

@defproc[(add-method [generic gls:generic?] [method gls:method?]) any]

@defproc[(remove-method [generic gls:generic?] [method gls:method?]) any]

@defproc[(replace-method [generic gls:generic?] [method gls:method?]) any]

@section{Augmenting methods}

@defproc[(add-before-method [generic gls:generic?] [method gls:method?]) any]

@defproc[(remove-before-method [generic gls:generic?] [method gls:method?]) any]

@defproc[(replace-before-method [generic gls:generic?] [method gls:method?]) any]

@defproc[(add-after-method [generic gls:generic?] [method gls:method?]) any]

@defproc[(remove-after-method [generic gls:generic?] [method gls:method?]) any]

@defproc[(replace-after-method [generic gls:generic?] [method gls:method?]) any]

@defproc[(add-around-method [generic gls:generic?] [method gls:method?]) any]

@defproc[(remove-around-method [generic gls:generic?] [method gls:method?]) any]

@defproc[(replace-around-method [generic gls:generic?] [method gls:method?]) any]

@section{Combinators}

@defproc[(and? [type gls:type?] ...) gls:type?]

@defproc[(or? [type gls:type?] ...) gls:type?]

@defproc[(compose? [type gls:type?] ...) gls:predicate?]

@defproc[(==? [value any/c]) gls:predicate?]

@defproc[(negate? [type gls:type?]) gls:type?]