#lang scribble/manual

@(require (for-label racket gls) gls scribble/eval)

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
  (define c (circle 10))
  (circle-radius c)
  (set-ellipse-v! c 20)
  (circle-radius c))

So @racket[c] is a circle only when both axis are equal.

@section{Base syntax}

@defform[(method name body ...+)]

@defform[(defgeneric name method ...)]

@section{Dynamic change methods}

@defproc[(add-method [generic generic?] [method method?]) any]

@defproc[(remove-method [generic generic?] [method method?]) any]

@defproc[(replace-method [generic generic?] [method method?]) any]

@section{Augmenting methods}

@defproc[(add-before-method [generic generic?] [method method?]) any]

@defproc[(remove-before-method [generic generic?] [method method?]) any]

@defproc[(replace-before-method [generic generic?] [method method?]) any]

@defproc[(add-after-method [generic generic?] [method method?]) any]

@defproc[(remove-after-method [generic generic?] [method method?]) any]

@defproc[(replace-after-method [generic generic?] [method method?]) any]

@defproc[(add-around-method [generic generic?] [method method?]) any]

@defproc[(remove-around-method [generic generic?] [method method?]) any]

@defproc[(replace-around-method [generic generic?] [method method?]) any]

@section{Combinators}

@defproc[(and? [type (or boolean? procedure?)] ...) (or boolean? procedure?)]

@defproc[(or? [type (or boolean? procedure?)] ...) (or boolean? procedure?)]

@defproc[(compose? [type (or boolean? procedure?)] ...) procedure?]

@defproc[(==? [value any/c]) procedure?]

@defproc[(negate? [type (or boolean? procedure?)]) procedure?]