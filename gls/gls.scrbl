#lang scribble/manual

@(require (for-label racket gls))

@title{GLS: Generic Little System}
@author{@(author+email "Roman Klochkov" "kalimehtar@mail.ru")}

@defmodule[gls]

This library allows to make generics on arbitrary predicates. Generic is a function,
that called with different dunction bodies, depending upon its argument types, and these
bodies may be changed at any time. Classic implementation is
@(hyperlink "https://wikipedia.org/wiki/CLOS" "CLOS").

@racket[gls] differs from CLOS (and Swindle): it allows not only classes as dispatching condititons, but
any possible predicates. 