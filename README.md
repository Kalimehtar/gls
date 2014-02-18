gls
===

GLS - Generic Little (Object, Type, Anything, ...) System

It provides multiple dispatch for Racket.

Differences from Swindle:

- Doesn't force you to change all. GLS is a small collection with a douzen of
  functions in API. It only adds generic functions.

- It based upon types, not classes. You may dispatch you function on any
  predicate you may imagine: `exact-integer?`, `(real-in 0 10)`, 
`(and? stream? (not/c stream-ampty?))`, ...

GLS is based on Greg Sullivan's <a
href="https://github.com/gregsgit/glos">GLOS</a>, that was witten for
scheme48. It has the same API, but without implemetation of own object system 
(glos-records). So GLS is not "generic little object system", but simply
"generic little system". But I added support for racket/class: you may use
class intead of type predicate and GLS corretly supports subtypes
(subclasses).

Sorry for bad documentation: for API look into <a href=https://github.com/Kalimehtar/gls/blob/master/gls/main.rkt> main.rkt</a>, for examples of use
-- <a href=https://github.com/Kalimehtar/gls/blob/master/gls/test.rkt>test.rkt</a>.

Some description of GLOS is in the paper <a
href="https://github.com/gregsgit/glos/blob/master/ref-dyn-patterns.pdf">ref-dyn-patterns</a>. 

Also, slides in <a href="https://github.com/gregsgit/glos/blob/master/proglangsandsofteng.pdf">proglangsandsofteng</a> work
through some examples using GLOS.

