#lang racket
(require "main.rkt" rackunit)

(define (fact x)
  (defgeneric fact0
    (method ((n (== 1)) (acc integer?))
            acc)
    (method ((n integer?) (acc integer?))
            (fact0 (- n 1) (* acc n))))
  (fact0 x 1))

(check-equal? (fact 5) 120)

(defmethod (m1 (i integer?))
  (format "m1<integer>(~a)" i))

(defmethod (m2 (i integer?) (s string?))
  (format "m2, i=~a, s=~a" i s))

(check-equal? (m1 2) "m1<integer>(2)")

(defgeneric g1
  m1
  (method ((n number?))
	  (format "g1<number>(~a)" n))
  (method ((s string?))
	  (format "g1string?(~a)" s)))

(check-equal? (g1 "hi") "g1string?(hi)")
(check-equal? (g1 2) "m1<integer>(2)")
(check-equal? (g1 2.1) "g1<number>(2.1)")

(add-method g1
            (method ((x (and? integer? even?)))
                    (displayln (format "g1 on even int, (~a)" x))
                    (call-next-method)))

(replace-method g1
                (make-signature-type #f integer?)
                (method ((i integer?))
                        (displayln (format "new g1<int>(~a)" i))
                        (call-next-method)))

(check-equal? (g1 "hi") "g1string?(hi)")
(let ([res #f])
  (define output (with-output-to-string
                  (lambda () (set! res (g1 3)))))
  (check-equal? res "g1<number>(3)")
  (check-equal? output "new g1<int>(3)\n"))

;;;; IMPORTANT: following examples are copied from original version of GLOS
;;;;            in GLS there are no defrectype, you may use racket/class classes instead

;; moving on:
;,load exceptions.scm typed-containers.scm
#|
(set! *dbg-tags* '(types calls exceptions))

(define-syntax test2
  (syntax-rules ()
    ((test2 e1 e2)
     (let ((v1 e1)
	   (v2 e2))
       (if (not (equal? v1 v2))
	   (begin
	     (cl-format #t "~%Test failed: ~a -> ~a not eq ~a -> ~a ~%" 'e1 v1 'e2 v2)
	     #f)
	   (begin
	     (cl-format #t ".")
	     #t))))))

(defmethod (m1 (i <integer>))
  (cl-format #t "m1<integer>(~a)~%" i)
  i)

(defmethod (m2 (i <integer>) (s string?))
  (cl-format #t "m2, i=~a, s=~a~%" i s)
  (list i s))

(m1 2)

(defgeneric g1 m1
  (method ((n number?))
	  (cl-format #t "g1<number>(~a)~%" n)
	  n)
  (method ((s string?))
	  (cl-format #t "g1string?(~a)~%" s)
	  s))
  
(g1 "hi")
(g1 2)
(g1 2.1)

(add-method
 g1
 (method ((x (and? <integer> even?)))
	 (cl-format #t "g1 on even int, (~a)~%" x)
	 (call-next-method)))

(replace-method
 g1 (make-signature-type #f <integer>)
 (method ((i <integer>))
	 (cl-format #t "new g1<int>(~a)~%" i)
	 (call-next-method)))

(test2 (g1 "hi") "hi")
(test2 (g1 3) 3)

(defmethod (bm1 x)
  (cl-format #t "in bm1, got ~a~%" x))

(defmethod (bm2 (x <int>))
  (cl-format #t "in bm2 <int>, got ~a~%" x))

(add-before-method g1 bm1)
(add-before-method g1 bm2)

(g1 "hi")
(g1 3)

(add-after-method
 g1 (method ((x string?))
	    (cl-format #t "in after method on string?, retval=~a~%"
		    (fluid *return-value*))))

(g1 "hi")
(g1 3)

(add-after-method
 g1 (method (x)
	    (cl-format #t "in after method on <top>~%")))

(g1 "hi")

(defgeneric fact
  (method ((n (and? <int> (== 1))))
	  1)
  (method ((n <int>))
	  (* n (fact (- n 1)))))

;; following gets ambiguous error
(defgeneric bad-fact
  (method ((n (curry = 1)))
	  1)
  (method ((n <int>))
	  (* n (bad-fact (- n 1)))))


(add-around-method
 g1
 (method ((x <int>))
	 (cl-format #t "in around method on <int>, calling next-method~%")
	 (call-next-method)))

(add-around-method
 g1
 (method ((x <string>))
	 (cl-format #t "in around method on <string>, returning 'foo~%")
	 'foo))

(add-around-method
 g1
 (method ((x <number>))
	 (cl-format #t "in around method on <number>, returning 'bar~%")
	 'bar))

(add-around-method
 g1
 (method ((x <string>))
	 (cl-format #t "in around method on <string>, calling next method~%")
	 (let ((val (call-next-method)))
	   (cl-format #t "back in around method on <string>, got ~a~%" val)
	   val)))

;; how to do cflow with fluid bindings and around methods.
;; (cflow? foo)
(defgeneric bar)
(defgeneric foo
  (method ((x <integer>))
	  (cl-format #t "foo on int ~%")
	  (bar x))
  (method ((x <string>))
	  (cl-format #t "foo on string ~%")
	  (bar x)))
  
(define *in-foo?* (make-fluid #f))
(add-around-method
 foo (make-method "around foo" <top> #f
		  (lambda args
		    (let-fluid *in-foo?* #t
			       (lambda ()
				 (call-next-method)))) #f))

(gfmethod (bar (x (and? <top> (fluid *in-foo?*))))
	  (cl-format #t "in bar, in cflow of foo~%"))
(gfmethod (bar (x <top>))
	  (cl-format #t "in bar, not in cflow of foo~%"))

(bar 3)
(foo 3)


(defrectype <tree-node> ()
  ((data))
  (data node-data set-node-data!))
(defrectype <interior-node> (<tree-node>)
  ((left-child <tree-node>)
   (right-child <tree-node>))
  (left-child node-left-child)
  (right-child node-right-child))
(defrectype <leaf-node> (<tree-node>)
  ())

(define ln1 (new <leaf-node> 'ln1))
(define ln2 (new <leaf-node> 'ln2))
(define in1 (new <interior-node> 'in1 ln1 ln2))

(defgeneric walk-node
  (method ((n <leaf-node>))
	  (cl-format #t "leaf node ~a~%" (node-data n)))
  (method ((n <interior-node>))
	  (cl-format #t "interior node ~a~%" (node-data n))
	  (walk-node (node-left-child n))
	  (walk-node (node-right-child n))))

(defrectype <mypair> ()
  ((head)
   (tail (false-or <mypair>)))
  (head head set-head!)
  (tail tail set-tail!))
(gfmethod (initialize (obj <mypair>) x (y (false-or <mypair>)))
   (set-by-name* obj 'head x 'tail y))
(define p1 (new <mypair> 'e1 (new <mypair> 'e2 (new <mypair> 'e3 #f))))

(head p1)
(head (tail p1))   ;; => 'e2
(head (tail (tail p1)))
  
(defrectype <point> ()
  ((x <integer>)
   (y <integer>))
  (x get-x set-x!)
  (y get-y set-y!))

(set! p1 (new <point> 'x 1 'y 2))
(define p2 ((glos-record-type-constructor <point>)))

(get-x p1)
(set-x! p1 3)
(get-x p1)

(defrectype <colorpoint> (<point>)
  ((color string? "black"))
  (color get-color set-color!))

(define cp1 (new <colorpoint> 'x 4 'y 5))
(get-color cp1)
(set-color! cp1 "red")
(get-color cp1)

(defgeneric describe
  (method ((o <point>))
	  `(point ,(get-x o) ,(get-y o)))
  (method ((o <colorpoint>))
	  `(colorpoint ,(get-x o) ,(get-y o) ,(get-color o))))

(describe p1)
(describe cp1)

(defgeneric move
  (method ((p <point>) (x <integer>) (y <integer>))
	  (set-x! p x)
	  (set-y! p y)))

(add-before-method
 move (method ((p <colorpoint>) (x <integer>) (y <integer>))
	      (cl-format #t "before move colorpoint (~a, ~a)~%" x y)))

(add-after-method
 move (method ((p <point>) (x <integer>) (y <integer>))
	      (cl-format #t "after move point (~a, ~a)~%" x y)))
		   
(move p1 2 3)
(move cp1 2 3)

(define cp2 (new <colorpoint> 4 5))

(add-before-method
 move (method ((p (== cp2)) x y)
	      (cl-format #t "before move == cp2~%")))

(move p1 2 3)
(move cp1 1 2)
(move cp2 2 2)

(add-after-method
 move (method ((p <colorpoint>) (x (and? <integer> even?)) (y <integer>))
	      (cl-format #t "after move cp to even x~%")))

(defgeneric g2
  (method ((o <number>))
	  (cl-format #t "g1 on number~%"))
  (method ((o <integer>))
	  (cl-format #t "g1 on integer~%")
	  (call-next-method)))
(g2 2)

(defrectype <c1> ()
  ((x <int>)
   (y <int> 0))
  (x get-x set-x!)
  (y get-y set-y!))

(add-after-method
 new
 (method ((class (== <c1>)) :rest rest)
	 (cl-format #t "in after new <c1>~%")
	 (set-y! (fluid *return-value*)
		 (+ 1 (get-x (fluid *return-value*))))))

(define c1 (new <c1> 2))

(get-y c1)

(remove-after-method new (make-signature-type <top> (== <c1>)))

;; Abstract Factory pattern
(defrectype <maze> () ())
(defrectype <wall> () ())
(defrectype <room> () 
  ((number <int>))
  (number room-number set-room-number!))
(defrectype <door> ()
  ((from <room>)
   (to <room>))
  (from door-from set-door-from!)
  (to door-to set-door-to!))

(defrectype <maze-factory> () ())

(defgeneric make-maze-element
  (method ((f <maze-factory>) (eltType (== <wall>))) => <wall>
     (new <wall>))
  (method ((f <maze-factory>) (eltType (== <room>)) :rest args) => <room>
     (apply new <room> args))
  (method ((f <maze-factory>) (eltType (== <door>)) :rest args) => <door>
     (apply new <door> args)))

(define the-factory (new <maze-factory>))
(define room1 (make-maze-element the-factory <room> 'number 1))
(define room2 (make-maze-element the-factory <room> 'number 2))
(define door1 (make-maze-element the-factory <door> 'from room1 'to room2))


(make-maze-element the-factory <wall>)  ;; =$>$ an instance of $<$wall$>$

(define m1 (new <maze>))
(define w1 (new <wall>))
(define r1 (new <room> 1))
(define r2 (new <room> 2))
(define d1 (new <door> r1 r2))



;; Factory Method pattern
(defrectype <wall> () ())
(defrectype <room> () 
  ((n <int>))
  (n get-n set-n!))
(defrectype <bombed-wall> (<wall>) ())
(defrectype <bombed-room> (<room>) ())
(gfmethod (initialize (o <room>) (n <int>))
	  (set-n! o n))
(gfmethod (make (c (== <wall>)))
   (make <bombed-wall>))
(gfmethod (make (c (== <room>)) (n <int>))
   (make <bombed-room> n))

(defgeneric show
  (method ((o <wall>)) (cl-format #t "<wall>~%"))
  (method ((o <room>)) (cl-format #t "<room> ~a~%" (get-n o)))
  (method ((o <bombed-wall>)) (cl-format #t "<bombed-wall>~%"))
  (method ((o <bombed-room>))(cl-format #t "<bombed-room> ~a~%" (get-n o))))
  
(show (new <wall>))
(show (new <room> 3))

;; Composite pattern
(defrectype <equipment> ()
  ((name <symbol>)
   (price))
  (name equipment-name set-equipment-name!)
  (price equipment-price set-equipment-price!))

(defgeneric power)
(defgeneric net-price)

(defrectype <composite-equipment> (<equipment>)
  ((parts <list>))
  (parts composite-parts set-composite-parts!))

(defrectype <floppy> (<equipment>) ())
(defrectype <chassis> (<equipment>) ())

(gfmethod (net-price (e <equipment>))
	  (equipment-price e))

(gfmethod (net-price (e <composite-equipment>))
   (fold (lambda (item total) 
            (+ total (net-price item)))
         (equipment-price e)
	 (composite-parts e)))

(defrectype <chassis> (<composite-equipment>) ())

(define f1 (new <floppy> 'name 'floppy1 'price 2))
(define f2 (new <floppy> 'name 'floppy2 'price 3))
(set! c1 (new <chassis> 'name 'chassis1 'price 1
		'parts (list f1 f2)))

(net-price c1)


;; Decorator pattern
;; direct translation:
(defrectype <visual-component> () ())

(defrectype <decorator> (<visual-component>)
  ((contents <visual-component>))
  (contents decorator-component set-decorator-component!))

(gfmethod (initialize (obj <decorator>) (comp <visual-component>))
	  (set-decorator-component! obj comp))

(defrectype <text-view> (<visual-component>) ())

(defrectype <window> ()
  ((component <visual-component>))
  (component window-contents set-window-contents!))

(defmethod (draw-border w)
  (cl-format #t "drawing border of width ~a~%" w))

(defmethod (draw-scroll-bar)
  (cl-format #t "drawing scrollbar~%"))

(defgeneric draw
  (method ((comp <visual-component>))
	  (cl-format #t "drawing <visual-component>~%"))
  (method ((w <window>))
	  (draw (window-contents w)))
  (method ((c <text-view>))
	  (cl-format #t "drawing <text-view>~%")))

(gfmethod (draw (comp <decorator>))
	  (draw (decorator-component comp)))

(defrectype <border-decorator> (<decorator>)
  ((width <int>))
  (width border-width set-border-width!))

(gfmethod (initialize (obj <border-decorator>)
		      (comp <visual-component>) (width <int>))
	  (initialize obj comp)
	  (set-border-width! obj width))

(gfmethod (draw (comp <border-decorator>))
	  (call-next-method)  ;; eventually, draw component
	  (draw-border (border-width comp)))

(defrectype <scroll-decorator> (<decorator>) ())

(gfmethod (draw (comp <scroll-decorator>))
	  (call-next-method)  ;; eventually, draw component
	  (draw-scroll-bar))

(set! w1 (new <window>))
(define tv1 (new <text-view>))
(set-window-contents!
 w1 (new <border-decorator>
	 (new <scroll-decorator> tv1) 3))

(draw w1)

;; Decorator pattern using method combination
(defrectype <visual-component> () ())
(defrectype <window> ()
  ((contents <visual-component>))
  (contents window-contents set-window-contents!))
(defrectype <text-view> (<visual-component>) ())
(defmethod (draw-border w)
  (cl-format #t "drawing border of width ~a~%" w))

(defmethod (draw-scroll-bar)
  (cl-format #t "drawing scrollbar~%"))
(defgeneric draw
  (method ((comp <visual-component>))
	  (cl-format #t "drawing <visual-component>~%"))
  (method ((w <window>))
	  (draw (window-contents w)))
  (method ((c <text-view>))
	  (cl-format #t "drawing <text-view>~%")))

(defrectype <border-decorator> (<visual-component>)
  ((width <int>))
  (width border-width set-border-width!))

(gfmethod (draw (comp <border-decorator>))
	  (draw-border (border-width comp)))

(defrectype <scroll-decorator> (<visual-component>) ())

(gfmethod (draw (comp <scroll-decorator>))
	  (draw-scroll-bar))

(defmethod (decorate (component <visual-component>) 
		     (decoration <visual-component>))
  (add-after-method draw
		    (method ((c (== component)))
			    (draw decoration))))

(set! tv1 (new <text-view>))
(set! w1 (new <window> 'contents tv1))
(decorate tv1 (new <scroll-decorator>))
(decorate tv1 (new <border-decorator> 'width 4))
(draw w1)


;; Flyweight pattern - factory component
(defrectype <character> ()
  ((char <char>))
  (char get-char set-char!))

(gfmethod (initialize (obj <character>) (char <char>))
	  (set-char! obj char))
(let ((char-table (make-integer-table)))
  (gfmethod
   (make (class (== <character>)) (char <char>))
   (let ((char-int (char->integer char)))
     (cond ((table-ref char-table char-int) => identity)
	   (else
	    (let ((new-char (call-next-method)))
	      (table-set! char-table char-int new-char)
	      new-char))))))

(set! c1 (new <character> #\a))
(set! c2 (new <character> #\a))
(eq? c1 c2)

(defrectype <xcharacter> ()
  ((char <char>))
  (char get-char set-char!))
(gfmethod (initialize (obj <xcharacter>) (char <char>))
	  (set-char! obj char))

;; Proxy pattern

(defmethod (bits-from-file (fn <string>))
  (cl-format #t "Reading bits from file ~a~%" fn)
  'some-bits)

(defrectype <graphic> () ())
;; <image> is imported from elsewhere
(defrectype <image> (<graphic>)
  ((bits))
  (bits image-bits set-image-bits!))
(gfmethod (initialize (obj <image>) (fn <string>))
	  (set-image-bits! obj (bits-from-file fn)))
(defgeneric draw
  (method ((i <image>))
	  (let ((bits (image-bits i)))
	    (cl-format #t "Drawing image bits ~a~%" bits))))

;; following is proxy code
(defrectype <image-proxy> (<image>)
  ((filename <string>)
   (empty? <boolean> #t))
  (filename image-proxy-filename
	    set-image-proxy-filename!)
  (empty? image-proxy-empty? set-image-proxy-empty?!))
;; hijack instantiation of <image> superclass
(gfmethod (make (class (== <image>)) :rest args)
	  (make <image-proxy>))
(gfmethod (initialize (obj <image-proxy>) (fn <string>))
	  (set-image-proxy-filename! obj fn))
(add-before-method
 image-bits
 (method ((obj <image-proxy>))
	 (if (image-proxy-empty? obj)
	     (begin
	       (set-image-bits! obj
				(bits-from-file (image-proxy-filename obj)))
	       (set-image-proxy-empty?! obj #f)))))
(defrectype <text-document> ()
  ((elts <list> '()))
  (elts text-document-elements
	set-text-document-elements!))
(defgeneric insert
  (method ((into <text-document>) (obj <graphic>))
	  (set-text-document-elements!
	   into (cons obj (text-document-elements into)))))
(defgeneric display
  (method ((obj <text-document>))
	  (for-each (lambda (elt) (draw elt))
		    (text-document-elements obj))))

(define td1 (new <text-document>))
(insert td1 (new <image> "image.gif"))
(display td1)				; reads and draws
(display td1)				; just draws


;; Chain of Responsibility pattern

(defrectype <topic> ()
  ((text))
  (text topic-text set-topic-text!))
(gfmethod (initialize (obj <topic>) text)
	  (set-topic-text! obj text))
(defgeneric display-topic
  (method ((t <topic>))
	  (cl-format #t "Topic: ~a~%" (topic-text t))))

(defrectype <handler> ()
  ((topic (false-or <topic>) #f)
   (next (false-or <handler>) #f))
  (topic handler-topic set-handler-topic!)
  (next handler-next set-handler-next!))

(defgeneric handle-help
  (method ((handler <handler>))
	  (cond
	   ((handler-topic handler)
	    => display-topic)
	   ((handler-next handler)
	    => handle-help)
	   (else
	    (cl-format #t "No topic found.~%")))))

;; widgets
(defrectype <widget> (<handler>)
  ((parent (false-or <widget>) #f))
  (parent widget-parent set-widget-parent!))
;; handle new widget with only a parent handler, not a parent widget:
(gfmethod (initialize (newobj <widget>) (parent-handler (false-or <handler>))
		      (topic (false-or <topic>)))
	  (set-handler-next! newobj parent-handler)
	  (set-handler-topic! newobj topic))
(gfmethod (initialize (newobj <widget>) (parent-widget (false-or <widget>))
		      (topic (false-or <topic>)))
	  (set-widget-parent! newobj parent-widget)
	  (set-handler-next! newobj parent-widget)
	  (set-handler-topic! newobj topic))
(defrectype <button> (<widget>) () )
(defrectype <dialog> (<widget>) () )

(defrectype <application> (<handler>) ())
(gfmethod (initialize (obj <application>) (topic <topic>))
	  (set-handler-topic! obj topic))

(gfmethod (handle-help (topic <topic>) (handler <application>))
	  (cl-format #t "Displaying list of application help topics.~%"))

(define printing-topic (new <topic> "help for printing"))
(define paper-orientation-topic (new <topic> "help for paper orientation"))
(define application-topic (new <topic> "help for application"))

(define application (new <application> application-topic))
(define dialog (new <dialog> application printing-topic))
(define button (new <button> dialog paper-orientation-topic))

(handle-help button)


;; Fancy Chain of Responsibility, where handlers can handle different 
;; types of requests.

(defrectype <request> () ())
(defrectype <handler> ()
  ((next (false-or <handler>))
   (local generic?))
  (next handler-next set-handler-next!)
  (local handler-local set-handler-local!))
(defgeneric handle-request
  (method ((request <request>) (handler <handler>))
	  ((handler-local handler) request)))

(gfmethod (initialize (this-handler <handler>) :rest args)
	  ;; install local handler generic
	  (set-handler-local! 
	   this-handler
	   (make-generic
	    ;; default method defers to next handler in chain
	    (method ((request <request>))
		    (if (handler-next this-handler)
			(handle-request request
					(handler-next this-handler))
			(cl-format #t "No handler found.~%")))))
	  ;; get any local handler methods from initialization args
	  (handle-key 'handler args
		      (lambda (handler-method)
			(add-method (handler-local this-handler)
				    handler-method))))

;; widgets
(defrectype <widget> (<handler>)
  ((parent (false-or <widget>) #f))
  (parent widget-parent set-widget-parent!))
(gfmethod (initialize (newobj <widget>) (parent-handler <handler>)
		      :rest args)
	  (call-next-method)
	  (set-handler-next! newobj parent-handler))
(gfmethod (initialize (newobj <widget>) (parent-handler <widget>)
		      :rest args)
	  (call-next-method)
	  (set-handler-next! newobj parent-handler)
	  (set-widget-parent! newobj parent-handler))

(defrectype <button> (<widget>) () )
(defrectype <dialog> (<widget>) () )

(defrectype <application> (<handler>) ())

(defrectype <help-request> (<request>) ())
(defrectype <print-request> (<request>) ())
(defrectype <preview-request> (<request>) ())

(define app1 (new <application>
		  'handler (method ((req <help-request>))
				   (cl-format #t "application help~%"))))

(set! d1 (new <dialog> app1
		'handler (method ((req <help-request>))
				 (cl-format #t "dialog help~%"))
		'handler (method ((req <preview-request>))
				 (cl-format #t "dialog preview~%"))))
(set! b1 (new <button> d1
		'handler (method ((req <help-request>))
				 (cl-format #t "button help~%"))))

(handle-request (new <help-request>) b1)
(handle-request (new <preview-request>) b1)


;; Command pattern

(define (make-open-command app)
  (lambda ()
    (let ((name (ask-user)))
      (if name
	  (let ((doc (new <document> name)))
	    (add app doc)
	    (open doc))))))

(add-menuitem some-menu (make-open-command the-app))
...
((menuitem-command some-menuitem))


;; Iterator pattern - internal version

(define employees (list ...))
(filter (lambda (e)
	  (> (employee-salary e) 100000))
	employees)


;; Mediator pattern
(defgeneric handle-mouse)

(defrectype <widget> ()
  ((notify-fn generic? (make-generic)))
  (notify-fn widget-notify-fn set-widget-notify-fn!))

(gfmethod (handle-mouse (obj <widget>))
	  ((widget-notify-fn obj) obj))

(defrectype <list-box> (<widget>)
  ((list))
  (list list-box-list set-list-box-list!))
(gfmethod (initialize (obj <list-box>) lst)
	  (set-list-box-list! obj lst))

(defmethod (get-selection (obj <list-box>))
  'the-list-box-selection)

(defrectype <entry-field> (<widget>)
  ((text))
  (text field-text set-field-text!))
(gfmethod (initialize (obj <entry-field>) text)
	  (set-field-text! obj text))

(defmethod (set-text (ef <entry-field>) text)
  (cl-format #t "setting text of entry field to ~a.~%" text)
  (set-field-text! ef text))

(defrectype <button> (<widget>)
  ((label))
  (label button-label set-button-label!))
(gfmethod (initialize (obj <button>) label)
	  (set-button-label! obj label))

(define a-font-dialog
  (let ((ok (new <button> 'ok))
	(cancel (new <button> 'cancel))
	(font-list (new <list-box> '(1 2 3)))
	(font-name (new <entry-field> 'font-name-here))
	(notify-fn (make-generic)))
    (for-each (lambda (w) (set-widget-notify-fn! w notify-fn))
	      (list ok cancel font-list font-name))
    (for-each (lambda (m) (add-method notify-fn m))
	      (list
	       (method ((obj (== font-list)))
		       (set-text font-name (get-selection font-list)))
	       (method ((obj (== ok)))
		       (cl-format #t "ok button pressed~%"))
	       (method ((obj (== cancel)))
		       (cl-format #t "cancel button pressed~%"))))
    (list ok cancel font-list font-name)))

(handle-mouse (list-ref a-font-dialog 2))	 
		 


;; Observer pattern
;; preliminaries:
(defrectype <widget> () ())
;; main course:
(defrectype <subject> ()
  ((notify-fn generic?
	      (really-make-generic 'update '()
				   chain-composer #f)))
  (notify-fn subject-notify-function set-subject-update-function!))

(defmethod (notify (s <subject>))
  ((subject-notify-function s)))

(defrectype <clock-timer> (<subject>)
  ((data))
  (data clock-timer-data set-clock-timer-data!))
  
;; we imagine that tick gets called by an internal timer
(defmethod (tick (timer <clock-timer>) time-data)
  (set-clock-timer-data! timer time-data)
  (notify timer))			; call the update generic

(defrectype <clock-widget> (<widget>)
  ((the-timer <clock-timer>))
  (the-timer clock-timer set-clock-timer!))

;; New clock-widgets register themselves with their timer.
(gfmethod (initialize (clock <clock-widget>) (timer <clock-timer>))
	  (set-clock-timer! clock timer)
	  (add-method (subject-notify-function timer)
		      (method ()
			      (draw clock))))

(defrectype <digital-clock> (<clock-widget>) ())

(gfmethod (draw (clock <digital-clock>))
	  (cl-format #t "Drawing digital clock, time = ~a~%"
		  (clock-timer-data (clock-timer clock))))

(defrectype <analog-clock> (<clock-widget>) ())

(gfmethod (draw (clock <analog-clock>))
	  (cl-format #t "Drawing analog clock, time = ~a~%"
		  (clock-timer-data (clock-timer clock))))

(define t1 (new <clock-timer>)) 
(define dc1 (new <digital-clock> t1))
(tick t1 1)
(define ac1 (new <analog-clock> t1))
(tick t1 2)

;; note how can extend above to only get notified of certain classes
;; of events by parameterizing notify-function methods

;; State pattern

(defrectype <tcp-connection> ()
  ((status <symbol> 'closed))
  (status connection-status set-connection-status!))

(define <open>
  (and? <tcp-connection>
	(lambda (c)
	  (eq? 'open (connection-status c)))))

(define <closed>
  (and? <tcp-connection>
	(lambda (c)
	  (eq? 'closed (connection-status c)))))

(defgeneric open
  (method ((c <tcp-connection>))
	  (error "Cannot open connection."))
  (method ((c <closed>))
	  (set-connection-status! c 'open)))

(defgeneric transmit
  (method ((c <tcp-connection>) data)
	  (error "Cannot transmit on connection."))
  (method ((c <open>) data)
	  (cl-format #t "Transmitting: ~a~%" data)))

(defgeneric close
  (method ((c <tcp-connection>))
	  (error "Cannot close connection."))
  (method ((c <open>))
	  (set-connection-status! c 'closed)))

(define c1 (new <tcp-connection>))
(open c1)
(transmit c1 "hi")
(open c1)				; should err


;; Visitor pattern

(defrectype <visitor> () ())

(defrectype <equipment> ()
  ((name <symbol>)
   (price <number>))
  (name equipment-name set-equipment-name!)
  (price equipment-price set-equipment-price!))

(defrectype <composite-equipment> (<equipment>)
  ((parts <list>))
  (parts composite-parts set-composite-parts!))

(defgeneric visit
  ;; default method does nothing
  (method ((e <equipment>) (v <visitor>)) #t)
  ;; visit for composite visits parts
  (method ((e <composite-equipment>) (v <visitor>))
	  (for-each (rcurry visit v) (composite-parts e))))
	   
(defrectype <floppy> (<equipment>) ())
(defrectype <chassis> (<composite-equipment>) ())

(defrectype <price-visitor> (<visitor>)
  ((total-price <number> 0))
  (total-price total-price set-total-price!))

;; visit floppy
(gfmethod (visit (f <floppy>) (v <price-visitor>))
	  (set-total-price!
	   v (+ 1 (total-price v) (equipment-price f))))
;; visit chassis
(gfmethod (visit (f <chassis>) (v <price-visitor>))
	  (call-next-method)		; visit subparts
	  (set-total-price!
	   v (+ 2 (total-price v) (equipment-price f))))

(define f1 (new <floppy> 'name 'floppy1 'price 1))
(define f2 (new <floppy> 'name 'floppy2 'price 3))
(define c1 (new <chassis> 'name 'chassis1 'price 2
		'parts (list f1 f2)))

(define v1 (new <price-visitor>))
(define v1val (visit c1 v1))
(total-price v1)





(define-callable-record-type foo call-foo :foo
  (make-foo x y) foo?
  (x get-x set-x)
  (y get-y))

(define (call-foo wrapped-foo a-foo . args)
  (cl-format #t "Calling foo[x=~a, y=~a] with args ~a~%" (get-x a-foo) (get-y a-foo) args))

(define f1 (make-foo 3 "hi"))


(define rt1 (make-record-type 'rt1
			      (list 'x)
			      (list 'y <int>)
			      (list 'z <int> (lambda () 3))))
			      
(defrectype rt3 (x <int>))
(define v3 (new rt3 3))
(get-by-name 'x v3)
(defrectype rt4 (x <int> 3) (y <int>) (z))

(define rt5 (make-record-type
	     (x)
	     (y <int>)
	     (z string? (lambda () "zval"))))

(define st1 (make-record-subtype (rt3) ((y <int>))))
(define st2 (make-record-subtype (rt4) (z)))
(define vv1 (new st1 2 3))
(get-by-name 'x vv1)
(get-by-name 'y vv1)
(describe-constructor st2)
(describe-fields st2)

(let ((foo (lambda (y) 25))
      (bar (lambda (y) "hi"))
      (y 1))
  (tlet ((x integer? (foo y))
	 (name string? (bar y)))
	(list x name)))


;; VESTIGIAL code follows

(call add-method g1 m2)

(define (foo x)
  (try
   (bar x)
   ((<cond1> (lambda (e) (cl-format #t "got cond1 ~a~%" e)))
    (<cond2> (lambda (e) (cl-format #t "got cond2 ~a~%" e))))
   (cl-format #t "top finally~%")))

(define (bar x)
  (try
   (baz x)
   ()
   (cl-format #t "finally in bar~%")))

(define (baz x)
  (cl-format #t "in baz, throwing cond2:~%")
  (throw <cond2> (list "this is a cond2 exception"))
  (cl-format #t "in baz, shouldn't get here.~%"))

(test2
 (isa? 42 <int>)
 #t)
(test2
 (isa? 42 <number>)
 #t)
(test2
 (subtype? <int> <number>)
 #t)
(test2
 (subtype? <number> <int>)
 #f)

;; tlet macro
(test2
 (tlet ((x 3)
	((y <int>) 4))
       (+ x y))
 7)

(test2
 (subtype? <b> <a>)
 #t)

(test2
 (isa? a1 <a>)
 #t)

(test2
 (isa? b1 <a>)
 #t)

(test2
 (isa? a1 <b>)
 #f)

(test2
 (call a-field a1)
 "a1")

(test2
 (call a-field b1)
 "a1")
  
(test2
 (call m1 a1)
 "a1")

(test2
 (call m1 b1)
 "a1")

(test2
 (call g1 a1)
 "a1")

(test2
 (call g1 a1)
 "a1")

(test2
 (call g1 b1)
 2)

(defpred <even> even?)
(defpred <odd> odd?)

(defmethod (m3 (x (make-and-type <int> <even>)))
  "in m3")
(call add-method g1 m3)

(defmethod (m4 (x <int>))
  "in m4")
(call add-method g1 m4)

(test2
 (isa? (method ((x <int>)) => <int> (+ x 2))
       (make-method-type #f <int> <int>))
 #t)

(test2
 (isa? (method (x) => <int> (+ x 2))
       (make-method-type #f <int> <int>))
 #t)

(test2
 (isa? (method ((x <int>)) => <int> (+ x 2))
       (make-method-type #f <int> <top>))
 #f)

(define n1
  (make-method "n1"
	       (pred (lambda (l)
		       (< (car l) (cadr l))))
	       <top>
	       (lambda (x y) (+ x y))))
(define n2
  (make-method "n2"
	       (pred (lambda (l)
		       (= (car l) (cadr l))))
	       <top>
	       (lambda (x y) (- x y))))
(define n3
  (make-method "n3"
	       (pred (lambda (l)
		       (> (car l) (cadr l))))
	       <top>
	       (lambda (x y) (* x y))))
(defgeneric g2 n1 n2 n3)
(call g2 2 3)

(defmethod (m31 (x <a>)) "m31: (x <a>)")
(defmethod (m32 (x (eq a1))) "m32: (x (eq a1))")
(defmethod (m33 (x (eq a2))) "m33: (x (eq a2))")
(defgeneric g30 m31 m32 m33)

(define <mypred1> (make-named-pred-type '<my-pred1>
					(lambda (v) (> v 5))))

(define <mypred2> (make-named-pred-type '<my-pred2>
					(lambda (v) (> v 6))))

(subtype? (make-and-type <mypred1> <even>) (make-and-type <mypred1> <number>))

(defgeneric fact
  (method ((x (eq 1)))
	  1)
  (method ((x <int>))
	  (* x (call fact (- x 1)))))

(gfmethod (fact (x (eq 3))) (begin (cl-format #t "in fact(3)~%") 6))
(call fact 2)
(call fact 4)

(define-syntax orig-define
  (syntax-rules ()
    ((orig-define ?var ?exp)
     (begin
       (cl-format #t "defining ~a...~%" '?var)
       (define ?var ?exp)))
    ((orig-define (?var . ?formals) ?body)
     (begin
       (cl-format #t "defining ~a...~%" '?var)
       (define (?var ?formals) ?body)))
    ((orig-define (?var ?formals) ?body)
     (begin
       (cl-format #t "defining ~a...~%" '?var)
       (define (?var ?formals) ?body)))))
    

|#
;; eof
