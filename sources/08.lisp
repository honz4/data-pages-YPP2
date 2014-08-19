;; -*- mode: lisp; encoding: utf-8; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Třída expression
;;;

(defclass expression ()
  ())

(defmethod deriv ((expr expression) var)
  (error "Method deriv has to be rewritten"))

(defmethod expr-subst ((expr expression) var substituent)
  (error "Method expr-subst has to be rewritten"))

(defmethod representation ((expr expression))
  expr)

(defmethod simplify ((expr expression))
  expr)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Třída const
;;;

(defclass const (expression)
  ((value :initform 0)))


(defmethod expr-subst ((expr const) var substituent)
  expr)

(defmethod deriv ((expr const) var)
  (make-instance 'const))

(defmethod representation ((expr const))
  (slot-value expr 'value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Třída var
;;;

(defclass var (expression)
  ((name :initform 'x)))

(defmethod deriv ((expr var) var)
  (parse
   (if (eql (slot-value expr 'name)
	    (slot-value var 'name))
       1
     0)))

(defmethod expr-subst ((expr var) var substituent)
  (if (eql (slot-value expr 'name)
	   (slot-value var 'name))
      substituent
    expr))

(defmethod representation ((expr var))
  (slot-value expr 'name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Třída binary-expression a potomci
;;;

(defclass binary-expression (expression)
  ((expr-1 :initform (make-instance 'const))
   (expr-2 :initform (make-instance 'const))))

(defmethod expr-subst ((expr binary-expression) 
		       var substituent)
  (let ((result (make-instance (type-of expr))))
    (setf (slot-value result 'expr-1)
	  (expr-subst (slot-value expr 'expr-1)
		      var
		      substituent)
	  (slot-value result 'expr-2)
	  (expr-subst (slot-value expr 'expr-2) 
		      var
		      substituent))
    result))

(defmethod simplify ((expr binary-expression))
  (parse `(,(bin-expr-symbol expr)
	   ,(simplify (slot-value expr 'expr-1))
	   ,(simplify (slot-value expr 'expr-2)))))

(defmethod representation ((expr binary-expression)) 
  `(,(bin-expr-symbol expr)
    ,(representation (slot-value expr 'expr-1))   
    ,(representation (slot-value expr 'expr-2))))

(defmethod bin-expr-symbol ((expr binary-expression))
  (error "Method bin-expr-symbol has to be rewritten."))

(defclass +-expr (binary-expression)
  ())

(defmethod bin-expr-symbol ((expr +-expr))
  '+)

(defmethod zero-const-p ((expr expression))
  nil)

(defmethod zero-const-p ((expr const))
  (zerop (slot-value expr 'value)))

(defmethod simplify ((expr +-expr))
  (let* ((result (call-next-method))
	 (expr-1 (slot-value result 'expr-1))
	 (expr-2 (slot-value result 'expr-2)))
    (cond ((zero-const-p expr-1) expr-2)
	  ((zero-const-p expr-2) expr-1)
	  (t result))))

(defclass --expr (binary-expression)
  ())

(defmethod bin-expr-symbol ((expr --expr))
  '-)

(defclass *-expr (binary-expression)
  ())

(defmethod bin-expr-symbol ((expr *-expr))
  '*)

(defmethod deriv ((expr *-expr) var)
  (let ((expr-1 (slot-value expr 'expr-1))
	(expr-2 (slot-value expr 'expr-2)))
    (parse `(+ (* ,(deriv expr-1 var) ,expr-2)
	       (* ,expr-1 ,(deriv expr-2 var))))))
(defclass /-expr (binary-expression)
  ())

(defmethod bin-expr-symbol ((expr /-expr))
  '/)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Funkce parse
;;;

(defvar *const-expr-class* 'const)
(defvar *var-expr-class* 'var)
(defvar *+-expr-class* '+-expr)
(defvar *--expr-class* '--expr)
(defvar **-expr-class* '*-expr)
(defvar */-expr-class* '/-expr)

(defun make-binary-expr (name expr-1 expr-2)  
  (let ((result (make-instance 
		 (cond   
		   ((eql name '+) *+-expr-class*) 
		   ((eql name '-) *--expr-class*)  
		   ((eql name '*) **-expr-class*) 
		   ((eql name '/) */-expr-class*)))))
    (setf (slot-value result 'expr-1)
	  (parse expr-1)
	  (slot-value result 'expr-2)
	  (parse expr-2))
    result))

(defun make-const (value)
  (let ((result (make-instance *const-expr-class*)))
    (setf (slot-value result 'value) value)
    result))

(defun make-var (name)
  (let ((result (make-instance *var-expr-class*)))
    (setf (slot-value result 'name) name)
    result))

(defun parse (repr)  
  (cond
    ((typep repr 'number) (make-const repr))
    ((typep repr 'symbol) (make-var repr))  
    ((typep repr 'list) (apply 'make-binary-expr repr))
    ((typep repr 'expression) repr)
    (t (error "Cannot parse ~s" repr))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Posloupnosti. Třída my-sequence
;;;

(defclass my-sequence ()
  ())

(defmethod after-end-p ((sequence my-sequence) position)
  (error "Method after-end-p has to be rewritten."))

(defmethod before-start-p ((sequence my-sequence) position)
  (error "Method before-start-p has to be rewritten."))

(defmethod first-position ((sequence my-sequence))
  (error "Method first-position has to be rewritten."))

(defmethod last-position ((sequence my-sequence))
  (error "Method last-position has to be rewritten."))

(defmethod next-position ((sequence my-sequence) position)
  (error "Method next-position has to be rewritten."))

(defmethod prev-position ((sequence my-sequence) position)
  (error "Method prev-position has to be rewritten."))

(defmethod position-item ((seq my-sequence) pos)
  (error "Method position-item has to be rewritten."))

(defmethod set-position-item ((seq my-sequence) pos item)
  (error "Method set-position-item has to be rewritten."))

(defmethod nth-pos ((seq my-sequence) index)
  (labels ((iter (position i)
	     (if (= i 0)
		 position
	       (iter (next-position seq position) 
		     (- i 1)))))
    (iter (first-position seq) index)))

(defmethod my-elt ((seq my-sequence) index)
  (position-item seq (nth-pos seq index)))

(defmethod my-set-elt ((seq my-sequence) index value)
  (set-position-item seq (nth-pos seq index) value))

(defmethod my-length (seq)
  (labels ((iter (index pos)
	     (if (after-end-p seq pos)
		 index
	       (iter (+ index 1)
		     (next-position seq pos)))))
    (iter 0 (first-position seq))))

(defmethod my-find ((seq my-sequence) elem)
  (labels ((iter (pos)
	     (unless (after-end-p seq pos)
	       (if (eql elem (position-item seq pos))
		   elem
		 (iter (next-position seq pos))))))
    (iter (first-position seq))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Třída my-vector
;;;

(defclass my-vector (my-sequence)
  ((representation :initform "")))

(defmethod representation ((vector my-vector))
  (copy-seq (slot-value vector 'representation)))

(defmethod set-representation ((vector my-vector) value)
  (setf (slot-value vector 'representation) 
	(copy-seq value))
  vector)

(defmethod after-end-p ((vec my-vector) position)
 (>= position (my-length vec))) 

(defmethod before-start-p ((vec my-vector) position)
  (<= position 0))

(defmethod first-position ((vec my-vector))
  0)

(defmethod last-position ((vec my-vector))
  (- (my-length vec) 1))

(defmethod next-position ((vec my-vector) position)
  (+ position 1))

(defmethod prev-position ((vec my-vector) position)
  (- position 1))

(defmethod position-item ((vec my-vector) position)
  (aref (slot-value vec 'representation)
	position))

(defmethod set-position-item ((vec my-vector) position value)
  (setf (aref (slot-value vec 'representation) 
	      position) 
	value)
  vec)

(defmethod my-length ((vec my-vector))
  (length (representation vec)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Třída my-list
;;;

(defclass my-list (my-sequence)
  ())

(defmethod my-list-to-list ((list my-list))
  '())

(defmethod my-empty-list-p ((list my-list))
  nil)

(defmethod after-end-p ((list my-list) position)
  (my-empty-list-p position))

(defmethod before-start-p ((list my-list) position)
  (my-empty-list-p position))

(defmethod first-position ((list my-list))
  list)

(defmethod next-position ((list my-list) position)
  (my-cdr position))

(defmethod prev-position ((list my-list) position)
  (cond ((my-empty-list-p position)
	   (error "There is no previous position"))
	((eql position (my-cdr list))
	   list)
	(t (prev-position (my-cdr list) position))))

(defmethod position-item ((list my-list) position)
  (my-car position))

(defmethod set-position-item ((list my-list) position value)
  (setf (slot-value position 'my-car) value)
  list)

(defclass my-empty-list (my-list)
  ())

(defmethod my-empty-list-p ((list my-empty-list))
  t)

(defmethod last-position ((list my-empty-list))
  list)

(defclass my-cons (my-list)
  ((my-car :initform nil)
   (my-cdr :initform (make-instance 'my-empty-list))))

(defmethod my-car ((cons my-cons))
  (slot-value cons 'my-car))

(defmethod my-cdr ((cons my-cons))
  (slot-value cons 'my-cdr))

(defmethod my-list-to-list ((list my-cons))
  (cons (my-car list)
	(my-list-to-list (my-cdr list))))

(defmethod last-position ((list my-cons))
  (if (my-empty-list-p (my-cdr list))
      list
    (last-position (my-cdr list))))

(defun list-to-my-list (list)
  (if (null list)
      (make-instance 'my-empty-list)
    (let ((new (make-instance 'my-cons)))
      (setf (slot-value new 'my-car) 
	    (car list)
	    (slot-value new 'my-cdr)
	    (list-to-my-list (cdr list)))
      new)))



