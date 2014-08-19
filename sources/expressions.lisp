;; -*- mode: lisp; encoding: utf-8; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Třída expression
;;;

(defclass expression ()
 ())

(defmethod deriv ((expr expression) var)
  (error "Metodu deriv je třeba přepsat"))

(defmethod expr-subst ((expr expression) var substituent)
  (error "Metodu expr-subst je třeba přepsat"))

(defmethod representation ((expr expression))
  expr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Třída binary-expression a potomci
;;;

(defclass binary-expression (expression)
  ((expr-1 :initform (make-instance 'const))
   (expr-2 :initform (make-instance 'const))))

(defmethod expr-subst ((expr binary-expression) var substituent)
  (let ((result (make-instance (type-of expr))))
    (setf (slot-value result 'expr-1)
	  (expr-subst (slot-value expr 'expr-1) var substituent)
	  (slot-value result 'expr-2)
	  (expr-subst (slot-value expr 'expr-2) var substituent))
    result))

(defmethod representation ((expr binary-expression)) 
  `(,(bin-expr-symbol expr)
    ,(representation (slot-value expr 'expr-1))   
    ,(representation (slot-value expr 'expr-2))))

(defmethod bin-expr-symbol ((expr binary-expression))
  (error "Metodu bin-expr-symbol je třeba přepsat."))

(defclass +-expr (binary-expression)
  ())

(defmethod bin-expr-symbol ((expr +-expr))
  '+)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
    (t (error "Nečitelná reprezentace výrazu"))))