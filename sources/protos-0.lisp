;; -*- mode: lisp; encoding: utf-8; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; protos-0, jednoduchý prototypový systém
;;;;
;;;; Kvůli jednoduchosti zdrojového kódu není makro define-method připraveno 
;;;; na deklaraci (declare (ignore název-proměnné)). Proto při kompilaci zdrojových
;;;; souborů systému protos-0 (soubor protos-0-graphics.lisp) vznikají warningy, 
;;;; které budeme (vyjímečně) tolerovat.


(defun make-object ()
  (list 'object))

(defun field-value (obj field)
  ;; field-not-found může být libovolný objekt, o kterém si jsme jisti,
  ;; že je jedinečný (nemůže být prvkem pole protos-objektu)
  (let* ((field-not-found (cons 0 0))
         (result (getf (cdr obj) field field-not-found)))
    (if (eql result field-not-found)
        (field-value (field-value obj :super-object) field)
      result)))

(defun set-field-value (obj field value) 
  (setf (getf (cdr obj) field) value))

(defun super-object (obj)
  (field-value obj :super-object))

(defun clone-object (obj)
  (list 'object :super-object obj))

(defun call-meth (obj1 obj2 message &rest args)
  (apply (field-value obj1 message) obj2 args))

(defun send (obj message &rest args)
  (apply #'call-meth obj obj message args))

(defmacro define-method (obj message lambda-list &body body)
  `(set-field-value ,obj ,message
                    (lambda (self ,@lambda-list)
                      (labels ((call-super (&rest args)
                                 (apply #'call-meth
                                        (super-object ,obj)
                                        self
                                        ,message
                                        args)))
                        ,@body))))

(defvar *object*)
(setf *object* (make-object))

(set-field-value *object* :super-object nil)

(define-method *object* :super ()
  (super-object self))

(define-method *object* :clone ()
  (clone-object self))

(define-method *object* :is (superobject)
  (and self ;ukončení rekurze
       (or (eql self superobject)
           (send (send self :super) :is superobject))))

