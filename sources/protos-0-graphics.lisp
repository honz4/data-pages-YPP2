;; -*- mode: lisp; encoding: utf-8; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Objektový systém protos-0
;;;; Některé grafické objekty
;;;;
;;;; Kvůli jednoduchosti zdrojového kódu systému (soubor protos-0.lisp) není makro 
;;;; define-method připraveno na deklaraci (declare (ignore název-proměnné)). Proto při
;;;; kompilaci tohoto souboru vznikají warningy, které budeme (vyjímečně) tolerovat.
;;;;


;;; *shape*

(defvar *shape*)
(setf *shape* (send *object* :clone))

(define-method *shape* :get-color ()
  (field-value self :color))

(define-method *shape* :set-color (value)
  (set-field-value self :color value))

(send *shape* :set-color :black)

(define-method *shape* :get-thickness ()
  (field-value self :thickness))

(define-method *shape* :set-thickness (value)
  (set-field-value self :thickness value))

(send *shape* :set-thickness 1)

(define-method *shape* :move (dx dy)
  self)

(define-method *shape* :rotate (angle)
  self)

(define-method *shape* :scale (coeff)
  self)

(define-method *shape* :contains-point-p (point)
  nil)

;;; *empty-shape* a *full-shape*

(defvar *empty-shape*)
(setf *empty-shape* (send *shape* :clone))
(defvar *full-shape*)
(setf *full-shape* (send *shape* :clone))

(define-method *full-shape* :contains-point-p (point)
  t)

;;; *point*

(defvar *point*)
(setf *point* (send *shape* :clone))

(define-method *point* :get-x ()
  (field-value self :x))

(define-method *point* :get-y ()
  (field-value self :y))

(define-method *point* :set-x (value)
  (set-field-value self :x value))

(define-method *point* :set-y (value)
  (set-field-value self :y value))

(send *point* :set-x 0)
(send *point* :set-y 0)

(define-method *point* :get-r ()
  (let ((x (send self :get-x))
        (y (send self :get-y)))
    (sqrt (+ (* x x) (* y y)))))

(define-method *point* :get-phi ()
  (let ((x (send self :get-x))
        (y (send self :get-y)))
    (cond ((plusp x) (atan (/ y x)))
          ((minusp x) (+ pi (atan (/ y x))))
          (t (* (signum y) (/ pi 2))))))

(defun set-r-phi (obj r phi)
  (send obj :set-x (* r (cos phi)))
  (send obj :set-y (* r (sin phi))))

(define-method *point* :set-r (value)
  (set-r-phi self value (send self :get-phi))
  value)

(define-method *point* :set-phi (value)
  (set-r-phi self (send self :get-r) value)
  value)

(define-method *point* :move (dx dy)
  (send self :set-x (+ (send self :get-x) dx))
  (send self :set-y (+ (send self :get-y) dy))
  self)

(define-method *point* :rotate (angle)
  (send self :set-phi (+ (send self :get-phi) angle))
  self)

(define-method *point* :scale (coeff)
  (send self :set-r (* (send self :get-r) coeff))
  self)

(define-method *point* :contains-point-p (point)
  ;;dodělejte
  )

;;; *circle*

(defvar *circle*)
(setf *circle* (send *shape* :clone))

(set-field-value *circle* :center (send *point* :clone))

(set-field-value *circle* :radius 0)

(define-method *circle* :get-center ()
  (field-value self :center))

(define-method *circle* :get-radius ()
  (field-value self :radius))

(define-method *circle* :set-radius (value)
  (set-field-value self :radius value))

(define-method *circle* :move (dx dy)
  (send (send self :get-center) :move dx dy)
  self)

(define-method *circle* :rotate (angle)
  (send (send self :get-center) :rotate angle)
  self)

(define-method *circle* :scale (coeff)
  (send (send self :get-center) :scale coeff)
  (send self :set-radius (* coeff (send self :get-radius)))
  self)

(define-method *circle* :contains-point-p (point)
  ;; dodělejte
   )

(define-method *circle* :get-filled-p ()
  (field-value self :filled-p))

(define-method *circle* :set-filled-p (value)
  (set-field-value self :filled-p value))

(send *circle* :set-filled-p t)

(define-method *circle* :clone ()
  (let ((result (call-super))
        (center (send (send self :get-center) :clone)))
    (set-field-value result :center center)
    result))

