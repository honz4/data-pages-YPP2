;; -*- mode: lisp; encoding: utf-8; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; mixins.lisp
;;;;
;;;; Příklady mixinů, tedy abstraktních tříd (bez instancí), které 
;;;; přimícháním k jiným třídám pomocí vícenásobné dědičnosti přidávají nějakou funkčnost.
;;;;
;;;; mixiny často ve svých metodách volají metody tříd, ke kterým mají být přimíchány
;;;; (například metodu set-color třídy shape v metodě mouse-down třídy random-color-mixin) a
;;;; implementují tyto metody (samotná metoda mouse-down v téže třídě). Proto je nelze
;;;; používat v jazyce C++.
;;;;
;;;; Je třeba načíst knihovnu micro-graphics a soubor 09.lisp
;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Třída random-color-mixin určená k přimíchání k potomkům třídy shape - instance po kliknutí 
;;; změní barvu.
;;;

(defun random-color ()
  (let ((colors (color:get-all-color-names))) 
    (nth (random (length colors)) colors)))

(defclass random-color-mixin ()
  ())

(defmethod mouse-down ((shape random-color-mixin) button position)
  (set-color shape (random-color))
  (call-next-method))

;;;
;;; přimíchání:
;;;

(defclass click-circle (random-color-mixin circle)
  ())

(defclass click-polygon (random-color-mixin polygon)
  ())

;;;
;;; příklad:
;;;

;; následující dvě funkce budeme potřebovat v celém souboru, proto je
;; definujeme mimo komentář:

(defun make-test-circle (class)
  (let ((c (make-instance class)))
    (move c 30 30)
    (set-radius c 20)
    (set-filledp c t)
    c))

(defun make-test-polygon (class)
  (let ((result (make-instance class)))
    (set-items result
               (mapcar (lambda (x y)
                         (move (make-instance 'point) x y))
                       '(10 50 50 10)
                       '(10 10 50 50)))
    (set-filledp result t)
    result))

#|
(set-shape (make-instance 'window) (make-test-circle 'click-circle))
(set-shape (make-instance 'window) (make-test-polygon 'click-polygon))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Ukázka více než dvojnásobné dědičnosti
;;;

(defclass click-1-mixin ()
  ())

(defmethod mouse-down ((shape click-1-mixin) button where)
  (call-next-method)
  (when (eql button :left)
    (scale shape 11/10 (make-instance 'point))))

(defclass click-1-circle (click-1-mixin circle)
  ())

#|
(set-shape (make-instance 'window) (make-test-circle 'click-1-circle))
|#

(defclass click-2-mixin ()
  ())

(defmethod mouse-down ((shape click-2-mixin) button where)
  (call-next-method)
  (when (eql button :left)
    (set-color shape (random-color))))

(defclass click-2-circle (click-2-mixin circle)
  ())

#|
(set-shape (make-instance 'window) (make-test-circle 'click-2-circle))
|#

(defclass click-1-2-circle (click-1-mixin click-2-mixin circle)
  ())

#|
(set-shape (make-instance 'window) (make-test-circle 'click-1-2-circle))
|#

(defclass click-3-mixin ()
  ())

(defmethod mouse-down ((shape click-3-mixin) button where)
  (call-next-method)
  (when (eql button :left)  
    (move shape 20 0)))

(defclass click-3-circle (click-3-mixin circle)
  ())

#|
(set-shape (make-instance 'window) (make-test-circle 'click-3-circle))
|#

(defclass click-1-2-3-circle (click-1-mixin click-2-mixin click-3-mixin circle)
  ())

#|
(set-shape (make-instance 'window) (make-test-circle 'click-1-2-3-circle))
|#

(defclass click-0-mixin ()
  ())

(defmethod mouse-down ((shape click-0-mixin) button where)
  (when (and (eql button :left) (yes-or-no-p "Opravdu?"))
    (call-next-method)))

(defclass click-0-1-2-3-circle (click-0-mixin click-1-mixin click-2-mixin click-3-mixin
                                              circle)
  ())

#|
(set-shape (make-instance 'window) (make-test-circle 'click-0-1-2-3-circle))
|#


;;;
;;; funguje bez úprav i na polygon:
;;;

(defclass click-0-1-2-3-poly (click-0-mixin click-1-mixin click-2-mixin click-3-mixin
                                            polygon)
  ())

#|
(set-shape (make-instance 'window) (make-test-polygon 'click-0-1-2-3-poly))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Složitější příklad: Třídy highlight-mixin a highlight-compound-shape-mixin, které společně 
;;; přidávají k objektům možnost označování.
;;;
;;; highlight-mixin je třeba přimíchat zepředu (aby měla metoda color přednost) ke třídám 
;;; grafických objektů, highlight-compound-shape-mixin ke třídám složených objektů, které je 
;;; obsahují (obvykle picture)
;;;

(defvar *highlight-color* :limegreen)

(defclass highlight-mixin ()
  ((highlightedp :initform nil)))

(defmethod color ((shape highlight-mixin))
  (if (highlightedp shape)
      *highlight-color*
    (call-next-method)))

(defmethod highlightedp ((shape highlight-mixin))
  (slot-value shape 'highlightedp))

(defmethod set-highlightedp ((shape highlight-mixin) value)
  (setf (slot-value shape 'highlightedp) value)
  (when value
    (send-event shape 'ev-highlight))
  value)

(defmethod mouse-down ((shape highlight-mixin) button where)
  (let ((result (call-next-method)))
    (set-highlightedp shape t)
    result))

(defclass highlight-compound-shape-mixin ()
  ())

(defmethod set-items ((shape highlight-compound-shape-mixin) items)
  (let ((result (call-next-method)))
    (dolist (item (items shape))
      (when (typep item 'highlight-mixin)
        (add-event item 'ev-highlight)))
    result))

(defmethod ev-highlight ((shape highlight-compound-shape-mixin) sender)
  (with-change (shape 'ev-highlight sender)
    (dolist (item (items shape))
      (when (and (typep item 'highlight-mixin)
                 (not (eql item sender))
                 (highlightedp item))
      (set-highlightedp item nil)))))

;;;
;;; přimíchání:
;;;

(defclass highlight-circle (highlight-mixin circle)
  ())

(defclass highlight-polygon (highlight-mixin polygon)
  ())

(defclass highlight-items-picture (highlight-compound-shape-mixin picture)
  ())

;;;
;;; příklad:
;;;

#|
(defun highlight-shape-items ()
  (list (make-test-circle 'highlight-circle)
        (move (make-test-polygon 'highlight-polygon) 60 0)
        (move (make-test-polygon 'highlight-polygon) 0 60)
        (move (make-test-circle 'highlight-circle) 60 60)))

(defun highlight-shape ()
  (let ((result (make-instance 'highlight-items-picture)))
    (set-items result (highlight-shape-items))
    result))

(set-shape (make-instance 'window)
           (highlight-shape))
|#

