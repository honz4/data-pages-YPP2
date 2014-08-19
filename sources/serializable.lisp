;; -*- mode: lisp; encoding: utf-8; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; serializable.lisp
;;;
;;; Zjednodušená vícenásobna dědičnost - rozhraní (interfaces)
;;;
;;; interfaces lze chápat jako třídy, u nichž nelze vytvářet instance, definovat
;;; sloty a kterým lze definovat pouze abstraktní metody. V některých jazycích
;;; s jednoduchou dědičností (Java, C#) lze používat zjednodušenou podobu
;;; vícenásobné dědičnosti tak, že každá třída smí mít v seznamu bezprostředních 
;;; předků pouze jednu pravou třídu, ostatní musí být pouze interfaces. Tím se 
;;; odstraní největší problém vícenásobné dědičnosti za cenu radikálního omezení 
;;; jejích možností.
;;;
;;; K otestování třídy serializable-circle je třeba načíst knihovnu
;;; micro-graphics a soubor 09.lisp.
;;;

;; Hraje roli rozhraní - má pouze abstraktní metody:
(defclass serializable-object ()
  ())

(defmethod serialize-data ((obj serializable-object))
  (error "Must be rewritten"))

(defmethod initialize ((obj serializable-object) data)
  (error "Must be rewritten"))

(defun serialize (obj)
  (cons (class-name (class-of obj))
        (serialize-data obj)))

(defun deserialize (list)
  (let ((object (make-instance (car list))))
    (initialize object (cdr list))
    object))

;;;
;;; Třída serializable-circle
;;;
;;; tato třída má mezi svými předky jednu skutečnou třídu (circle)
;;; a jedno rozhraní (serializable-object), jehož metody musí implementovat
;;; 
;;; Podobně je možno přidat rozhraní serializable-object k libovolné
;;; jiné třídě. Pak je třeba implementovat metody serialize-data
;;; a initialize. Serializace se používá voláním funkcí serialize
;;; a deserialize.

(defclass serializable-circle (circle serializable-object)
  ())

(defmethod serialize-data ((obj serializable-circle))
  (list (x (center obj))
        (y (center obj))
        (radius obj)))

(defmethod initialize ((obj serializable-circle) data)
  (move obj (first data) (second data))
  (set-radius obj (third data)))
        

#|
(setf circ (make-instance 'serializable-circle))
(move circ 30 20)
(serialize circ)

(deserialize '(serializable-circle 30 20 5))
|#