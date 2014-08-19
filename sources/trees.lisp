;; -*- mode: lisp; encoding: utf-8; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; trees.lisp
;;;;
;;;; Příklad použití vícenásobné dědičnosti, pokud chceme s libovolnými objekty pracovat
;;;; jako se stromy
;;;;
;;;; Ukázka simuluje prostředí C++ - všechny objekty, které mají metodu tree-node-children, musí být
;;;; přímými nebo nepřímými instancemi třídy tree-node. V Closu to není nutné a problém lze řešit
;;;; bez použití vícenásobné dědičnosti.
;;;;
;;;; Kvůli příkladu je třeba načíst knihovnu micro-graphics a soubor 09.lisp.
;;;;

(defclass tree-node ()
  ())

;;;
;;; Fyzická vrstva: metoda, která vrací seznam následníků uzlu
;;;

(defmethod tree-node-children ((node tree-node))
  '())

;;;
;;; Dva z mnoha možných příkladů použití - vyhledávání ve stromu (průchod do hloubky)
;;;

(defmethod find-tree-node ((start-node tree-node) what)
  (if (eql what start-node)
      start-node
    (find-if (lambda (n)
               (find-tree-node n what))
             (tree-node-children start-node))))

(defmethod find-tree-node-if ((start-node tree-node) test)
  (if (funcall test start-node)
      start-node
    (find-if (lambda (n)
               (find-tree-node-if n test))
             (tree-node-children start-node))))

;;;
;;; Testovací příklad. Instance třídy simple-tree-node mají seznam následníků ve slotu children
;;; (přepisují metodu tree-node-children) a ve slotu value mají data.
;;;

(defclass simple-tree-node (tree-node)
  ((value :initform nil)
   (children :initform '())))

(defmethod simple-tn-value ((tn simple-tree-node))
  (slot-value tn 'value))

(defmethod tree-node-children ((tn simple-tree-node))
  (slot-value tn 'children))

;;; Pomocná funkce, umožňuje snadné vytvoření stromu:
(defun make-simple-tree (list)
  (let ((result (make-instance 'simple-tree-node)))
    (setf (slot-value result 'value) (car list))
    (setf (slot-value result 'children) (mapcar #'make-simple-tree (cdr list)))
    result))

#|
(setf tn (make-simple-tree '(1 (2 (3)) (4) (5 (6 (7))))))
(find-tree-node-if tn (lambda (n) (eql (simple-tn-value n) 4)))
|#

;;;
;;; Aplikace v knihovně clmg3. Grafické objekty lze považovat za uzly, kromě obrázků
;;; mají všechny prázdný seznam následníků (dědí metodu tree-node-children ze třídy tree-node):
;;;

(defclass tree-circle (tree-node circle)
  ())

(defclass tree-polygon (tree-node polygon)
  ())

(defclass tree-empty-shape (tree-node empty-shape)
  ())

(defclass tree-full-shape (tree-node full-shape)
  ())

(defclass tree-point (tree-node point)
  ())

(defclass tree-picture (tree-node picture)
  ())

;;; Seznam následníků obrázku jsou všechny prvky jeho seznamu items:
(defmethod tree-node-children ((node tree-picture))
  (items node))

;;; Nyní lze v obrázcích vyhledávat do hloubky.

#|
;; Ukázka: hledání kolečka v japonské vlajce a nastavení správné barvy

(defclass flag-rectangle (tree-picture) ())

(defun flag-rect-points ()
  (mapcar (lambda (coords)
            (apply #'move (make-instance 'point) coords))
          '((20 20) (200 20) (200 140) (20 140))))

(defmethod initialize-instance ((fr flag-rectangle) &rest initargs)
  (call-next-method)
  (let ((rect1 (make-instance 'tree-polygon))
        (rect2 (make-instance 'tree-polygon)))
    (set-items rect1 (flag-rect-points))
    (set-items rect2 (flag-rect-points))
    (set-filledp rect2 t)
    (set-color rect2 :white)
    (set-items fr (list rect1 rect2)))
  fr)

(defun jp-flag-items ()
  (let ((circle (make-instance 'tree-circle)))
    (move circle 110 80)
    (set-filledp circle t)
    (set-radius circle 36)
    (set-color circle :red)
    (list circle
          (make-instance 'flag-rectangle))))

(defclass jp-flag (tree-picture)
  ())

(defmethod initialize-instance ((flag jp-flag) &rest args)
  (call-next-method)
  (set-items flag (jp-flag-items))
  flag)

(setf flag (make-instance 'jp-flag))

(setf w (make-instance 'window))

(set-shape w flag)

(setf node (find-tree-node-if flag (lambda (item)
                                     (typep item 'circle))))

(set-color node (color:make-rgb (/ #xce 255.0)
                                (/ #x11 255.0)
                                (/ #x26 255.0)))
|#