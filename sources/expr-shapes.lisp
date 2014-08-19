;; -*- mode: lisp; encoding: utf-8; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; expr-shapes.lisp
;;;;
;;;; Ukázka použití vícenásobné dědičnosti: grafické objekty, které jsou současně vzorci.
;;;; Narozdíl od jednoduššího příkladu ze souboru trees.lisp dědí nově definované třídy
;;;; (const-shape, var-shape, +-expr-shape atd.) z obou stran nejen metody, ale i sloty.
;;;;
;;;; Při použití vícenásobné dědičnosti je třeba vždy zvážit, zda výhody, které poskytuje,
;;;; vyváží případné problémy, které způsobuje. U tohoto příkladu je zcela rozumnou možností
;;;; použít pro definici třídy expr-shape a jejích potomků pouze jednoduchou dědičnost.
;;;; Necháváme na zvážení, jak.
;;;;
;;;; Je třeba načíst knihovnu micro-graphics a soubory 09.lisp a expressions.lisp.
;;;;

(defclass expr-shape (text-shape)
  ())

(defmethod text ((shape expr-shape))
  (format nil "~a" (representation shape)))

(defmethod set-text ((shape expr-shape) value)
  (error "Cannot set text of expr-shape"))

(defclass const-shape (const expr-shape)
  ())

(defclass var-shape (var expr-shape)
  ())

(defclass +-expr-shape (+-expr expr-shape)
  ())

(defclass --expr-shape (--expr expr-shape)
  ())

(defclass *-expr-shape (*-expr expr-shape)
  ())

(defclass /-expr-shape (/-expr expr-shape)
  ())

;; Nastavení globálních proměnných, aby funkce parse vytvářela instance
;; požadovaných tříd (podívejte se na její definici).
(setf *const-expr-class* 'const-shape)
(setf *var-expr-class* 'var-shape)
(setf *+-expr-class* '+-expr-shape)
(setf *--expr-class* '--expr-shape)
(setf **-expr-class* '*-expr-shape)
(setf */-expr-class* '/-expr-shape)

#|
(setf e (parse '(+ 1 (/ x y))))
(setf w (make-instance 'window))
(set-shape w e)
(move e 50 50)
|#