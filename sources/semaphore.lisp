;; -*- mode: lisp; encoding: utf-8; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Příklad k 7. části: semaphore.lisp
;;;
;;; Vyžaduje knihovnu micro-graphics a soubory 07.lisp a lights.lisp
;;;
;;; Umožňuje vytvářet semafory jakéhokoli počtu světel, jakýchkoliv barev a chování.
;;; Stačí upravit proměnnou *semaphore-descs*

(defvar *semaphore-dimension* 50 
  "Současně šířka semaforu a výška části pro jedno světlo")
(defvar *light-diameter-part* 3/4
  "Podíl šířky semaforu, který zabírá jedno světlo (průměr-světla/*semaphore-dimension*)")

(defvar *semaphore-descs*
  '(((:red :green) ((t nil) (nil t)))
    ((:red :orange :green) ((t nil nil) (t t nil) (nil nil t) (nil t nil))))
  "Údaje o barvách a chování jednotlivých typů semaforu. První podseznam je seznam barev světel, který současně určuje počet světel. Druhý podseznam obsahuje popisy jednotlivých fází (t je zapnuto, nil vypnuto)")

#|
;; Ukázka přidání semaforu o čtyřech světlech a divokém chování:
(setf *semaphore-descs*
      (cons '((:red :blue :orange :green)
              ((t nil nil nil) (t t nil nil) (nil t t nil) (nil nil nil t)
              (nil t nil t) (nil t t t) (t nil t t)))
            *semaphore-descs*))
|#

(defun semaphore-descs (light-count)
  "Vrací data semaforu daného počtu světel"
  (let ((result (find-if (lambda (desc)
                           (= (length (first desc))
                              light-count))
                         *semaphore-descs*)))
    (unless result
      (error "Uncorrect number of lights: ~s" light-count))
    result))

(defun semaphore-colors (light-count)
  "Seznam barev semaforu daného počtu světel"
  (first (semaphore-descs light-count)))

(defun semaphore-phase-descs (light-count)
  "Seznam popisů fází semaforu daného počtu světel"
  (second (semaphore-descs light-count)))

(defun semaphore-phase-desc (light-count phase)
  "Popis fází semaforu daného počtu světel a fáze. Fáze může být libovolné číslo (počítá se zbytek po dělení"
  (let ((phases (semaphore-phase-descs light-count)))
    (nth (mod phase (length phases)) phases)))

(defclass semaphore (picture)
  ((lights :initform (make-instance 'picture))
   (box :initform (set-filledp (make-instance 'polygon) t))
   (phase :initform 0)))

(defun make-light (position color)
  "Vytvoření světla dané pozice a barvy"
  (move
   (set-radius 
    (set-on-color (make-instance 'light) color)
    (* 1/2 *light-diameter-part* *semaphore-dimension*))
   (* 1/2 *semaphore-dimension*)
   (+ (* 1/2 *semaphore-dimension*)
      (* position *semaphore-dimension*))))

(defun make-light-list (count)
  "Vytvoření seznamu světel semaforu dané délky"
  (let ((colors (semaphore-colors count))
        result)
    (dotimes (i count)
      (setf result (cons (make-light i (nth i colors)) result)))
    (reverse result)))
  
(defun make-box-points (count)
  "Vytvoření seznamu bodů určujících krabici semaforu daného počtu světel"
  (list (make-instance 'point)
        (move (make-instance 'point) *semaphore-dimension* 0)
        (move (make-instance 'point) 
              *semaphore-dimension*
              (* count *semaphore-dimension*))
        (move (make-instance 'point) 0 (* count *semaphore-dimension*))))

(defmethod light-count ((sem semaphore))
  "Počet světel semaforu"
  (length (items (slot-value sem 'lights))))

(defmethod set-light-count ((sem semaphore) count)
  "Nastavení počtu světel semaforu. Nastavuje fázi na 0."
  (set-items (slot-value sem 'lights)
             (make-light-list count))
  (set-items (slot-value sem 'box)
             (make-box-points count))
  ;;Také kvůli správnému překreslení:
  (set-sem-phase sem 0)
  sem)

(defun set-light-phase (lights phase)
  "Nastavení fáze podle popisu danému seznamu světel semaforu"
  (dolist (pair (mapcar #'cons lights phase))
    (set-onp (car pair) (cdr pair))))
  
(defmethod sem-phase ((sem semaphore))
  "Číslo fáze semaforu"
  (slot-value sem 'phase))

(defmethod set-sem-phase ((sem semaphore) phase-no)
  "Nastavení fáze semaforu"
  (set-light-phase (items (slot-value sem 'lights))
                   (semaphore-phase-desc (light-count sem) phase-no))
  (setf (slot-value sem 'phase) phase-no)
  sem)

(defmethod next-phase ((sem semaphore))
  "Přechod na další fázi semaforu"
  (set-sem-phase sem (+ (sem-phase sem) 1)))

(defmethod initialize-instance ((sem semaphore) &key)
  (call-next-method)
  (set-items sem (list (slot-value sem 'lights)
                       (slot-value sem 'box)))
  (set-light-count sem 2))

#|
(setf w (make-instance 'window))
(setf sem (make-instance 'semaphore))
(set-shape w sem)
(redraw w)
(next-phase sem)
(set-light-count sem 3)
(set-light-count sem 2)
;;Bude fungovat jen po přidání definice semaforu o čtyřech světlech:
(set-light-count sem 4)
|#