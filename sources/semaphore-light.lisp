;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; P��klad k 7. ��sti: semaphore-light.lisp
;;;;

#|
T��da light je potomkem t��dy circle s n�sleduj�c� p�idanou funk�nost�:

Instance t��dy light p�edstavuj� kulat� sv�tlo, kter� lze zapnout a vypnout.
K zap�n�n� a vyp�n�n� slou�� zpr�vy set-onp, set-on, set-off a toggle. 
Ke zji��ov�n� sou�asn�ho stavu zpr�va onp. Barvu zapnut�ho a vypnut�ho sv�tla
lze ��st a nastavovat zpr�vami on-color, off-color, set-on-color, 
set-off-color.

Projd�te si testovac� k�d na konci souboru.
|#

(defclass light (circle)
  ((onp :initform t)
   (on-color :initform :red)
   (off-color :initform :grey)))

(defmethod onp ((l light))
  (slot-value l 'onp))

(defmethod ensure-color ((l light))
  (set-color l 
             (slot-value l (if (onp l)
                               'on-color
                             'off-color))))

(defmethod initialize-instance ((l light) &key)
  (call-next-method)
  (set-filledp l t)
  (ensure-color l))

(defmethod set-onp ((l light) value)
  (setf (slot-value l 'onp) value)
  (ensure-color l))

(defmethod set-on ((l light))
  (set-onp l t))

(defmethod set-off ((l light))
  (set-onp l nil))

(defmethod toggle ((l light))
  (set-onp l (not (onp l))))

(defmethod on-color ((l light))
  (slot-value l 'on-color))

(defmethod set-on-color ((l light) value)
  (setf (slot-value l 'on-color) value)
  (ensure-color l))

(defmethod off-color ((l light))
  (slot-value l 'off-color))

(defmethod set-off-color ((l light) value)
  (setf (slot-value l 'off-color) value)
  (ensure-color l))

#|
;; Testy t��dy light (ka�d� v�raz vyhodnocujte zvl᚝ tla��tkem F8):

(setf w (make-instance 'window))

(setf light (move (set-radius (make-instance 'light)
                              50)
                  100 100))

(set-shape w light)
(redraw w)

(toggle light)
(redraw w)

(set-on-color light :green)
(redraw w)

|#