;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Pøíklad k 7. èásti: semaphore-light.lisp
;;;;

#|
Tøída light je potomkem tøídy circle s následující pøidanou funkèností:

Instance tøídy light pøedstavují kulaté svìtlo, které lze zapnout a vypnout.
K zapínání a vypínání slouí zprávy set-onp, set-on, set-off a toggle. 
Ke zjišování souèasného stavu zpráva onp. Barvu zapnutého a vypnutého svìtla
lze èíst a nastavovat zprávami on-color, off-color, set-on-color, 
set-off-color.

Projdìte si testovací kód na konci souboru.
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
;; Testy tøídy light (kadı vıraz vyhodnocujte zvláš tlaèítkem F8):

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