;; -*- mode: lisp; encoding: utf-8; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; 09-examples.lisp - příklady použití grafické knihovny clmg
;;;;
;;;;
;;;; Vykoušejte si testy na konci souboru.
;;;;
;;;;

(defun random-color () 
  (let ((colors (color:get-all-color-names))) 
    (nth (random (length colors)) colors)))

;;;
;;; Flags
;;;

;;Vlajka 2:3 - podklad
(defclass flag-rectangle (picture) ())

(defun flag-rect-points ()
  (mapcar (lambda (coords)
            (apply #'move (make-instance 'point) coords))
          '((20 20) (200 20) (200 140) (20 140))))

(defmethod initialize-instance ((fr flag-rectangle) &key)
  (call-next-method)
  (let ((rect1 (make-instance 'polygon))
        (rect2 (make-instance 'polygon)))
    (set-items rect1 (flag-rect-points))
    (set-items rect2 (flag-rect-points))
    (set-filledp rect2 t)
    (set-color rect2 :white)
    (set-items fr (list rect1 rect2)))
  fr)

(defmethod set-color ((fr flag-rectangle) value)
  (call-next-method)
  (set-color (second (items fr)) value))

#|
(setf rect (make-instance 'flag-rectangle))
(setf w (make-instance 'window))
(set-shape w rect)
(set-color rect :yellow)
|#


(defun jp-flag-items ()
    (list (set-color 
           (set-radius 
            (set-filledp
             (move (make-instance 'circle) 110 80)
             t) 
            36)
           (color:make-rgb (/ #xce 255.0)
                           (/ #x11 255.0)
                           (/ #x26 255.0)))
          (make-instance 'flag-rectangle)))

(defclass jp-flag (picture)
  ())

(defmethod initialize-instance ((flag jp-flag) &key)
  (call-next-method)
  (set-items flag (jp-flag-items)))

(defun make-point (x y)
  (move (make-instance 'point) x y))

(defun make-polygon (coord-list filledp closedp color)
  (set-closedp (set-filledp
                (set-color
                 (set-items (make-instance 'polygon)
                            (mapcar (lambda (pair)
                                      (apply #'make-point pair))
                                    coord-list))
                 color)
                filledp)
               closedp))

(defun cz-flag-items ()
  (list (make-polygon '((20 20) (200 20) (200 140) (20 140)) nil t :black)
        (make-polygon '((20 20) (110 80) (20 140)) t t :blue)
        (make-polygon '((20 20) (200 20) (200 80) (110 80)) t t :white)
        (make-polygon '((110 80) (200 80) (200 140) (20 140)) t t :red)))

(defclass cz-flag (picture)
  ())

(defmethod initialize-instance ((flag cz-flag) &key)
  (call-next-method)
  (set-items flag (cz-flag-items)))

;;Peticipa hvezda
(defclass star (polygon) ())

(defun star-points ()
  (let ((result '())
        (origin (make-instance 'point)))
    (dotimes (i 5)
      (let ((pt1 (make-instance 'point))
            (pt2 (make-instance 'point)))
        (move pt1 0 -1)
        (rotate pt1 (* pi 2/5 i) origin)
        (move pt2 0 (- 1 0.618))
        (rotate pt2 (* pi 2/5 (+ i 2)) origin)
        (setf result (cons pt1 (cons pt2 result)))))
    result))

(defmethod initialize-instance ((star star) &key)
  (call-next-method)
  (set-items star (star-points))
  (set-closedp star t))

#|
(setf star (make-instance 'star))
(setf w (make-instance 'window))
(set-shape w star)
(scale star 100 (make-instance 'point))
(move star 110 110)
(set-filledp star t)
(set-color star :blue)
|#

(defclass eu-stars (picture)
  ())

(defun eu-star (index center)
  (let ((position (make-instance 'point))
        (result (make-instance 'star)))
    (set-filledp result t)
    (move position (x center) (y center))
    (move position 0 -40)
    (rotate position (* index (/ pi 6)) center)
    (move result (x position) (y position))
    (scale result (* 1/18 120) position)
    result))

(defmethod initialize-instance ((stars eu-stars) &key)
  (call-next-method)
  (let ((center (move (make-instance 'point) 110 80))
        (items '()))
    (dotimes (i 12)
      (setf items (cons (eu-star i center) items)))
    (set-items stars items)
    (set-propagate-color-p stars t)
    stars))
    
#|
(setf stars (make-instance 'eu-stars))
(setf w (make-instance 'window))
(set-shape w stars)
(set-color stars :yellow)
|#

(defclass eu-flag (picture)
  ((black-and-white-p :initform t)))

(defun eu-flag-background (f)
  (second (items f)))

(defun eu-flag-stars (f)
  (first (items f)))

(defmethod black-and-white-p ((f eu-flag))
  (slot-value f 'black-and-white-p))

(defmethod set-black-and-white-p ((f eu-flag) value)
  (setf (slot-value f 'black-and-white-p) value)
  (cond (value (set-color (eu-flag-background f) :white)
               (set-color (eu-flag-stars f) :black))
        (t (set-color (eu-flag-background f) 
                      (color:make-rgb 0.0 (/ 51.0 255) (/ 153.0 255)))
           (set-color (eu-flag-stars f) (color:make-rgb 1.0 (/ 204.0 255) 0.0)))))

(defmethod initialize-instance ((flag eu-flag) &key)
  (call-next-method)
  (set-items flag (list (make-instance 'eu-stars)
                        (make-instance 'flag-rectangle)))
  (set-black-and-white-p flag nil)
  flag)

(defclass eu-flag-window (window) ())

(defmethod initialize-instance ((w eu-flag-window) &key)
  (call-next-method)
  (set-shape w (make-instance 'eu-flag))
  w)

(defmethod window-mouse-down ((w eu-flag-window) button position)
  (set-black-and-white-p (shape w) 
			 (not (black-and-white-p (shape w)))))

#|
(make-instance 'eu-flag-window)
|#

;;;
;;; poly-window
;;;

(defclass poly-window (window)
  ())

(defmethod initialize-instance ((w poly-window) &key)
  (call-next-method)
  (set-shape w (set-closedp (make-instance 'polygon) nil))
  w)

(defmethod window-mouse-down ((w poly-window) button point)
  (call-next-method)
  (set-items (shape w)
             (cons point (items (shape w)))))

;;;
;;; circ-window
;;;

(defclass circ-window (window)
  ())

(defmethod initialize-instance ((w circ-window) &key)
  (call-next-method)
  (set-shape w (set-radius 
                (set-filledp (make-instance 'circle) t)
                30))
  w)

(defun move-to (circle point)
  (move circle
        (- (x point) (x (center circle)))
        (- (y point) (y (center circle)))))

(defmethod window-mouse-down ((w circ-window) button point)
  (call-next-method)
  (move-to (shape w) point)
  w)

;;;
;;; click-circle
;;;

(defclass click-circle (circle) ())

(defmethod mouse-down ((circ click-circle) button position) 
  (set-color circ (random-color)) 
  ;; Oznámení o kliknutí delegátovi (umožňuje použít klikací kolečko
  ;; současně k jiným účelům):
  (call-next-method))
          
;;;
;;; circle-with-arrow - ukázka použití ev-mouse-down v obrázku
;;;

(defun make-arr (color)
  (make-polygon '((0 -30) (0 -15) (30 -15) (30 15) (0 15) (0 30) (-30 0))
                t
                t
                color))

(defun cwa1-items ()
  (let ((arr1 (make-arr :blue))
        (circ (make-instance 'click-circle)))
    (rotate arr1 (/ Pi 2) (make-instance 'point))
    (move arr1 60 150)
    (set-radius circ 40)
    (move circ 60 60)
    (set-filledp circ t)
    (list circ arr1)))

(defclass circle-with-arrow (picture)
  ())

(defmethod initialize-instance ((pic circle-with-arrow) &key)
  (call-next-method)
  (set-items pic (cwa1-items))
  pic)

(defmethod cwa-circle ((p circle-with-arrow))
  (first (items p)))

(defmethod cwa-arrow ((p circle-with-arrow))
  (second (items p)))

(defmethod ev-mouse-down ((p circle-with-arrow) sender origin button position)
  (when (eql sender (cwa-arrow p))
    (move (cwa-circle p) 0 -10))
  (call-next-method))

;;;
;;; circle-with-arrows - ukázka použití více ev-mouse-down v obrázku - dva způsoby
;;;

(defun cwa2-items (arr-color)
  (let ((arr1 (make-arr arr-color))
        (arr2 (make-arr arr-color))
        (circ (make-instance 'click-circle)))
    (move arr1 70 150)
    (rotate arr2 pi (make-instance 'point))
    (move arr2 250 150)
    (set-radius circ 50)
    (move circ 160 60)
    (set-filledp circ t)
    (list circ arr1 arr2)))

;;; První způsob:

(defclass circle-with-arrows-1 (picture)
  ())

(defmethod initialize-instance ((cwa circle-with-arrows-1) &key)
  (call-next-method)
  (set-items cwa (cwa2-items :dark-blue))
  cwa)

(defmethod cwa1-circle ((cwa circle-with-arrows-1))
  (first (items cwa)))

(defmethod cwa1-left-arr ((cwa circle-with-arrows-1))
  (second (items cwa)))

(defmethod cwa1-right-arr ((cwa circle-with-arrows-1))
  (third (items cwa)))

(defmethod ev-mouse-down ((cwa circle-with-arrows-1) sender origin button position)
  (call-next-method)
  (when (eql button :left)
    (cond ((eql sender (cwa1-left-arr cwa)) (move (cwa1-circle cwa) -10 0))
          ((eql sender (cwa1-right-arr cwa)) (move (cwa1-circle cwa) 10 0)))))

;;; Druhý způsob, vhodný do složitějších situací:

(defclass circle-with-arrows-2 (picture)
  ())

(defmethod cwa2-circle ((cwa circle-with-arrows-2))
  (first (items cwa)))

(defmethod cwa2-left-arr ((cwa circle-with-arrows-2))
  (second (items cwa)))

(defmethod cwa2-right-arr ((cwa circle-with-arrows-2))
  (third (items cwa)))

(defmethod initialize-instance ((cwa circle-with-arrows-2) &key)
  (let ((items (cwa2-items :dark-blue)))
    (call-next-method)
    (set-items cwa items)
    (add-event (cwa2-left-arr cwa) '(ev-mouse-down ev-mouse-down-1))
    (add-event (cwa2-right-arr cwa) '(ev-mouse-down ev-mouse-down-2)))
  cwa)
    
(defmethod ev-mouse-down-1 ((cwa circle-with-arrows-2) sender origin button position)
  (when (eql button :left)
    (move (cwa2-circle cwa) -10 0))
  (send-event cwa 'ev-mouse-down origin button position))

(defmethod ev-mouse-down-2 ((cwa circle-with-arrows-2) sender origin button position)
  (when (eql button :left)
    (move (cwa2-circle cwa) 10 0))
  (send-event cwa 'ev-mouse-down origin button position))

#|
(setf w (make-instance 'window))
(set-shape w (make-instance 'jp-flag))
(set-background w :skyblue)
(set-shape w (make-instance 'cz-flag))

;; Zkuste do okna klikat:
(make-instance 'eu-flag-window)

;; Do následujících oken zkuste klikat:
(make-instance 'poly-window)
(make-instance 'circ-window)

;;; Další příklady již nevyžadují definici potomka třídy window
;;; (větší flexibilita, znovupoužitelnost)

;;; stačí udělat instanci třídy window, přesto bude na klikání správně reagovat
(setf w (make-instance 'window))
(setf circle (make-instance 'click-circle))
(set-radius circle 20)
(move circle 40 40)
(set-filledp circle t)
(set-shape w circle)

;;; a to i když do něj umístíme klikacích koleček více jako součást obrázku:
(setf pic (make-instance 'picture))
(let ((circles '()))
  (dotimes (i 3)
    (let ((circle (make-instance 'click-circle)))
      (set-radius circle 20)
      (set-filledp circle t)
      (move circle 40 (+ 40 (* i 50)))
      (setf circles (cons circle circles))))
  (set-items pic circles))

(setf w (make-instance 'window))
(set-shape w pic)

;;; Opět stačí třída window (a na kolečko lze klikat):
(setf w (make-instance 'window))
(set-shape w (make-instance 'circle-with-arrow))

(setf w (make-instance 'window))
(set-shape w (make-instance 'circle-with-arrows-1))
;; Chování je stejné, implementace jiná:
(set-shape w (make-instance 'circle-with-arrows-2))

;;; A to i když bude obrázků víc (roztáhněte si okno):
(setf w (make-instance 'window))
(let ((pic1 (make-instance 'circle-with-arrows-1))
      (pic2 (make-instance 'circle-with-arrows-2))
      (bigpic (make-instance 'picture)))
  (scale pic1 2/3 (make-instance 'point))
  (scale pic2 2/3 (make-instance 'point))
  (move pic2 0 120)
  (set-items bigpic (list pic1 pic2))
  (set-shape w bigpic))

|#

