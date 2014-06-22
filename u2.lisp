#|
exec sbcl --noinform --load $0 --end-toplevel-options "$@"
|#
;[[clhs>Body/m_defcla.htm#defclass|defclass]]  funguje :type?
(defclass time-spec () (
  (min :type (integer 0))
  (sec :type (integer 0 59))
  ))

(defmethod   t-min ((self time-spec))
  (slot-value self 'min))

;kontrola: je value nezaporny integer? typespec, todo vyzkouset type-check
(defmethod set-min ((self time-spec) value)
  (unless (typep value '(integer 0)) (error "minuty: cele nezaporne cislo!"))
  (setf (slot-value self 'min) value)
  self)

(defmethod   t-sec ((self time-spec))
  (slot-value self 'sec))

;kontrola: je value integer 0-59?
(defmethod set-sec ((self time-spec) value)
  (unless (typep value '(integer 0 59)) (error "sekundy 0-59!"))
  (setf (slot-value self 'sec) value)
   self)

(defmethod     time-in-seconds ((self time-spec))
  (+ (* (slot-value self 'min) 60 ) (slot-value self 'sec)))

;kontrola: je value integer? [[clhs>Body/f_mod_r.htmBody/f_mod_r.htm|mod]]ulo
; funkce floor a multi/values!!!
(defmethod set-time-in-seconds ((self time-spec) value)
  (unless (typep value '(integer 0)) (error "sekundy zaporne!"))
  (multiple-value-bind (min sec) (floor value 60)
  (set-sec (set-min self min) sec)))

(defun make-time (min sec)
  (set-sec (set-min (make-instance 'time-spec) min) sec))

#|testiky
(defvar *x*)
(setf *x* (make-instance 'time-spec))
(set-min *x* 3)
(set-sec *x* 30)
(inspect *x*)
(time-in-seconds *x*)
(make-time 5 15)
;(inspect *)
|#

(defclass track () (
  name
  (len :type 'time-spec)
  ))

(defmethod name ((self track))
  (slot-value self 'name))

(defmethod set-name ((self track) value)
  (setf (slot-value self 'name) value)
  self)

(defmethod len ((self track))
  (slot-value self 'len))

(defmethod set-len ((self track) value)
  (unless (typep value 'time-spec) (error "delka stopy neni time-spec!"))
  (setf (slot-value self 'len) value)
  self)

;[[clhs>Body/22_c.htm|format]]
(defmethod print-track ((self track))
  (format t "~A~1,40T~2D:~2D~%"
          (name self)
          (t-min (len self))
          (t-sec (len self))))

(defun make-track (name-value len-value)
  (set-len (set-name (make-instance 'track) name-value) len-value))
#|
(defvar *track*)
(setf  *track* (make-track "Pharaon Dance" (make-time 3 20)))
(name  *track*)
(len   *track*)
(t-min (len *track*))
(t-sec (len *track*))
(print-track *track*)
(set-name *track* "New Name")
(name *track*)
 |#

;======album======
(defclass album () (
  artist
  title
  (tracks :type '(list track))
  year-of-release
  ))
(defmethod     artist ((self album))
  (slot-value self 'artist))

(defmethod set-artist ((self album) value)
  (setf (slot-value self 'artist) value)
  self)

(defmethod     title  ((self album))
  (slot-value self 'title ))

(defmethod set-title  ((self album) value)
  ;kontrola?
  (setf (slot-value self 'title) value)
  self)

(defmethod     tracks ((self album))
  (slot-value self 'tracks))

(defmethod set-tracks ((self album) value)
  (unless (every (lambda (e) (typep e 'track)) value) (error "neni seznam traku!"))
  (setf (slot-value self 'tracks) value)
  self)

(defmethod     year-of-release ((self album))
  (slot-value self 'year-of-release))

(defmethod set-year-of-release ((self album) value)
  (unless (typep value '(integer 1900)) (error "spatny rok vydani"))
  (setf (slot-value self 'year-of-release) value)
  self)

(defmethod track-count ((self album))
  (length (tracks self)))

(defmethod album-length ((self album))
  (let ((seconds 0))
    (dolist (track (tracks self))
      (incf seconds (time-in-seconds (len track)))
      )
    (set-time-in-seconds (make-time 0 0) seconds)
    ))

(defun make-album (artist title tracks yrelease)
  (let ((self (make-instance 'album)))
    (set-artist self artist)
    (set-title  self title)
    (set-tracks self tracks)
    (set-year-of-release self yrelease)
    self))

(defmethod print-album ((self album))
  (let ((total-length (album-length self)))
    (format t "~A by ~A, released ~D~%~%" (title self) (artist self) (year-of-release self))
    (dolist (track (tracks self)) (print-track track))
    (format t "Total length:~D:~2D~%" (t-min total-length) (t-sec total-length))
    ))

#|
 (defvar *album*
   (make-album "Miles Davis"
               "Bitches Brew"
               (list (make-track "Pharaoh's Dance" (make-time 20 05))
                     (make-track "Bitches Brew" (make-time 26 58))
                     (make-track "Spanish Key" (make-time 17 32))
                     (make-track "John McLaughlin" (make-time 4 22))
                     (make-track "Miles Runs the Voodoo Down" (make-time 14 01))
                     (make-track "Sanctuary" (make-time 10 56))
                     (make-track "Feio" (make-time 11 49))
                     )
               1969))
(track-count  *album*)
(tracks       *album*)
(album-length *album*)
(print-album  *album*)
 |#
