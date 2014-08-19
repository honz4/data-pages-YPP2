;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Kód z cvièení PP2I, 28. února 2008
;;;;

(defvar *rooms*)
(setf *rooms* '((kitchen "Kuchyò" (entrance toilet))
                (toilet "WC" (entrance kitchen))
                (cellar "sklep" (entrance))
                (entrance "pøedsíò" (toilet kitchen cellar))))

(defvar *where*)
(setf *where* 'entrance)

(defun room-name (room)
  (first room))

(defun room-neighbors (room)
  (third room))

(defun has-name-p (room name)
  (eql name (room-name room)))

(defun find-room (room-name)
  (find-if (lambda (room)
	     (has-name-p room room-name))
	   (room-neighbors *where*)))

(defun can-move-p (room-name)
  (find room-name
        (room-neighbors (find-room *where*))))