;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; K�d z cvi�en� PP2I, 28. �nora 2008
;;;;

(defvar *rooms*)
(setf *rooms* '((kitchen "Kuchy�" (entrance toilet))
                (toilet "WC" (entrance kitchen))
                (cellar "sklep" (entrance))
                (entrance "p�eds��" (toilet kitchen cellar))))

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