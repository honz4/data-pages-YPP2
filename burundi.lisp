(change-directory "~/Documents/GitHub/data-pages-YPP2")
(load "micro-graphics/load.lisp")

;spocita n bodu na kruznici o polomeru r, phi0 = pi/2?
(defun x.y-body-na-kruznici (n r &optional (phi0 0))
  (loop with phi = phi0 and krok = (/ (* 2 pi) n)
        for i from 0 below n
        collect (cons (* r (cos phi)) (* r (sin phi)))
        do (incf phi krok)
        )
  )

(defun x.y-move (x.y x0 y0)
  (loop for (x . y) in x.y
	collect (cons (+ x x0) (+ y y0))))

;http://stackoverflow.com/questions/11905723/common-lisp-whats-the-best-way-to-loop-through-consecutive-pairs-in-a-list
;xy list je sudy seznam x y ... pro polynom
(defun x.y->xy (x.y)
  (loop for (x . y) in x.y
	append (list x y)))

(defun x.y-hvezda (n r phi0)
  (loop with phi = phi0
        with krok = (/ (* 2 pi) n)
        with phi2 = (+ phi (/ krok 2)) and r2 = (/ r 2)
        for i from 0 below n
        append (list (cons (* r  (cos phi )) (* r  (sin phi )))
                     (cons (* r2 (cos phi2)) (* r2 (sin phi2))))
        do (incf phi krok) (incf phi2 krok)
        )
  )

(defun set-params (win &key (foreground :black fg-p)
                            (background :white bg-p)
                            (thickness 1 t-p)
                            (filledp NIL f-p)
                            (closedp nil c-p))
 (when fg-p (mg:set-param win :foreground foreground))
 (when bg-p (mg:set-param win :background background))
 (when t-p (mg:set-param  win :thickness thickness))  
 (when f-p (mg:set-param  win :filledp filledp))
 (when c-p (mg:set-param  win :closedp closedp))
)

(defun burundi (&key (h 300) (w 400))
(let (win (h/2 (/ h 2)) (w/2 (/ w 2)))
  (setf win (mg:display-window :height h :width w))
  (mg:set-param win :background :red)
  (mg:clear win)

  (set-params win :foreground :green :closedp t :filledp t)
  (mg:draw-polygon win (list 0 0  w h  w 0  0 h))

  (set-params win :foreground :white :closedp nil :filledp nil :thickness (/ h 6))
  (mg:draw-polygon win (list 0 0 w h))
  (mg:draw-polygon win (list w 0 0 h))

  (mg:set-param win :filledp t)
  (mg:draw-circle win w/2 h/2 (/ h 3))

  (loop with x.y-hvezda = (x.y-hvezda 6 (/ h 10) (/ pi 6))
	for (x . y) in (x.y-move (x.y-body-na-kruznici 3 (/ h 6) (- (/ pi 2))) w/2 h/2)
	do (let ((xy-hvezda (x.y->xy (x.y-move x.y-hvezda x y))))
	     (set-params win :foreground :red :filledp T :closedp T)
	     (mg:draw-polygon win xy-hvezda)
	     (set-params win :foreground :green :filledp NIL  :thickness 4)
	     (mg:draw-polygon win xy-hvezda))
   )
 win
)
)

