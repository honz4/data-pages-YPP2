;Zadání: Naprogramujte třídu -- checklist.
;
;Požadavky:
;  * bude mít seznam položek, které je možno kliknutim zaškrtnout a odskrtnout
;  * zaškrtnutí a odskrtnoutí položky vyvolá událost (viz [[demo.lisp]])
;  * použijte kód [..:09.lisp]], needitujte ho
;  * na zobrazení textu použijte [[..:text-shape.lisp]]
;  * kód [[demo.lisp]] by měl býl kompatibilní s vašim kódem
;  * nemusíte řešit transformace rotate a scale
;  * move by ale mělo být funkční.
;  * žádné warningy
;
;Odevzdání:
;  * řešení buďto pošlete na můj e-mail nebo doneste na notebooku či flashdisku.
;  * nemusíte k řešení vyrábět jakoukoli dokumentaci.
;(change-directory "~/Documents/GitHub/data-pages-YPP2/zkouska1")
(load "../micro-graphics/load.lisp")
(load "../09.lisp")
(load "../text-shape.lisp")

(defun mp (x y) (set-y (set-x (make-instance 'point) x) y))

(defclass checked-text (picture) ;(compound-shape);todo
  ((checked :initform nil)
   ))

(defmethod initialize-instance ((self checked-text) &key)
  (call-next-method)
  (set-items self (list
                    ;first checked fajfka:
                    (set-closedp (set-color
                    (set-items (make-instance 'polygon) (list (mp 0 15) (mp 15 30) (mp 30 0))) :red) nil)
                    ;second
                    (move (make-instance 'text-shape) 40 20)
                    (set-filledp (set-color
                    (set-items (make-instance 'polygon) (list (mp 0 0) (mp 0 30) (mp 30 30) (mp 30 0))) :grey) t)
                    ))
  self)

(defmethod check-item ((self checked-text) item) t) ;todo: typep!
(defmethod     checked ((self checked-text))         (slot-value self 'checked))

;checked udalosti? pokresleni polynomu
(defmethod set-checked ((self checked-text) b)
  (setf (slot-value self 'checked) b)
  ;prebarvime fajfku
  (set-color (first (items self)) (if b :red :grey))
  self)

(defmethod        text ((self checked-text))
  (text (second (items self)))
  )

(defmethod    set-text ((self checked-text) text)
  (set-text (second (items self)) text)
  self)

#|
(setf w (make-instance 'window))
(set-shape w (make-instance 'checked-text))
(set-text (shape w) "Kuna nese nanuk")
(redraw w)
 |#


(defclass item-list (picture)
  ())
(defmethod initialize-instance ((self item-list) &key)
  (call-next-method);super
)


;(defmethod items ((self item-list))  )

(defmethod set-items ((self item-list) items)
  (loop with dy = 0
     for item in items
     do (move item 0 dy) (incf dy 40))
  (call-next-method)
 )

(defun make-checklist-item (text)
  (set-text (make-instance 'checked-text) text)
)
 
(load "demo.lisp")


 ; vim: syntax=lisp fenc=utf-8
