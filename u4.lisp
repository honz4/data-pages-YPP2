(change-directory "~/Documents/GitHub/data-pages-YPP2")
 ; vim: fenc=utf-8 syntax=lisp

(load "micro-graphics/load.lisp")
(load "07.lisp")

;======Digitální sedmisegmentový displej ======
;Úkolem je naprogramovat digitální displej, který bude složen z tzv. sedminsegmentovek. 
;Sedmisegmentovka obsahuje sedm políček, které je možné individuálně rozsvěcet a 
;zhasínat. Sedmisegmentovka bude schopná zozbrazovat čísla od 0 do 9, hodnota nil se 
;bude interpretovat jako všechny segmenty zhasnuty. 

;======Třída 7-segment======
;Vytvořte třídu 7-segment, která bude reprezentovat jednu sedmisegmentovku.
;Třída bude potomek třídy picture a bude mít tyto sloty: 
;=====value:=====
;Bude reprezentovat hodnotu, která se má zobrazit. 
;=====on-p:=====
;Bude indikovat, zda je sedmisegmentovka zapnutá nebo vypnutá
;(vypnutá sedmisegmentovka ale má stále hodnotu ve slotu value). 
(defclass 7-segment (picture)
  ((value :initform 0)
   (on-p  :initform nil)
   ))
;     _____
;     __0__
;  | |     | |
;  |1|     |2|
;  | |     | |
;     _____
;     __3__
;  | |     | |
;  |4|     |5|
;  | |     | |
;     _____
;     __6__
  ;;jsme potomkem picture, do .items nacpeme segmenty
(defmethod initialize-instance ((self 7-segment) &key)
(call-next-method)
(set-items self
  (append
(loop with pi2 = (/ pi 2)
   ;posunuti a pootoceni jednotlivych segmentu
   ;                   0   1   2   3   4   5   6
      for dx in '(     5   5  75   5   5  75   5)
      for dy in '(     5   5   5  75  75  75  145)
      for fi in (list  0 pi2 pi2   0 pi2 pi2   0)
  collect
  (let ((points (loop for x in '(10 20 70 80 70 20);souradnice segmentu
		      for y in '(10  0  0 10 20 20)
		  collect (set-x (set-y (make-instance 'point) y) x))))
	(move
        (rotate
	(set-items (set-filledp (set-color (make-instance 'polygon) :gray) t) points)
	 fi (car points))
         dx dy)
))
   ;podkladovy polygon:
   (list (set-items (set-filledp (set-color (make-instance 'polygon) :black) t)
     (loop for (x . y) in '((0 . 0) (100 . 0) (100 . 170) (0 . 170))
       collect (set-y (set-x (make-instance 'point) x) y))))
))
self
)

;cislice cisla, rekurze
(defun digits (n &optional (radix 10) (digits nil))
  (if (< n radix)
      (cons n digits)
      (multiple-value-bind (n d) (floor n radix)
         (digits n radix (cons d digits)))))

;======Metody: ======
;=====segments: =====
;Vrátí segmenty sedmisegmentovky (ta bude reprezentována osmi polygony,
;tj. sedmi segmenty a podkladovým polygonem). 
;honza: pouzijeme butlast (posledni podklad nas nezajima) nebo loop?
(defmethod segments ((self 7-segment))
  (butlast (items self))
)

;=====set-segments: =====
;Nastaví (tj. rozsvítí nebo zhasne) jednotlivé segmenty sedmisegmentovky.
;Vyberte si, jak budete hodnoty pro jednotlivé segmenty reprezentovat.
;Jedna možnost je reprezentace seznamem hodnot t a nil,
;kde každá hodnota odpovídá jednomu segmentu,
;t značí rozsvícený segment, nil zhasnutý segment.
;todo: pouzit segments(), blokovat on-p? 
(defmethod set-segments ((self 7-segment) segments)
  (loop for s in segments
        for poly in (segments self)
        do (set-color poly (if s :red :grey)))
  self
)


;=====set-on=====
;Zapne, resp. vypne sedmisegmentovku 
;honza: tj. manipulace s .on-p slotem?
(defmethod set-on ((self 7-segment))
  (let ((on (slot-value self 'on-p)))
    (unless on
          (setf on t)
          (set-segments self
              (case (slot-value self 'value)
              (0 '(t   t   t   t   t   t   t  ))
              (1 '(nil nil t   nil nil t   nil))
              (2 '(t   nil t   t   t   nil t  ))
              (3 '(t   nil t   t   nil t   t  ))
              (4 '(nil t   t   t   nil t   nil))
              (5 '(t   t   nil t   nil t   t  ))
              (6 '(t   t   nil t   t   t   t  ))
              (7 '(t   nil nil t   nil t   nil))
              (8 '(t   t   t   t   t   t   t  ))
              (9 '(t   t   t   t   nil t   t  )))))
  self)
)
;=====set-off=====
(defmethod set-off ((self 7-segment))
  (let ((on (slot-value self 'on-p)))
    (when on
          (setf on nil)
          (set-segments self '(nil nil nil nil nil nil nil)))
    self)
)

;=====set-value=====
;Nastaví hodnotu, přípustné hodnoty jsou 0-9 
;honza: nerozvecet/nezhasinat?
(defmethod set-value ((self 7-segment) value)
  (unless (typep value '(integer 0 9)) (error "hodnota neni v rozsahu 0-9!"))
  (setf (slot-value self 'value) value)
  self
)

#|test/demo
  (setf win (make-instance 'window))
  (setf 7s  (make-instance '7-segment))
  (set-shape win 7s)
 ;(set-segments 7s '(t t t nil nil t))
  (set-on (set-value 7s 9))
  (redraw win)
|#

;======Třída segment-display======
;Další částí úkolu je vytvořit displej složený se sedmisegmentovek.
;Displej bude reprezentován třídou segment-display,
;která bude potomkem třídy picture. Výchozí počet segmentů je šest. 
;
;Třída segment-display nebude mít žádné sloty (kromě zděděných, pochopitelně). 
(defclass segment-display (picture)
  ()
  )

;Metody: 
(defmethod initialize-instance ((self segment-display) &key)
  (call-next-method)
  (set-items self (loop for i from 0 below 6
                collect (move (make-instance '7-segment) (* i 100) 0)))
  self
)

;=====segment-count: =====
;Zjistí počet sedmisegmentovek displeje (počet sedmisegmentovek segmentů není třeba 
;ukládat do slotu, dá se dynamicky zjistit z items). 
(defmethod segment-count ((self segment-display))
  (length (items self))
)

;=====set-segment-count: =====
;Nastaví počet sedmisegmentovek a provede reset.
;udelame set-items a/nebo push/pop na items?
(defmethod set-segment-count ((self segment-display) n)
   (set-items self (loop for i from 0 below n
                    collect (move (make-instance '7-segment) (* i 100) 0)))
   (reset self)
)

;=====reset: =====
;Provede reset displeje, tj. nastaví každý sedmisegment na nil 
(defmethod reset ((self segment-display))
   (dolist (item (items self))
      (set-off item)
      )
)

;=====value: =====
;Vrátí seznam hodnot v sedmisegmentech.
;Nespecifikované sedmisegmenty budou mít hodnotu NIL.
;Poznámka: opět nemáme slot na hodnotu, kterou dynamicky zjistíme zhodnot jednotlivých sedmisegmentů. 
(defmethod value ((self segment-display))
  (loop for item in (items self)
     collect (slot-value item 'value))
)

;=====set-value: =====
;Nastaví hodnoty sedmisegmentů podle zadaného seznamu hodnot.
;Jednotlivé hodnoty seinterpretují zleva doprava,
;pokud je seznam hodnot kratší než je počet segmentů,
;pak sezadané hodnoty zarovnají doprava (například pro seznam hodnot '(1 2 3 4) budou u 
;displeje s šesti sedmisegmenty první dvě (zleva) sedmisegmentovky vypnuté
;dále budou následovat hodnoty 1, 2, 3, a 4.
;Při pokusu o nastavení většího počtu hodnot než displej obsahuje sedmisegmentovek, musí být signalizována chyba. 
(defmethod set-value ((self segment-display) values)
 (let ((count (segment-count self)))
   (when (> (length values) count) (error "maly display"))
   (loop for value in values
         for 7-seg in (items self)
         do (set-value 7-seg value))
   self)
)

;set-on, set-off: 
;Zapne, resp. vypne displej.
(defmethod set-on ((self segment-display))
   (dolist (item (items self)) (set-on item) )
   self
)

(defmethod set-off ((self segment-display))
   (dolist (item (items self)) (set-off item) )
   self
)

#|
 (setf w (make-instance 'window))
 (set-shape w (make-instance 'segment-display))

 (set-value (shape w) '(1 2 3 4)) 
 (redraw w) 

 (value (shape w)) ;=>(NIL NIL 1 2 3 4) 

 (set-segment-count (shape w) 4);zkratime 6->4 
 (redraw w) 

 (set-value (shape w) '(9 0)) 
 (redraw w) 

 (set-off (shape w)) 
 (redraw w) 

 (set-on (shape w)) 
 (redraw w) 
|#
