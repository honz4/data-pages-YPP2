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
      for dx in '(     0   0  80   0   0  80   0)
      for dy in '(     0   0   0  80  80  80  160)
      for fi in (list  0 pi2 pi2   0 pi2 pi2   0)
  collect
  (let ((points (loop for x in '(10 20 80 90 80 20);souradnice segmentu
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
     (loop for (x . y) in '((0 . 0) (100 . 0) (100 . 180) (0 . 180))
       collect (set-y (set-x (make-instance 'point) x) y))))
))
self
)

;======Metody: ======
;=====segments: =====
;Vrátí segmenty sedmisegmentovky (ta bude reprezentována osmi polygony,
;tj. sedmi segmenty a podkladovým polygonem). 
(defmethod segments ((self 7-segment))
  self
)

;=====set-segments: =====
;Nastaví (tj. rozsvítí nebo zhasne) jednotlivé segmenty sedmisegmentovky.
;Vyberte si, jak budete hodnoty pro jednotlivé segmenty reprezentovat.
;Jedna možnost je reprezentace seznamem hodnot t a nil,
;kde každá hodnota odpovídá jednomu segmentu,
;t značí rozsvícený segment, nil zhasnutý segment.
;todo: pouzit segments(), blokovat on-p? 
(defmethod set-segments ((self 7-segment) segments)
  (loop for n from 1 to 7
        for s in segments
        for item in (items self)
   do (set-color item (if s :red :grey)))
  self
)


;=====set-on=====
;Zapne, resp. vypne sedmisegmentovku 
;honza: tj. manipulace s .on-p slotem?
(defmethod set-on ((self 7-segment))
  self
)
;=====set-off=====
(defmethod set-off ((self 7-segment))
  self
)

;=====set-value=====
;Nastaví hodnotu, přípustné hodnoty jsou 0-9 
(defmethod set-value ((self 7-segment) value)
)

#|test/demo
(let ((win (make-instance 'window)) pic)
  (set-shape win (make-instance '7-segment))
  (set-segments (shape win) '(t t t nil nil t))
  (redraw win)
)
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

#|
;=====segment-count: =====
;Zjistí počet sedmisegmentovek displeje (počet sedmisegmentovek segmentů není třeba 
;ukládat do slotu, dá se dynamicky zjistit z items). 
(defmethod segment-count ((self segment-display))
  (length (items self))
)

;=====set-segment-count: =====
;Nastaví počet sedmisegmentovek a provede reset. 
(defmethod set-segment-count ((self segment-display))
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
     collect (value item))
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
   self
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
|#

;CL-USER 51 > (set-value (shape w) '(1 2 3 4)) 
;#<SEGMENT-DISPLAY 200D922F> 

;CL-USER 52 > (redraw w) 
;#<WINDOW 201088E3> 

;CL-USER 53 > (value (shape w)) 
;(NIL NIL 1 2 3 4) 

;CL-USER 54 > (set-segment-count (shape w) 4) 
;#<SEGMENT-DISPLAY 200D922F> 


;CL-USER 55 > (redraw w) 
;#<WINDOW 201088E3> 

;CL-USER 57 > (set-value (shape w) '(9 0)) 
;#<SEGMENT-DISPLAY 200D922F> 

;CL-USER 58 > (redraw w) 
;#<WINDOW 210C66A7> 

;CL-USER 59 > (set-off (shape w)) 
;#<SEGMENT-DISPLAY 200D922F> 

;CL-USER 60 > (redraw w) 
;#<WINDOW 210C66A7> 

;CL-USER 61 > (set-on (shape w)) 
;#<SEGMENT-DISPLAY 200D922F> 

;CL-USER 62 > (redraw w) 
;#<WINDOW 210C66A7> 
