(change-directory "~/Documents/GitHub/data-pages-YPP2")
 ; vim: fenc=utf-8 syntax=lisp

(load "u4.lisp")


(let (w)
 (setf w (make-instance 'window))
 (set-shape w (make-instance 'segment-display))
 (set-value (shape w) '(1 2 3 4)) 
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

)
