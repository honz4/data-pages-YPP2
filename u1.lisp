#|
exec sbcl --noinform --load $0 --end-toplevel-options "$@"
|#
;viz [[4.scm|Úkoly pro lekci č.4]]
;Procedury musí mít stejné názvy a brát argumenty v pořadí, které odpovídá uvedeným
;testovacím výrazům. E-maily budou strojově zpracovávány.

;======make-city======
;1.Vytvořte abstrakční bariéry pro níže uvedenou reprezentaci měst
;konstruktor make-city selektory name pro název, region, habitants pro počet obyvatel
;<code scheme>
;(define (make-city name region habitants) (list name region habitants))
; ;v Lispu mu(ze|sí)me pouzit neco takoveho: ;(setf (symbol-function 'alias) #'fname)
;(define name car)
;(define region cadr)
;(define habitants caddr)
;</code>
(defun make-city (name region habitants) (list name region habitants))
(setf (symbol-function 'name) #'car)
(setf (symbol-function 'region) #'cadr)
 ;(setf (symbol-function 'habitants) #'caddr)
(defun habitants (city) (caddr city))

(defvar *Zelec* (make-city 'Zelec 'Stredni-Morava 650))
(format t "name: ~A region: ~A habitants: ~D~%" (name *Zelec*) (region *Zelec*) (habitants *Zelec*))

(defvar *cities*
(list *Zelec*
(make-city 'Olomouc 'Stredni-Morava 100000)
(make-city 'Prerov 'Stredni-Morava 40000)
(make-city 'Prostejov 'Stredni-Morava 30000)
(make-city 'Brno 'Jizni-Morava 300000)
(make-city 'Ostrava 'Severni-Morava 350000)
(make-city 'Praha 'Stredni-Cechy 1200000)
(make-city 'Hradec-Kralove 'Vychodni-Cechy 100000)))
(format t "~A~%" *cities*)
;Hmm, Lisp: symbol 'Praha bude VELKYMI PISMENY

;======resctiction======
;2.Napište bez použití filter proceduru restriction, která bude provádět operaci
;restrikce – t.j. filtrování řádků podle zadané procedury.
;<code scheme>
; ;scheme:
;(define (restriction f cities)
;  (foldr (lambda (c a)
;           (if (f c)
;             (cons c a) ;#pridam
;             a
;             ))
;         '()
;         cities))
;</code>
 ;reduce se chova jako foldl/genuine? callback ma argumenty (acc el) ???
(defun restriction (f cities)
  (reduce (lambda (a c)
            (if (funcall f c)
              (cons c a)
              a
              ))
          cities :initial-value nil))

 ;(testovani restriction)
(restriction (lambda (c) (equalp (region c) 'Stredni-Morava)) *cities*)
 ;=>((Olomouc Stredni-Morava 100000) (Prerov Stredni-Morava 40000) (Prostejov Stredni-Morava 30000))
(> (habitants *Zelec*) 100000)

(restriction (lambda (c) (> (habitants c) 100000)) *cities*)
 ;=>
 ;((Brno Jizni-Morava 300000)
 ;(Ostrava Severni-Morava 350000)
 ;(Praha Stredni-Cechy 1200000))

(restriction (lambda (c) (< (habitants c) 1000)) *cities*)


;======aggregate======
;3.Napište proceduru aggregate, která bude pomocí procedury agregovat hodnoty v daném sloupci.
(defvar *morava-cities*
  (restriction (lambda (c) (or (equalp (region c) 'Jizni-Morava)
                               (equalp (region c) 'Stredni-Morava)
                               (equalp (region c) 'Severni-Morava)))
               *cities*))
(format t "~A~%" *morava-cities*)

(defun aggregate (f s cities)
  (reduce (lambda (a e) (funcall f a e)) (mapcar s cities) ))

(format t "aggregate + ~D~%" (aggregate #'+   #'habitants *morava-cities*))
 ;=>820650
(aggregate #'min #'habitants *morava-cities*)
 ;=>30000

;======bin2dec======
;4.Napište proceduru bin2dec, která převede seznam číslic 0 a 1 na dekadické číslo.
;Řešení založte na apply. hmm, jak na apply proc [arg ...] arglist?
;  * viz [[bin2dec.scm]]
;  * ekvivalent **apply** v lispu je zase apply [[clhs>Body/f_apply.htm]]
(defun bin2dec (list01)
  (apply #'+ (mapcar #'*
                (reverse list01)
                (loop with rad = 1
                      for i from 1 to (length list01)
                      collect (prog1 rad (incf rad rad))))))

#|
(defun power2-list (n)
  (loop with rad = 1
        for i from 1 to n
        collect (prog1 rad (incf rad rad))
  ))
(power2-list 5)
(print (power2-list 5))
|#

(bin2dec '(0 1 0 1 1)) ;=>11
(bin2dec '(0 1 1 0 1 1 1)) ;=>55
(bin2dec '(1 1 0 0)) ;=>12

;5.Napište proceduru euclid, která zjistí největšího společného dělitele dvou zadaných
;čísel pomocí Euklidova algoritmu pro hledání NSD. Popis viz. http://cs.wikipedia.org/wiki/Euklidův_algoritmus. Řešení založte na rekurzi.
(defun euclid (a b)
  (/ a b)
  )
(euclid 12 4) ;=>4
(euclid 12 5) ;=>1
(euclid 12 6) ;=>6
(euclid 666 36) ;=>18

;6.Napište proceduru leibniz-pi, která provede odhad čísla pí pomocí Leibnizovy řady,
;popis viz http://en.wikipedia.org/wiki/Leibniz_formula_for_pi. Řešení založte na rekurzi.
(defun leibniz-pi (n)
  (if (> n 10) 3.1415926 3.14)
  )
(leibniz-pi 10) ;=>3.232315809405594
(leibniz-pi 100) ;=>3.1514934010709914
(leibniz-pi 2000) ;=>3.1420924036835256
(leibniz-pi 20000) ;=>3.1416426510898874
(leibniz-pi 200000) ;=>3.141597653564762

;Poznámka: kvůli zaokrouhlovacím chybám vám mohou vycházet mírně odlišné výsledky odhadu čísla pí.
;7.Napište proceduru perfect?, která zjistí zda zadané číslo je tzv. dokonalým číslem, to
;jest takovým, které je součtem všech svých dělitelů kromě sebe sama. Například 6 je
;dokonalé číslo, protože jeho děliteli jsou 3, 2 a 1, přičemž platí, že 3+2+1=6.
(defun perfect? (n)
  (if (> n 0) t nil)
  )
(perfect?  6) ;=>#t
(perfect?  28) ;=>#t
(perfect?  29) ;=>#f
(perfect?  496) ;=>#t

; vim: syntax=lisp
