#!/usr/bin/racket
#lang racket
;4.Napište proceduru bin2dec, která převede seznam číslic 0 a 1 na dekadické číslo.
;Řešení založte na apply. hmm, jak na apply proc [arg ...] arglist? viz [[PAPR1:L6]]
;TODO: PAPR2 power-list memoizovat?
;======bindec_apply======
(define (bin2dec list01)
  (letrec ([power2-list (lambda (n)
                         (let iter ([rad 1] [powers '()] [n n])
                           (if (<= n 0)
                             powers
                             (iter (+ rad rad)
                                   (cons rad powers)
                                   (- n 1)))))]
           )
  (apply + (map * list01 (power2-list (length list01))))))
                

(define list01 '(1 1 0 0))
(build-list (length list01) (lambda (x) (let ([x (+ x 1)]) (+ x x))))

(define (power-list k n)
 (apply map *
;;(display
         (map
           (lambda (r)
             (build-list n (lambda (i)
                             (if (<= i r)
                               1
                               k))))
           (build-list n (lambda (i) i)))))
(power-list 10 10)
(power-list  2 10)
;hmm, diky cons ma iter verze větší mocniny 'vpredu' ;-) => nepotřebujeme reverse
(define (power-list_iter r n)
  (let iter ([rad 1] [powers '()] [n n])
    (if (<= n 0)
      powers
      (iter (* rad r)
            (cons rad powers)
            (- n 1)))))

(power-list_iter  2 10)
(power-list_iter  2 0)
(power-list_iter  2 4)


;======bin2dec_fold======
(define (bin2dec_fold list01)
  (car (foldr (lambda (d ctx)
                (let  ([sum (car ctx)]
                       [rad (cdr ctx)])
                  (cons (if (= d 1)
                          (+ sum rad)
                          sum)
                        (+ rad rad))))
              (cons 0 1)
              list01))
  )

;======bin2dec_iter======
(define (bin2dec_iter list01)
  (let iter ([sum 0]
             [rad 1]
             [digits (reverse list01)])
    (if (null? digits)
      sum
      (iter (if (= 1 (car digits)) (+ sum rad) sum)
            (+ rad rad)
            (cdr digits)))))

(bin2dec '(0 1 0 1 1))     ;=>11
(bin2dec '(0 1 1 0 1 1 1)) ;=>55
(bin2dec '(1 1 0 0))
(bin2dec_iter '(1 1 0 0))
(bin2dec_fold '(1 1 0 0)) ;=>12
