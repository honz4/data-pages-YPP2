(define make-point
  
   (lambda ()

      (letrec ((x ())
               (y ())
               (get-x (lambda () x))
               (get-y (lambda () y))
               (set-x (lambda (val) (set! x val)))
               (set-y (lambda (val) (set! y val))))

          (lambda (msg . args)
      
            (cond ((equal? msg 'x) (get-x))
                  ((equal? msg 'y) (get-y))
                  ((equal? msg 'set-x) (apply set-x args))
                  ((equal? msg 'set-y) (apply set-y args)))))))

(define send-message (lambda (obj msg . args)
                       (apply obj msg args)))

;;===================================================

(define-macro class
  (lambda (slots methods)
       
    `(lambda ()
        (let ,slots
    (letrec ,methods
           (lambda (msg . args)
             (cond ,@(map (lambda (method)
                     `((equal? msg ',(car method)) (apply ,(car method) args)))
                   methods))))))))


(define point (class 
                  ;sloty
                  ((x 0) (y 0))
                  ;metody
                  ((get-x (lambda (this) x))
                   (get-y (lambda (this) y))
                   (set-x (lambda (this val) (set! x val)))
                   (set-y (lambda (this val) (set! y val))))))
    
(define send-message (lambda (obj msg . args)
                       (apply obj msg obj args)))
    
    

(define bod1 (point))
(send-message bod1 'get-x)
(send-message bod1 'set-x 10)
(send-message bod1 'get-x)

;;===================================================


(define-macro class
  (lambda (super slots methods)
       
    `(lambda ()
        (let ((super ,(if super `(,super) ()))
              ,@slots)
    (letrec ,methods
           (lambda (msg this . args)
             (cond ,@(map (lambda (method)
                     `((equal? msg ',(car method)) (apply ,(car method) this args)))
                   methods)
                   (else (apply super msg this args))
                   )))))))


(define point (class 
                  ;predek
                  #f
                  ;sloty
                  ((x 0) (y 0))
                  ;metody
                  ((get-x (lambda (this) x))
                   (get-y (lambda (this) y))
                   (set-x (lambda (this val) (set! x val)))
                   (set-y (lambda (this val) (set! y val))))))
    
(define send-message (lambda (obj msg . args)
                       (apply obj msg obj args)))
    
(define bod1 (point))
(send-message bod1 'get-x)
(send-message bod1 'set-x 10)
(send-message bod1 'get-x)


(define colored-point (class 
                  ;predek
                  point
                  ;sloty
                  ((color 'black))
                  ;metody
                  ((get-color (lambda (this) color))
                   (set-color (lambda (this val) (set! color val))))))


;;===================================================

(define bod2 (colored-point))
(send-message bod2 'get-x)
(send-message bod2 'set-x 10)
(send-message bod2 'get-x)
(send-message bod2 'get-color)
(send-message bod2 'set-color 'pink)
(send-message bod2 'get-color)
