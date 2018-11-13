(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.

(define (cons-all first rests)
  (if
  	(null? rests) nil
  	(cons (cons first (car rests)) 
  		(cons-all first (cdr rests)))))

(define (zip pairs)
  (list (map car pairs) (map cadr pairs)))

;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17
  (define (numer s n)
	  (if (null? s) nil
	  	(cons (list n (car s)) 
            (numer (cdr s) (+ n 1)))))
	(numer s 0)
  )
  ; END PROBLEM 17

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 18
  ;make a list of all possible combinations 
; if total == 0
	(cond 
		((null? denoms) nil) ;done once u go thru all the denoms
		((< total 0) nil) ;filter out decrementing too much
		((= 0 total) (list nil)) ;happens when total and denom are equal basically
		(else (append (cons-all 
					(car denoms) 
					(list-change 
						(- total (car denoms))
				     	denoms)) 
				(list-change total (cdr denoms))
				))))

  
  ; END PROBLEM 18

;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 19
         expr ;normal
         ; END PROBLEM 19
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 19
         expr ;normal
         ; END PROBLEM 19
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
                 ;   (print 'yikes)
           ; BEGIN PROBLEM 19
           ;lambda formals body
           ;define procedure is like lambda in scheme.py
           ;(print 'yikes1)
           (cons form  
                 (cons (map let-to-lambda params)   
                       (let-to-lambda body)))))

           ; END PROBLEM 19
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
                ;(print 'yikes2)

         	;(let ((a 1) (b 2)) (+ a b))
			; Is equivalent to:
			;((lambda (a b) (+ a b)) 1 2)
           ; BEGIN PROBLEM 19
           ; car zip values will be the parameters
           ; cdr zip values will be the arguments (passed in at end wihtout parenthesis)
           ; body is just body okay
           ;(print 'lambda)
           ;(print  (zip '((a 1) (b 2))))
           (append (list (list 'lambda 
           				 (car (zip (let-to-lambda values)))
           					  (car (let-to-lambda body))))
           		 (cadr (zip (let-to-lambda values))))
           ; END PROBLEM 19
           ))
        (else
         ; BEGIN PROBLEM 19
         (map let-to-lambda expr)
         ; END PROBLEM 19
         )))
