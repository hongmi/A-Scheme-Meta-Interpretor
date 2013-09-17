#!/usr/local/bin/guile -s
!#

(define calc
  (lambda (exp)
    (if (number? exp)
	exp
	(let ([op (car exp)]
	      [arg1 (calc (cadr exp))]
	      [arg2 (calc (caddr exp))])
	  (cond [(eq? op '+) (+ arg1 arg2)]
		[(eq? op '-) (- arg1 arg2)]
		[(eq? op '*) (* arg1 arg2)]
		[(eq? op '/) (/ arg1 arg2)]
		[else (display "Error Operator")])))))
