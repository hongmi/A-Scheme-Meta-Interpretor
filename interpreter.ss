#!/usr/local/bin/guile -s
!#





(define atom?
  (lambda (e)
    (cond ((null? e) #f)
	  ((pair? e) #f)
	  (else #t))))

(define build
  (lambda (x y)
    (cons x (cons y '()))))
(define first car)
(define second cadr)
(define third caddr)
(define new-entry build)
(define extend-table cons)
(define text-of second)
(define initial-table
  (lambda (name)
    (car (quote ()))))
(define names car)
(define values cdr)
(define cond-lines-of cdr)
(define question-of first)
(define answer-of second)
(define else?
  (lambda (x)
    (cond
     ((atom? x) (eq? x (quote else)))
     (else #f))))
(define table-of first)
(define formals-of second)
(define body-of third)
(define function-of car)
(define arguments-of cdr)
(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))
(define non-primitive?
  (lambda (l)
    (eq? (first l) (quote non-primitive))))


(define lookup-in-entry-helper
  (lambda (name names values entry-f)
    (cond
     ((null? names) (entry-f name))
     ((eq? name (car names)) (car values))
     (else (lookup-in-entry-helper
	    name
	    (cdr names)
	    (cdr values)
	    entry-f)))))

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-helper
     name
     (names entry)
     (values entry)
     entry-f)))

(define lookup-in-table
  (lambda (name table table-f)
    (lookup-in-entry
     name
     (car table)
     (lambda (x)
       (lookup-in-table
	x (cdr table) table-f)))))

(define *const
  (lambda (e table)
    (cond
     ((number? e) e)
     ((eq? e #t) #t)
     ((eq? e #f) #f)
     (else (build (quote primitive) e)))))

(define *quote
  (lambda (e table)
    (text-of e)))

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define evcon
  (lambda (lines table)
    (cond
     ((else? (question-of (car lines)))
      (meaning (answer-of (car lines)) table))
     ((meaning (question-of (car lines)))
      (meaning (answer-of (car lines))))
     (else (evcon (cdr lines) table)))))

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define *lambda
  (lambda (e table)
    (build
     (quote non-primitive)
     (cons table (cdr e)))))

(define evlis
  (lambda (l table)
    (cond
     ((null? l) (quote ()))
     (else
      (cons (meaning (car l) table)
	    (evlis (cdr l) table))))))

(define _atom?
  (lambda (x)
    (cond
     ((atom? x) #t)
     ((null? x) #f)
     ((eq? (car x) (quote primitive)) #t)
     ((eq? (car x) (quote non-primitive)) #t)
     (else #f))))

(define add1
  (lambda (x)
    (+ x 1)))
(define sub1
  (lambda (x)
    (- x 1)))

(define apply-primitive
  (lambda (name vals)
    (cond
     ((eq? name (quote cons)) (cons (first vals) (second vals)))
     ((eq? name (quote car)) (car (first vals)))
     ((eq? name (quote cdr)) (cdr (first vals)))
     ((eq? name (quote null?)) (null? (first vals)))
     ((eq? name (quote pair?)) (pair? (first vals)))
     ((eq? name (quote atom?)) (_atom? (first vals)))
     ((eq? name (quote eq?)) (eq? (first vals) (second vals)))
     ((eq? name (quote zero?)) (zero? (first vals)))
     ((eq? name (quote add1)) (add1 (first vals)))
     ((eq? name (quote sub1)) (sub1 (first vals)))
     ((eq? name (quote number?)) (number? (first vals))))))


(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
	     (extend-table
	      (new-entry
	       (formals-of closure)
	       vals)
	      (table-of closure)))))
(define apply
  (lambda (fun vals)
    (cond
     ((primitive? fun)
      (apply-primitive (second fun) vals))
     (else
      (apply-closure (second fun) vals)))))

(define *application
  (lambda (e table)
    (apply
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))

(define atom-to-action
  (lambda (e)
    (cond ((eq? e #t) *const)
	  ((eq? e #f) *const)
	  ((number? e) *const)
	  ((eq? e (quote car)) *const)
	  ((eq? e (quote cdr)) *const)
	  ((eq? e (quote cons)) *const)
	  ((eq? e (quote null?) *const))
	  ((eq? e (quote pair?) *const))
	  ((eq? e (quote eq?)) *const)
	  ((eq? e (quote number?) *const))
	  ((eq? e (quote zero?) *const))
	  ((eq? e (quote add1)) *const)
	  ((eq? e (quote sub1)) *const)
	  (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
     ((atom? (car e))
      (cond
       ((eq? (car e) (quote quote)) *quote)
       ((eq? (car e) (quote lambda)) *lambda)
       ((eq? (car e) (quote cond)) *cond)
       (else *application)))
     (else *application))))

(define expression-to-action
  (lambda (e)
    (cond ((atom? e) (atom-to-action e))
	  (else (list-to-action e)))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))  

(define value
  (lambda (e)
    (meaning e '())))
