;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;Operator Overloading;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;the idea here it to allow things like (+ '(1 2 (3 4)) '(5 6 (7 8))) => (6 8 (10 12))

(if (not (boundp 'depricated-established)) ;don't do this twice!
    (progn
      ;save the original versions, we'll need them
      (setq depricated+ (symbol-function '+))
      (setq depricated- (symbol-function '-))
      (setq depricated* (symbol-function '*))
      (setq depricated/ (symbol-function '/))
      (setq depricated% (symbol-function '%))
      ;I know, there's nothing depricated here, but it makes the code neater
      (setq depricated^ (symbol-function 'expt))
      )
  )
(setq depricated-established 1)

(defun dotop (a b f) "take function f of a and b handling lists intellegently"
  (cond
   ((or (null a) (null b))
    nil)
   ;handle (+ '(2 3) '(4 5)) yielding '(6 8)
   ((and (consp a) (consp b))
    (cons (dotop (car a) (car b) f) (dotop (cdr a) (cdr b) f)))
   ;handle (+ '(2 3) 4) yielding '(6 7)
   ((and (consp a) (not (consp b)))
    (cons (dotop (car a) b f) (dotop (cdr a) b f)))
   ((and (not (consp a)) (consp b))
    (cons (dotop a (car b) f) (dotop a (cdr b) f)))
   ;handle blank cells
   ((eq a 'identidy-element) b)
   ((eq b 'identidy-element) a)
   ;the base case: (+ 2 2) yields 4
   ((and (not (consp a)) (not (consp b)))
    (funcall f a b))))

(defun mondotop (a f) "apply f to all of a"
  (cond
   ((null a) nil)
   ((consp a) (cons (mondotop (car a) f) (mondotop (cdr a) f)))
   ((eq a 'identidy-element) 'identidy-element)
   (t (funcall f a))))

(defun onlist (l f) "takes list l (1 2 3 4) and function f / and makes (/ (/ (/ 1 2) 3) 4)"
  (if (cdr l)
      (onlist (cons
	       (dotop (car l) (cadr l) f)
	       (cddr l))
	      f)
    (car l)))

(defun + (&rest arg) (if (cdr arg) (onlist arg depricated+) (mondotop (car arg) depricated+)))
(defun - (&rest arg) (if (cdr arg) (onlist arg depricated-) (mondotop (car arg) depricated-)))
(defun * (&rest arg) (if (cdr arg) (onlist arg depricated*) (mondotop (car arg) depricated*)))
(defun / (&rest arg) (if (cdr arg) (onlist arg depricated/) (mondotop (car arg) depricated/)))
(defun % (a b) (dotop a b depricated%))
(defun ^ (a b) (dotop a b depricated^))
