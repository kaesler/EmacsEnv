;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Statistical Functions;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;What's a spreadsheet without a good statistics library? 

(defun sum (&rest x) 
  "sum all the elements in x.  x can be a list, a number, or a list of lists, 
or a list of lists of lists or ..."
  (if (and (consp x) (null (cdr x))) (setq x (car x)))
  (float (cond
   ((null x) 0)
   ((consp x) (+ (sum (car x)) (sum (cdr x))))
   ((eq x 'identidy-element) 0)
   (t x))))

;these two are unneeded
(defun adhd (&rest x) 
  "sum all the elements in x.  x can be a list, a number, or a list of lists, 
or a list of lists of lists or ..."
  (if (and (consp x) (null (cdr x))) (setq x (car x)))
  (if (null x) 0
    (if (consp x) (+ (adhd (car x)) (adhd (cdr x))) x)))

(defun totalsum (&rest x) 
  "sum all the elements in x.  x can be a list, a number, or a list of lists, 
or a list of lists of lists or ...\n\nThis function is not called \"sum\" because of the strange errors it caused when it was.  THere is no function partialsum."
  (if (and (consp x) (null (cdr x))) (setq x (car x)))
  (if (null x) 0
    (if (consp x) (+ (totalsum (car x)) (totalsum (cdr x))) x)))


(defun product (&rest x) 
  "product all the elements in x.  x can be a list, a number, or a list of
lists, or a list of lists of lists or ..."
  (if (and (consp x) (null (cdr x))) (setq x (car x)))
  (if (null x) 1
    (if (consp x) (* (product (car x)) (product (cdr x))) x)))

(defun n (&rest l) "determine the number of elements in l"
  (if (and (consp l) (null (cdr l))) (setq l (car l)))
  (if (consp l)
      (+ (n (car l)) (n (cdr l)))
    (if (and l (not (symbolp l))) 1 0)))

(defun mean (&rest l) 
  "calculate the mean of a range"
  (/ (sum l) (n l)))

(defun geomean (&rest l) 
  "calculate the geometric mean of a range"
  (^ (product l) (/ 1.0 (n l))))

(defun stdev (&rest l) 
  "calculate the standard deviation of a range"
  (^ (/ (sum (^ (- l (mean l)) 2)) (- (n l) 1)) 0.5))

(fset 'stddev #'stdev) ;some people like each

(defun sortmtx (d k)
  (sort d #'(lambda (a b) (lesstk a b k))))

(defun lesstk (a b k)
  (cond
   ((not k) nil)
   ((< (nth (car k) a) (nth (car k) b)) t)
   ((> (nth (car k) a) (nth (car k) b)) nil)
   (t (lesstk a b (cdr k)))))

(defun transpose (m)
  (if m (cons (carall m)
	(consall (cdar m) (transpose (cdrall (cdr m))))) nil))

(defun carall (a)
  (if a (cons (caar a) (carall (cdr a))) nil))
(defun cdrall (a)
  (if a (cons (cdar a) (cdrall (cdr a))) nil))
(defun consall (a b)
  (if a (cons (cons (car a) (car b)) (consall (cdr a) (cdr b))) nil))

(defun z-score (l) 
  (/ (- l (mean l)) (stdev l)))

(defun correl (a b)
  "find the correllation coefficient for two ranges"
  (/ (sum (* (z-score a) (z-score b))) (- (n a) 1)))

