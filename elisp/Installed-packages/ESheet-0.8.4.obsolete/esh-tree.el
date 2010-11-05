

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;Tree Handlers;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;I need trees that will take conses, and it seemed easier to write them 
;than to find them
;They're far from optomized
;They definitely should have their own file 

(defun lt (a b) "less than, with support for conses"
  (if (and (number-char-or-marker-p a) (number-char-or-marker-p b)) (< a b)
    (if (and (consp a) (consp b))
	(if (lt (car a) (car b)) t (lt (cdr a) (cdr b))))
    )
  )

(defun gt (a b) "greater than, with support for conses"
  (if (and (number-char-or-marker-p a) (number-char-or-marker-p b)) (> a b)
    (if (and (consp a) (consp b))
	(if (gt (car a) (car b)) t (gt (cdr a) (cdr b))))
    )
  )

(defun for-all-in-tree (tr f) "perform function f on every element of tree tr"
  (if tr (progn
	   (funcall f (car tr))
	   (for-all-in-tree (cadr tr) f)
	   (for-all-in-tree (cddr tr) f)
	   )
    )
  )

(defun real-add-to-tree (tr n) "return a tree like tr only with node n added"
  (cond
   ((null tr) n)
   ((equal (car n) (car tr)) tr)
   ((lt (car n) (car tr)) (cons (car tr) (cons (real-add-to-tree (cadr tr) n) (cddr tr))))
   ((gt (car n) (car tr)) (cons (car tr) (cons (cadr tr) (real-add-to-tree (cddr tr) n))))
   )
  )

(defun add-to-tree (tr d) "front end for real-add-to-tree that takes data"
  (real-add-to-tree tr (cons d '(nil))))

(defun find-in-tree (tr d) "returns t iff d is in tr, nil otherwise"
  (cond
   ((null tr) nil)
   ((equal d (car tr)) t)
   ((lt d (car tr)) (find-in-tree (cadr tr) d))
   ((gt d (car tr)) (find-in-tree (cddr tr) d))
   )
  )

(defun kill-tree-elem (e) "destroy tree-node e and re-arange everything below"
  (cond
   ((and (null (cadr e)) (null (cddr e))) nil)
   ((null (cadr e)) (cddr e))
   ((null (cddr e)) (cadr e))
   (t (real-add-to-tree (cadr e) (cddr e))) ;default case
   )
  )

(defun remove-from-tree (tr d) "return a tree like tr only without d"
  (cond
   ((null tr) nil)
   ((equal d (car tr)) (kill-tree-elem tr))
   ((lt d (car tr)) (cons (car tr) (cons (remove-from-tree (cadr tr) d) (cddr tr))))
   ((gt d (car tr)) (cons (car tr) (cons (cadr tr) (remove-from-tree (cddr tr) d))))
   )
  )