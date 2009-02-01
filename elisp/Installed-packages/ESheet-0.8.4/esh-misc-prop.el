(defun add-misc-prop (x y p v) "add to property p of cell x,y the value v"
  (let ((n (find-curs-n x y)))
    (if n
	(let ((prop (get-text-property n p)))
	  (add-text-properties n (+ n 1) 
			       (list p (add-to-tree prop v)))
	  )
      )
    )
  )

(defun tell-misc-prop (x y p) "give the values of p at x,y as a list"
  (setq flag-misc-prop nil)
  (for-all-in-tree (get-text-property (find-curs-n x y) p) 
		   #'(lambda (a) (setq flag-misc-prop 
				       (cons a flag-misc-prop))))
;  (message flag-misc-prop)
  flag-misc-prop
  )

(defun kill-misc-prop (x y p v) "remove from property p of x,y the value v"
  (let ((n (find-curs-n x y)))
    (let ((prop (get-text-property n p)))
      (add-text-properties n (+ n 1) 
			   (list p (remove-from-tree prop v)))
      )
    )
  )
