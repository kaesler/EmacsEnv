;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Graphing;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun graph-point (d l c)
  (if d (progn
	  (insert (if (> (car d) l) (make-string 10 c) "          "))
	  (graph-point (cdr d) l (+ c 1)))))

(defun graph-cluster (d l)
  (if d (progn
	  (graph-point (car d) l ?#)
	  (insert "  ")
	  (graph-cluster (cdr d) l)
	  )))

(defun graph-line (dat l)
  (insert "\"")
  (graph-cluster dat l)
  (insert "\",\n")
)

(defun mat-max (m) 
  (cond
   ((null m) -99999999)
   ((consp m) (max (mat-max (car m)) (mat-max (cdr m))))
   (t m)))

(defun color (i)
  (let ((c (mod i 9)))
    (cond
     ((= c 0) "FFFF00000000")
     ((= c 1) "000077770000")
     ((= c 2) "00000000FFFF")
     ((= c 3) "999999990000")
     ((= c 4) "0000FFFFFFFF")
     ((= c 5) "FFFF0000FFFF")
     ((= c 6) "777777777777")
     ((= c 7) "FFFFAAAA0000")
     ((= c 8) "0000FFFF0000")
     )
    )
  )


(defun insert-color-codes (n)
  (let ((i 0))
    (while (< i n)
      (insert (concat "\"" (cons (+ ?# i) nil) "      c #" (color i) "\",\n")) 
      (setq i (+ i 1))
      )))

(defun graph (data)
  (find-file "/tmp/graph")
  (let ((m (mat-max data)) (h 100))
    (let ((i h))
      (while (> i 0)
	(graph-line data (* m (/ (float i) h)))
	(setq i (- i 1))
	)
      )
    (insert-legend (car data) (length data))
    (delete-char -2)
    (insert "};")
    (let ((hei (- (count-lines (point-min) (point-max)) 4)))
      (beginning-of-buffer)
      (end-of-line)
      (let ((wid (- (point) 4)))
	(beginning-of-line)
	(insert (concat "/* XPM */\nstatic char * graph[] = {\n\""
			(int-to-string wid)
			" "
			(int-to-string hei)
			" "
			(int-to-string (+ 2 (length (car data))))
			" 1\",\n")
		)
	)
      (insert "\"       c None\",\n")
      (insert "\"!      c #000000000000\",\n")
      (insert-color-codes (length (car data)))
      (image-mode)
      (write-file "/tmp/graph.xpm")
      (image-toggle-decoding)
      )
    )
  )

(defun graph-region () (interactive)
  (if esheet-region
      (let ((x1 (min (caar esheet-region) (cadr esheet-region)))
	    (x2 (max (caar esheet-region) (cadr esheet-region)))
	    (y1 (min (cdar esheet-region) (cddr esheet-region)))
	    (y2 (max (cdar esheet-region) (cddr esheet-region))))
	(graph (transpose (rrange x1 y1 x2 y2 nil nil))))
    (message "no region active!")))

(setq ALPHABET [
"       !               !         !!        !                 !      !!                                                                                         !   !!!!   !!!  !!!!  !!!!! !!!!!  !!!  !   !  !!!    !!! !   ! !     !   !  !!!  !   ! !!!!   !!!  !!!!   !!!  !!!!! !   ! !   ! !   ! !   ! !   ! !!!!!   !     !    !!!  !!!!!    !  !!!!!  !!!  !!!!!  !!!   !!!    !   "
"       !               !        !  !       !       !      !  !       !                                              !                                         ! !   !  ! !   !  !  ! !     !     !   ! !   !   !      !  !   ! !     !   ! !   ! !!  ! !   ! !   ! !   ! !   !   !   !   ! !   ! !   ! !   ! !   !     !  ! !   !!   !   !     !    !  !     !   !     ! !   ! !   !  ! !  "
"       !               !        !          !                 !       !                                              !                                        !   !  !  ! !      !  ! !     !     !     !   !   !      !  !  !  !     !! !! !   ! !!  ! !   ! !   ! !   ! !       !   !   ! !   ! !   !  ! !   ! !     !  !   ! ! !   !   !    !    !!  !     !        !  !   ! !   ! !   ! "
"  !!!  !!!!   !!!   !!!!  !!!   !     !!!  ! !!   !!     !!  !  !    !   !! !   !!!  ! !!  !!!!   !!!! ! !!   !!!  !!!!  !   ! !   ! !   ! !   ! !   ! !!!!! !   !  !  ! !      !  ! !     !     !     !   !   !      !  ! !   !     ! ! ! !   ! ! ! ! !   ! !   ! !   ! !       !   !   ! !   ! !   !  ! !   ! !     !  !   !   !       !   !    ! !  ! !!  !        !  !   ! !   ! !   ! "
"     ! !   ! !   ! !   ! !   ! !!!!  !   ! !!  !   !      !  ! !     !   ! ! ! !   ! !!  ! !   ! !   ! !!  ! !   !  !    !   ! !   ! !   !  ! !  !   !    !  !   !  !!!  !      !  ! !!!!  !!!!  !     !!!!!   !      !  !!    !     ! ! ! !   ! ! ! ! !!!!  !   ! !!!!   !!!    !   !   !  ! !  ! ! !   !     !     !   !   !   !      !   !!!   ! !  !!  ! !!!!    !    !!!   !!!! !   ! "
"  !!!! !   ! !     !   ! !!!!!  !    !   ! !   !   !      !  !!      !   ! ! ! !   ! !   ! !   ! !   ! !      !!    !    !   ! !   ! ! ! !   !   !   !   !   !!!!!  !  ! !      !  ! !     !     !  !! !   !   !      !  ! !   !     !   ! !   ! !  !! !     !   ! ! !       !   !   !   !  ! !  ! ! !  ! !    !    !    !   !   !     !       ! !  !      ! !   !   !   !   !     ! !   ! "
" !   ! !   ! !     !   ! !      !    !   ! !   !   !      !  ! !     !   ! ! ! !   ! !   ! !   ! !   ! !        !   !    !   !  ! !  ! ! !   !   !  !!  !    !   !  !  ! !      !  ! !     !     !   ! !   !   !      !  !  !  !     !   ! !   ! !  !! !     !   ! !  !      !   !   !   !  ! !  ! ! !  ! !    !    !    !   !   !    !        ! !!!!!     ! !   !  !    !   !     ! !   ! "
" !   ! !   ! !   ! !   ! !   !  !     !!!! !   !   !      !  !  !    !   ! ! ! !   ! !   ! !!!!   !!!! !     !   !  !  ! !  !!  ! !  ! ! !  ! !   !! ! !     !   !  !  ! !   !  !  ! !     !     !   ! !   !   !   !  !  !   ! !     !   ! !   ! !   ! !     ! ! ! !   ! !   !   !   !   !   !   !! !! !   !   !   !      ! !    !   !     !   !    !  !   ! !   !  !    !   ! !   !  ! !  "
"  !!!! !!!!   !!!   !!!!  !!!   !        ! !   !  !!!  !  !  !   !  !!!  !   !  !!!  !   ! !         ! !      !!!    !!   !! !   !    ! !  !   !     ! !!!!! !   ! !!!!   !!!  !!!!  !!!!! !      !!!  !   !  !!!   !!   !   ! !!!!! !   !  !!!  !   ! !      !!!  !   !  !!!    !    !!!    !   !   ! !   !   !   !!!!!   !   !!!!! !!!!!  !!!     !   !!!   !!!   !     !!!   !!!    !   "
"                                     !   !             !  !                                !         !                                           !   !                                                                                                           !                                                                                                                         "
"                                      !!!               !!                                 !         !                                            !!!                                                                                                                                                                                                                                      "])


(defun char-line (ch li)
  (cond
   ((and (>= ch ?a) (<= ch ?z)) (substring (aref ALPHABET li) (* (- ch ?a) 6) (* (- ch ?a -1) 6)))
   ((and (>= ch ?A) (<= ch ?Z)) (substring (aref ALPHABET li) (* (- ch ?A -26) 6) (* (- ch ?A -27) 6)))
   ((and (>= ch ?0) (<= ch ?z)) (substring (aref ALPHABET li) (* (- ch ?0 -52) 6) (* (- ch ?0 -53) 6)))
   )
  )



(defun insert-legend (lab len) nil)