(defun dequotify-recurse (l f f2)
  (cond
   ((null l) nil)
   ((and (not f) (equal (car l) ?,)) (cons ?\n (dequotify-recurse (cdr l) nil nil)))
   ((and (not f) (not f2) (equal (car l) ?\")) (dequotify-recurse (cdr l) t nil))
   ((and f (not f2) (equal (car l) ?\")) (dequotify-recurse (cdr l) nil t))
   ((and f2 (equal (car l) ?\")) (cons ?\" (dequotify-recurse (cdr l) t nil)))
   (t (cons (car l) (dequotify-recurse (cdr l) f nil)))))

(defun dequotify (str)
  "Takes a line of csv and removes the quoting, also changes unquoted commas to \\n"
  (concat
   (dequotify-recurse (mapcar #'(lambda (x) x) str) nil nil)))


(defun csv-to-esheet (str)
  "convert a string of csv (entire sheet) into esheet format" 
  (concat
   ";;Esheet\n("
   (int-to-string (+ 1 (count ?\n (dequotify (aref (splice str ?\n) 0)))))
   " . "
   (int-to-string (count ?\n str))
   ")\n"
   (dequotify str)))

(defun csv-mode ()
  "Converts file format and loads esheet.
See esheet-mode for useful information"
  (interactive)
  (let ((str (buffer-substring (point-min) (point-max))))
    (erase-buffer)
    (insert (csv-to-esheet str)))
  (esheet-mode))
