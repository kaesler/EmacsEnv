;; stringset.el
;;
;; Lisp code for converting between regions of text and sets of strings,
;; and for manipulating sets of strings.
;;
;; This is quite useful for comparing lists of things in a file of text.
;;

(defun esler-mem (x l pred)
  (let ((done nil))
    (while (and l (not done))
      (if (funcall pred x (car l))
          (setq done t)
        (setq l (cdr l))))
    l))

(defun stringset-extract-region (begin end symbol)
  
  "Produce a set of strings from the lines in a region.Bind the set to a symbol."
  
  (interactive "r\nSSymbol: ")
  
  (let ((set-of-strings '()))
    (iterate-over-lines-in-region
     begin
     end
     '(lambda ()
        ;; This should match every line.
        ;; We slice out text excluding leading and trailing white-space.
        ;;
        (if (looking-at "^[ \t]*\\(.*\\)[ \t]*")
            (setq set-of-strings (cons (buffer-substring (match-beginning 1) (match-end 1)) set-of-strings))
          (error "No match"))))
    (set symbol set-of-strings)))

(defun stringset-member (s sset)
  (esler-mem s sset 'string=))

(defun stringset-union (ss1 ss2)
  (let ((result ss1)
        (cursor ss2))
    (while (not (null cursor))
      (if (not (stringset-member (car cursor) result))
          (setq result (cons (car cursor) result)))
      (setq cursor (cdr cursor)))
    result))

(defun stringset-intersection (ss1 ss2)
  (let ((result '())
        (cursor ss2))
    (while (not (null cursor))
      (if (stringset-member (car cursor) ss1)
          (setq result (cons (car cursor) result)))
      (setq cursor (cdr cursor)))
    result))

(defun stringset-difference (ss1 ss2)
  (let ((result '())
        (cursor ss1))
    (while (not (null cursor))
      (if (not (stringset-member (car cursor) ss2))
          (setq result (cons (car cursor) result)))
      (setq cursor (cdr cursor)))
    result))

(defun stringset-to-text (ss)
  (mapconcat 'identity ss "\n"))

(defun stringset-insert (symbol)
  
  (interactive "SStringset name: ")
  (insert (stringset-to-text
           
           ;; This dodge is made necessary by the fact that "sort"
           ;; returns a correctly sorted list, BUT erroneously clobbers
           ;; the original location.
           ;; We allow for this by assigning to the symbol, the sorted list.
           ;;
           (let ((sorted-set (sort (symbol-value symbol) 'string<)))
             (set symbol sorted-set)
             sorted-set))))

(defun stringset-size (ss)
  (length ss))
