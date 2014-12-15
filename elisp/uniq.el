;; uniq.el
;;
;; Does what the Unix command, uniq(1) does with no arguments.
;; It removes duplicate lines from a region of text.

(defun current-line-to-string ()
  (save-excursion
    (let ((left-edge  (progn (beginning-of-line) (point)))
          (right-edge (progn (end-of-line) (point))))
      (buffer-substring left-edge right-edge))))

(defun uniq (begin end)
  "Removes duplicate lines from a region. The region is assumed to be sorted,
so that duplicates are contiguous."
  (interactive "r")
  
  (let ((current-string "")
        (first-line t))
    (iterate-over-lines-in-region
     begin end
     '(lambda ()
        (if first-line
            ;; We're on the first line, so just set the current-string
            ;; for subsequent matches.
            ;;
            (progn
              (setq current-string (current-line-to-string))
              (setq first-line nil))
          
          ;; Else...
          (if (string= current-string (current-line-to-string))
              (kill-line 1)
            (setq current-string (current-line-to-string))))))))
        
(defun non-uniq (begin end)
  "Keeps only duplicated lines from a region. The region is assumed to be sorted,
so that duplicates are contiguous."
  (interactive "r")
  
  (let ((previous-line "")
        (first-line t))
    (iterate-over-lines-in-region
     begin end
     '(lambda ()
        (if first-line
            
            ;; We're on the first line, so just set the previous-line
            ;; for subsequent matches, then kill the line.
            ;;
            (progn
              (setq previous-line (current-line-to-string))
              (kill-line 1)
              (setq first-line nil))
          
          ;; Else...if this line isn't the same as the last, kill it.
          ;;
          (if (not (string= previous-line (current-line-to-string)))
              (progn
                (setq previous-line (current-line-to-string))
                (kill-line 1))
            (setq previous-line (current-line-to-string)))))))
  ;; Finally, remove duplicates of duplicates.
  ;;
  (uniq begin end))
