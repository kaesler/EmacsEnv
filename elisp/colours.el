;; Stuff for discovering nice-looking colours.
;;
(defun extract-colour-name ()
  (interactive)
  (if (looking-at "[ \t]*[0-9]+[ \t]+[0-9]+[ \t]+[0-9]+[ \t]+\\([a-zA-Z].*\\)")
      (buffer-substring (match-beginning 1) (match-end 1))
    nil))

(defun colour-file-to-colour-list (colour-file)
  (save-excursion
    (set-buffer (find-file-noselect colour-file))
    (let ((colour-list nil))
      (iterate-over-lines-in-region (point-min) (point-max)
                                    '(lambda ()
                                       (let ((colour-name (extract-colour-name)))
                                         (if colour-name
                                             (setq colour-list (cons (extract-colour-name) colour-list))))))
      colour-list)))

(defun filter-out-greys (colour-list)
  (let ((result-list nil))
    (while colour-list
      (let ((colour-name (car colour-list)))
        (if (not (string-match "gr[ea]y[0-9]*" colour-name))
            (setq result-list (cons colour-name result-list))))
      (setq colour-list (cdr colour-list)))
    result-list))

(defun filter-out-dups (colour-list)
    (let ((result-list nil))
    (while colour-list
      (let ((colour-name (car colour-list)))

        ;; Duplicate names tend to have spaces:
        ;; (should really look at the number fields)
        ;;
        (if (not (string-match " " colour-name))
            (setq result-list (cons colour-name result-list))))
      (setq colour-list (cdr colour-list)))
    result-list))

(defun filter-out-fours (colour-list)
  (let ((result-list nil))
    (while colour-list
      (let ((colour-name (car colour-list)))
        (if (not (string-match "4" colour-name))
            (setq result-list (cons colour-name result-list))))
      (setq colour-list (cdr colour-list)))
    result-list))
                          
(defun colour-menu ()
  "Generate a buffer containing all possible colours.
Uses /usr/lib/X11/rgb.txt."
  (interactive)
  (let* ((colour-file "/usr/lib/X11/rgb.txt")
         (colour-list (colour-file-to-colour-list colour-file))
         (non-grey-colour-list (filter-out-greys colour-list))
         (non-dup-colour-list (filter-out-dups non-grey-colour-list))
         (non-4-colour-list (filter-out-fours non-dup-colour-list)))
    (set-buffer (get-buffer-create "*Colours*"))
    (erase-buffer)
    (mapcar '(lambda (colour-name)

               ;; Make the face symbol
               ;;
               (let ((face-symbol (make-symbol colour-name)))

                 (insert colour-name ": ")

                 ;; Try to make the face.
                 ;; Sometimes the X server cannot allocate colours,
                 ;; so handle this gracefully.
                 ;;
                 (make-face face-symbol)

                 (if (condition-case err
                         (progn
                           (set-face-foreground face-symbol colour-name)
                           t)
                       (error nil))
                     (progn

                       ;; Create a line of text illustrating the face
                       ;;
                       (let ((begin (point)))
                         (insert "abcdefghijklmnop")
                         (put-text-property begin (point) 
                                            'face
                                            face-symbol)))
                   (insert "couldn't allocate this colour"))
                 (insert "\n")))
            
            non-4-colour-list)))

