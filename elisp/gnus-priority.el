;; gnus-priority.el
;; Copyleft (c) Ashwin Ram, 7/1/90.  Usual GNU rules apply.
;;
;; Modified heavily by Kevin Esler.  December, 1991.
;;
;; Code to allow GNUS to maintain .newsrc in order of priority of newsgroups.
;; Default order is alphabetical, but certain type hierarchies (e.g., "comp")
;; or certain newsgroup name substrings (e.g., "ai") can be boosted up if
;; desired.
;;
;; How to customize:
;;     (setq newsgroup-type-priority-list ...)
;;     (setq newsgroup-name-priority-list ...)
;;
;; How to use:
;;     (require 'gnus-priority)
;;     Manually, in .newsrc buffer:  M-x esler-gnus-sort-newsrc-buffer
;;     To have new newsgroups inserted in the appropriate position automatically:


;; Variables for customization
;; ---------------------------
;; Example:
;; 
;;    To read "comp" groups first, then "soc" groups, then "sci" groups, and
;;    everything else alphabetically:
;;        (setq newsgroup-type-priority-list '("comp" "sci" "soc"))
;; 
;;    To read "general" groups first, then groups about "ai", then groups
;;    about "gnus", and everything else alphabetically:
;;        (setq newsgroup-name-priority-list '("\\.general" "\\.ai"))
;;
;; Either list can include arbitrary regexps.  The newsgroups are sorted
;; alphabetically, but weighted by the above priorities.  So for example
;; given the above variables you'd get "comp.ai" before "comp.emacs.gnus"
;; before "comp.arch" (say).
;; 
;; If you don't care about priorities, you can leave these variables alone
;; and stay with regular alphabetical order.

(defvar newsgroup-type-priority-list nil   
   "List of newsgroup type/hierarchy names in descending order of priority.
Types not mentioned are of zero priority.  See also newsgroup-name-priority-list.")

(defvar newsgroup-name-priority-list nil
   "List of newsgroup name components/substrings in descending order of priority.
Components not mentioned are of zero priority.  See also newsgroup-type-priority-list.")

;; Main prioritization functions.

(defun newsgroup< (a b)
   "Newsgroup A comes before newsgroup B if it is higher priority.
If priorities are equal, alphabetical order is used."
   (let ((pa (newsgroup-priority a))
         (pb (newsgroup-priority b)))
      (cond ((= pa pb) (string< a b))
            ((> pa pb) t)
            (t nil))))

(defun newsgroup-priority (newsgroup-name)
   (let* ((newsgroup-type
              (and (string-match "^[^.]+" newsgroup-name)
                   (substring newsgroup-name 0 (match-end 0))))
          (newsgroup-type-index (gnus-priority-mem newsgroup-type newsgroup-type-priority-list 'equal))
          (newsgroup-name-index (gnus-priority-mem newsgroup-name newsgroup-name-priority-list 'equal)))
     (if newsgroup-name-index
         (+ (length newsgroup-type-priority-list) (length newsgroup-name-index))
         (if newsgroup-type-index (length newsgroup-type-index)
             0))))

;; Main user functions.

(defun esler-gnus-sort-newsrc-file ()

   "Sort newsgroups in a .newsrc file using newsgroup< predicate."

   (interactive)

   (save-excursion
     (find-file gnus-startup-file)
     (esler-gnus-sort-newsrc-buffer (current-buffer))
     (beep)))


(defun esler-gnus-sort-newsrc-buffer (buffer)

   "Sort newsgroups in a .newsrc buffer using newsgroup< predicate."

   (interactive "bBuffer: ")

   (require 'sort)

   (save-excursion
     (set-buffer buffer)
     (goto-char (point-min))
     (esler-sort-subr nil
                      'forward-line
                      'end-of-line
                      'beginning-of-line
                      '(lambda ()
                         (while (not (looking-at ":"))
                           (forward-char 1)))
                      'newsgroup<)))

;; Uncomment this if you want new newsgroups to be placed at the
;; appropriate position automatically.
;;

(setq gnus-subscribe-newsgroup-method
      (function
        (lambda (newgroup)
          "Insert new newsgroups into a place chosen by interpreting
the variables 'newsgroup-type-priority-list, and 'newsgroup-name-priority-list."
          (let ((groups (cdr gnus-newsrc-alist))
                (before nil))
            (while (and (not before) groups)
              (if (newsgroup< newgroup (car (car groups)))
                  (setq before (car (car groups)))
                (setq groups (cdr groups))))
            (gnus-subscribe-newsgroup newgroup before)))))

;; Simple Lisp utilities.
;; ----------------------

(defun gnus-priority-mem (x l pred)
   (let ((done nil))
      (while (and l (not done))
         (if (funcall pred x (car l))
             (setq done t)
             (setq l (cdr l))))
      l))



(provide 'gnus-priority)

;; End of gnus-priority.el
