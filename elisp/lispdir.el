;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lispdir.el --- Lisp code directory formatter and apropos
;; (Originally lisp-code-directory.el)
;; Authors         : Ashwin Ram (Ram-Ashwin@cs.yale.edu)
;;                 : Dave Sill (dsill@relay.nswc.nay.mil)
;; Created On      : Wed Jan 25, 1989
;; Last Modified By: tale@pawl.rpi.edu
;; Last Modified On: Mon Feb 20 23:30:02 1989
;; Update Count    : 3
;; Status          : No known bugs.
;; Version         : 1.01

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; History              
;; 16-Feb-1989          dsill   
;;    Added lisp-dir-apropos function
;; 20-Feb-1989          tale@pawl.rpi.edu
;;    format-lisp-code-directory makes seperate buffer
;;    removed lisp-dir-apropos-buffer -- why use more space in memory?
;;    added lisp-dir-[apropos-]hook
;;      (I like (setq lisp-dir-hook 'delete-other-windows))
;;    other aesthetic changes
(require 'picture)

(defvar lisp-code-directory "~/Info/elisp-code-directory"
  "Database of free lisp code.  Entries are in the form:
Name|Author|Contact|Description|Date|Version")

(defun format-lisp-code-directory ()
   "Convert GNU Emacs Lisp code directory into something a human could read.
Calls value of lisp-dir-hook with no args if that value is non-nil."
   (interactive)
   (pop-to-buffer "*Lisp Code Directory*")
   (fundamental-mode)
   (setq buffer-read-only nil)
   (erase-buffer)
   (buffer-flush-undo (current-buffer))
   (insert-file lisp-code-directory)
   (insert "GNU Emacs Lisp code directory, " (current-time-string) ".\n\n")
   (message "Formatting %s ..." lisp-code-directory)
   (delete-region (progn (beginning-of-line) (point))
                  (progn (end-of-line) (point)))
   (format-lisp-code-directory-line
    "Name" "Author" "Contact" "Description" "Date" "Version")
   (insert "\n")
   (insert-char ?- 79)
   (while (re-search-forward
           "\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)" nil t)
      (let ((name (buffer-substring (match-beginning 1) (match-end 1)))
            (author (buffer-substring (match-beginning 2) (match-end 2)))
            (contact (buffer-substring (match-beginning 3) (match-end 3)))
            (description (buffer-substring (match-beginning 4) (match-end 4)))
            (date (buffer-substring (match-beginning 5) (match-end 5)))
            (version (buffer-substring (match-beginning 6) (match-end 6))))
         (delete-region (progn (beginning-of-line) (point))
                        (progn (end-of-line) (point)))
         (format-lisp-code-directory-line
          name author contact description date version)))
   (goto-char (point-min))
   (center-line)
   (message "Formatting %s ... done" lisp-code-directory)
   (set-buffer-modified-p nil)
   (run-hooks 'lisp-dir-hook))

(defun lisp-dir-apropos (topic)
  "Display entries in Lisp Code Directory for TOPIC in separate window.
Calls value of lisp-dir-apropos-hook with no args if that value is non-nil."
  (interactive (list
                (read-string
                 (concat "Lisp Directory apropos (" (current-word) "): "))))
  (if (equal "" topic) (setq topic (current-word)))
  (save-excursion
    (set-buffer (get-buffer-create "*Lisp Directory Apropos*"))
    (fundamental-mode)
    (setq buffer-read-only nil)
    (erase-buffer)
    (buffer-flush-undo (current-buffer))
    (insert-file lisp-code-directory)
    (message "Searching for %s ..." topic)
    (delete-non-matching-lines topic)
    (insert "Emacs Lisp Code Apropos -- \"" topic "\"\n\n\n")
    (backward-char 1)
    (format-lisp-code-directory-line
     "Name" "Author" "Contact" "Description" "Date" "Version")
    (insert "\n")
    (insert-char ?- 79)
    (while (re-search-forward
            "\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)" nil t)
      (let ((name (buffer-substring (match-beginning 1) (match-end 1)))
            (author (buffer-substring (match-beginning 2) (match-end 2)))
            (contact (buffer-substring (match-beginning 3) (match-end 3)))
            (description (buffer-substring (match-beginning 4) (match-end 4)))
            (date (buffer-substring (match-beginning 5) (match-end 5)))
            (version (buffer-substring (match-beginning 6) (match-end 6))))
        (delete-region (progn (beginning-of-line) (point))
                       (progn (end-of-line) (point)))
        (format-lisp-code-directory-line
         name author contact description date version)))
    (goto-char (point-min))
    (center-line)
    (message "Searching for %s ... done" topic)
    (set-buffer-modified-p nil))
  (display-buffer "*Lisp Directory Apropos*")
  (run-hooks 'lisp-dir-apropos-hook))

(defun format-lisp-code-directory-line
  (name author contact description date version)
  "Format one line of GNU Emacs Lisp code directory.

Provided as a separate function for customizability.  Should not insert
final newline."
   (insert-at-column 0  name)
   (insert-at-column 17 description)
   (insert-at-column 56 author)
   (insert-at-column 4  contact)
   (insert-at-column 56 date)
   (insert-at-column 72 version))
   
(defun insert-at-column (col string)
   (if (> (current-column) col) (insert "\n"))
   (move-to-column-force col)
   (insert string))

;; Snatched from unix-apropos by Henry Kautz
(defun current-word ()
   "Word cursor is over, as a string."
   (save-excursion
      (let (beg end)
         (re-search-backward "\\w" nil 2)
         (re-search-backward "\\b" nil 2)
         (setq beg (point))
         (re-search-forward "\\w*\\b" nil 2)
         (setq end (point))
         (buffer-substring beg end))))
