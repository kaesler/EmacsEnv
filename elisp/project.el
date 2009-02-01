;;; PROJECT.EL --- Keep track of time devoted to projects

;; Copyright (C) 1996,2002, 2003 Alan Shutko <ats@acm.org>

;; Author: Alan Shutko <ats@acm.org>
;; Maintainer: Alan Shutko <ats@acm.org>
;; Created: Fri Jan 26 1996
;; Version: $Revision: 1.5 $
;; Keywords: calendar

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to Alan Shutko
;; <ats@acm.org>) or from the Free Software Foundation, Inc.,
;; 675 Mass Ave, Cambridge, MA 02139, USA.
;;

;;; LCD Archive Entry:
;;; project|Alan Shutko|ats@acm.org
;;; |Keep track of time devoted to projects
;;; |$Date: 2003/04/13 22:02:53 $|$Revision: 1.5 $|

;;; Commentary:

;;  project-summarize : Sumarize time spent on heirarchy in given time
;;  period.  Default to all projects, beginning of file, and present.
;;  Show subtotals ala du

;; project-report: Report time spent on heirarchy in terms of time
;; worked per day.  Option to subdivide.

;;; Todo:

;; Clean up naming of functions, especially stuff like project-elapsed-time.

;;; Code:

(require 'timezone)
(require 'appt)
(require 'cl)

(defgroup project nil
  "Project time tracking utility")

(defconst project-version (substring "$Revision: 1.5 $" 11 -2)
  "$Id: project.el,v 1.5 2003/04/13 22:02:53 ats Exp $.

Report bugs to: Alan Shutko <ats@acm.org>")

(defcustom project-file "~/Projects"
  "*File in which to store project time information."
  :type 'file )

(defvar project-current nil
  "Current project you are working on.
Set by `project-alist'.")

(defvar project-open nil
  "T if  project is open.
Set by `project-alist'.")

(defvar project-level-separator-char ?/
  "*Separater in project name delineating projects FIXME description.")

(defvar project-subproject-indent 2
  "*Number of spaces each level is indented in display.")

(defun project-prompt-for-project (&optional default)
  "Prompt for a project, offering completion from all projects defined.
May be passed a string for the DEFAULT project, or it will choose the
current project."
  (project-alist)
  (let ((completion-ignore-case t))
    (completing-read (concat "Project (" (or default project-current) "): ")
		     (project-alist)
		     nil
		     nil
		     nil
		     nil
		     (or default project-current))))

(defun project-format-entry (project op)
  "Formats a PROJECT entry for OP.
OP can be start or end."
  (concat (format-time-string "%Y-%m-%d %H:%M:%S" (current-time))
	  " -- "
	  project
	  " -- "
	  op "\n"))

(defun project-insert-entry (project op)
  "Insert a PROJECT entry for OP.
OP is usually start or stop, indicating that one has begun or
completed work on a project at the current time."
  (save-excursion
    (let ((buffer (find-file-noselect project-file)))
      (set-buffer buffer)
      (goto-char (point-max))
      (if (equal project "")
	  (setq project project-current))
      (insert (project-format-entry project op))
      (save-buffer))))

(defun project-start (project)
  "Log the beginning of PROJECT.
This function logs the start of a project to `project-file'.  If
another project is active when this function is called, it is ended."
  (interactive
   (list (project-prompt-for-project)))
  (if project-open
      (project-end project-open))
  (project-insert-entry project "start")
  (setq project-current project
	project-open t))


(defun project-end (project)
  "Log the ending of PROJECT.
This function logs the ending time of a project to `project-file'."
  (interactive
   (list (project-prompt-for-project)))
  (project-insert-entry project "end")
  (setq project-open nil project-current nil))

(defun project-encode-date (date)
  "Convert DATE to seconds since epoch.
If an empty string is passed, return today's date"
  (save-match-data
    (let* ((parse (timezone-parse-date date))
	   (date (mapcar (lambda (d) (and d (string-to-int d))) parse))
	   (time (mapcar 'string-to-int (timezone-parse-time (aref parse 3))))
	   (secs (encode-time (caddr time) (cadr time) (car time)
			      (caddr date) (cadr date) (car date) (nth 4 date))))
      (and secs
	   (+ (* (car secs) (float 65536))
	      (cadr secs))))))

(defun project-summarize (project start end)
  "Summarize time spent on PROJECT between START and END.
This command will show the time spent per project and per subproject.   It
will also show percentages of time spent on each project.  PROJECT can be
nil to display all projects."
  (interactive
   (list (project-prompt-for-project "all projects")
	 (read-from-minibuffer "Starting date: " )
	 (read-from-minibuffer "Ending date: ")))
  (if (or (null project)
	  (equal project "all projects"))
      (setq project ""))
  (if (or (null start)
	  (equal start ""))
      (setq start "1970-01-01 00:00:01"))
  (if (or (null end)
	  (equal end ""))
      (setq end (format-time-string "%Y-%m-%d %H:%M:%S" (current-time))))
  (let ((alist (project-sum-projects project start end)))
    (if alist
	(project-display-summary alist)
      (message "Nothing to display."))))

(defun project-summarize-today (project)
  "Summarize time spent on PROJECT today."
  (interactive
   (list (project-prompt-for-project "all projects")))
  (project-summarize project 
		     (format-time-string "%Y-%m-%d 00:00:00" (current-time))
		     (format-time-string "%Y-%m-%d 23:59:59" (current-time))))

(defun project-summarize-this-week (project)
  "Summarize time spent on PROJECT today."
  (interactive
   (list (project-prompt-for-project "all projects")))
  (let* ((decoded (decode-time))
	 (dow (nth 6 decoded))
	 (
	 (year (sixth decoded))
	 (month (fifth decoded))
         ))))


(defun project-display-summary (alist)
  "Temporary display summary.
Argument ALIST projects to display in form ((\"project\" . seconds))."
  (save-excursion
    (with-output-to-temp-buffer "*Project Summary*"
      (let ((alist (sort alist 'project-compare-alist-elements))
	    (max-name-length 0))
	(dolist (elt alist)
	  (setf (car elt) (project-nested-subproject-name (car elt))
		max-name-length (max max-name-length (length (car elt)))))
	(with-current-buffer standard-output
	  (dolist (elt alist)
	    (insert (project-format-summary-entry elt max-name-length)
		    "\n")))))))
  
  

(defun project-format-summary-entry (entry name-length)
  "Format a project ENTRY of form (\"project\" . seconds).
NAME-LENGTH is the amount of space the name should take up (to line up
with other items."
  (let ((project (car entry))
	(time (cdr entry))
	(format-string (format "%%-%ds: %%s" name-length)))
    (format format-string project (project-elapsed-time time))))
  
(defun project-sum-projects (project start-time end-time)
  "Sum all projects under PROJECT between START-TIME and END-TIME."
  (save-excursion
    (set-buffer (find-file-noselect project-file))
    (goto-char (point-min))
    (let ((regexp (concat
		   "^\\([0-9]+-[0-9]+-[0-9]+[ \t]+[0-9]+:[0-9]+:[0-9]+\\)[ \t]+--[ \t]+\\("
		   project
		   "[^-]*\\)[ \t]+--[ \t]+\\(start\\|end\\)$"))
	  (start-time (project-encode-date start-time))
	  (end-time (project-encode-date end-time))
	  alist
	  start
	  end)
      (while (and (re-search-forward regexp nil t)
		  (>= end-time
		     (project-encode-date (match-string 1))))
	(if (<= start-time (project-encode-date (match-string 1)))
	    (progn
	      (if (equal (match-string 3) "end")
		  (error "Project ending without a beginning on %s"
			 (what-line)))
	      (setq start (match-string 1))
	      (if (and (re-search-forward regexp nil t)
		       (equal (match-string 3) "start"))
		  (error
		   "Previous project didn't end.  New project started on %s"
		   (what-line)))
	      (setq end (match-string 1))
	      (setq alist
		    (project-add-time-to-alist (match-string 2)
					       (- (project-encode-date end)
						  (project-encode-date start))
				   alist)))))
	  alist)))

(defun project-add-time-to-alist (project secs alist)
  "Given PROJECT, add the time given in SECS to the project ALIST.
If a PROJECT is already in the ALIST, the time given will be added to
the existing time.  Otherwise, an entry will be added to the alist.

If `project-level-separator-char' is non-nil, this function will also add
the time for a project to all the parent projects."
  (let ((element (assoc project alist))
	(parent (project-parent project)))
    (if element
	(setcdr element (+ secs (cdr element)))
      (setq alist (cons (cons project secs) alist)))
    (if parent
	(project-add-time-to-alist parent secs alist)
      alist)))

		     

(defun project-report (project)
  "Create a report of time spent on a specific PROJECT by day."
(interactive
   (list (project-prompt-for-project)))
  (save-excursion
    (let ((day-alist)
	  (tmp)
	  (buffer (find-file-noselect project-file))
	  (tmp-buffer (get-buffer-create " *Projects*"))
	  (out-buffer (get-buffer-create "*Project Report*")))
      (if (equal project "")
	  (setq project project-current))
      (set-buffer buffer)
      (save-excursion (set-buffer tmp-buffer) (erase-buffer))
      (save-excursion (set-buffer out-buffer) (erase-buffer))
      (goto-char (point-min))
      (while (re-search-forward (concat "^\\([0-9]+-[0-9]+-[0-9]+\\)[ \t]+\\([0-9]+:[0-9]+:[0-9]+\\)[ \t]+--[ \t]+"
					project
					"[ \t]+--[ \t]+start$")
				nil t)
	(let ((date (match-string 1))
	      (start-time (match-string 2))
	      (end-time))
	  (if (re-search-forward (concat "^\\([0-9]+-[0-9]+-[0-9]+\\)[ \t]+\\([0-9]+:[0-9]+:[0-9]+\\)[ \t]+--[ \t]+"
					 project
					 "[ \t]+--[ \t]+end$")
				 nil t)
	      (setq end-time (match-string 2))
	    (setq end-time (format-time-string "%R" (current-time))))
	  (save-excursion
	    (set-buffer tmp-buffer)
	    (goto-char (point-max))
	    (insert date " " (number-to-string (- (appt-convert-time end-time)
						  (appt-convert-time start-time))) "\n"))))
      (save-excursion			;Create totals
	(set-buffer tmp-buffer)
	(goto-char (point-min))
	(while (re-search-forward "^\\([0-9]+-[0-9]+-[0-9]+\\)[ \t]+\\([0-9]+\\)$"
				  nil t) ;find all days
	  (let ((date (match-string 1))
		(total (string-to-number (match-string 2))))
	    (while (re-search-forward (concat "^" date "[ \t]+\\([0-9]+\\)$")
				      nil t) ;find other times with same days
	      (setq total (+ total (string-to-number (match-string 1)))))
	    (save-excursion		;should have total number minutes
	      (set-buffer out-buffer)
	      (insert date " -- "
		      (format "%.1f" (/ total 60.0)) "\n" )))))
      (pop-to-buffer out-buffer))))



(defun project-alist ()
  "Return the project names from the project file."
  (save-excursion
    (let ((buffer (find-file-noselect project-file))
	  (alist))
      (set-buffer buffer)
      (goto-char (point-min))
      (while (re-search-forward
	      "^\\([0-9]+-[0-9]+-[0-9]+\\)[ \t]+\\([0-9]+:[0-9]+:[0-9]+\\\)[ \t]+--[ \t]+\\([^-]+\\)[ \t]+--[ \t]+\\(start\\|end\\)$"
	      nil t)
	(setq project-current (match-string 3))
	(if (assoc (match-string 3) alist)
	    nil
	  (setq alist (cons (cons (match-string 3) t) alist)))
	(cond ((equal (match-string 4) "start")
	       (setq project-open (match-string 3)))
	      ((equal (match-string 4) "end")
	       (setq project-open nil))))
      alist)))

(defun project-elapsed-interval (start end)
  "Calculate elapsed time between START date and END date."
  (let* ((startsec (project-encode-date start))
	 (endsec (project-encode-date end))
	 (sec (- endsec startsec)))
    (project-elapsed-time sec)))
	 

(defun project-elapsed-time (sec)
  "Return formatted version of elapsed time, given SEC."
  (let (days hours min string)
    ;; Calculate the various unit times
    (setq min (floor (/ sec 60))
	  sec (mod sec 60)
	  hours (floor (/ min 60))
	  min (mod min 60)
	  days (floor (/ hours 24))
	  hours (mod hours 24))
    ;; Generate formatted string containing only units which aren't 0
    (setq string
	  (concat
	   (if (plusp days)
	     (format "%d days, " days))
	   (if (plusp hours)
	     (format "%d hours, " hours))
	   (if (plusp min)
	     (format "%d min, " min))
	   (if (plusp sec)
	     (format "%d sec" sec))))
    ;; Remove any trailing commas
    (if (string-match "[a-zA-Z0-9 ]+\\(, [a-zA-Z0-9 ]+\\)*" string)
	(match-string 0 string)
      "none")))

(defun project-parent (project)
  "Return the parent of PROJECT.
A parent is the project left after removing the last
`project-level-separator-char' and everything after.  If this project has
no parent, return NIL."
  (save-match-data
    (if (string-match (concat "\\`\\(.*\\)["
			      (char-to-string project-level-separator-char)
			      "][^"
			      (char-to-string project-level-separator-char)
			      "]+\\'")
		      project)
	(match-string 1 project)
      nil)))

(defun project-subproject-name (project)
  "Return the subproject part of the name of PROJECT."
  (save-match-data
    (if (string-match (concat "["
			      (char-to-string project-level-separator-char)
			      "]"
			      "\\([^"
			      (char-to-string project-level-separator-char)
			      "]*\\)$")
		      project)
	(match-string 1 project)
      project)))

(defun project-level (project)
  "Return the level of a PROJECT.
The level is the count of project-level-separator-chars in the
project."
  (let ((count 0)
	(start 0))
    (while (string-match (concat "[" (char-to-string project-level-separator-char) "]")
			 project start)
      (setq count (1+ count)
	    start (match-end 0)))
    count))

(defun project-nested-subproject-name (project)
  "Return the subproject part of the name of this PROJECT, indented.
Indents by `project-subproject-indent' * level."
  (let* ((level (project-level project))
	 (subproject (project-subproject-name project))
	 (indent (make-string (* level project-subproject-indent)
			      ?\ )))
    (concat indent subproject)))

(defun project-compare-alist-elements (el1 el2)
  "Compare the string cdr of EL1 and EL2."
  (string< (car el1) (car el2)))
(provide 'project)
;;; PROJECT.EL ends here
