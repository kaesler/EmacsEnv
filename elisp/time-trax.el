;;; time-trax.el --- automatically fill out a time sheet

;;; Copyright (C) 1998 Chris Bone
;;
;; Author: Chris Bone (bone@al.com.au)
;; Version: 0.4
;; Keywords:      timesheet
;;
;;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;  Pre-Requisites
;;
;;      This package will require the following packages to be
;;      available on the load-path:
;;
;;          easymenu (should already be so for 20.2)
;;          cl       (should already be so for 20.2)
;;
;;      Only tested in emacs 20.2
;;
;;; Commentary:
;;
;; This file enables a primitive timesheet filler-outer.  The premise
;; is that work consists of modifying files and that all the time
;; between one modification and the next was spent thinking about the
;; next modification.
;;
;; As we generally tend to group files into directories for different
;; projects we can derive a project name to charge the time against from
;; the file name attached to the buffer.
;;
;; at any time you can switch off the time accounting by invoking
;; `time-trax-end'.  restart with `time-trax'
;;
;; it can be kinda fun to pop up a frame and monitor the *time-trax*
;; buffer as you work
;;
;; when emacs exits (or when `time-trax-end' is invoked) the contents
;; of the *time-trax* buffer is appended to the file indicated by
;; the `time-trax-file'.
;;
;; the first emacs instance to call `time-trax' will create a lock
;; file denoted by `time-trax-lock-file' so that other emacs you start
;; won't double bill your time out
;;
;; use the menu to create new projects.  the file pattern should be
;; a regex, if the project has no file (ie: a 'meetings' project)
;; the pattern should be a garbage string (so that no files will match)
;;

;;; Installation:
;;
;; Place this file in a directory in your `load-path'.
;;
;; perform the following modifications to your .emacs file:
;;
;;  (require 'time-trax)
;;  (time-trax)
;;

;;; Change log:
;; 0.1  Initial revision posted to gnu.emacs.sources  -cb1/18/98.
;;
;; 0.2  get rid of stupid dependency on a function from gnus
;;
;;      make sure file patterns are sorted in order of descending
;;      length otherwise pattern sequences like the following will
;;      not return "proj" like they should:
;;
;;           ("/usr/people/me/src/.*"      . "generaldev")
;;           ("/usr/people/me/src/proj/.*" . "proj")
;;
;;      add ability to get rid of old project names or file patterns
;;      `time-trax-delete-project' and `time-trax-delete-pattern'
;;
;; 0.3  add customize support, options can be found in group:
;;           emacs->calendar->time trax
;;
;;      provide a default value for file pattern
;;
;;      moved to the (hopefully) portable easymenu package
;;
;; 0.4  debugging, `time-trax-assign-duration' could get called before
;;      a `time-trax-current-project' had been set
;;
;;      enhanced menus
;;
;;; TO DO:
;;
;;   be nice to write a digester that would create a report summarizing
;;   by project between two dates
;;
;;   allow comments, should make sure the project name is only matched
;;   if it's at the start of the line then make sure that any comment
;;   be indented.

;;; Code:

(require 'cl)
(require 'easymenu)

(eval-and-compile                       ; Removable for installation in
                                        ; Emacs 20.
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable))
      nil ;; We've got what we needed
    ;; We have the old custom-library, hack around it!
    (defmacro defgroup (&rest args)
      nil)
    (defmacro defcustom (var value doc &rest args)
      (` (defvar (, var) (, value) (, doc))))))

(defgroup time-trax nil
  "Keep track of where you've spent your time."
  :group 'calendar)

(defcustom time-trax-file (expand-file-name "~/.time-trax")
  "File to keep data in."
  :tag "Data file"
  :type 'file
  :group 'time-trax)

(defcustom time-trax-lock-file (expand-file-name "~/.time-trax.lock")
  "The first emacs to start time trax will create this file.
All other emacs will refuse to start time trax if the file exists.
When time trax shuts down it removes this file.
NB: this is to avoid double billing, if you need to move from emacs
to emacs you should stop time trax in the old emacs then start it
in the new one."
  :tag "Lock file"
  :type 'file
  :group 'time-trax)

(defcustom time-trax-config-file (expand-file-name "~/.time-trax.eld")
  "File to keep configuration in."
  :tag "Project config file"
  :type 'file
  :group 'time-trax)

(defcustom time-trax-day-marker "--------- "
  "Prefix to seperate days."
  :tag "Day seperator"
  :type 'string
  :group 'time-trax)

(defcustom time-trax-frame-config
  '((width . 80)
    (height . 10)
;;    (font . "-adobe-courier-medium-r-normal--10-72-100-100-m-60-iso8859-1")
    (minibuffer . nil))
  "The properities of the frame to use in `time-trax-view-timesheet'."
  :type 'sexp
  :tag "Frame parameters"
  :group 'time-trax)

;; no user configurable parameters below here
(defvar time-trax-buffer-to-project-alist nil
  "List of file name or buffer name regexps to project names.
At startup the list is loaded from `time-trax-config-file'.
Make add new projects from the menu or use `time-trax-new-project'.
When the time trax system is turned off the configuration file is
rewritten.")

(defvar time-trax-project-to-time-alist nil
  "All projects in this session that have been charged.")

(defvar time-trax-current-buffer nil
  "Last used buffer.")

(defvar time-trax-current-buffer-size nil
  "Size of last used buffer, see `time-trax-current-buffer'.")

(defvar time-trax-current-project nil
  "Project for the current buffer (to avoid looking it up all the time).")

(defvar time-trax-start-time (current-time)
  "The beginning of the current interval.
When a change occurs the difference between `current-time' and
`time-trax-start-time' yields the amount of time to charge to the
`time-trax-current-project'.")

(defvar time-trax-buffer nil
  "Buffer to keep results in.")

(defvar time-trax-frame nil
  "Frame that is displaying timesheet.")

(defvar time-trax-variable-list
  '(time-trax-buffer-to-project-alist)
  "Time-trax variables saved in the config file.")

(defun time-trax-diff-time (end-time start-time)
  "Return the number of seconds between END-TIME and START-TIME."
  (let ((et (car (cdr end-time)))
        (st (car (cdr start-time))))
    (if (< et st)
        (+ 65535 (- et st))
      (- et st))))

(defun time-trax-do-update-current-project ()
  ;; don't reset current-project... if the buffer doesn't map to a known
  ;; project then just use the most recent one
  (if (not (minibuffer-window-active-p (minibuffer-window)))
      (let ((name (if buffer-file-name
		      (file-name-sans-versions buffer-file-name)
		    (buffer-name)))
	    (keep-going t))
	(setq time-trax-current-buffer (buffer-name))
	(setq time-trax-current-buffer-size (buffer-size))
	(while keep-going
	  (setq keep-going nil)
	  (let ((alist time-trax-buffer-to-project-alist)
		(project nil))
	    (let ((case-fold-search
		   (memq system-type '(vax-vms windows-nt))))
	      (while (and (not project) alist)
		(if (string-match (car (car alist)) name)
		    (if (and (consp (cdr (car alist)))
			     (nth 2 (car alist)))
			(progn
			  (setq project (car (cdr (car alist)))
				name (substring name 0 (match-beginning 0))
				keep-going t))
		      (setq project (cdr (car alist))
			    keep-going nil)))
		(setq alist (cdr alist))))
	    (if project
		(setq time-trax-current-project project)))))))

(defun time-trax-insert-seconds-as-hhmmss (seconds)
  (let ((hours (/ seconds 3600)))
    (let ((mins  (/ (- seconds (* hours 3600)) 60)))
      (let ((secs  (- seconds (+ (* hours 3600) (* mins 60)))))
	(insert-string (format "%02d:%02d:%02d" hours mins secs))))))

(defun time-trax-repaint-buffer ()
  (save-excursion
    (set-buffer time-trax-buffer)
    (goto-char (point-min))
    (forward-line 1)
    (delete-region (point) (point-max))
    (let ((alist time-trax-project-to-time-alist))
      (while alist
	(let ((curr (car alist)))
	  (setq alist (cdr alist))
	  (time-trax-insert-seconds-as-hhmmss (cdr curr))
	  (insert-string (concat " " (car curr) "\n")))))))

(defun time-trax-assign-duration ()
  (if time-trax-current-project
      (let ((duration (time-trax-diff-time (current-time)
					   time-trax-start-time)))
	(if (> duration 0)
	    (let ((item (assoc time-trax-current-project
			       time-trax-project-to-time-alist)))
	      (if (not item)
		  (setq time-trax-project-to-time-alist
			(cons (cons time-trax-current-project duration)
			      time-trax-project-to-time-alist))
		(setcdr item (+ (cdr item) duration)))
	      (time-trax-repaint-buffer)))
	(setq time-trax-current-buffer-size (buffer-size))
	(setq time-trax-start-time (current-time)))))

(defun time-trax-assign-time-to-project ()
  (if (not (eq time-trax-current-buffer (buffer-name)))
      (time-trax-do-update-current-project)
    (if (not (eq time-trax-current-buffer-size (buffer-size)))
	(time-trax-assign-duration))))

(defun time-trax-insert-day-marker ()
  (goto-char (point-max))
  (insert time-trax-day-marker (format-time-string "%Y-%B-%e\t%A\t%T %Z\n")))

(defun time-trax-save-setup ()
  "Write the .time-trax.eld file."
  (save-excursion
    (let ((buf (generate-new-buffer "*time-trax.eld*"))
	  (variable nil))
      (set-buffer buf)
      (setq buffer-file-name time-trax-config-file)
      (let ((variables time-trax-variable-list))
	(while variables
	  (when (and (boundp (setq variable (pop variables)))
		     (symbol-value variable))
	    (insert "(setq " (symbol-name variable) " '")
	    (prin1 (symbol-value variable) (current-buffer))
	    (insert ")\n"))))
      (save-buffer)
      (kill-buffer nil))))

(defun time-trax-read-setup ()
  "Read the .time-trax.eld file."
  (load time-trax-config-file t t t))

(defun time-trax-touch-lock ()
  (save-excursion
    (let ((buf (generate-new-buffer "*time-trax.eld*")))
      (set-buffer buf)
      (setq buffer-file-name time-trax-lock-file)
      (time-trax-insert-day-marker)
      (save-buffer)
      (kill-buffer nil))))

(defun time-trax-make-project-completions ()
  (let ((alist time-trax-buffer-to-project-alist)
	(resalist nil))
    (while alist
      (let ((proj (cdr (car alist))))
	(if (not (assoc proj resalist))
	    (setq resalist (cons (cons proj nil) resalist))))
      (setq alist (cdr alist)))
    resalist))

(defun time-trax-regex-sort-pred (keya keyb)
  (> (length (car keya)) (length (car keyb))))

(defun time-trax-default-pattern ()
  (if buffer-file-name
      (concat (file-name-directory (file-name-sans-versions buffer-file-name))
	      ".*")
    (buffer-name)))

(defun time-trax-charge-project1 (project-name)
  (setq time-trax-current-project project-name)
  (time-trax-assign-duration))

(defun time-trax-delete-project1 (project-name)
  (let ((alist time-trax-buffer-to-project-alist))
    (setq time-trax-buffer-to-project-alist nil)
    (while alist
      (if (not (string= project-name (cdr (car alist))))
	  (setq time-trax-buffer-to-project-alist
		(cons (car alist) time-trax-buffer-to-project-alist)))
      (setq alist (cdr alist))))
  (setq time-trax-buffer-to-project-alist
	(sort time-trax-buffer-to-project-alist 'time-trax-regex-sort-pred))
  (time-trax-update-menus))

(defun time-trax-delete-pattern1 (file-regex)
  (let ((alist time-trax-buffer-to-project-alist))
    (setq time-trax-buffer-to-project-alist nil)
    (while alist
      (if (not (string= file-regex (car (car alist))))
	  (setq time-trax-buffer-to-project-alist
		(cons (car alist) time-trax-buffer-to-project-alist)))
      (setq alist (cdr alist))))
  (setq time-trax-buffer-to-project-alist
	(sort time-trax-buffer-to-project-alist 'time-trax-regex-sort-pred))
  (time-trax-update-menus))

;;; User level commands
;;
;;;###autoload
(defun time-trax-end ()
  "Save the `time-trax' data and cleanup."
  (interactive)
  (if (not time-trax-buffer)
      (message "time-trax not running")
    (save-excursion
      (time-trax-save-setup)
      (set-buffer time-trax-buffer)
      (if time-trax-frame (delete-frame time-trax-frame t))
      (append-to-file (point-min) (point-max) time-trax-file)
      (kill-buffer time-trax-buffer)
      (remove-hook 'post-command-hook 'time-trax-assign-time-to-project)
      (remove-hook 'kill-emacs-hook 'time-trax-end)
      (setq time-trax-buffer nil)
      (delete-file time-trax-lock-file))))

;;;###autoload
(defun time-trax ()
  "Start the `time-trax' system."
  (interactive)
  (if time-trax-buffer
      (message "time-trax already running")
    (if (file-exists-p time-trax-lock-file)
	(message "time-trax already running in another emacs")
      (save-excursion
	(time-trax-touch-lock)
	(time-trax-read-setup)
	(easy-menu-add time-trax-menu)
	(time-trax-update-menus)
	(setq time-trax-buffer (generate-new-buffer "*time-trax*"))
	(set-buffer time-trax-buffer)
	(time-trax-insert-day-marker)
	(add-hook 'post-command-hook 'time-trax-assign-time-to-project)
	(add-hook 'kill-emacs-hook 'time-trax-end)))))

;;;###autoload
(defun time-trax-new-project ()
  "Add a new file regex to project entry."
  (interactive)
  (let ((project-name (read-string "Project Name: "))
	(file-regex   (read-string "File Pattern: "
				   (time-trax-default-pattern))))
    (if (assoc file-regex time-trax-buffer-to-project-alist)
	nil
      (setq time-trax-buffer-to-project-alist
	    (sort (cons (cons file-regex project-name)
			time-trax-buffer-to-project-alist)
		  'time-trax-regex-sort-pred))
      (time-trax-update-menus))))

;;;###autoload
(defun time-trax-charge-project ()
  "Add the previous interval to a project of the users choosing.
Useful for assigning time to projects with no file-regex."
  (interactive)
  (let ((project-name (completing-read
		       "Project Name: "
		       (time-trax-make-project-completions) nil t)))
    (time-trax-charge-project1 project-name)))

;;;###autoload
(defun time-trax-delete-project ()
  "Remove a project."
  (interactive)
  (let ((project-name (completing-read
		       "Project to delete: "
		       (time-trax-make-project-completions) nil t)))
    (time-trax-delete-project1 project-name)))

;;;###autoload
(defun time-trax-delete-pattern ()
  "Remove a file matching pattern."
  (interactive)
  (let ((file-regex (completing-read
		     "File pattern to delete: "
		     time-trax-buffer-to-project-alist nil t)))
    (time-trax-delete-pattern1 file-regex)))

;;;###autoload
(defun time-trax-view-timesheet ()
  "Display the timesheet in a special frame."
  (interactive)
  (if time-trax-frame
      (raise-frame time-trax-frame)
    (setq time-trax-frame (make-frame time-trax-frame-config))
    (select-frame time-trax-frame)
    (switch-to-buffer "*time-trax*")))

;;; Menus and menu manipulation functions
;;

(easy-menu-define
 time-trax-menu global-map "Time-Trax"
 '("Time-Trax"
   ("Charge To Project"
     ["no projects" nil nil]
    )
   ["New Project"
    time-trax-new-project
    time-trax-buffer]
   ["View Timesheet"
    time-trax-view-timesheet
    time-trax-buffer]
   "---"
   ["Start Time Tracking"
    time-trax
    (not time-trax-buffer)]
   ["End Time Tracking"
    time-trax-end
    time-trax-buffer]
   "---"
   ("Delete Project"
    ["no projects" nil nil]
    )
   ("Delete Pattern"
    ["no patterns" nil nil]
    )))

(defun time-trax-update-menu (hlist menu-name empty-item callback)
  (let (items)
    (while hlist
      (let ((item (car (car hlist))))
	(setq items (append items
			    (list (vector item
					  (list callback item)
					  t)))))
      (setq hlist (cdr hlist)))
    (if (not items)
	(setq items (list (vector empty-item nil nil))))
    (easy-menu-change '("Time-Trax") menu-name items)))

;; this is not added to the `menu-bar-update-hook'
;; instead it's called when the lists change (which is less frequent)
(defun time-trax-update-menus ()
  (interactive)
  (time-trax-update-menu
   (time-trax-make-project-completions)
   "Charge To Project"
   "no projects"
   'time-trax-charge-project1)

  (time-trax-update-menu
   (time-trax-make-project-completions)
   "Delete Project"
   "no projects"
   'time-trax-delete-project1)

  (time-trax-update-menu
   time-trax-buffer-to-project-alist
   "Delete Pattern"
   "no patterns"
   'time-trax-delete-pattern1))

(provide 'time-trax)
;;; time-trax.el ends here
