The scheduler has been changed quite a bit.  It's now a minor mode,
and support integration with timeclock (if that module is available on
the system).  This integration is silent right now.  If timeclock is
on your load-path, all of the right things will happen for it to be
available under the C-c t keybindings.

The scheduler supports overdue estimates, extra estimates, "completed"
tasks, multiple nesting of projects, etc.  See the "Changes" section
for full details.

And my copyright strings have been updated.  :)

John Wiegley <johnw@oneworld.new-era.com>

----------[ cut here ]------------------------------------------------

;;; schedule.el -- Simple schedule maintainer

;; Copyright (C) 1999 John Wiegley.

;; Author: John Wiegley <johnw@oneworld.new-era.com>
;; Created: 20 Jan 1999
;; Version: 1.2
;; Keywords: pim, todo, schedule
;; X-URL: http://oneworld.new-era.com/johnw/emacs.html

;; The program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;; Commentary:

;; This module works with outline-mode, calendar and diary to provide a
;; simple way of keeping track of schedules.  It's recommended that
;; you autoload the `schedule-mode' command in your .emacs file:
;;
;;    (autoload 'schedule-mode "schedule" nil t)
;;
;; If you have timeclock.el on your `load-path', schedule will enable
;; timeclock integration (with C-c t commands), which makes it easier
;; to keep track of the how much time you spend on each task (and
;; whether you're making your estimates).

;; A typical schedule file might look like the following:
;;
;;    -*-mode: outline; mode: schedule-*-
;;    
;;    * +(Apr09) 1.1d  73%  Small project [2.1d]
;;    ** (Apr07)   1h   0%  Begin the work
;;    ** [Apr07]   4h 100%  Think about the work
;;    ** (Apr07)   1h  80%  Modify the work
;;    ** (Apr09)   3h 110%  Final procrastination [2d]
;;    *  (Apr12)   1d  20%  Some small task
;;    *  (Apr13)   1d   0%  Another small task
;;    *  (Apr14)   1d   0%  Make sure that I did the other tasks!
;;
;;    * +(Apr14)   4h   7%  Very small project [3h]
;;    ** (Apr14)   1h  13%  Sub-task 1
;;    ** (Apr14)   3h   0%  Sub-task 2
;;    *  (Apr15)   4h   0%  Maybe I'll go check my e-mail now...
;;    *  (Apr16)   1d   0%  Finish writing schedule.el
;;
;; Entries can be grouped as project and sub-projects, to any level of
;; nesting (although it stops looking so aesthetic once you go very
;; deep).  Completed tasks have their date bracketed by square
;; brackets.  This records the date that the task was actually
;; finished.  A date in parentheses is a projected date, and will
;; always be on or after the present day.
;;
;; As you work on a task, you must update the percentage complete
;; figure by hand.  Alternately, you can use my timeclock.el mode (see
;; the URL mentioned at the top of this file), which will update it
;; automatically whenever you "checkout" of, or "change" your current
;; working task (provided that point is in the schedule buffer, and
;; located on the same line as the task you want to checkout of).
;;
;; If you want to see how much time a task is expected to take, add
;; the string " []" anywhere within the task (typically at the end).
;; The scheduler will update this figure depending on how much time is
;; remaining to complete the task (based on current projections).
;;
;; If a task runs over-schedule, you can use the " []" string to
;; indicate how much more time is left (an additional estimate).
;; Unless you use timeclock mode to update your task strings for you,
;; you will have to modify both the percentage figure, and the time
;; remaining string, whenever you do work on the task.

;; There are many pieces of data being displayed on each line of a
;; schedule file.  They are:
;;
;;   1) The date when it's expected that this task will be completed,
;;      in parentheses.
;;
;;   2) The date that the task was actually completed, in square
;;      brackets.
;;
;;   3) The original time estimated for the task.
;;
;;   4) The percentage complete.  If this is different from 100%
;;      (either above or below) at the time that the task is finished,
;;      it reflects the amount of time above or below your original
;;      estimate that it took you to complete the task.  Beware.  This
;;      can be very sobering.
;;
;;   5) A description of the task, or project.
;;
;;   6) In square brackets within the task description, the amount of
;;      remaining for "on schedule" tasks, or the amount of extra time
;;      needed for overdue tasks.  "Extra time" is figured into the
;;      amount of time needed for the project as a whole, and the date
;;      projection for when its believed that the task will be
;;      finished.
;;
;; The usefulness of preserving the original time estimate, having the
;; percentage complete figure reflect the amount of time you actually
;; spent, and using the time remaining string to indicate further time
;; estimates, allows you both to keep your manager happy by projecting
;; a schedule which remains valid, and also lets you (and your
;; manager) know if you're learning how to estimate your time better
;; or not.  Whether you want this feature is up to you.  Some say that
;; ignorance is bliss, others that the unexamined life...

;; Use the command `schedule-create-item' (bound to C-c s c in
;; schedule-mode) to create a new scheduling item.  It will use the
;; same nesting depth as the entry closest after point.

;; Whenever the file is saved, the time estimates will be recomputed
;; based on whatever changes have been made.  Alternatively, you can
;; call `schedule-refresh' to do that at any time.
;;
;; Note that this mode is very sensitive to the format of the file.
;; Sub-projects can only go one level deep right now.  I also don't have
;; integration with diary-mode in yet.  When that's complete, the time
;; estimator will also skip over meetings -- where you obviously don't
;; get any work done!  :)


;;; Changes from 1.1:

;; * Now a minor mode, intended to be used with outlines, although
;;   this is not required.  If not in an outline, no sub-projects are
;;   allowed, and task text can begin anywhere within a line.
;;
;; * Sub-projects can be nested to any depth.
;;
;; * If timeclock.el is available, timeclock integration will be
;;   enabled.  Type C-c t C-h to see the timeclock keybindings while
;;   visiting a schedule buffer.
;;
;; * Added `schedule-create-item' for creating new task item.  A new
;;   task item appears above the one under point, and inherits its
;;   level of nesting.  If the outline level doesn't come out right,
;;   just fix it manually and run `schedule-refresh' again (C-c s r).
;;
;; * Added `schedule-time-left-regexp'.  By default, if you put the
;;   string " []" anywhere within a task description, the space
;;   between the brackets will be updated with the time left to
;;   complete the current task, based on the data provided (if
;;   percentage complete figure is less than 100).  If the percentage
;;   complete figure is greater than 100, and the task has not yet
;;   been completed, the meaning of the "time left" string changes.
;;   Its meaning is now to indicate the amount of time that's really
;;   left before the task will be completed, over and above what
;;   you've worked so far.  Any time-left string at the project level
;;   will use this revised estimated when computing its own time-left
;;   string.
;;
;;   My recommendation is to add a time-left string to all project
;;   heading lines, and to individual task item lines when they exceed
;;   100% complete.  Otherwise, the time-estimating value of the
;;   scheduler diminishes.  Time-left strings are the only way to
;;   reflect the "new" (slipped) ending date for under-estimated task.
;;   And when used in conjunction with timeclock, they are easily
;;   maintained.


;;; Header:

(require 'calendar)
(require 'holidays)
(require 'outline)

(provide 'schedule)

(defconst schedule-version "1.2"
  "This version of schedule.")

(defgroup schedule nil
  "Keeping track of project schedules."
  :group 'data)


;;; User Variables:

(defcustom schedule-load-hook nil
  "*Hook that gets run after schedule has been loaded."
  :type 'hook
  :group 'schedule)

(defcustom schedule-mode-hook nil
  "*A series of function to be run upon entering schedule mode."
  :type 'hook
  :group 'schedule)

(defcustom schedule-day-begin 9
  "The hour of the day when things begin."
  :type 'integer
  :group 'schedule)

(defcustom schedule-day-end 17
  "The hour of the day when things end."
  :type 'integer
  :group 'schedule)

(defcustom schedule-days-off '(0 6)
  "A list of days of the week when no work should occur.
Sunday is day 0."
  :type '(set integer)
  :group 'schedule)

(defcustom schedule-time-left-regexp
  '("\\s-+\\[\\(.*\\)\\]" 1)
  "*A regexp which gets updated with the time left for each task.
If this regexp is found anywhere in the description text for a task,
it will be updated with the time left for that task each time the
schedule is refreshed.  Marking the task as done via `timeclock-out'
(when timeclock is available) will cause the entire text string
specified by this regexp to be deleted (since it's no longer needed).

Similar to `schedule-regexp', this variable should be a list comprised
of a regular expression string, and a parenthesis group index to
indicate the text to be updated."
  :type '(list regexp (set integer))
  :group 'schedule)

(defcustom schedule-regexp
  (list (concat "\\s-*\\(\\+\\)?\\(\\(\\[\\|(\\)"
                "[A-Za-z0-9]*\\(\\]\\|)\\)\\)"
                "\\(\\s-*[0-9.]+[hdw]\\)"
                "\\(\\s-*[0-9]+\\)%"
                "\\s-*\\(.*\\)")
        '(1 2 5 6 7))
  "*A regexp for matching lines within a schedule file.
The first element of this list should be the regexp to match with.
The second identifies the meaningful parenthesis groups within the
regular expression, in this order:

  (HEADING-P DATE DURATION PERCENT-COMPLETE)

Where HEADING-P is a non-empty string if the line identifies a project
heading (rather than a regular schedule entry).  DATE is the
modifiable section of the line where date calculations should be
updated.  DURATION is the time code representing the length of time it
is estimated that this task will take.  And PERCENT-COMPLETE states
how much of the task has been completed so far.

Note that DATE should include the delimiter text.  Parentheses are
used for tasks that have not yet been completed, while square brackets
are used for completed entries.  This allows PERCENT-COMPLETE to be
above or below 100%, and yet the task can still be marked as complete
\(this becomes significant when tracking the variance from projected
durations in the schedule).

Finally, if `outline-mode' is the major mode, then the
`outline-regexp' string will be expected at the beginning of each
line.  If `outline-mode' is not being used as the major mode, project
groups will be ignored, even if HEADING-P identifies a non-empty
string."
  :type '(list regexp (set integer))
  :group 'schedule)


;;; User Functions:

;;;###autoload
(defun schedule-refresh (&optional no-message)
  "Refresh the current schedule, computing projected completion dates
based on the amount of time available each day, the number of days a
week you plan to work, and factoring out any holidays that have been
recorded in the Emacs calendar.  If NO-MESSAGE is non-nil, don't
print anything to the minibuffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let* ((regexp (schedule-determine-regexp))
           (sum (schedule-update-group
                 (schedule-align-day (current-time))
                 (car regexp) (cadr regexp) 0)))
      (unless no-message
        (message "Schedule ends on %s, requiring %s, with %d%% done so far."
                 (nth 3 sum) (schedule-hours-to-string (nth 1 sum))
                 (nth 2 sum)))))
  nil)

;;;###autoload
(defun schedule-create-item (description duration &optional project)
  "Create a new schedule item before point."
  (interactive "sDescription of task: \nsTime required: ")
  (let ((regexp (schedule-determine-regexp)) depth)
    (save-excursion
      (beginning-of-line)
      (when (eq major-mode 'outline-mode)
        (save-excursion
          (if (re-search-forward (car regexp) nil t)
              (setq depth (match-string 1)))))
      (if depth
          (insert depth " "))
      (if project
          (insert "+"))
      (insert (format "() %4s   0%%  %s\n" duration description)))
    (schedule-refresh)))


;;; Internal Functions:

(defun schedule-determine-regexp ()
  (let ((regexp (car schedule-regexp))
        (paren-groups (cadr schedule-regexp)))
    ;; if we're in outline major mode, use the `outline-regexp' to
    ;; differentiate task items at different levels; also, since the
    ;; outline regexp is being recorded in a parenthesis group, we
    ;; need to increase all the `paren-groups' by 1
    (if (eq major-mode 'outline-mode)
        (setq regexp
              (concat "^\\(" outline-regexp "\\)" regexp)
              paren-groups (mapcar '1+ paren-groups)))
    (list regexp paren-groups)))
      
(defun schedule-hours (code)
  "Convert the given CODE into an integer representing a number of
hours."
  (if (string-match "\\([0-9.]+\\)\\([hdw]\\)" code)
      (let ((amount (string-to-number (match-string 1 code)))
            (kind (match-string 2 code)))
        (cond ((equal kind "h")
               amount)
              ((equal kind "d")
               (* 8 amount))
              ((equal kind "w")
               (* 5 8 amount))
              (t
               (error "Invalid hours code."))))
    (error "Invalid hours code.")))

(defun schedule-time-to-seconds (time)
  "Convert TIME to a floating point number."
  (+ (* (car time) 65536.0)
     (cadr time)
     (/ (or (car (cdr (cdr time))) 0) 1000000.0)))

(defun schedule-seconds-to-time (seconds)
  "Convert SECONDS (a floating point number) to an Emacs time structure."
  (list (floor seconds 65536)
	(floor (mod seconds 65536))
	(floor (* (- seconds (ffloor seconds)) 1000000))))

(defun schedule-hours-to-string (hours)
  "Convert HOURS to a compact time string."
  (let ((daylen (- schedule-day-end schedule-day-begin))
        (weeklen (- 7 (length schedule-days-off))))
    (cond ((< hours daylen)
           (format "%dh" hours))
          ((< (/ hours daylen) weeklen)
           (format "%.1fd" (/ (float hours) daylen)))
          (t
           (format "%.1fw" (/ (float hours) daylen weeklen))))))

(defun schedule-time-date (then)
  "Return the DATE part of THEN, in calendar friendly format."
  (let* ((elems (decode-time then)))
    (list (nth 4 elems)
          (nth 3 elems)
          (nth 5 elems))))

(defun schedule-off-or-holiday-p (then)
  "Returns 't' if THEN is a weekend or holiday."
  (let* ((date (schedule-time-date then)))
    (or (member (calendar-day-of-week date) schedule-days-off)
        (check-calendar-holidays date))))

(defun schedule-align-day (then)
  "Given THEN, move it ahead to the next valid moment in time."
  (let ((hour (nth 2 (decode-time then))))
    (if (>= hour schedule-day-end)
        (setq then (schedule-seconds-to-time
                    (+ (schedule-time-to-seconds then)
                       (* (+ (- 24 hour) schedule-day-begin) 3600))))))
  (while (schedule-off-or-holiday-p then)
    (setq then (schedule-seconds-to-time
                (+ (schedule-time-to-seconds then) 86400))))
  then)

(defun schedule-advance-hour (then count)
  "Advance THEN by COUNT hours, skipping the weekends and holidays."
  (while (> count 0)
    (let ((elems (decode-time then)))
      (setcar (nthcdr 2 elems) (1+ (nth 2 elems)))
      (setq then (apply 'encode-time elems))
      (if (>= (nth 2 elems) schedule-day-end)
          (setq then (schedule-align-day then))))
    (setq count (1- count)))
  then)

(defun schedule-update-group (begin-time regexp groups depth)
  "Refresh the current schedule project after point.
Returns the computed totals for that project in a list of the form:

  (COUNT HOURS COMPLETE FINAL-DATE UNDONE-COUNT NEW-TIME TIME-LEFT)

COUNT is the number of tasks in the project.  HOURS is the total
number of hours predicted that the project will take.  COMPLETE is the
percentage complete, based on the hours worked so far, and the
predicted duration.  FINAL-DATE is the estimated date on which work
will be completed.  UNDONE-COUNT is the number of tasks that have yet
to be completed for the project.  NEW-TIME is the next available
working time, which is essentially BEGIN-TIME plus the amount of time
necessary for all the tasks in this project.  TIME-LEFT is the
computed (and adjusted) amount of time left on this project.  If the
users embeds strings watched for by `schedule-time-left-regexp', and
the task has exceeded 100% complete, then this is only way to record
the amount of time that is now believed to be left, and yet still
track how far over estimates the project ended up being."
  (let ((now begin-time)
        (hours 0)
        (complete 0)
        (count 0)
        (undone 0)
        (time-left 0)
        final-date level begin)
    (while
        (and (setq begin (point))
             (re-search-forward regexp nil t)
             (not (and (eq major-mode 'outline-mode)
                       (<= (setq level (length (match-string 1)))
                           depth)
                       (goto-char begin))))
      (let* ((heading-p (match-string (nth 0 groups)))
             (date-beg
              (copy-marker (match-beginning (nth 1 groups))))
             (date-end
              (copy-marker (match-end (nth 1 groups)))))

        (if (and heading-p level)
            (let* ((h-beg (match-beginning (nth 2 groups)))
                   (h-end (match-end (nth 2 groups)))
                   (c-beg (match-beginning (nth 3 groups)))
                   (c-end (match-end (nth 3 groups)))
                   (project (schedule-update-group
                             now regexp groups level)))
              (if (> (nth 0 project) 0)
                  (save-excursion
                    (goto-char c-beg)
                    (delete-region c-beg c-end)
                    (insert " " (format "%3.f" (nth 2 project)))

                    (goto-char h-beg)
                    (delete-region h-beg h-end)
                    (insert " " (format "%4s"
                                        (schedule-hours-to-string
                                         (nth 1 project))))

                    (goto-char date-beg)
                    (delete-region date-beg date-end)
                    (if (= (nth 4 project) 0)
                        (progn
                          (insert "[Ended]")
                          (schedule-update-time-left nil nil))
                      (insert "(" (nth 3 project) ")")
                      (schedule-update-time-left
                       (schedule-hours-to-string (nth 6 project)) nil))

                    (setq now        (nth 5 project)
                          hours      (+ hours (nth 1 project))
                          complete   (+ complete (nth 2 project))
                          count      (+ count (nth 0 project))
                          undone     (+ undone (nth 4 project))
                          time-left  (+ time-left (nth 6 project))
                          final-date (nth 3 project)))))
          (let* ((duration
                  (match-string (nth 2 groups)))
                 (task-complete
                  (float (string-to-number
                          (match-string (nth 3 groups)))))
                 (task-hours (schedule-hours duration))
                 (prev-hours task-hours))
                
            (setq hours    (+ hours task-hours)
                  complete (+ complete task-complete)
                  count    (1+ count))
            
            ;; if the date of the task is enclosed in square
            ;; brackets, it means that the task has been completed,
            ;; independent of the percentage complete figure
            (goto-char date-beg)
            (unless (looking-at "\\[")
              (setq undone (1+ undone))
              
              (if (> task-complete 0.0)
                  (setq task-hours
                        (- task-hours
                           (* task-hours
                              (/ task-complete 100.0)))))

              (setq task-hours (schedule-update-time-left
                                task-hours task-complete)
                    time-left  (+ time-left task-hours))

              (setq now (schedule-advance-hour now task-hours))
          
              (let ((then (format-time-string "%b%d" now)))
                (delete-region date-beg date-end)
                (insert "(" then ")")
                (setq final-date then)))

            (end-of-line)))))

    (list count hours
          (if (> count 0)
              (/ complete count)
            complete)
          final-date undone now time-left)))

(defun schedule-update-time-left (hours complete)
  "Update the time left string for the task under point."
  (when schedule-time-left-regexp
    (save-excursion
      (let ((groups (schedule-task-under-point-p)))
        (when groups
          (goto-char (match-end (nth 3 groups)))
          (let ((eol (save-excursion (end-of-line) (point))))
            (if (re-search-forward
                 (car schedule-time-left-regexp) eol t)
                (if (not hours)
                    (delete-region (match-beginning 0)
                                   (match-end 0))
                  (let ((b (match-beginning
                            (cadr schedule-time-left-regexp)))
                        (e (match-end
                            (cadr schedule-time-left-regexp))))
                    (if (and complete (>= complete 100.0))
                        (let ((str (buffer-substring b e)))
                          (if (string-match "^[0-9.]+[hdw]$" str)
                              (setq hours (schedule-hours str))))
                      (goto-char b)
                      (delete-region b e)
                      (if complete
                          (insert (schedule-hours-to-string hours))
                        (insert hours)))))))))))
  (and (numberp hours) (max hours 0)))

(defvar schedule-mode nil
  "Non-nil if using schedule mode as a minor mode of some other
mode.")

(make-variable-buffer-local 'schedule-mode)
(or (assq 'schedule-mode minor-mode-alist)
    (setq minor-mode-alist
          (append minor-mode-alist
                  (list '(schedule-mode " Schd")))))

(defun schedule-task-under-point-p ()
  "Returns non-nil if there is a task under point.
The return value is the set of parenthesis groups that identify the
contents of the task."
  (let ((regexp (schedule-determine-regexp)))
    (save-excursion
      (beginning-of-line)
      (and (looking-at (car regexp))
           (cadr regexp)))))
  
(defun schedule-apply-task-as-project (timeclock-func)
  "Call TIMECLOCK-FUNC, using task under point as the project."
  (let ((groups (schedule-task-under-point-p)))
    (if groups
        (funcall timeclock-func (not (null current-prefix-arg))
                 (match-string (nth 4 groups))))))

;;; integrate with timeclock.el, if present

(defvar schedule-timeclock-available
  (and (require 'timeclock "timeclock" t)
       (equal timeclock-version "1.1"))
  "Non-nil if timeclock mode is available for use.
To make timeclock available, simply put it somewhere on your
`load-path'.  The version of schedule.el is meant to work with version
1.1 of timeclock.el.")

(defun schedule-timeclock-out ()
  (interactive)
  (schedule-apply-task-as-project 'timeclock-out))

(defun schedule-timeclock-change ()
  (interactive)
  (schedule-apply-task-as-project 'timeclock-change))

(defun schedule-update-task ()
  "Update the current task with the value from `timeclock-last-period'."
  (let ((groups (schedule-task-under-point-p)))
    (if groups
        (let* ((duration
                (match-string (nth 2 groups)))
               (task-complete
                (float (string-to-number
                        (match-string (nth 3 groups)))))
               (prev-complete task-complete)
               (c-beg (match-beginning (nth 3 groups)))
               (c-end (match-end (nth 3 groups)))
               (task-hours (float (schedule-hours duration)))
               (hours-so-far
                (if (> task-complete 0.0)
                    (* task-hours
                       (/ task-complete 100.0))
                  0.0))
               (period (timeclock-last-period))
               (hours-spent (/ (float period) 60.0 60.0)))
          (setq hours-so-far (+ hours-so-far hours-spent)
                task-complete (* (/ hours-so-far task-hours) 100.0))
          (save-excursion
            (goto-char c-beg)
            (delete-region c-beg c-end)
            (insert " " (format "%3.f" task-complete)))
          (if (and (< prev-complete 100.0)
                   (>= task-complete 100.0))
              (schedule-update-time-left "???" nil)
            (if (> task-complete 0.0)
                (setq task-hours
                      (- task-hours
                         (* task-hours
                            (/ task-complete 100.0)))))
            (setq task-hours
                  (schedule-update-time-left task-hours
                                             task-complete))
            (when (>= task-complete 100.0)
              (setq task-hours (- task-hours hours-spent))
              (schedule-update-time-left
               (if (<= task-hours 0)
                   "???"
                 (schedule-hours-to-string task-hours)) nil)))
          (schedule-refresh)))))

(defun schedule-complete-task ()
  "Mark the current task as completed today.
If it's already been completed, just update the date string."
  (let ((groups (schedule-task-under-point-p)))
    (if groups
        (let ((date-beg (match-beginning (nth 1 groups)))
              (date-end (match-end (nth 1 groups)))
              (then (format-time-string "%b%d" (current-time))))
          (save-excursion
            (goto-char date-beg)
            (delete-region date-beg date-end)
            (insert "[" then "]")
            (schedule-update-time-left nil nil))))))

(defvar schedule-mode-map ())
(if schedule-mode-map
    ()
  (setq schedule-mode-map (make-sparse-keymap))
  (define-key schedule-mode-map "\C-csc" 'schedule-create-item)
  (define-key schedule-mode-map "\C-csr" 'schedule-refresh)
  (when schedule-timeclock-available
    (define-key schedule-mode-map "\C-cti" 'timeclock-in)
    (define-key schedule-mode-map "\C-cto" 'schedule-timeclock-out)
    (define-key schedule-mode-map "\C-ctc" 'schedule-timeclock-change)))

(or (assq 'schedule-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'schedule-mode schedule-mode-map)
		minor-mode-map-alist)))

;;;###autoload
(defun schedule-mode (&optional arg)
  "Toggle schedule mode.
With arg, turn schedule mode on if arg is positive, off otherwise.
\\<schedule-mode-map>A schedule is an outline with certain special
entries, whose contents represent progress toward a scheduled goal.

\\{schedule-mode-map}"
  (interactive "P")
  (setq schedule-mode
	(if (null arg) (not schedule-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if schedule-mode
      (progn
	;; turn off this mode if we change major modes.
	(make-local-hook 'change-major-mode-hook)
	(add-hook 'change-major-mode-hook
		  '(lambda () (schedule-mode -1))
		  nil t)
        
        ;; make sure to always refresh schedule before writing to disk
        (add-hook 'local-write-file-hooks 'schedule-refresh)
        
        ;; tie in with timeclock.el, if present
        (when schedule-timeclock-available
          (make-local-hook 'timeclock-out-hook)
          (add-hook 'timeclock-out-hook 'schedule-update-task)
        
          (make-local-hook 'timeclock-done-hook)
          (add-hook 'timeclock-done-hook 'schedule-complete-task))
         
	(run-hooks 'schedule-mode-hook))
    ;; jww (1999-04-06): how do we get back the localness of
    ;; `write-contents-hooks'?
    (remove-hook 'write-contents-hooks 'schedule-refresh))
  (force-mode-line-update))

(run-hooks 'schedule-load-hook)


;;; schedule.el ends here
