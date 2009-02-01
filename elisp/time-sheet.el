;; Path:    /disk3/esler/library/src/elisp/time-sheet.el
;; Version: Tue Jun 23 15:52:47 1992
;; Author:  Kevin Esler

;; Useful functions for updating my daily time-sheet
;; This cries out for a package for manipulating dates and times.

(require 'crypt++)

(defvar time-sheet-dir-name "~/time-sheet")

(defconst private-file-modes (* 64 6))
(defconst private-dir-modes (* 64 7))

(defun time-sheet ()
  
  "Edit the appropriate weekly time-sheet file, initialising it if necessary."

  (interactive)

  (setq time-sheet-dir-name (expand-file-name time-sheet-dir-name))

  (let ((today (todays-canonical-date)))
    
    (let ((monday (nearest-monday-canonical-date today)))

      (let ((time-sheet-file-name (compute-time-sheet-file-name monday)))

	;; Create the user's time-sheet directory, if necessary.
        ;;
	(if (not (file-directory-p time-sheet-dir-name))
	    (progn
	     (message "Making time-sheet directory: %s" time-sheet-dir-name)
	     (call-process "mkdir" nil nil nil time-sheet-dir-name)
	     (message "")
	     (if (not (file-directory-p time-sheet-dir-name))
		 (error "Cannot create time-sheet directory: %s"
			time-sheet-dir-name))
	     (set-file-modes time-sheet-dir-name private-dir-modes)))

	;; Find this week's time-sheet file.
        ;;
	(let ((time-sheet-path (concat time-sheet-dir-name "/" time-sheet-file-name)))
	  (find-file time-sheet-path)

	  ;; Create the time-sheet file in template form, if necssary.
          ;;
	  (if (zerop (buffer-size))
	      (progn
		(message "Creating new time-sheet file: %s" time-sheet-path)
                
                ;; Arrange to have the buffer encrypted when written:
                ;;
                (if (not (boundp crypt-buffer-encryption-key))
                    (setq crypt-buffer-encryption-key nil))
                (if (not crypt-buffer-encryption-key)
                    (call-interactively 'crypt-set-encryption-key))
                (if (equal crypt-buffer-encryption-key "")
                    (message "No key given, buffer %s assumed normal." (buffer-name))
                  (crypt-encrypted-mode 1))

                ;; Write the initial text into it:
                ;;
		(initialise-time-sheet-buffer monday)
		(save-buffer)
		(set-file-modes time-sheet-path private-file-modes)))
	  
	  ;; Find the paragraph labelled with today's day.
          ;;
	  (goto-char (point-min))
	  (if (not (re-search-forward
		    (concat "^" (current-day-string) ":")
		    (point-max)
		    t))
	      (error "Status file buffer format incorrect"))

	  ;; Find the line before the next day's heading, or end-of-file.
          ;;
	  (if (re-search-forward "^[A-Z]" (point-max) t)
	      (progn
		(forward-line -1))
	    (goto-char (point-max)))
	      
	  ;; Indent for a new line.
	  ;; Probably should also trim trailing white space,
	  ;; and perhaps verify that we're looking at a blank line.
          ;;
	  (insert-string "\t")

	  ;; Use Indented Text Mode.
          ;; We must turn on encryption after 'indented-text-mode
          ;; kills all local variables.
          ;;
          (let ((old-buffer-encryption-key crypt-buffer-encryption-key))
            (progn
              (indented-text-mode)
              (setq crypt-buffer-encryption-key old-buffer-encryption-key)
              (crypt-encrypted-mode 1)))

	  ;; For now, bind the key like this.
	  ;; This binds it for all buffers using Text mode.
	  ;; Refine this a bit later.
          ;;
	  (local-set-key "\C-ct" 'time-sheet-milestone))))))

(defun initialise-time-sheet-buffer (monday)

  "Initialise a time-sheet file buffer, given the canonical date of
the appropriate Monday."

  (let ((heading
	 (format
	  "Timesheet for %s  --  week of Monday %s %d, %d"
	  (user-full-name)
	  (month-longname (cd-month monday))
	  (cd-day monday)
	  (cd-year monday))))

    (insert-string "\t\t")
    (insert-string heading)
    (insert-string "\n")

    (insert-string "\t\t")
    (insert-string (make-string (length heading) ?=))
    (insert-string "\n"))

  (insert-string "\nSaturday:\n")
  (insert-string "\nSunday:\n")
  (insert-string "\nMonday:\n")
  (insert-string "\nTuesday:\n")
  (insert-string "\nWednesday:\n")
  (insert-string "\nThursday:\n")
  (insert-string "\nFriday:\n")
  
  (goto-line 1)
  (center-line)
  (goto-line 2)
  (center-line))

(defun compute-time-sheet-file-name (cd)

  "Given a date, make a filename like jun.31.1999"

  (concat
   (aref [ "jan" "feb" "mar" "apr" "may" "jun" "jul" "aug"
	  "sep" "oct" "nov" "dec" ]
	 (1- (cd-month cd)))
   "."
   (int-to-string (cd-day cd))
   "."
   (int-to-string (cd-year cd))))

;; Function to add a milestone to my time-sheet file

(defun time-sheet-milestone ()

  "Insert string like \"-> 10:33 \" into buffer at point."

  (interactive)
  (insert-string
   (concat "-> " (time-hhmm-rounded) " ")))

(defun todays-canonical-date ()
	   
  "Return today's date in canonical-date form (an array of
4 integers: year, month (1..12), day, weekday (0..6)."

  (string-to-canonical-date (current-time-string)))

(defun string-to-canonical-date (s)

  "Convert a time-string to a canonical date."

  (let ((string-year     (substring s 20 24))
	(shortname-month (substring s  4 7))
	(string-day      (substring s  8 10))
	(shortname-day   (substring s  0 3)))
    
    (let ((month
	   (cdr
	    (assoc shortname-month
		   '(("Jan" . 1) ("Feb" . 2)  ("Mar" . 3)  ("Apr" . 4)
		     ("May" . 5) ("Jun" . 6)  ("Jul" . 7)  ("Aug" . 8)
		     ("Sep" . 9) ("Oct" . 10) ("Nov" . 11) ("Dec" . 12))))))
      
      (vector
       (string-to-int string-year)
       month
       (string-to-int string-day)
       (cdr
	(assoc shortname-day
	       '(("Mon" . 0) ("Tue" . 1) ("Wed" . 2) ("Thu" . 3)
		 ("Fri" . 4) ("Sat" . 5) ("Sun" . 6))))))))

(defun nearest-monday-canonical-date (cd)

  "Given a canonical date, return the canonical date of the
appropriate Monday, for time-sheet reporting purposes.  Weekend
days activities are reported on the following Monday."

  (bump-canonical-date
   cd
   (if (< (cd-weekday cd) 5)
       (- (cd-weekday cd))
     (- 7 (cd-weekday cd)))))

(defun bump-canonical-date (cd days-increment)

  "Increment a canonical date by a number in the range [-6..+6]."

  ;; No need for generality here.
  (let ((day (+ (cd-day cd) days-increment))
	(weekday (% (+ days-increment (cd-weekday cd)) 7))
	(month (cd-month cd))
	(year (cd-year cd)))

    (let ((this-month-length (month-length month year)))

      (if (> day this-month-length)
	
	  (progn
	   (setq day (- day this-month-length))
	   (setq month (+ 1 month))
	   (if (> month 12)
	       (progn
		(setq month 1)
		(setq year (+ 1 year))))))

      (if (< day 1)

	  (progn
	   (setq month (- month 1))
	   (if (< month 1)
	       (progn
		(setq month 12)
		(setq year (- year 1))))
	   (setq day (+ day (month-length month year)))))

      (vector year month day weekday))))

(defun month-length (month year)

  "Return the length of a given month in a given year."

  (if (leap-year-p year)
      (if (= 2 month)
	  29))
  (aref [ 31 28 31 30 31 30 31 31 30 31 30 31 ] (1- month)))
    
(defun month-longname (mm)

  "Return the text name of a month, given it's number [1..12]."

  (aref
   [ "January" "February" "March" "April" "May" "June"
     "July" "August" "September" "October" "November" "December" ]
   (1- mm)))

(defun leap-year-p (year)

  "Returns true if YEAR is a Gregorian leap year, and false if not."

  (or
    (and (=  (% year   4) 0)
         (/= (% year 100) 0))
    (= (% year 400) 0)))
   
;; Operations on canonical dates

(defun cd-year (cd)
  (aref cd 0))

(defun cd-month (cd)
  (aref cd 1))

(defun cd-day (cd)
  (aref cd 2))

(defun cd-weekday (cd)
  (aref cd 3))

(defun current-day-string ()
  
  "Return current day's name."
  
  (cdr
   (assoc
    (substring (current-time-string) 0 3)
    '(("Sun" . "Sunday")
      ("Mon" . "Monday")
      ("Tue" . "Tuesday")
      ("Wed" . "Wednesday")
      ("Thu" . "Thursday")
      ("Fri" . "Friday")
      ("Sat" . "Saturday")))))

;; Function to return the time as string "hh:mm."

(defun time-hhmm ()
  (substring (current-time-string) 11 16))

;; Function to return the time as string "hh:mm." rounded to nearest
;; quarter hour.

(defun time-hhmm-rounded ()
  (let ((string (time-hhmm)))
    (let ((mins  (string-to-int (substring string 3 5)))
	  (hours (string-to-int (substring string 0 2))))
      (let ((modulus (% mins 15)))
	(if (> modulus 7)
	    (progn
	      (setq mins (+ mins (- 15 modulus)))
	      (if (= 60 mins)
		  (progn
		    (setq mins 0)
		    (setq hours
			  (if (= 23 hours)
			      0
			    (1+ hours))))))
	  (setq mins (- mins modulus)))

	;; Avoid printing the time like "9:0"; want " 9:00".
	;; Would like capabilities of printf().
	(format 
	 (concat
	  (if (< hours 10)
	      " %d"
	    "%d")
	  ":"
	  (if (< mins 10)
	      "0%d"
	    "%d"))
	  hours mins)))))


;; Test code
;(defconst test-date-list
;  '("Wed Dec 26 15:19:47 1989"
;    "Thu Dec 27 15:19:47 1989"
;    "Fri Dec 28 15:19:47 1989"
;    "Sat Dec 29 15:19:47 1989"
;    "Sun Dec 30 15:19:47 1989"
;    "Mon Dec 31 15:19:47 1989"
;    "Tue Jan  1 15:19:47 1990"
;    "Tue Jan  1 15:19:47 1991"))
;
;(defun testit ()
;  (mapcar
;   '(lambda (s)
;      (princ
;       (format "\n%s"  s))
;
;      (let ((cd (string-to-canonical-date s)))
;	(terpri)
;	(princ " => ")
;	(princ cd)
;
;	(let ((monday (nearest-monday-canonical-date cd)))
;	  (terpri)
;	  (princ " => ")
;	  (princ monday))))
;
;   test-date-list)
;
;  nil) ; Don't return a list
