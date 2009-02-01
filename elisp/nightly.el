;; Emacs lisp program for scanning the nightly build logfiles.
;;
;; Author: Kevin Esler <esler@atria.com>, July 1995.
;;
;; This file has been formatted so it can be easily viewed/edited in
;; Emacs' Folding Minor Mode (that's what the ";;{{{" and ";;}}}" are for).

;;{{{ How to use:

;;   1. interactively from Emacs:
;;        M-x load-file
;;            nightly.elc
;;        M-x nb-report-logs
;;
;;      You will be prompted for a set of log file paths.  To
;;      terminate your input just type RETURN at the prompt.  The
;;      output appears in a buffer called
;;      "nightly_build_problem_report"
;;
;;   2. in batch from from a shell command. E.g.:
;;
;;      /usr/local/bin/emacs-19.29 \
;;          -batch \
;;          -l nightly.elc \
;;          -funcall nb-report-logs-batch \
;;           LOGFILE...

;;}}}
;;
;;{{{ How it works:

;;
;;  For each logfile provided
;;    - scan the logfile to produce a list of build failures
;;    - scan the logfile to produce a list of compilation warnings
;;
;;  Concatenate the two lists.
;;    Each element of the list is an array of 8 fields, some of which
;;    may be NIL if they can't be determined:
;;  
;;    Field                 Type            Contents
;;    =====                 ====            ========
;
;;    problem-type          symbol          'nb-build-failure
;;                                          'nb-compilation-warning
;;    platform              symbol          'sun4, 'sgi5, ...
;;    build-directory       string          Pathname of directory where
;;                                          problem occurred.
;;    target                string          Pathname of target being built.
;;    source-element        string          Pathname of source element involved.
;;    check-in-info         pair of strings (userid of checker-in,
;;                                           date-time of check-in)
;;    logfile               string          Pathname of the logfile.
;;    logfile-paragraph     string          Relevant extract from the logfile.
;;
;;  Sort the list so that problems attributed to a given user are
;;  grouped together, with build failures before compilation warnings.
;;
;;  Write a report form the sorted list.
;;
;;  The resultant report can be chopped up into sections and emailed
;;  to those apparently responsible for the various problems.
;;
;;
;; Improvements needed
;; ===================
;;
;;      -2. Use `defstruct'.
;;      -1. Have separate pass looking for core dumps, and other system
;;          failures.
;;
;;      0. Currently the emacs process must be running in the same
;;         view as that in which the builds occurred, otherwise it
;;         can  get the checker-in information wrong.
;;         This should be fixed.
;;
;;      1. Provide summary info on first page of the report
;;         - logs processed
;;         - number of build failures
;;         - number of warnings
;;
;;      2. Sorting improvements:
;;           - for unattributable errors, sort them by element ?
;;
;;      3. Efficiency -- in function nb-discover-checker-in:
;;           keep a persistent cleartool process around.
;;
;;      4. Look for known sys-admin problems such as:
;;         "error writing for file ...: No space left on device"
;;

;;}}}

;;{{{ Constants.

(defconst nb-platform-list '(
                             aix3_power
                             aix4_power
                             hp9_800
                             hp9_pa
                             hp10_pa
                             osf1_axp
                             sgi5
                             sgi6
                             sun5
                             sun4
                             uw2_x86
                             )
  "List of platforms we build for.")

(defconst nb-log-suffix ".log"
  "Suffix of nightly build log files.")

(defconst nb-report-buffer-name "nightly_build_problem_report"
  "Name of the buffer where the nightly build report is generated.")

;;}}}

;;{{{ Variables.

(defvar nb-build-failure-regexp
  "\\(Build script failed for \\|Warning: .* is not a derived object\\|clearmake: Error:.*\\)\"\\(.+\\)\"")

(defvar nb-logs-directory "/usr/src/atria/nightly_builds/mainline/"
  "The directory containing the nightly build logs.")

(defvar nb-report-buffer nil)

(defvar nb-endof-last-tgt-message "^[ \t]*$"
  "Regexp describing how to find the last error/warning message in a group.")

;;}}}

;;{{{ Emacs-interactive top-level.

;; This is the function to invoke as an interactive Emacs command.
;;
(defun nb-report-logs (log-list)

  "Generate a report of nightly build problems found in all the
log files mentioned in LOG-LIST. When called interactively, the user is
prompted for a set of log files to process."
  
  (interactive (list (nb-prompt-for-log-list)))

  (let ((total-list
         (save-excursion
           (apply 'append
                  (mapcar
                   '(lambda (logfile)
                      (nb-check-logfiles-apparent-view logfile)
                      (if (not (file-exists-p logfile))
                          (error "%s does not exist." logfile))
                      (if (not (file-readable-p logfile))
                          (error "%s is not readable." logfile))
                      (append (nb-build-failures-logfile logfile)
                              (nb-compilation-warnings-logfile logfile)))
                   log-list)))))
    (nb-report-problem-list total-list)))

(defun nb-report-marked-logs ()
  "Take currently marked dired files as input to nb-report-logs"
  (interactive)
  (nb-report-logs (dired-get-marked-files)))

(defun nb-dired-logs ()
  "Start dired on default log directory"
  (interactive)
  (dired 
   (read-file-name "Log directory: " "" nil nil nb-logs-directory)
   "-lt")
  (define-key dired-mode-map "N" 'nb-report-marked-logs))

(defun nb-prompt-for-log-list ()

  "Interactively prompt the user for a list of nightly build logs."
  
  (let ((log-list nil)
        (dir (expand-file-name (file-name-as-directory nb-logs-directory)))
        (first-iteration t))
    (catch 'loop-exit
      (while t
        (let ((next-entry (read-file-name
                           (if first-iteration
                               "First log file (RETURN to end input): "
                             "Next log file (RETURN to end input): ")
                           dir
                           ""
                           t)))
          (if (zerop (length next-entry)) ;; null entry
              (throw 'loop-exit t)
            (let ((next-file (expand-file-name next-entry)))
              (setq log-list (append log-list (list next-file)))
              (setq dir (file-name-directory next-file)))))
        (setq first-iteration nil)))
    log-list))

;;}}}

;;{{{ Batch top-level.

;; This is the function that gets invoked from a shell-script.
;;
(defun nb-report-logs-batch ()
  
  "Batch version of nb-report-logs. It expects the logfile
names as command-line arguments."

  ;; Scan the logs into the buffer
  ;;
  (nb-report-logs command-line-args-left)
  (let ((nb-report-buffer (get-buffer-create nb-report-buffer-name)))
    (set-buffer nb-report-buffer)
    (princ (buffer-substring (point-min) (point-max)))))

;;}}}

;;{{{ Finding build failures in a logfile.

(defun nb-build-failures-logfile (logfile &optional platform)

  "Return a list of build failures for a LOGFILE.
If optional second argument PLATFORM is not given, the logfile filename is
examined to infer a platform name."
  
  (if (null platform)
      (setq platform (nb-infer-platform-from-logfile-name logfile)))
  (let ((platform-name (symbol-name platform)))
    (if (file-exists-p logfile)
        (if (file-readable-p logfile)
            (progn
              (find-file logfile)
              (let ((result nil)
                    (fbuffer-size (float (buffer-size))))

                (goto-char (point-min))
                (while
                    (re-search-forward nb-build-failure-regexp
                                       nil
                                       t)
                  (beginning-of-line)
                  (message "Looking for build failures in %s...(%d%%)"
                           logfile
                           (/ (* 100.0 (float (point))) fbuffer-size))
                  (let* ((failed-target (buffer-substring (match-beginning 2) (match-end 2)))
                         (log-paragraph (nb-extract-current-paragraph
					 ;;; use generic regexp to avoid
					 ;;; excess cruft in leftovers buffer
					 "\\(^[ \t]*$\\)"))
                         (build-directory (nb-discover-build-directory))
                         (source-element (nb-discover-source-element build-directory failed-target))
                         (check-in-info (nb-discover-check-in-info source-element)))
                    
                    (setq result (cons
                                  (vector 
                                   'nb-build-failure
                                   platform
                                   build-directory
                                   failed-target
                                   source-element
                                   check-in-info
                                   logfile
                                   log-paragraph
                                   (nb-platform-indep-target-name failed-target))
                                  result)))
                  (re-search-forward nb-endof-last-tgt-message nil t))
                result))))))

;;}}}

;;{{{ Finding compilation warnings in a logfile

(defun nb-compilation-warnings-logfile (logfile &optional platform)
  
  "Return a list of compilation warnings for a LOGFILE."

  (if (null platform)
      (setq platform (nb-infer-platform-from-logfile-name logfile)))
  (let ((platform-name (symbol-name platform)))
    (if (file-exists-p logfile)
        (if (file-readable-p logfile)
            (progn
              (find-file logfile)
              (let ((result nil)
                    (warning-regexp (nb-cc-warning-regexp platform))
		    (eow-regexp (nb-end-of-warnings-regexp platform))
                    (fbuffer-size (float (buffer-size))))
              
                (goto-char (point-min))
                (while
                    (re-search-forward warning-regexp
                                       nil
                                       t)
                  (beginning-of-line)

                  ;; Skip clearmake warnings
                  ;;
;;                  (if (not (looking-at "^clearmake: Warning: "))
                      (progn
                        (message "Looking for compilation warnings in %s...(%d%%)"
                                 logfile
                                 (/ (* 100.0 (float (point))) fbuffer-size))
                        (let* ((warned-target (nb-discover-warned-target))
                               (log-paragraph (nb-extract-current-paragraph
					       eow-regexp))
                               (build-directory (nb-discover-build-directory))
                               (source-element (nb-discover-source-element build-directory warned-target))
                               (check-in-info (nb-discover-check-in-info source-element)))
                          
                          ;; Don't report warnings if they have already been reported as
                          ;; build failures
                          ;;
                          (if (not (string-match nb-build-failure-regexp
                                                 log-paragraph))
                              (setq result (cons
                                            (vector 
                                             'nb-compilation-warning
                                             platform
                                             build-directory
                                             warned-target
                                             source-element
                                             check-in-info
                                             logfile
                                             log-paragraph
                                             (nb-platform-indep-target-name warned-target))
                                            result))))
                        
                        ;; Skip to end of build-log paragraph
                        ;;
                        (re-search-forward eow-regexp nil t))

                    ;; Skip over clearmake warnings to avoid infinite loop.
                    ;;
;;                    (end-of-line))
		  )
                
                result))))))

;; NB. This should be a static a-list.
;;
(defun nb-cc-warning-regexp (platform)
  
  "Return the C compiler warning regexp for a given PLATFORM."
  
  (cond 
   ((or (eq platform 'hp9_pa)
        (eq platform 'hp9_800)
        (eq platform 'hp10_pa)) (concat
                                 ;; C
                                 ;;
                                 "^cc: .*warning [0-9]+:" "\\|"
                                 "^cpp: .*warning [0-9]+:"
				 ;;
                                 ;; C++
                                 ;;
				 "\\|"
                                 "line [0-9]+: warning:"
                                 ))
   ((eq platform 'osf1_axp) (concat
                             ;; C
                             ;;
                             "cfe: Warning: .*[0-9]+:"
                             ;; Optimiser
                             ;;
                             "\\|"
                             "uopt: Warning:"
                             ;; C++
                             ;;
                             "\\|"
                             ":[0-9]+: warning:"
                             ))
   ((or (eq platform 'sgi5)
	(eq platform 'sgi6)) (concat
			      ;; C
			      ;;
			      "cfe: Warning [0-9]+:.* line [0-9]+:"
			      ;; C++
			      ;;
			      "\\|"
			      "line [0-9]+: warning([0-9]+):"
			      ;; linker--but this is too much noise, so ignore
			      ;; it for now.
			      ;;
			      ;;"\\|"
			      ;;"^ld: WARNING"
			      ))
   ((eq platform 'sun5) " line [0-9]+: [Ww]arning")
   ((eq platform 'sun4) " line [0-9]+: [Ww]arning")
   ((eq platform 'aix4_power) "line [0-9]+\.[0-9]+: [0-9]+-[0-9]+:? ([WE])")
   ((eq platform 'uw2_x86) "\\(WARNING\\|warning\\).*:")

   ;; Unixware:
   ;; UX:acomp: WARNING: "warn.c", line 13: integral constant too large
   ;; "../vob_browser_app.cxx", line 668: warning: variable "kst" was set but never used

   ;; Guesses:
   ;;
   ((eq platform 'aix3_power) "[Ww]arning")
   (t "[Ww]arning")))

(defun nb-end-of-warnings-regexp (platform)
  
  "Return the end-of-warnings regexp for a given PLATFORM."
  (cond 
   ((or (eq platform 'sgi5)
        (eq platform 'sgi6))
    "\\(^[ \t]*-*^\n\\|^=+$\\)\n[^\"]")
   (t "\\(^[ \t]*$\\)")))

;;}}}


;;{{{ Discovering the interesting details about each build problem

(defun nb-extract-current-paragraph (endof-tgt-regexp)
  
  "From a compiler warning or error point, return the relevant piece of
the build transcript, as a string."

  ;; This is all a bit heuristic.  Also, it depends somewhat on
  ;; Clearmake separating individual build transcripts by blank lines.
  ;;  
  (beginning-of-line)
  (let ((end (save-excursion
               (if (re-search-forward endof-tgt-regexp nil t)
                   (match-end 1)
                 (point-max))))
        (begin (save-excursion
                 (if (looking-at "^[ \t]*$")
                     (backward-char 1))
                 (re-search-backward "rm -f \\(-r \\)?" nil t)
                 (if (re-search-backward "^[ \t]*$" nil t)
                     (point)
                   (point-min)))))
    (buffer-substring begin end)))

(defun nb-discover-build-directory ()
  
  "From a compiler warning or error point, return the build directory
where the problem occurred."
  
  (save-excursion
    (if (re-search-backward
         "\\(installing\\|making\\) PASS_.* in \\(/vobs/.*\\)$"
         nil
         t)
        (buffer-substring (match-beginning 2) (match-end 2))
      nil)))

(defun nb-discover-source-element (build-directory failed-target)
  
  "Given a BUILD-DIRECTORY and a FAILED-TARGET, return the source
element being compiled, as a string."
  
  (if (and build-directory failed-target)
      (let ((apparent-src-dir build-directory)
            (build-dir-leaf (file-name-nondirectory build-directory)))

        ;; See if the source dir is probably the parent of the build
        ;; directory.
        ;;
        (if (or (member (intern build-dir-leaf)
                        nb-platform-list)
                (string-match "^\\(sun\\|nt\\|sgi\\|osf\\|uw2\\|aix\\|hp\\)" build-dir-leaf))
            (setq apparent-src-dir (file-name-directory build-directory)))

        (if (file-directory-p apparent-src-dir)
            (cond
             
             ;; Was it a ".o" file ?
             ;; If so it's source could be a ".c", ".cxx" or ".tpd"
             ;; file.
             ;;
             ((string-match "\.o$" failed-target)
              
              (let* ((stem (concat
                            apparent-src-dir
                            (substring failed-target 0 -2)))

                     (c-src-element (concat stem ".c"))
                     (cxx-src-element (concat stem ".cxx"))
                     (tpd-src-element (concat stem ".tpd")))

                (cond
                 ((file-exists-p c-src-element) c-src-element)
                 ((file-exists-p cxx-src-element) cxx-src-element)
                 ((file-exists-p tpd-src-element) tpd-src-element)))))))))

(defun nb-discover-check-in-info (element)
  
  "Given an ELEMENT, return a pair consisting of two strings:
(1) the name of the person who last checked it in, and
(2) the date and time of the checkin."

  (if element
      (if (file-exists-p element)
          (let ((element-desc (nb-command-output-as-string
                               (concat "/usr/atria/bin/cleartool desc "
                                       element))))
            (if element-desc
                ;; Format 1: created 05-May-95.17:13:40 by Jim Herron (jherron.user@silicon)
                ;;
                (if (string-match "created \\([^ ]+\\) by .* (\\(.*\\)\\..*@.*)" element-desc)
                    (cons (substring element-desc (match-beginning 2) (match-end 2))
                          (substring element-desc (match-beginning 1) (match-end 1)))

                  ;; Format 2: created 07-Jun-95.15:05:00 by eric.user@match
                  ;;
                  (if (string-match "created \\([^ ]+\\) by +\\(.*\\)\\..*@.*" element-desc)
                      (cons (substring element-desc (match-beginning 2) (match-end 2))
                            (substring element-desc (match-beginning 1) (match-end 1))))))))))

(defun nb-discover-warned-target ()
  
  "From a compiler warning point, return the target being built, as a string."
  
  (save-excursion
    (if (re-search-backward "^\trm -f \\(-r \\)?\\(.*\.o\\)$"
                            nil
                            t)
        (buffer-substring (match-beginning 2) (match-end 2)))))

;;}}}

;;{{{ Sorting the problem list

;; This variable contains the function used for sorting the problem list.
;;
;;;(defvar nb-compare-function 'nb-compare-even-better)
(defvar nb-compare-function 'nb-compare-by-checker-in)

;; Here we get a report ordered as: person - problemtype - element
;; (OBSOLETE)
;;
(defun nb-compare-by-checker-in (vec-1 vec-2)
  
  "Function used to compare two nightly build problems, for sorting.
Sort order is: source-element within problem-type within checker-in."
  
  (let* ((check-in-info-1 (elt vec-1 5))
         (checker-in-1 (if check-in-info-1
                           (car check-in-info-1)))
         (problem-type-1 (elt vec-1 0))
         (source-element-1 (elt vec-1 4))
         (check-in-info-2 (elt vec-2 5))
         (checker-in-2 (if check-in-info-2
                           (car check-in-info-2)))
         (problem-type-2 (elt vec-2 0))
         (source-element-2 (elt vec-2 4)))
    
     (if (and checker-in-1 checker-in-2)
         (if (string= checker-in-1 checker-in-2)
             (if (eq problem-type-1 problem-type-2)
                 (string< source-element-1 source-element-2)
               (eq 'nb-build-failure problem-type-1))
           (string< checker-in-1 checker-in-2))
       checker-in-1)))

;; This seemed more useful.
;;
(defun nb-compare-even-better (vec-1 vec-2)
  
  "Function used to compare two nightly build problems, for sorting.
Sort order is: compilation failures first; then other build failures;
then compilation warnings; then other warnings. Within these categories,
problems are grouped by build-target."
  
  (let* ((check-in-info-1 (elt vec-1 5))
         (checker-in-1 (if check-in-info-1
                           (car check-in-info-1)))
         (problem-type-1 (elt vec-1 0))
         (canonical-target-1 (elt vec-1 8))
         (source-element-1 (elt vec-1 4))
         (check-in-info-2 (elt vec-2 5))
         (checker-in-2 (if check-in-info-2
                           (car check-in-info-2)))
         (problem-type-2 (elt vec-2 0))
         (canonical-target-2 (elt vec-1 8))
         (source-element-2 (elt vec-2 4)))

    ;; problem-type : source-files : target name ; person name
    ;; 
    (if (eq problem-type-1 problem-type-2)
        (if (and source-element-1 source-element-2)
            (string< source-element-1 source-element-2)
          (if (and (null source-element-1) (null source-element-2))
              (if (and canonical-target-1 canonical-target-2)
                  (string< canonical-target-1 canonical-target-2)
                canonical-target-1)
            source-element-1))
      (eq 'nb-build-failure problem-type-1))))

;;}}}

;;{{{ Producing a report from the problem list

(defun nb-report-problem-list (problem-list)

  "From a PROBLEM-LIST produce the report."
  
  (message "Sorting...")
  (let ((sorted-error-list (sort problem-list nb-compare-function)))
    (message "Formatting report...")
    (let ((nb-report-buffer (get-buffer-create nb-report-buffer-name)))
      (set-buffer nb-report-buffer)
      (erase-buffer)
      (mapcar 
       'nb-report-one-error-or-warning
       sorted-error-list)
      (switch-to-buffer nb-report-buffer)
      (message "Formatting report...done"))))
  
(defun nb-report-one-error-or-warning (vec)
  (let* ((problem-type (elt vec 0))
	(platform (elt vec 1))
	(build-directory (elt vec 2))
	(target  (elt vec 3))
	(source-element (elt vec 4))
	(check-in-info (elt vec 5))
	(log (elt vec 6))
	(rpt-buffer nb-report-buffer)
	(mail-buffer-name (if check-in-info
			      (format "*mail-to-%s*" (car check-in-info))
			    nil))
	(log-paragraph (elt vec 7)))
    (if check-in-info
	(progn (setq rpt-buffer (get-buffer mail-buffer-name))
	       (if (not rpt-buffer)
		   (progn (mail-other-window nil
					     (format "%s" (car check-in-info))
					     (format "Build log problems for review (starting with %s)" (symbol-name platform))
					     nil
					     nil
					     nil
					     nil)
			  (save-excursion
			    (set-buffer (get-buffer "*mail*"))
			    (rename-buffer mail-buffer-name))
			  (setq rpt-buffer (get-buffer mail-buffer-name))))))
		 
	      
    (nb-accum "=======================================================\n"
	      rpt-buffer)
    (nb-accum (format "PROBLEM TYPE: %s\n"
                      (cond
                       ((eq problem-type 'nb-build-failure) "build failure")
                       ((eq problem-type 'nb-compilation-warning) "compilation warning")
                       (t nil)))
	      rpt-buffer)
    (if (null build-directory)
        (nb-accum (format "PLATFORM: %s\n" (symbol-name platform))
		  rpt-buffer))
    (nb-accum (format "TARGET: %s\n" 
                      (if build-directory 
                          (concat build-directory "/" target)
                        target))
	      rpt-buffer)
    (if source-element
        (nb-accum (format "SOURCE ELEMENT: %s\n" source-element) rpt-buffer))
    (if check-in-info
        (nb-accum (format "CHECKED IN BY: %s  (%s) \n" (car check-in-info)
			  (cdr check-in-info))
		  rpt-buffer))
    (nb-accum (format "\nLOG EXTRACT: (from %s)\n" log) rpt-buffer)
    (nb-accum log-paragraph rpt-buffer)
    (nb-accum "\n" rpt-buffer)))

(defun nb-accum (string &optional report-buffer)
  
  "Append a STRING to the nightly build report work buffer."
  
  (nb-append-string-to-buffer (if (null report-buffer)
				  nb-report-buffer
				report-buffer)
			      string))


(defun nb-append-string-to-buffer (buffer string)
  
  "Append to a BUFFER a STRING."
  
  (save-excursion
    (set-buffer buffer)
    (goto-char (point-max))
    (insert string)))

;;}}}

;;{{{ Utility functions

;; Compute a unique name for a target that is the same for all platforms.
;; Eg.  "/vobs/atria/bin/mmake/sgi5/bldr.o" ==> "/vobs/atria/bin/mmake/bldr.o"
;; Used for sorting reports handily.
;;
(defun nb-platform-indep-target-name (target-path)
  (if target-path
      (let ((basename (file-name-nondirectory target-path))
            (return-value target-path))
        (let ((platform-dir (file-name-directory target-path)))
          (if platform-dir
              (let ((platform-name (file-name-nondirectory (directory-file-name platform-dir))))
                (if (member (intern platform-name) nb-platform-list)
                    (let ((source-dir (file-name-directory (directory-file-name platform-dir))))
                      (if source-dir
                          (setq return-value (concat source-dir basename))))))))
        return-value)
    nil))
            
(defun nb-infer-platform-from-logfile-name (logfile-name)
  (cond ((string-match "hp9_pa" logfile-name) 'hp9_pa)
        ((string-match "hp10_pa" logfile-name) 'hp10_pa)
        ((string-match "hp9_800" logfile-name) 'hp9_800)
        ((string-match "sgi5" logfile-name) 'sgi5)
        ((string-match "sgi6" logfile-name) 'sgi6)
        ((string-match "sun5" logfile-name) 'sun5)
        ((string-match "sun4" logfile-name) 'sun4)
        ((string-match "osf1_axp" logfile-name) 'osf1_axp)
        ((string-match "aix4_power" logfile-name) 'aix4_power)
        ((string-match "aix3_power" logfile-name) 'aix3_power)
        ((string-match "uw2_x86" logfile-name) 'uw2_x86)
        (t (error "Unable to infer a platform name from filename %s" logfile-name))))

(defun nb-command-output-as-string (command)
  
  "Capture all of a shell command's output as a string."
  
  (let ((buffer (get-buffer-create "*active function*")))
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (call-process shell-file-name
                    nil
                    buffer
                    nil
                    "-c"
                    command)
      (buffer-substring (point-min) (point-max)))))

(defun nb-check-logfiles-apparent-view (logfile)
  
  "Make sure that the Emacs process is running in the same view that
apparently produced the logfile. It would be even better to allow
Emacs to run outside of any view and accept a view-tag as one of its
inputs.  It could then to `cleartool startview' and use /view/tag/....
paths to examine elements."
  
  (let ((clearcase-root (getenv "CLEARCASE_ROOT")))
    (if (null clearcase-root)
        (error "Not in a view"))

    ;; Expecting "/view/VIEW-TAG"
    ;;
    (if (not (string-equal "/view/" (substring clearcase-root 0 6)))
        (error "Funny value in CLEARCASE_ROOT"))

    (let ((process-view-tag (substring clearcase-root 6)))
      (if (string-match "/usr/src/atria/nightly_builds/\\([^/]+\\)/.+" logfile)
          (let ((apparent-logfile-view-tag (match-string 1 logfile)))
            (if (not (string= process-view-tag apparent-logfile-view-tag))
                (if noninteractive
                    (error "Wrong process view")
                  (if (not (y-or-n-p
                            (format
                             "Your view (%s) and apparent logfile view (%s) differ; continue anyway ?"
                             process-view-tag
                             apparent-logfile-view-tag)))
                      (error "Wrong process view")))))))))

;;}}}


;; Local variables:
;; folded-file: t
;; End:
