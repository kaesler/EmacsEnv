;; insitu.el  --  written by Kevin Esler Thu Dec 28 10:45:32 1989
;;
;; Various functions to filter buffer contents in-situ using Unix
;; processes.
;;
     
(defun c-format ()

  "Calls indent(1) on the current buffer of C source code,
replacing the contents."

  (interactive "*")
  (shell-command-on-region (point-min)
			   (point-max)
			   "indent -st"
			   t))

(defun filter-current-buffer (command)

  "Filters the current buffer through COMMAND, replacing
the contents."

  (interactive "*sCommand string: ")
  (shell-command-on-region (point-min)
			   (point-max)
			   command
			   t))

  
