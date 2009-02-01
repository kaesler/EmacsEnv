;; From: ajcos1@penfold.cc.monash.edu.au (Andrew J. Cosgriff)
;; Newsgroups: gnu.emacs.sources
;; Subject: add-face.el (save and reload faces associated with files, sort of)
;; Date: 16 Jun 1994 09:45:16 GMT
;; Organization: Monash University
;; NNTP-Posting-Host: penfold.cc.monash.edu.au
;; X-Silly-Header: Well, i had to put one in...
;; 
;; 
;; I was a little dismayed to find that the quite fun facemenu.el barfed when i
;; tried it at work under 19.23 and 19.25 (tho' it had worked ok at uni under
;; 19.24), so i wrote a few routines of my own to add faces to the region.
;; 
;; Then i thought about the comments by facemenu's author with respect to saving
;; the faces.  After finding the right routines, i gave it a whirl and here it
;; is.  The relevant bits for saving are near the end, so feel free to chuck them
;; in facemenu if you so desire.
;; 
;; Since I'm only just getting the hang of writing in [Emacs-] Lisp, apologies
;; for the code :)
;; 
;; Currently, face properties are saved as <filename>.props - it's a bit messy, I
;; suppose, but it works. (perhaps we could save them as .<filename>.props ?)
;; 
;; And things like hilit19, that use overlays or whatever, don't get affected by
;; this, since they're not using faces.
;; 
;; I hope all this makes some sort of sense...
;; 
;; I'd really really appreciate bugfixes, additions and comments about this.
;; (especially things to do with the programming technique ;).
;; (i know, i haven't put in any doc strings yet...)
;; 
;; If you follow up by email, send it to andrew@unico.com.au (i can't post news
;; from there until i have time to fix the news system).
;; 
;; Enjoy,
;;  Andrew.
;; PS. watch out for the automatic .sig at the end.
;; 
;; --cut--
;; 
;;
;; add-face.el - add a face to a region
;;
;; 1994 Andrew J. Cosgriff
;; andrew@bing.apana.org.au (home)
;; andrew@unico.com.au      (work)
;;
;; Creation Date : Wed Jun 15 10:00:24 1994
;; !Revision: 1.3 !
;; !Date: 1994/06/16 05:34:50 !
;;
;; Free for use as long as you don't claim you wrote it or charge money for it.
;; I'm not responsible for your inability to use this program for its intended
;; purpose.  Use at your own risk.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun internal-change-region-face (START END face)
	(put-text-property START END 'face face)
)

(defun add-face-to-region (START END face)
	(interactive "r\nd")
	(let* (
			(face (if (interactive-p)	(read-face-name "Add face : ")))
			(the-face (internal-get-face face)))
		(internal-change-region-face START END face)
	)
)

(defun remove-face-from-region (START END)
	(interactive "r")
	(put-text-property START END 'face nil)
)

(defun make-region-bold (START END)
	(interactive "r")
	(internal-change-region-face START END 'bold)
)

(defun make-region-underline (START END)
	(interactive "r")
	(internal-change-region-face START END 'underline)
)

(defun make-region-italic (START END)
	(interactive "r")
	(internal-change-region-face START END 'italic)
)

(defun make-region-bold-italic (START END)
	(interactive "r")
	(internal-change-region-face START END 'bold-italic)
)

(defun find-text-properties (bufname)
	(interactive "bBuffer to find properties in : ")
	(if (null bufname)
		(setq bufname (buffer-name))
	)
	(save-excursion
		(let* ((pos (point-min)) (props (list (list (point-min) (get-char-property (point-min) 'face)))))
			(while pos
				(let* ((newpos (next-single-property-change pos 'face (get-buffer bufname))))
					(if newpos
						(setq props
							(append props
								(list (list newpos (get-char-property newpos 'face)))
							)
						)
					)
					(setq pos newpos)
				)
			)
			(if (interactive-p) (message "Properties are : %s" props))
			props
		)
	)
)

(defun save-text-properties ()
  (interactive)
	(save-excursion
		(let* (
				(curprop (find-text-properties (buffer-name)))
				(bufname (concat (buffer-file-name) ".props"))
				(buf (get-buffer-create (concat "**temp**" bufname "**")))
				(from (car (car curprop)))
				(face (car (cdr (car curprop))))
				(curprop (cdr curprop))
				(to (car (car curprop)))
			)
			(set-buffer buf)
			(kill-region (point-min) (point-max))
			(while curprop
					(insert "(put-text-property "
						(prin1-to-string (if from from (point-min)))
						" "
						(prin1-to-string to)
						" 'face '"
						(prin1-to-string face)
						")\n"
					)
				(setq face (car (cdr (car curprop))))
				(setq from to)
				(setq curprop (cdr curprop))
				(setq to (car (car curprop)))
			)
			(if (> (buffer-size) 0)
				(write-file bufname)
			)
			(kill-buffer buf)
		)
	)
)

(defun write-text-properties ()
	(if (not (string-match "\.props$" (buffer-file-name)))
		(save-text-properties)
	)
	nil
)

(defun read-text-properties ()
	(load (concat (buffer-file-name) ".props") t t)
)

(add-hook 'write-file-hooks 'write-text-properties)
(add-hook 'find-file-hooks 'read-text-properties)

;; end
