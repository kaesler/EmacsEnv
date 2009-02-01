;;!emacs
;;
;; FILE:         hmous-info.el
;; SUMMARY:      Walks through Info networks using one key.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     docs, help, hypermedia, mouse
;;
;; AUTHOR:       Bob Weiner
;; ORIG-DATE:    04-Apr-89
;; LAST-MOD:      1-Nov-95 at 20:33:46 by Bob Weiner
;;
;; This file is for use with Hyperbole.
;;
;; Copyright (C) 1989, 1990, 1991  Free Software Foundation, Inc.
;; Available for use and distribution under the same terms as GNU Emacs.
;;
;;
;; DESCRIPTION:  
;;
;;  This code is machine independent.
;;
;;  To install:  See hui-mouse.el
;;
;; DESCRIP-END.

;;;###autoload
(defun smart-info ()
  "Walks through Info documentation networks using one key or mouse key.

If key is pressed within:
 (1) the first line of an Info Menu Entry or Cross Reference, the desired node
       is found;
 (2) the Up, Next, or Previous entries of a Node Header (first line),
       the desired node is found;
 (3) the File entry of a Node Header (first line),       
       the 'Top' node within that file is found;
 (4) at the end of the current node, the Next node is found (this will
       descend subtrees if the function 'Info-global-next' is bound);
 (5) anywhere else (e.g. at the end of a line), the current node entry is
       scrolled up one windowful.

Returns t if key is pressed within an Info Node Header, Cross Reference,
or a Menu; otherwise returns nil."

  (interactive)
  (cond 
    ;;
    ;; If at end of node, go to next node
    ;;
    ((last-line-p)
     (if (fboundp 'Info-global-next) (Info-global-next)
       (Info-next)))
    ((Info-handle-in-node-hdr))
    ((Info-handle-in-note))
    ((Info-handle-in-menu))
    ((pos-visible-in-window-p (point-max))
     (if (fboundp 'Info-global-next) (Info-global-next)
       (Info-next)))
    ;;
    ;; If nothing else scroll forward a windowful.
    ;;
    ((smart-scroll-up))))

;;;###autoload
(defun smart-info-assist ()
  "Walks through Info documentation networks using one assist-key or mouse assist-key.

If assist-key is pressed within:
 (1) the first line of an Info Menu Entry or Cross Reference, the desired node
       is found;
 (2) the Up, Next, or Previous entries of a Node Header (first line),
       the last node in the history list is found;
 (3) the File entry of a Node Header (first line),       
       the 'DIR' root-level node is found;
 (4) at the end of the current node, the Previous node is found (this will
       return from subtrees if the function 'Info-global-prev is bound);
 (5) anywhere else (e.g. at the end of a line), the current node entry is
       scrolled down one windowful.

Returns t if assist-key is pressed within an Info Node Header, Cross Reference,
or a Menu; otherwise returns nil."

  (interactive)
  (cond
    ;;
    ;; If at end or beginning of node, go to previous node
    ;;
    ((last-line-p)
     (if (fboundp 'Info-global-prev) (Info-global-prev)
       (Info-prev)))
    ((Info-handle-in-node-hdr-assist))
    ((Info-handle-in-note))
    ((Info-handle-in-menu))
    ((pos-visible-in-window-p (point-min))
     (if (fboundp 'Info-global-prev) (Info-global-prev)
       (Info-prev)))
    ;;
    ;; If anywhere else, scroll backward a windowful.
    ;;
    ((smart-scroll-down))))

(defun Info-handle-in-node-hdr ()
  "If within an Info node header, move to <FILE>Top, <Up>, <Previous>, or
<Next> node, depending on which label point is on, and return t.
Otherwise, return nil."
  ;;
  ;; Test if on 1st line of node, i.e. node header
  ;;
  (if (not (first-line-p))
      nil
    (let ((nodename "Top") (filep nil))
      (save-excursion
	(if (and
	      (re-search-forward "[:, \t\n]" nil t)
	      (re-search-backward
		"\\(File\\|Node\\|Up\\|Prev\\|Previous\\|Next\\):[ \t]" nil t))
	    (progn (setq filep (string-equal
				 "file"
				 (downcase (buffer-substring
					     (match-beginning 1)
					     (match-end 1)))))
		   (if (re-search-forward (concat ":[ \n]\\([^,\t\n"
						  (if filep " ")
						  "]*\\)") nil t)
		       (setq nodename (buffer-substring
					(match-beginning 1)
					(match-end 1)))))
	  (error "Node header not found.")))
      (setq nodename
	    (cond ((= (aref nodename 0) ?\() nodename)
		  (filep (concat "(" nodename ")" "Top"))
		  (buffer-file-name (concat "(" buffer-file-name ")" nodename))
		  (t nodename)))
      (if hyperb:lemacs-p
	  (Info-goto-node nodename nil t)
	(Info-goto-node nodename))
      t)))

(defun Info-handle-in-node-hdr-assist ()
  "If within an Info node header when the 'smart-info-assist' command is
executed, when within the <FILE> header go to the DIR top-level node.  When
within any other header (<Up>, <Previous>, or <Next>) go to last node from
history list.  Return t if in Info node header.  Otherwise return nil."
  ;;
  ;; Test if on 1st line of node, i.e. node header
  ;;
  (if (not (first-line-p))
      nil
    (save-excursion
      (if (and 
	    (re-search-forward "[:, \t\n]" nil t)
	    (re-search-backward
	      "\\(File\\|Node\\|Up\\|Prev\\|Previous\\|Next\\):[ \t]" nil t) )
	  ;; If in <FILE> hdr
	  (progn (if (string-equal
		       "file"
		       (downcase (buffer-substring
				   (match-beginning 1)
				   (match-end 1))))
		     (Info-directory)
		   (Info-last))
		 t)
	(error "Node header not found.")
	nil))))

;;;###autoload
(defun Info-handle-in-note ()
  "Follows an Info cross-reference.
If point is within the first line of an Info note (cross-reference), follows
cross-reference and returns t; otherwise returns nil."
  (let ((note-name) (opoint (point)))
    (save-excursion
      (skip-chars-forward "^:")
      (if (and (re-search-backward
		"\*\\(Ref\\|Note\\|See\\)\\([ \t\n]+\\|$\\)" nil t)
	       (looking-at "\*\\(Ref\\|Note\\|See\\)[ \t\n]+\\([^:]*\\):")
	       (<= (match-beginning 0) opoint)
	       (> (match-end 0) opoint))
	  ;; Remove newline and extra spaces from 'note-name'
	  (setq note-name (hypb:replace-match-string
			   "[ \n\t]+"
			   (buffer-substring
			    (match-beginning 2) (match-end 2))
			   " " t))))
    (if note-name
	(progn (Info-follow-reference note-name) t))))

(defun Info-handle-in-menu ()
  "Displays node referred to by an Info Menu Entry.
If point is within an Info menu entry, goes to node referenced by
entry and returns t; otherwise returns nil."
  ;;
  ;; Test if there is a menu in this node
  ;;
  (let ((in-menu nil) (curr-point (point)))
    (save-excursion
      (goto-char (point-min))
      (setq in-menu 
	    (and (search-forward "\n* menu:" nil t)
		 (< (point) curr-point))))
    (if (not in-menu)
	nil
      (let ((node))
	(save-excursion
	  (forward-char) ; Pass '*' char if point is in front of
	  (if (search-backward "\n*" nil t)
	      (progn (forward-char 2)
		     (setq node (Info-extract-menu-node-name)))))
	(if (null node)
	    nil
	  (if hyperb:lemacs-p
	      (Info-goto-node node nil t)
	    (Info-goto-node node))
	  t)))))

(provide 'hmous-info)
