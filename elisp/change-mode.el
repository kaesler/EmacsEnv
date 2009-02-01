;; From nobody Tue Jul  7 11:29:49 1998
;; Path: atria.com!cam-news-feed2.bbnplanet.com!news.bbnplanet.com!ulowell.uml.edu!newspump.monmouth.com!newspeer.monmouth.com!newsfeed.internetmci.com!209.89.75.15!News.Toronto.iSTAR.net!News.Ottawa.iSTAR.net!news.istar.net!nntp.magma.ca!news.magma.ca!not-for-mail
;; Sender: rs@sphere.magma.ca
;; Newsgroups: gnu.emacs.sources
;; Subject: change-mode.el
;; From: Richard Sharman <rsharman@magma.ca>
;; X-Newsreader: Gnus v5.5/Emacs 20.2
;; Message-ID: <871zrybe3v.fsf@sphere.magma.ca>
;; Lines: 1274
;; Date: Tue, 07 Jul 1998 02:16:45 GMT
;; NNTP-Posting-Host: sphere.magma.ca
;; NNTP-Posting-Date: Mon, 06 Jul 1998 22:16:45 EDT
;; 
;; Here's a revised version of change-mode.   This is a minor mode in
;; which changes made to a buffer are highlighted in a different face,
;; so you can see what you've changed.
;; 
;; * it has active and passive modes,  so [in passive mode] you can
;;   accumulate changes without the distraction of seeing them,  toggling
;;   to active mode makes them visible.
;; 
;; * you can rotate through a series of different faces (typically
;;   different colours) to see different "ages" of changes.
;; 
;; * compare-with-file marks changes compared with another file.
;; 
;; * you can distinguish between insertions and deletions.  If the face
;;   you use only changes foreground colour you don't see whitespace
;;   highhighed;  by default the face used for deletions has underline
;;   set.
;; 
;; * you can have it automatically turned on, optionally for certain
;;   modes.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; change-mode.el - minor mode displaying buffer changes with special face

;; Copyright (C) 1998 Richard Sharman

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;; Commentary:

;; A minor mode: "change-mode".
;;
;; Change-mode has 2 submodes: active and passive.
;; When active,  changes to the buffer are displayed in a different
;; face.  When passive,  any existing displayed changes are saved and 
;; new ones recorded but are not displayed differently.
;; Why active and passive?  Having the changes visible can be
;; handy when you want the information but very distracting
;; otherwise.   So, you can keep change-mode in passive state while
;; you make your changes,  toggle it on to active mode to see them,
;; then toggle it back off to avoid distraction.
;; 
;; When a change-mode is on (either active or passive) you can
;; go to the next or previous change with change-mode-next-change &
;; change-mode-previous-change.
;;
;; You can "age" different sets of changes by using
;; change-mode-rotate-colours.  This rotates different through a
;; series of different faces,  so you can distinguish "new" changes
;; from "older" changes.
;;
;; You can also use the command compare-with-file to show changes in
;; this file compared with another file (typically the previous
;; version of the file).
;;
;;
;; If you use font-lock mode you need to decide which form of
;; highlighting is being used at any one time.
;;   
;; * Do NOT turn on font-lock mode while change-mode is active, it
;;   will remove all changes. 
;; * It IS safe to turn on font-lock mode when change-mode is in the
;;   passive state.
;;   
;; If font-lock mode is on when change-mode is first called,  or is
;; called when in passive mode,  then
;; - when change-mode is active font-locks mode is deactivated and
;; changes are highlighted,
;; -  when in passive mode font-lock's faces are used instead.   
;;
;; If both font-lock-mode and change-mode are invoked by a mode-hook,
;; the order off adding the hook matters.   But in any case,  toggling
;; change-mode will toggle between (change-mode-passive and
;; font-lock-mode active) and (change-mode-active and no font-lock mode).
;; 
;;
;; You can define your own faces using the various set-face-*
;; functions (these can be used interactively).  Or you can evaluate
;; something like this before loading change-mode.el:
;;   (make-face 'change-face)
;;   (set-face-background 'change-face "DarkGoldenrod4")
;;
;; There are currently two hooks run by change-mode:
;;   change-mode-enable-hook and change-mode-disable-hook
;; which are called from change-mode when the mode is being turned on
;; or off respectively.  (The enable hook is not only called when the
;; mode is intially turned on,  it is called each time -- e.g. when
;; toggling between active and passive modes.
;; (I'm not happy with this,  and may change this soon.)
;; 
;; Example usage:
;;  (defun my-change-mode-enable-hook ()
;;    (or (facep 'change-face)
;;        (progn
;;  	;; stuff to do the very first time change-mode is called
;;  	;; (in any buffer)
;;  	(make-face 'change-face)
;;  	(set-face-foreground 'change-face "DarkGoldenrod4")
;;  	(set-face-background 'change-face "lavender")))
;;    ;; stuff to do each time
;;    (add-hook 'local-write-file-hooks 'change-mode-rotate-colours)
;;  )
;; 
;;  (defun my-change-mode-disable-hook ()
;;    (remove-hook 'local-write-file-hooks 'change-mode-rotate-colours)
;;  )
;;  
;;  (add-hook 'change-mode-enable-hook 'my-change-mode-enable-hook)
;;  (add-hook 'change-mode-disable-hook 'my-change-mode-disable-hook)

;;           Explciit vs. Implicit
;;

;; Normally, change-mode is turned on explicitly for certain buffers.
;;
;; If you prefer to have it automatically invoked you can do it as
;; follows.
;; 
;; 1. Most modes have a major-hook, typically called MODE-hook.  You
;; can use add-hook to call change-mode.  
;;
;;   Example:
;;	(add-hook 'c-mode-hook 'change-mode)
;;
;;  If you want to make it start up in passive mode (regardless of the
;;  setting of change-mode-initial-state):
;;      (add-hook 'emacs-lisp-mode-hook 
;; 	    (lambda ()
;; 	      (change-mode 'passive)))
;;
;; However, this cannot be done for fundamental-mode for there is no
;; such hook.
;;
;; 2. You can use the function global-change-mode
;; This function, which is fashioned after the way global-font-lock
;; works,  toggles on or off global change mode.
;; When activated, it turns on change mode in all "suitable" existings
;; buffers and will turn it on in new "suitable" buffers to be
;; created.
;; 
;; A buffer's "suitability" is determined by variable
;; change-mode-global-modes,  as follows.  If the variable is
;; * nil  -- then no buffers are suitable;
;; * a function -- this function is called and the result is used.  As
;;   an example,  if the value is 'buffer-file-name then all buffers
;;   who are visiting files are suitable, but others (like dired
;;   buffers) are not;
;; * a list -- then if the buufer is suitable iff its mode is in the
;;   list,  exccept if the first element is nil in which case the test
;;   is reversed (i.e. it is a list of unsuitable modes).
;; * Otherwise,  the buffer is suitable if its name does not begin with
;;   ' ' or '*' and (buffer-file-name) returns true.
;;
;; It is recommended to set it to a list of specific modes,  e.g.
;;   (setq change-mode-global-modes
;;      '(c-mode emacs-lisp-mode text-mode fundamental-mode))
;; While the default cases works fairly reasonably,  it does include
;; several cases that probably aren't wanted (e.g. mail buffers).


;;     Possible bindings:
;; (global-set-key '[C-right] 'change-mode-next-change)
;; (global-set-key '[C-left]  'change-mode-previous-change)
;;
;;     Other interactive functions (which could be bound if desired):
;; change-mode
;; change-mode-remove-change-face
;; change-mode-save-changes-as-face
;; change-mode-rotate-colours
;; compare-with-file

;;     Possible autoloads:
;;
;; (autoload 'change-mode "change-mode"
;;   "Show changes in a distincive face" t)
;;
;; (autoload 'change-mode-next-change "change-mode" "\
;; Move to the beginning of the next change, if minor-mode
;; change-mode is in effect." t)
;; 
;; (autoload 'change-mode-previous-change "change-mode" "\
;; Move to the beginning of the previous change, if minor-mode
;; change-mode is in effect." t)
;; 
;; (autoload 'compare-with-file "change-mode"
;;   "Compare this saved buffer with a file,  showing differences
;; in a distinctive face" t)
;;
;; (autoload 'change-mode-remove-change-face "change-mode" "\
;; Remove the change face from the region.  This allows you to
;; manually remove highlighting from uninteresting changes." t)
;;
;; (autoload (quote change-mode-save-changes-as-face) "change-mode" "\
;; Change all current changes to face FACE,  which is prompted for
;; when called interactively.   Mixing this function and
;; change-mode-rotate-colours doesn't work well:  if you use this to
;; change to a different face then it will remain in that face and will
;; not be rotated." t nil)
;; 
;; (autoload (quote change-mode-rotate-colours) "change-mode" "\
;; Rotate the faces used by change-mode.  Current changes will be
;; display in the face described by the first element of
;; change-mode-face-list,  those (older) changes will be shown in the
;; face descriebd by the second element,  and so on.   Very old changes
;; reamin in the last face in the list.
;; 
;; You can automatically rotate colours when the buffer is saved
;; by adding this to local-write-file-hooks,  by evaling (in the
;; buffer to be saved):
;; \(add-hook 'local-write-file-hooks 'change-mode-rotate-colours)
;; " t nil)
;;
;; (autoload (quote global-change-mode) "change-mode" "\
;; Turn on or off global change mode."
;; `change-mode-global-modes'." t nil)


;;; Bugs:

;; - compare-with-file won't workf if if the buffer is read-only
;;   since changing faces changes the buffer.


;;; To do (maybe),  notes, ...

;; - having different faces for deletion and non-deletion: is it
;;   really worth the hassle?
;; - should have better hooks:  when should they be run?
;; - compare-with-file should allow RCS files - e.g. nice to be able
;;   to say show changes compared with version 2.1.     
;; - Maybe we should have compare-with-buffer as well.  (When I tried
;;   a while back I ran into a problem with ediff-buffers-internal.)


;;; History:

;; R Sharman (rsharman@magma.ca) Feb 1998:
;; - initial release.
;; Ray Nickson (nickson@mcs.vuw.ac.nz) 20 Feb 1998: 
;; - deleting text causes immediately surrounding characters to be highlighted.
;; - change-mode is only active for the current buffer.
;; Jari Aalto <jari.aalto@ntc.nokia.com> Mar 1998
;; - fixes for byte compile errors 
;; - use eval-and-compile for autoload
;; Marijn Ros <J.M.Ros@fys.ruu.nl> Mar 98
;; - suggested turning it on by default
;; R Sharman (rsharman@magma.ca) Mar 1998:
;; - active/passive added
;; - allow different faces for deletions and additions/changes
;; - allow changes to be hidden (using 'saved-face attribute)
;; - added compare-with-file
;; - coexist with font-lock-mode
;; June 98
;; - try and not clobber other faces if change-mode-dont-clober-other-faces
;; - allow initial default state to be passive or active
;; - allow rotation of old changes to different faces
;; - added hooks
;; - added automatic stuff
;; Adrian Bridgett <adrian.bridgett@zetnet.co.uk> June 98:
;; - make hide/unhide not affect the buffer modified status
;; - change-mode-rotate-colours: only if in change-mode!
;; - changed meaning of ARG for global-change-mode
;; July 98
;; compare-with-file:  clear buffer modification status,  return to
;;    original point.  Complain if read-only.  Force change-mode-active.


;;; Code:

;; ====== defvars of variables that the user may which to change =========

;; Face information: How the changes appear.

;; Defaults for face: red foreground, no change to background,
;; and underlined if a change is because of a deletion.
;; Note: underlining is helpful in that is shows up changes in white
;; space.  However,  having it set for non-delete changes can be
;; annoying because all indentation on inserts gets underlined (which
;; can look pretty ugly!).

;; These can be set before loading change-mode,  or
;; changed on the fly,  by evaling something like this:
;; (set-face-foreground 'change-face "green")
;; (set-face-underline-p 'change-face t)

(defvar change-face-foreground "red"
  "Foreground colour of changes other than deletions.
If set to nil,  the foreground is not changed.")
(defvar change-face-background nil
  "Background colour of changes other than deletions.
If set to nil,  the background is not changed."
)
(defvar change-face-underlined nil
"If non nil, changes other than deletions are underlined.")

(defvar change-delete-face-foreground "red"
"Foreground colour of changes due to deletions.
If set to nil,  the foreground is not changed.")

(defvar change-delete-face-background nil
"Background colour of changes due to deletions.
If set to nil,  the background is not changed.")

;; This looks pretty ugly, actually...  Maybe it should default to nil.
(defvar change-delete-face-underlined t
"If non nil, changes due to deletions are underlined.")

(defvar change-mode-face-list nil
  "*A list of faces used when rotataing changes.

Normally this list is created from change-mode-colours when needed.
However, you can set this variable to any list of faces.  You will
have to do this if you want faces which don't just differ from
change-face by the foreground colour.  Otherwise,  this list will be
constructed when needed from  change-mode-colours.

The face names should begin with \"change-\" so that they will be
hidden and restored when toggling between active and passive modes.")

;; A (not very good) default list of colours to rotate through.
;; This assumes that normal characters are roughly black foreeground
;; on a ligh background:
;;
(defvar change-mode-colours '(
			      "blue"
			      "firebrick"	
			      "green4"	
			      "DarkOrchid"
			      "chocolate4"
			      "NavyBlue")
  "*Colours used by change-mode-rotate-colours.
The current change will be displayed in the first element of this
list,  the next older will be in the second element etc." )


;; Set this if you are using faces for some other purpose (other than
;; using font-lock) and don't want change-mode mucking them up.
;;
(defvar change-mode-dont-clober-other-faces nil
  "*It non-nil,  change-mode won't affect text which contains
some other face.")

;; If you invoke change-mode with no argument,  should it start in
;; active or passive mode?
;;
(defvar change-mode-initial-state 'active
  "*What state (active or passive) change-mode should start in
if change-mode is called with no argument.

This variable must be set to either 'active or 'passive.
")

(defvar change-mode-global-initial-state 'passive
  "*What state (active or passive) global-change-mode should start in
if global-change-mode is called with no argument.

This variable must be set to either 'active or 'passive.
")

;; The strings displayed in the mode-line for the minor mode:
(defvar change-mode-active-string " Change-active")
(defvar change-mode-passive-string " Change-passive")

;; I think the test of the buffer name is not necesary and
;; may be removed soon.
(defvar change-mode-global-modes t
  "Used to determine whether a buffer is suitable for global-change mode.

Nil means no buffers are suitable for global-change-mode.
A function means that function is called:  if it returns non-nil the
buffer is suitable.
A list is a list of modes for which it is suitable,  or a list whose
first element is 'not followed by modes which are not suitable.
t means the buffer is suitable if its name does not begin with ' ' or
'*' and the buffer has a filename.

Examples:
        (c-mode c++-mode)
means that Change mode is turned on for buffers in C and C++ modes only."
)

(defvar change-mode-debug nil)

(defvar global-change-mode nil)

(defvar change-mode-global-changes-existing-buffers nil
"*Normally,  global-change-mode means affects only new buffers (to be
created).  However, if change-mode-global-changes-existing-buffers is
non-nil then turning on global-change-mode will turn on change-mode in
suitable buffers and turning the mode off will remove it from existing
buffers." )

;; ========================================================================

;; These shouldn't be changed!

(defvar change-mode nil)
(defvar change-mode-attribute nil)	;; 'face or 'saved-face

(defvar change-mode-string " ??")
(or (assq 'change-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(change-mode change-mode-string) minor-mode-alist)
	  ))
(make-variable-buffer-local 'change-mode)
(make-variable-buffer-local 'change-mode-string)
(make-variable-buffer-local 'change-mode-attribute)
;; putting make-local-hook here doesn't seem to work...
;;(make-local-hook 'after-change-functions)
(make-variable-buffer-local 'change-mode-dont-clober-other-faces)

(defvar change-mode-previous-font-lock-mode nil)
(make-variable-buffer-local 'change-mode-previous-font-lock-mode)

(eval-when-compile (require 'advice))

(eval-and-compile
  ;;  For compare-with-file
  (defvar ediff-number-of-differences)
  (autoload 'ediff-setup		"ediff")
  (autoload 'ediff-with-current-buffer	"ediff")
  (autoload 'ediff-really-quit		"ediff")
  (autoload 'ediff-make-fine-diffs	"ediff")
  (autoload 'ediff-get-fine-diff-vector "ediff")
  (autoload 'ediff-get-difference	"ediff")

  (defvar font-lock-mode)
  (autoload 'font-lock-mode		"font-lock" t t)
  )

;; See change-mode-set-face-on-change for why undo is adviced.
(defvar command-is-undo nil) ;; only undo should change this!
(defadvice undo (around record-this-is-an-undo activate)
  (let ((command-is-undo t))
    ad-do-it))


;;; Functions...




;;;###autoload
(defun change-mode-remove-change-face (beg end) 
  "Remove the change face from the region.  This allows you to
manually remove highlighting from uninteresting changes."
  (interactive "r")
  (let ((after-change-functions nil))
    (remove-text-properties beg end  '(face nil))))

(defun change-mode-set-face-on-change (beg end leng-before 
					   &optional face delete-face)
  "A function added to after-change-functions hook to record changes
and optionally display them in a distinctive face."
  ;;
  ;; This function is called by the after-change-functions hook, which
  ;; is how we are notified when text is changed.
  ;; If no face is supplied,  we use normal change-face.
  ;;
  ;; We do NOT want to do this if this is an undo command, because
  ;; otherwise an undone change shows up as changed.  But, we can't
  ;; use this-command because undo sets this-command to t (and some
  ;; other commands do also).  So we advise the function undo.
  (save-match-data
    (cond
   (command-is-undo
    nil) ;; (message "*** undo ***"))
   ;; This test hopefully isn't needed,  since it probably was put in
   ;; because of the bug about not keeping the hook properly local.
   ;;   ((string-match "^[ *]"  (buffer-name))
   ;;    nil) ;; (message "ignoring this in minibuffer!"))
   (t
    ;; (message "Change: %d %d %d" beg end leng-before)
    (or face
	(setq face 'change-face))
    (or delete-face
	(setq delete-face 'change-delete-face))
    
    (if (and (= beg end) (> leng-before 0))
	;; deletion
	(let ((beg-decr 1) (end-incr 1))
	  ;; The eolp and bolp tests are a kludge!  But they prevent rather
	  ;; nasty looking displays when deleting text at the end
	  ;; of line,  such as normal corrections as one is typing and
	  ;; immediately makes a corrections,  and when deleting first
	  ;; character of a line.
	  (if (= leng-before 1)
	      (if (eolp)
		  (setq beg-decr 0 end-incr 0)
		(if (bolp)
		    (setq beg-decr 0))))
	  (setq beg (max (- beg beg-decr) (point-min)))
	  (setq end (min (+ end end-incr) (point-max)))
	  (setq face delete-face))
      ;; other change (not deletion)
      ;; no change to beg, end nor face
      )
    (let (old)
      (if (and change-mode-dont-clober-other-faces
	       (text-property-not-all beg end 'face nil)
	       (setq old (get-text-property beg 'face))
	       (if (symbolp old)
		   (not (string-match "^change-" (symbol-name old)))
		 t))
	  nil ; (message "Not clobbering face %s from %s to %s" old beg end)
	;; (message "Change: %d %d %s %s" beg end change-mode-attribute face)
	(put-text-property beg end change-mode-attribute face))
      ))
   )))



(defun change-mode-hide-faces (&optional inhibit-restore)
  "Change properties face to saved-face for change-face & change-delete-face.

When change-mode is passive we don't use faces for the changes,
but we want to still keep any changes and continue recording new ones.
So we change the text attribute face to saved-face,  which is swapped
back in change-mode-unhide-faces when going to active mode.
If INHIBIT-RESTORE is non-nil, we just remove the property."
  (let* ((start (point-min))
	 (buffer-was-modified (buffer-modified-p))
	 old end new x)
    (while (< start (point-max))
      (setq old (get-text-property start 'face))
      (setq end 
	    (next-single-property-change start 'face nil (point-max)))
      ;; (message (format "From %d to %d face is %s" start end old))
      (if (and old
	       (symbolp old);; not a list
	       (string-match "^change-" (symbol-name old)))
	  (progn
	    ;; (message "Removing face %s from %d to %d" old start end)
	    (remove-text-properties start end '(face nil))
	    (or inhibit-restore
		(set-text-properties start end (list 'saved-face old))))
	;; (message (format "NOT changing face %s" old))
	)
      (setq start end)
      )
    (message "setting buffer-modified-p to %s" buffer-was-modified)
    (set-buffer-modified-p buffer-was-modified)
    ))

(defun change-mode-unhide-faces (&optional inhibit-restore)
  "Change properties saved-face to face for change-face & change-delete-face.

This is the opposite of change-mode-hide-faces (which see)."
  (let* ((start (point-min))
	 (buffer-was-modified (buffer-modified-p))
	 old old2 end new x)
    (while (< start (point-max))
      (setq old (get-text-property start 'saved-face))
      (setq old2 (get-text-property start 'face))
      (setq end 
	    (next-single-property-change start 'saved-face nil (point-max)))
      ;; (message (format "From %d to %d face is %s (%s)" start end old old2))
      ;; this was just "if old... "
      (if (and old 
	       (symbolp old);; not a list
	       (string-match "^change-" (symbol-name old)))
	  (progn
	    (remove-text-properties start end '(saved-face nil))
	    (if (or old2 inhibit-restore)
		nil ; (message "NOT restoring face %s from %d to %d" old start end)
	      ;; (message "Restoring face %s from %d to %d" old start end)
	      (set-text-properties start end (list 'face old)))))
      (setq start end)
      )
    (message "setting buffer-modified-p to %s" buffer-was-modified)
    (set-buffer-modified-p buffer-was-modified)
    ))



(defun change-mode-remove-faces ()
  "Remove face and save-face change properties."
  (change-mode-hide-faces t)
  (change-mode-unhide-faces t))

(defun change-mode-make-faces ()
  "Define the faces for change-mode if not already done so."
  ;; This code moved from  change-mode  so that the faces can
  ;; be used from elsewhere, without invoking change-mode.
  (or (facep 'change-face)
      (progn
	(make-face 'change-face)
	(set-face-foreground 'change-face change-face-foreground)
	(set-face-underline-p 'change-face change-face-underlined)))
  (or (facep 'change-delete-face)
      (progn
	(make-face 'change-delete-face)
	(set-face-foreground 'change-delete-face change-delete-face-foreground)
	(set-face-underline-p 'change-delete-face change-delete-face-underlined))))

(defun change-mode-set (value)
  "Turn on change-mode for this buffer."
  (setq change-mode value)
  (remove-hook 'after-change-functions 'change-mode-set-face-on-change t)
  ;; (message (format "change-mode is now %s" (prin1-to-string change-mode)))
  (change-mode-make-faces)
  (if (eq change-mode 'active)
      (progn
	(setq change-mode-string change-mode-active-string)
	;; replace any change-face-hidden with change-face
	(setq change-mode-attribute 'face)
	(if (featurep 'font-lock)
	    (progn
	      (setq change-mode-previous-font-lock-mode
		    (if font-lock-mode 1 0))
	      ;; this must be called before we restore from saved-face
	      ;; or else font-lock will zap our face
	      (font-lock-mode -1);; turn off font-lock-mode
	      )
	  (setq change-mode-previous-font-lock-mode nil)
	  )
	(or buffer-read-only
	    (change-mode-unhide-faces)))
    ;; mode is passive
    (setq change-mode-string change-mode-passive-string)
    (or buffer-read-only
	(change-mode-hide-faces))
    (setq change-mode-attribute 'saved-face)
    (if (and change-mode-previous-font-lock-mode (featurep 'font-lock))
	(font-lock-mode change-mode-previous-font-lock-mode)))
  (make-local-hook 'after-change-functions)
  (add-hook 'after-change-functions 'change-mode-set-face-on-change nil t)
)

(defun change-mode-clear ()
  "Remove change-mode for this buffer."
  (remove-hook 'after-change-functions 'change-mode-set-face-on-change t)
  (or buffer-read-only
      (change-mode-remove-faces))
  (setq change-mode nil)
  ;; If we type:  C-u -1 M-x change-mode
  ;; we want to turn it off,  but change-mode-post-command-hook
  ;; runs and that turns it back on!
  (remove-hook 'post-command-hook 'change-mode-post-command-hook)
  ;; (setq change-mode-attribute 'saved-face)  ;; why???
  (if (and change-mode-previous-font-lock-mode (featurep 'font-lock))
     (font-lock-mode change-mode-previous-font-lock-mode)))

;;;###autoload
(defun change-mode (&optional arg)
  "Toggle (or initially set) change mode.

Without an argument,  
  if change-mode is not enabled, then enable it (to either active
      or passive as determined by variable change-mode-initial-state);
  otherwise, toggle between active and passive states.

With an argument,
  if just C-u  or  a positive argument,  set state to active;
  with a zero argument,  set state to passive;
  with a negative argument,  disable change-mode completely.

Active state -  means changes are shown in a distinctive face.
Passive state - means changes are kept and new ones recorded but are
		not displayed in a different face.

Functions:
\\[change-mode-next-change] - move point to beginning of next change
\\[change-mode-prevoius-change] - move point to beginning of previous change
\\[compare-with-file] - mark text as changed by comparing this buffer
against the contents of a file
\\[change-mode-remove-change-face] - remove the change face from the
region
\\[change-mode-save-changes-as-face] - change the face used for
changes to a named face
\\[change-mode-rotate-colours] - rotate different \"ages\" of changes
through various faces.


Hook variables:
change-mode-enable-hook - when called entering active or passive state
change-mode-disable-hook - when turning off change-mode.
"
  (interactive "P")
  (if window-system
      (let ((new-change-mode
	     (cond
	      ((null arg)
	       ;; no arg => toggle (or set to active initially)
	       (if change-mode
		   (if (eq change-mode 'active) 'passive 'active)
		 change-mode-initial-state))
	      ;; an argument is given
	      ((eq arg 'active)
	       'active)
	      ((eq arg  'passive)
	       'passive)
	      ((> (prefix-numeric-value arg) 0)
	       'active)
	      ((< (prefix-numeric-value arg) 0)
	       nil)
	      (t
	       'passive)
	      )))
	(if new-change-mode
	    ;; mode is turned on -- but may be passive
	    (progn
	      (run-hooks 'change-mode-enable-hook)
	      (change-mode-set new-change-mode))
	  ;; mode is turned off
	  (run-hooks 'change-mode-disable-hook)
	  (change-mode-clear))
	)
    (message "Change-mode only works when using a window system"))
  )

(defun change-mode-in-a-change (&optional position)
  "Return non-nil iff buffer is in change mode and POSTION is marked
as being changed. POSITION defaults to point if not given." 
  (or position
      (setq position (point)))
  (if change-mode
      (let ((prop (get-text-property
		   position (if (eq change-mode 'active) 'face 'saved-face))))
	(and prop
	     (symbolp prop)
	     (string-match "^change-" (symbol-name prop))))
    nil))

;;;###autoload
(defun change-mode-next-change ()
  "Move to the beginning of the next change, if minor-mode
change-mode is in effect."
  ;; If there is no next change it signals an error.  Arguably it
  ;; should wrap round to the first change,  or possibly go the end of
  ;; the buffer.
  (interactive)
  (or change-mode
      (error "Buffer is not in in change-mode"))
  (let ((type (if (eq change-mode 'active) 'face 'saved-face))
	(next (point))
	)
    (if (change-mode-in-a-change)
	;; We are at a change,  find a non-change...
	(while (and next
		      (setq next (next-single-property-change next type))
		      (change-mode-in-a-change next))
	    ))
    (while (and next
		(setq next (next-single-property-change next type))
		(not (change-mode-in-a-change next)))
      )
    (if next
	(progn
	  (goto-char next)
	  ;; if at a new line,  it looks better to go to the next line.
	  (if (eq (char-after (point)) ?\n)
	      (forward-char 1)))
      (error "no next change found"))))

;;;###autoload
(defun change-mode-previous-change ()
  "Move to the beginning of the previous change, if minor-mode
change-mode is in effect."
  ;; If there is no previous change it signals an error.  Arguably it
  ;; should wrap round to the last change, or possibly go the
  ;; beginning of the buffer.
  (interactive)
  (or change-mode
      (error "Buffer is not in in change-mode"))
  (let ((type (if (eq change-mode 'active) 'face 'saved-face))
	(pos (point))
	(prev nil))
    (if (eq (point-max) (point))
	;; If we are at the end (point) will not be marked as a change
	;; even though the last character is actually changed.
	(forward-char -1))
    (if (change-mode-in-a-change)
	;; if we are in a change already,  find a non-change
	(while (and pos
		    (setq pos (previous-single-property-change pos type))
		    (change-mode-in-a-change pos))
	  ))
    ;; go back and find a change
    (while (and pos
		(setq pos (previous-single-property-change
			   pos type))
		(not (change-mode-in-a-change pos)))
      )
    (setq prev pos)
    (while (and pos
		(setq pos (previous-single-property-change 
			   pos type))
		;; removed  nil (point-min)
		(change-mode-in-a-change pos))
      (if pos 
	  (setq prev pos)))
    (or prev
	(if (change-mode-in-a-change (point-min))
	      (setq prev (point-min))
	      ))
    (if prev
	(progn
	  (goto-char prev)
	  ;; if at a new line,  it looks better to go to the next line.
	  (if (eq (char-after (point)) ?\n)
	      (forward-char 1)))
      (error "no previous change found  pos=%s" pos)))
  )

;; ========================================================================



(defun change-mode-replace-prop (prop old new)
  (alter-text-property (point-min) (point-max) prop
			 (lambda (a)
			   (if (eq a old)
			       new
			     a
			     ))
			 ))

;; Changing to a face with different properties can look odd.  For
;; example, switching to one with a different background or with
;; underline on may show a lot more "changes" than were visible before
;; (mainly, from the end of line to the end of window).
;;
;;;###autoload
(defun change-mode-save-changes-as-face (face)
  "Change all current changes to face FACE,  which is prompted for
when called interactively.   Mixing this function and
change-mode-rotate-colours doesn't work well:  if you use this to
change to a different face then it will remain in that face and will
not be rotated." 
  (interactive (list
		(completing-read
		 "Replace face of current changes as which facc? "
				 (mapcar (lambda (s)
					   (list (symbol-name s) 1))
					 (face-list))
				 nil t )))
  (if (stringp face)
      (setq face (intern face)))
  (or (facep face)      (error "%s is not a face!" face))
  (let ((after-change-functions nil))
    (change-mode-replace-prop 'face 'change-face face)
    (change-mode-replace-prop 'face 'change-delete-face face)
    ))

(defun change-mode-make-colour-list (&optional force)
  "Construct change-mode-face-list from change-mode-colour-list."
  (if (or (null change-mode-face-list)  ; Don't do it if it
	  force) ; already exists unless FORCE non-nil.
      (let ((p change-mode-colours) 
	    (n 1) name)
	(setq change-mode-face-list nil)
	(change-mode-make-faces);; ensure change-face is valid!
	(while p
	  (setq name (intern (format "change-face-%d" n)))
	  (copy-face 'change-face name)
	  (set-face-foreground name (car p))
	  (setq change-mode-face-list 
		(append change-mode-face-list (list name)))
	  (setq p (cdr p))
	  (setq n (1+ n))))))

;;;###autoload
(defun change-mode-rotate-colours ()
  "Rotate the faces used by change-mode.  Current changes will be
display in the face described by the first element of
change-mode-face-list,  those (older) changes will be shown in the
face descriebd by the second element,  and so on.   Very old changes
remain shown in the last face in the list.

You can automatically rotate colours when the buffer is saved
by adding this to local-write-file-hooks,  by evaling (in the
buffer to be saved):
  (add-hook 'local-write-file-hooks 'change-mode-rotate-colours)
"
  ;; You can do this:
  ;; (add-hook 'local-write-file-hooks 'change-mode-rotate-colours)
  (interactive)
  (if (eq change-mode 'active)
      (let ((after-change-functions nil)
	    (prop 'face)
	    old p new)
	(change-mode-make-colour-list);; create it if needbe
	(setq p (reverse change-mode-face-list))
	(setq new (car p))
	(setq p (cdr p))
	(while p
	  (setq old (car p))
	  ;; (message "Changing %s from %s to %s" prop old new)
	  (change-mode-replace-prop prop old new)
	  (setq new old)
	  (setq p (cdr p)))
	(change-mode-replace-prop prop 'change-face new)
	(change-mode-replace-prop prop 'change-delete-face new)
	))
  ;; This always returns nil so it is safe to use in
  ;; local-write-file-hook
  nil)

;; ========================================================================
;; Comparing with an existing file.   
;; This uses ediff to find the differences.

;;;###autoload
(defun compare-with-file (file-b)
  "Compare this buffer (which must be an unmodified file buffer which
is not read-only) with a file.   The default file name is the backup
filename if it exists.

If a buffer is visiting the file being compared against,  it also will
have its differences highlighted.   Otherwise, the file is read in
temporarily but the buffer is deleted.

If faces change-compare-face and change-compare-delete-face are
defined [as valid faces] those are used for the differences.
Otherwise the faces used by compare-mode are used."
  (interactive (list
	      (read-file-name
	       "File to compare with? " ;; prompt
	       ""			;; directory
	       nil			;; default
	       'yes			;; must exist
	       (let ((f (make-backup-file-name 
			 (or (buffer-file-name (current-buffer))
			     (error "no file for this buffer")))))
		 (if (file-exists-p f) f ""))
	       )))
  (if buffer-read-only
      (error "Buffer is read-only"))
  (let* ((buf-a (current-buffer))
	 (orig-pos (point))
	 (file-a (buffer-file-name))
	 (existing-buf (get-file-buffer file-b))
	 (buf-b (or existing-buf
		    (find-file-noselect file-b)))
	 xy  xx yy p q
	 a-start a-end len-a
	 b-start b-end len-b
	 )
    ;; We use the fact that the buffer is not marked modified at the
    ;; end where we clear its modified status
    (if (buffer-modified-p buf-a)
	(if (y-or-n-p (format "OK to save %s?  " file-a))
		       (save-buffer buf-a)
	  (error "Buffer must be saved before comparing with a file.")))
    (change-mode 'active)
    (save-window-excursion
      (setq xy (change-mode-get-diff-info buf-a file-a buf-b file-b)))
    (setq xx (car xy))
    (setq p xx)
    (setq yy (car (cdr xy)))
    (setq q yy)
    (change-mode-make-faces)
    (let ((change-mode-attribute 'face))
      (while p
	(setq a-start (nth 0 (car p)))
	(setq a-end (nth 1 (car p)))
	(setq b-start (nth 0 (car q)))
	(setq b-end (nth 1 (car q)))
	(setq len-a (- a-end a-start))
	(setq len-b (- b-end b-start))
	(set-buffer buf-a)
	;; (message (format "%d %d %d"  a-start a-end len-b))
	(change-mode-set-face-on-change 
	 a-start a-end len-b
	 (if (facep 'change-compare-face)
	     'change-compare-face nil)
	 (if (facep 'change-compare-delete-face)
	     'change-compare-delete-face nil))
	(set-buffer-modified-p nil)
	(goto-char orig-pos)
	(if existing-buf
	    (progn
	      (set-buffer buf-b)
	      (change-mode-set-face-on-change 
	       b-start b-end len-a
	       (if (facep 'change-compare-face)
		   'change-compare-face nil)
	       (if (facep 'change-compare-delete-face)
		   'change-compare-delete-face nil)
					)))
	(setq p (cdr p))
	(setq q (cdr q))
	)
      )
    (if existing-buf
	(progn
	  (set-buffer-modified-p nil)
	  (change-mode 'active))
      (kill-buffer buf-b))
    ))


(defun change-mode-get-diff-info (buf-a file-a buf-b file-b)
  (let ((e nil) x y)   ;; e is set by function change-mode-get-diff-list-hk
    (ediff-setup buf-a file-a buf-b file-b
	       nil nil   ; buf-c file-C
	       'change-mode-get-diff-list-hk
	       (list (cons 'ediff-job-name 'something))
	       )
    (ediff-with-current-buffer e (ediff-really-quit nil))
    (list x y)))


(defun change-mode-get-diff-list-hk ()
  ;; (message "change-mode-get-diff-list-hk started")
  ;; x and y are dynamically bound by change-mode-get-diff-info 
  ;; which calls this function as a hook
  (defvar x)  ;; placate the byte-compiler
  (defvar y)
  (setq  e (current-buffer))
  (let ((n 0) extent p va vb a b)
    (setq  x nil  y nil)    ;; x and y are bound by change-mode-get-diff-info
    (while (< n ediff-number-of-differences)
      ;; (message (format "diff # %d of %d" n ediff-number-of-differences))
      ;;
      (ediff-make-fine-diffs n)
      (setq va (ediff-get-fine-diff-vector n 'A))
      ;; va is a vector if there are fine differences
      (if va
	  (setq a (append va nil))
	;; if not,  get the unrefined difference
	(setq va (ediff-get-difference n 'A))
	(setq a (list (elt va 0)))
	)
      ;; a list a list
      (setq p a)
      (while p
	(setq extent (list (overlay-start (car p))
			   (overlay-end (car p))))
	(setq p (cdr p))
	(setq x (append x (list extent) ))
	);; while p
      ;;
      (setq vb (ediff-get-fine-diff-vector n 'B))
      ;; vb is a vector
      (if vb
	  (setq b (append vb nil))
	;; if not,  get the unrefined difference
	(setq vb (ediff-get-difference n 'B))
	(setq b (list (elt vb 0)))
	)
      ;; b list a list
      (setq p b)
      (while p
	(setq extent (list (overlay-start (car p))
			   (overlay-end (car p))))
	(setq p (cdr p))
	(setq y (append y (list extent) ))
	);; while p
      ;;
      (setq n (1+ n))
      );; while
    ;; ediff-quit doesn't work here.
    ;; No point in returning a value, since this is a hook function.
    ))

;; ======================= automatic stuff ==============

;; Default t case - buffer name is probably redunndant?  Why not just
;; use (buffer-file-name) ?

(defun change-mode-major-mode-hook ()
  (add-hook 'post-command-hook 'change-mode-post-command-hook)
  )

(defun change-mode-post-command-hook ()
  ;; This is called after changeing a major mode, but also after each
  ;; M-x command,  in which case the current buffer is a minibuffer.
  ;; In that case, do not act on it here,  but don't turn it off
  ;; either,  we will get called here again soon-after. 
  ;; Also,  don't enable it for other special buffers.
  (if (string-match "^[ *]"  (buffer-name))
      nil ;; (message "ignoring this post-command-hook")
    (remove-hook 'post-command-hook 'change-mode-post-command-hook)
    ;; The following check isn't necessary,  since 
    ;; change-mode-turn-on-maybe makes this check too.
    (or change-mode	;; don't turn it on if it already is
	(change-mode-turn-on-maybe change-mode-global-initial-state))
    ))



;;;###autoload
(defun global-change-mode (&optional arg)
  "Turn on or off global change mode.

When called interactively:
- if no prefix, toggle global change mode on or off
- if called with a positive prefix (or just C-u) turn it on in active mode
- if called with a zero prefix  turn it on in passive mode
- if called with a negative prefix turn it off

When called from a program:
- if ARG is nil or omitted, turn it off
- if ARG is 'active,  turn it on in active mode
- if ARG is 'passive, turn it on in passive mode
- otherwise just turn it on 

When global change mode is enabled, change mode is turned on for
future \"suitable\" buffers (and for \"suitable\" existing buffers if
variable change-mode-global-changes-existing-buffers is non-nil).
\"Suitablity\" is determined by variable change-mode-global-modes."

  (interactive 
   (list
    (cond
     ((null current-prefix-arg)
      ;; no arg => toggle it on/off
      (setq global-change-mode (not global-change-mode)))
     ;; positive interactive arg - turn it on as active
     ((> (prefix-numeric-value current-prefix-arg) 0)
      (setq global-change-mode t)
      'active)
     ;; zero interactive arg - turn it on as passive
     ((= (prefix-numeric-value current-prefix-arg) 0)
      (setq global-change-mode t)
      'passive)
     ;; negative interactive arg - turn it off
     (t
      (setq global-change-mode nil)      
      nil))))

  (if arg
      (progn
	(if (eq arg 'active)
	    (setq change-mode-global-initial-state 'active)
	  (if (eq arg  'passive)
	      (setq change-mode-global-initial-state 'passive)))
	(setq global-change-mode t)
	(message "turning ON global change mode in %s state"
		 change-mode-global-initial-state)
	(add-hook 'change-major-mode-hook 'change-mode-major-mode-hook)
	(if change-mode-global-changes-existing-buffers
	    (change-mode-update-all-buffers change-mode-global-initial-state))
	)
    (message "turning OFF global change mode")
    (remove-hook 'change-major-mode-hook 'change-mode-major-mode-hook)
    (remove-hook 'post-command-hook
		 'change-mode-post-command-hook)
    (if change-mode-global-changes-existing-buffers
	(change-mode-update-all-buffers nil))
    )
  )





(defun change-mode-turn-on-maybe (value)
  "Turn on change-mode if it is appropriate for this buffer:
- the buffer is not a special buffer (one whose name begins with '*'
  or ' ')
- the buffer's mode is suitable as per variable change-mode-global-modes
- change-mode is not already on for this buffer.

This function is called from change-mode-update-all-buffers 
from global-change-mode when turning on global change mode.
"
  (or change-mode			; do nothing if already on
      (if
	  (cond
	   ((null change-mode-global-modes)
	    nil)
	   ;; Unfortunately, functionp isn't in emacs 19.34
	   ;; ((functionp change-mode-global-modes)
	   ;;    (funcall change-mode-global-modes))
	   ((or
	     (and (fboundp 'functionp)
		  (functionp change-mode-global-modes))
	     (and (symbolp change-mode-global-modes)
		  (fboundp change-mode-global-modes)))
	    (funcall change-mode-global-modes))
	    ((listp change-mode-global-modes)
	     (if (eq (car-safe change-mode-global-modes) 'not)
		 (not (memq major-mode (cdr change-mode-global-modes)))
	       (memq major-mode change-mode-global-modes)))
	    (t
	     (and 
	      (not (string-match "^[ *]"  (buffer-name)))
	      (buffer-file-name))
	     ))
	  (change-mode-set value))
      ))
    

(defun change-mode-turn-off-maybe ()
  (if change-mode
      (change-mode-clear)))



(defun change-mode-update-all-buffers (value)
  ;; with-current-buffer is not in emacs-19.34 
  (if (boundp 'with-current-buffer)
      (mapcar
       (function (lambda (buffer)
		   (with-current-buffer buffer
		     (if value
			 (change-mode-turn-on-maybe value)
		       (change-mode-turn-off-maybe))
		     )))
       (buffer-list))
    ;; I hope this does the same thing...
    (save-excursion
      (mapcar
       (function (lambda (buffer)
		   (set-buffer buffer)
		   (if value
		       (change-mode-turn-on-maybe value)
		     (change-mode-turn-off-maybe))
		   ))
       (buffer-list)))
    ))

;; ===================== debug ==================
(defun change-mode-show-faces ()
  ;; Look in the *Messages* buffer for the output...
  (interactive)
  (message "--- show-faces ---")
  (let* ((start (point-min))
	 old
	 end new x)
    ;; (setq start (text-property-not-all start (point-max) 'face nil))
    (while (< start (point-max))
      (setq old (get-text-property start 'face))
      (setq end 
	    (next-single-property-change start 'face nil (point-max)))
      (if old
	  (message (format "From %d to %d face is %s" start end old)))
      (setq start end)
      )))

(defun change-mode-show-saved-faces ()
  (interactive)
  (message "--- show-saved-faces ---")
  (let* ((start (point-min))
	 old
	 end new x)
    ;; (setq start (text-property-not-all start (point-max) 'saved-face nil))
    (while (< start (point-max))
      (setq old (get-text-property start 'saved-face))
      (setq end 
	    (next-single-property-change start 'saved-face nil (point-max)))
      (if old
	  (message (format "From %d to %d saved-face is %s" start end old)))
      (setq start end)
      )))



(defun change-mode-show-all ()
  (interactive)
  (change-mode-show-faces)
  (change-mode-show-saved-faces))

;; ================== end of debug ===============


(provide 'change-mode)
;; end of change-mode.el
