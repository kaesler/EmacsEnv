; Make it easy to find emacs source.
;
; Suggested filename: emacs-tags.el.  Suggested binding (in your .emacs):
;
;  (autoload 'find-emacs-tag "emacs-tags"
;	    "Package for finding tags in emacs sources themselves.")
;
;  (global-set-key "." 'find-emacs-tag)
;
;;; Little function to enable finding tags in the emacs tags file itself.
;;; Credit Bob Webber, Wayne Mesard, Ashwin Ram
;;; Build the tags file with:
;;; % cd /usr/local/emacs/lisp
;;; % ../etc/etags -f ~/Info/emacs.tags *.el ../src/*.[ch]

(defvar last-emacs-tag nil
  "Tag found by the last find-emacs-tag.")

(autoload 'find-tag-tag "tags"
	  "This hack is here  because there's no provide in tags.el!")

(defun find-emacs-tag (emacs-tagname &optional next other-window)
  "Invoke find-tag on EMACS-TAGNAME using the Emacs tag
table.  The state of the tags variables (tags-file-name and
last-tag) are preserved so that a user can interleave calls
to find-tag and find-emacs-tag.
 If second arg NEXT is non-nil (interactively, with prefix
arg), searches for the next tag in the tag table that
matches the tagname used in the previous find-emacs-tag."
  (interactive (if current-prefix-arg
		   '(nil t)
		 (find-tag-tag "Find Emacs tag: ")))
  (let ((tags-file-name "/gnuemacs/src/TAGS")
	(last-tag last-emacs-tag))
    (find-tag emacs-tagname next other-window)
    (setq last-emacs-tag last-tag)))
