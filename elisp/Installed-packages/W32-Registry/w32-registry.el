;;; w32-registry.el --- A mode for interactively viewing the registry.

;; Copyright (C) 1999, 2000 by Steve Kemp

;; Author: Steve Kemp <skx@tardis.ed.ac.uk>
;; Homepage: http://www.gnusoftware.com/
;; Keywords: tools, convenience, data, outlines
;; Revision: 0.8.6

;; This file is [not yet] part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
 
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTAILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;
;;  This mode presents a regedit.exe like interface for viewing the
;; Windows Registry.
;;
;;  This mode doesn't provide much functionality above that the
;; RegEdit itself provides, (Except for the bookmarking, and the undo
;; support), but I would like to enhance this mode to make it more
;; usefull over time.
;;
;;  Currently using this mode relies upon a patch to your copy of Emacs,
;; in future versions of Emacs, when the required registry functionality
;; is present, there will be no need for this.
;;

;;; Usage:
;;
;; * Put w32-registry.el in your emacs' site-lisp directory, or your
;;   home directory, and optionally, byte-compile it.
;;
;; * Place the following line in your .emacs file;
;; (require 'w32-registry)
;;
;; * To use it then type M-x w32-registry, or use the default keybinding
;;     C-x r
;;
;; * To customize this mode, use the menu option, or execute
;;  "M-x customize-w32-registry"
;;

;;; Limitations:
;;
;;  Currently the mode will only work well with STRING and DWORD
;; values.  Hex (binary) value  editting has been disabled -
;;
;; Actually support is beginning to creep in for this, but its
;; going rather slowly...

;;  TODO:
;;         1.  Add more default bindings.
;;
;; History:
;;
;;   The history grew too much, so it has been relegated to a file of its own.
;;  Find it in "history.txt"
;;
;; Availability:
;;
;;   The most recent version of this file should always be available from
;; http://GNUSoftware.com/ - The source of GNU software for Windows users.
;;
;;;;


;;; Setup - Preamble:

;;  This mode is going to work best of all on a system that has had the patch
;; from http://www.gnusoftware.com/Emacs/Registry/ applied.  
;;
;;  It will work with the replacement interface "w32-reg", that uses
;; an external process to do the interfaceing.
;;
(defvar w32-registry-using-patch (and (eq system-type 'windows-nt)
				     (fboundp 'w32-registry-query-value))
  "Non-nil if the registry patch has been applied")

(defvar w32-registry-external (require 'w32-reg-int)
  "Non-nil if the external interface library is being used.")

(if (and (not w32-registry-using-patch)
	(not w32-registry-external))
    (error "w32-registry.el: No registry patch, or external library detected."))


(defun w32-registry-setup-library ()
  "Setup the `w32-registry' mode to use the external library,
for interfacing with the registry.
  This is not recommended as the default mode of operation,
as it is going to be slower than using the built in patch
interface - However this is simpler for most users to install,
as it doesn't require re-building Emacs from source."
  (interactive)
  (if (require 'w32-reg-int)
      (progn
	;; Finding root keys.
	(defalias 'w32-registry-root-list 'w32-reg-interface-get-root-keys)
	(defalias 'w32-registry-enum-keys 'w32-reg-interface-enum-keys)
	(defalias 'w32-registry-enum-values 'w32-reg-interface-enum-values)
	(defalias 'w32-registry-query-value 'w32-reg-interface-query-value)
	)
    (error "External interface to registry not found : w32-reg-int.el")
    )
  )

;;
;;  If we have the external library, but not the patch then use setup the library,
;; otherwise use the patch - as its faster.
;;
(if (and w32-registry-external
	 (not w32-registry-using-patch))
    (w32-registry-setup-library))



;;; Code:

;;;
;;  Configuration for this mode.
;;
(defgroup w32-registry nil
  "w32-registry customization"
  :group 'applications
  :prefix "w32-registry-")

(defcustom w32-registry-menu-name "Registry"
"The name of the menubar item in which the `w32-registry' items are located."
  :type 'string
  :group 'w32-registry)

(defcustom w32-registry-buffer-name "Registy Viewer"
  "The name of the buffer in which the `w32-registry' will run."
  :type 'string
  :group 'w32-registry)

(defcustom w32-registry-on-hook nil
   "*Hook run when `w32-registry' is turned on."
  :type 'hook
  :group 'w32-registry)

(defcustom w32-registry-off-hook nil
   "*Hook run when `w32-registry' is turned off.
This is run when the user runs `w32-registry-off' from within
a registry buffer, or when a registry mode buffer is killed."
  :type 'hook
  :group 'w32-registry)

(defcustom w32-registry-indent-value 2
  "The level of indentation that is used for each subkey, or value."
  :type 'integer
  :group 'w32-registry)

(defcustom w32-registry-write-enabled t
  "Non-nil if write support is enabled, leave at nil to disable write support."
  :type 'boolean
  :group 'w32-registry)

(defcustom w32-registry-undo-directory (getenv "TEMP")
  "Where undo information is saved after every write, or nil to disable."
  :type 'string
  :group 'w32-registry)

(defcustom w32-registry-enable-bookmarks t
  "Set to non-nil to enable the `w32-registry' bookmark menu entry."
  :type 'boolean
  :group 'w32-registry)

(defcustom w32-registry-key-seperator "->"
  "The seperator the registry key, and its associated value.
This value will be padded out to align all the entries."
  :type 'string
  :group 'w32-registry)

(defcustom w32-registry-default-dword-value 0
  "This is the default value for all newly created DWORD values.
This value is used every time you create a new key, of DWORD type."
  :type 'int
  :group 'w32-registry)

(defcustom w32-registry-default-string-value ""
  "This is the default value for all newly created string values.
This value is used every time you create a new key, of String type."
  :type 'string
  :group 'w32-registry)

(defcustom w32-registry-default-hex-value "0x00"
  "This is the default value for all newly created binary values.
This value is used every time you create a new key, of hex type."
  :type 'string
  :group 'w32-registry)

(defconst w32-registry-version "v0.8.6"
  "The current version number for `w32-registry'.")

(defconst w32-registry-name "Simpson"
  "`w32-registry' Release name, probably chosen at random..")

(defconst w32-registry-maintainer "Steve Kemp <skx@tardis.ed.ac.uk>"
  "Maintainer of `w32-registry'; to whom bug reports should be sent.")

(defvar w32-registry-map ()
  "Keymap for `w32-registry', a major mode for editting the Registry.")

(defvar w32-registry-widget-alist nil
 "`w32-registry' Internal.
A list of keys + widgets that have been displayed, (needed so that the widgets
can be destroyed when they are finished with).")
(make-variable-buffer-local 'w32-registry-widget-alist)

(defvar w32-registry-search-item nil
  "`w32-registry' Internal.
Holds the name of the item that is being searched for.")
(make-variable-buffer-local 'w32-registry-search-item)

(defvar w32-registry-search-key nil
  "`w32-registry' Internal.
Holds the name of the key that is being searched for.")
(make-variable-buffer-local 'w32-registry-search-key)

;;;
;;  Configuration for the Bookmark menu item(s).
;;
(defvar w32-registry-bookmark-list nil
  "List of `w32-registry' registry keys that have been bookmarked.
This is not made buffer local, so that the bookmark editting code can play with it.")

(defvar w32-registry-bookmark-update-menu-p t
  "Non-nil if the `w32-registry' bookmark menu must be updated.")

(defvar w32-registry-bookmark-initialized-p nil
  "Non-nil if `w32-registry' bookmarks are already initialized.")
(make-variable-buffer-local 'w32-registry-bookmark-initialized-p)

(defvar w32-registry-bookmark-save-file (expand-file-name "~/.registry-bookmarks")
  "File to save the contents of `w32-registry-bookmark-list' into.")

(defvar w32-registry-bookmark-menu-title "Bookmarks"
  "Name of the w32-registry-bookmark menu.")

(defvar w32-registry-bookmark-menu-path nil ; '("Registry")
  "Path where to add the `w32-registry-bookmark menu'.
If nil add it at top-level (see also `easy-menu-change')."
)

(defvar w32-registry-bookmark-menu-before "Toggle Key"
  "Name of the menu before which the bookmark menu will be added.
If nil add it add at end of menu (see also `easy-menu-change').")

(defvar w32-registry-bookmark-menu-action 'w32-registry-goto-key
  "Function to invoke with an filename item of the `w32-registry-bookmark' menu.")

(defvar w32-registry-bookmark-max-menu-items 20
  "Maximum number of items saved and displayed in the `w32-registry-bookmark' menu.")

(defvar w32-registry-bookmark-menu-items-for-commands
  (list ["Add Bookmark" w32-registry-add-bookmark t]
	["Edit Bookmarks" w32-registry-edit-bookmarks t]
	)
  "List of menu items to be appended to the bookmark menu.")

;;;
;; Required packages.
;;
(require 'widget)
(require 'easymenu)
(require 'wid-edit)
(require 'cl)

;;;
;; Mode map
;;
(if w32-registry-map
    ()
  (setq w32-registry-map (copy-keymap widget-keymap)))
(define-key w32-registry-map "\C-m" 'w32-registry-return)
(define-key widget-keymap "\C-m" 'w32-registry-return)
(define-key w32-registry-map " " 'w32-registry-toggle-key)
(define-key w32-registry-map "q" 'w32-registry-off)
(define-key w32-registry-map "g" 'w32-registry-goto-key)
(define-key w32-registry-map "c" 'w32-registry-copy-key)
(define-key w32-registry-map "\\d" 'w32-registry-mode-delete-value)
(define-key w32-registry-map "r" 'w32-registry-refresh)

;;;
;; Menu creation
;;
(easy-menu-define
 w32-registry-menu
 w32-registry-map
 "Registry Menu"
 (list
  w32-registry-menu-name
  ;; Submenu for the bookmarks.
  ["Toggle Key" w32-registry-toggle-key t]
  ["Copy Key" w32-registry-copy-key t]
  ["Goto Key" w32-registry-goto-key t]
  "-----"
  ["Collapse all" w32-registry-collapse-all t]
  "-----"
;  ["Find Key" w32-registry-find-key t]
;  ["Find Item" w32-registry-find-item t]
;  "-----"
  (list
   "New"
    ["New Key" w32-registry-new-key t]
    ["New DWORD Value" w32-registry-new-dword-value t]
    ["New Hex Value" w32-registry-new-hex-value t]
    ["New String Value" w32-registry-new-string-value t]
    )
  "-----"
  ["Delete Key" w32-registry-mode-delete-key t]
  ["Delete Value" w32-registry-mode-delete-value t]
  "-----"
  ["Enable Write Support"
   (setq w32-registry-write-enabled (not w32-registry-write-enabled))
   :style toggle :selected w32-registry-write-enabled]
  ["Customize" customize-w32-registry t]
  "-----"
  ["Quit"  w32-registry-off t]
  "-----"
  ["Mail Bug Report" w32-registry-bug-report t]
  ))



;;;###autoload
(defun customize-w32-registry ()
  "Invoke the customization interface on `w32-registry'.
This allows you to change a number of aspects of the mode, including
the indentation of the tree structure, and hooks that are run when the
mode is started / terminated."
  (interactive)
  (customize-group "w32-registry"))


;;;###autoload
(defun w32-registry (  )
  "Start the `w32-registry' Current version : `w32-registry-version'.
This mode is used to view and edit the Windows Registry, for
simplicity it acts in a similar manner to the native program \"RegEdit\".

Currently most of the operations that RegEdit supports are provided,
the only thing that is missing at the moment is a means of searching for
registry items.

Additional features provided include a method of specifying bookmarks,
allowing a read-only access to the registry, and the ability to automatically
create \"undo\" files to protect against erroneous changes.

In the future more features would be nice, planned features for future
versions include :

  Completion for registry key entry.
  Links to web sites that contain information about the registry.
  Syntax colouring for the main hive keys.

w32-registry is currently bound to \\[w32-registry], and may be
customized  via `customize', or or by simply invoking
`customize-w32-registry'.

Bug reports should be sent to `w32-registry-maintainer'"
  (interactive)
  (w32-registry-on))

(defalias 'w32-registry-mode 'w32-registry)

(defun w32-registry-on ()
  "`w32-registry' Internal.
Turns on `w32-registry', setting up the menus, and the `major-mode' name, etc."
  (switch-to-buffer (get-buffer-create w32-registry-buffer-name))
  (kill-all-local-variables)
  (widget-minor-mode 1)
  (setq inhibit-read-only t)
  (setq major-mode 'w32-registry
	mode-name "Registry"
	truncate-lines t)
  (w32-registry-collapse-all)
  (use-local-map w32-registry-map)
  (run-hooks 'w32-registry-on-hook)
  (w32-registry-bookmark-initialize))


(defun w32-registry-off ()
  "Quit `w32-registry', destroying the currently active buffer.
This function will run the hook `w32-registry-off-hook' after the buffer
has been destroyed."
  (interactive)
  ;; Perform this step to delete all the widgets.
  ;; CONFIRM: Probably not necessary, as the buffer will die.
  (w32-registry-collapse-all)
  (kill-buffer (current-buffer))
  (run-hooks 'w32-registry-off-hook))


;; FIXME: DONE
(defun w32-registry-copy-key ()
  "Copy the name of the currently selected registry key to the kill ring."
  (interactive)
  (let ((key (w32-registry-get-current-path)))
    (if (listp key)
	(setq key (concat (car key) (cdr key))))
    (kill-new key)
    (message "Copied %s" key)))


(defun  w32-registry-goto-key ( key )
  "Navigate to a particular key in the registry, expanding the tree as necessary."
  (interactive "sNavigate to key : ")
  (goto-char (point-min))
  (let ((keys (split-string key "[\\]+")))
    (while keys
      (let ((search (concat "\\[\\([+\\|-]\\)\\] "
			    (regexp-quote (car keys )))))
	(if (re-search-forward search nil t)
	    (progn
	      (if (equal (match-string 1) "+")
		  (w32-registry-toggle-key)
		(message "%s" (match-string 1))))
	  (re-search-forward (car keys) nil t))
	(setq keys (cdr keys)))
    )))

;; FIXME:
; (defun w32-registry-find-key (val)
;   "Recursively search for a registry key whose name contains some specified text.
;   The text is searched in a case insensitive manner."
;   (interactive "sKey substring to search for (Regexp) : ")
;   (setq w32-registry-search-key val)
;   (let ((key (w32-registry-get-current-path)))
;     (message "Recursively searching from %s" key)
;     (w32-registry-apply key 'w32-registry-find-key-function)))

; ;; FIXME:
; (defun w32-registry-find-key-function (key)
;   "w32-registry Internal.  Function to find a matching string in a registry key name."
;   (interactive "sInternal")
;   (if (w32-registry-string-match w32-registry-search-key key)
;       (progn
; 	(w32-registry-goto-key key))))

; ;; FIXME:
; (defun w32-registry-find-item (val)
;   "Recursively search for a registry key whose data contains some specified text.
;   The text is searched in a case insensitive manner."
;   (interactive "sData substring to search for (Regexp) : ")
;   (setq w32-registry-search-item val)
;   (let ((key (w32-registry-get-current-path)))
;     (message "Recursively searching from %s" key)
;     (w32-registry-apply key 'w32-registry-find-item-function)))

; ;; FIXME:
; (defun w32-registry-find-item-function (key)
;   "w32-registry Internal.  Function to find a matching string in a registry key value"
;   (interactive "sInternal")
;   (let ((data (w32-registry-read-value key)))
;     (if (w32-registry-string-match w32-registry-search-item data)
;       (progn
; 	(w32-registry-goto-key key)))))


(defun w32-registry-string-match(REGEXP STRING &optional START)
  "Like `string-match', but will convert integers to strings if necessary.
This function will return `nil' if the arguments are nil."
  (interactive)
  (if (not STRING)
      nil
   (progn
     (if (numberp REGEXP)
	 (setq REGEXP (int-to-string REGEXP)))
     (if (numberp STRING)
	 (setq STRING (int-to-string STRING)))
     (string-match REGEXP STRING START))))
   

(defun w32-registry-toggle-key ()
  "Toggle the state of the current branch of the registry.

This function will expand, or contract the current view of the registry
as necessary.

The act of toggling the state of a branch twice should be enough to see
newly created keys."
  (interactive)
  (save-excursion
    (end-of-line)
    (let ((eol (point)))
      (beginning-of-line)
      (re-search-forward "\\[" eol t)
      (if (looking-at "+")
	  (registry-expand-region)
	(if (looking-at "-")
	    (w32-registry-contract-region)
	  (progn
	    (end-of-line)
	    (re-search-backward "\\[\\+\\|-\\]" nil t)
	    (w32-registry-toggle-key)))))))

;; FIXME: DONE
(defun w32-registry-contract-region ()
  "Contract the currently expand region.
This will remove all of the expanded keys beneath itself, recursively.
One complication of this is that all the visible widgets must be
destroyed - otherwise errors can occur."
  (interactive)
  (delete-char 1)
  (insert "+")
  (end-of-line)
  ;; Delete all widgets in the region
  (if (listp (w32-registry-get-current-path))
      (setq w32-registry-widget-alist (w32-registry-remove-widgets-from-list
					    (car (w32-registry-get-current-path))))
    (setq w32-registry-widget-alist (w32-registry-remove-widgets-from-list
					  (w32-registry-get-current-path))))

  ;; Now do the real work
  (let ((pos (point))
	(end nil)
	(level (w32-registry-get-indent-level))
	(finished nil))
    (forward-line 1)
    (beginning-of-line)
    (while (not finished)
      (setq end (re-search-forward "^\\([ ]*\\[\\)" nil t))
      (if (eq end nil)
	  (setq finished t))
      (if (<= (w32-registry-get-indent-level) level)
	  (setq finished t)))
    (if end
	(progn
	  (beginning-of-line)
	  (delete-region pos (- (point) 1))
	  )
      (delete-region pos (point-max)))))

;; FIXME:
(defun registry-expand-region ()
  "w32-registry Internal.
Expand the currently contracted region, inserting all subkeys, and sub
values into the buffer."
  (interactive)
  (delete-char 1)
  (insert "-")
  (end-of-line)
  (let ((path (w32-registry-get-current-path)))
    (message "Key : %s " path)
    (let ((depth (w32-registry-get-indent-level)))
      (let ((temp nil))
	;; Current indentation depth
	(setq depth (+ depth w32-registry-indent-value))
	;; Insert values, if any first.
	(w32-registry-insert-values)
	;; Insert all subkeys
	(let ((keys (w32-registry-enum-keys path)))
	  (if keys
	      (progn
		(setq keys (sort keys 'string<))
		(while keys
		  (insert "\n")
		  (setq temp depth)
		  (while (> temp 0)
		    (insert " ")
		    (setq temp (- temp 1)))
		  (insert "[+] ")
		  (insert (car keys))
		  (setq keys (cdr keys)))))
	  )
	)
      )
    )
  )


(defun w32-registry-insert-values ()
  "w32-registry Internal.
Insert the values beneath the current key - This function has to manage the
creation of the widgets."
  (setq path (w32-registry-get-current-path))
  (let ((values (w32-registry-enum-values path))
	(width 0))
    (if values
	(progn
	  (setq values (sort values 'string<))
	  (setq width (w32-registry-longest-in-list values))
	  (while (not (eq (car values) nil))
	    (insert "\n")
	    (setq temp depth)
	    (while (> temp 0)
	      (insert " ")
	      (setq temp (- temp 1)))
 	    ;; Insert key name
	    (if (> (length (car values)) 1)
		(w32-registry-insert-and-pad (car values) width)
	      (w32-registry-insert-and-pad "(Default)" width))
	    (insert " ")
	    (insert w32-registry-key-seperator)
	    (insert " ")
	    ;; Insert the value, in an editable form.
	    (let ((keyname  (w32-registry-get-current-path)))
	      (w32-registry-insert-key-value keyname)
	      (setq values (cdr values))))))))


(defun w32-registry-longest-in-list ( list )
  "Return the length of the longest item in the list."
  (interactive)
  (let ((longest 0))
    (while (car list)
      (if (> (length (car list)) longest)
	  (setq longest (length (car list))))
      (setq list (cdr list)))
    longest))


(defun w32-registry-insert-and-pad (string width)
  "Insert `string', and pad with spaces so that it takes up width characters."
  (let ((strlen 0))
    (setq strlen (length string))
    (if string
	(insert string))
    (while (< strlen width)
      (insert " ")
      (setq width (- width 1)))))


(defun w32-registry-insert-key-value ( key )
  "w32-registry Internal.
Insert the name of a subkey, and its value into the current buffer,
using widgets.
The inserted key will have an appropriate description of the key type
inserted as well."
  (interactive)
  (let ((root (car key))    ;; Root key
 	(subkey (cdr key))  ;; Subkey name
	(result nil)        ;; Result of read call
	(type   nil)        ;; Type of the registry entry
	(value  nil))       ;; Value contained in the registry key.

    (if (not subkey)
	(setq subkey ""))

    (setq result (w32-registry-query-value root subkey))
    (let ((type (car result))
	  (value (cdr result)))
      ;; Insert a description of the type, then the entry field for the type itself.
      ;; If the value is not a hex one then users may edit it.
      ;; Should we have REG_DWORD_LITTLE_ENDIAN ?
      (cond ((or (eq type 'REG_DWORD_BIG_ENDIAN)
		 (eq type 'REG_DWORD)
		 (equal type "DWORD"))
	     (insert "DWORD  ")
	     (w32-registry-insert-widget value t))
	    ((or (eq type 'REG_SZ)
		 (equal type "STRING")
		 (eq type 'REG_MULTI_SZ)  ;; Probably wrong.
		 (equal type "MULTI_STRING"))
	     (insert "STRING ")
	     (w32-registry-insert-widget value t))
	    ((or (eq type 'REG_BINARY)
		 (equal type "BINARY"))
	     (insert "BINARY ")
	     (w32-registry-insert-widget (w32-registry-binary-value-to-editable-string value) nil))
	    ((eq type 'REG_FULL_RESOURCE_DESCRIPTOR)
	     (insert "RES    ")
	     (w32-registry-insert-widget (w32-registry-binary-value-to-editable-string value) nil))
	    ((or (eq type 'REG_EXPAND_SZ)
		 (equal type "E_STRING"))
	     (insert "E.STRING ")
	     (w32-registry-insert-widget value t))
	    (t (insert "ERROR! ")
	       (message "Unknown type: %s" type)))
      )))


(defun w32-registry-insert-widget (value non-binary)
  "`w32-registry' Internal.
Insert a widget entry field, holding the value specified, if non-binary
is t then the text is entered as-is, otherwise it is encoded to/from
Hex values before display.
(i.e. The \"set\" button is included)."
  (let ((stringValue nil))
    (if (stringp value)
	(setq stringValue value))
    (if (numberp value)
	(setq stringValue (int-to-string value)))
    (if (listp value)
	(setq stringValue (list-to-string value)))

    (let ((widget
	   ;; The text field.
	   (widget-create 'editable-field
			  :size 25
			  :format "Value: %v "
			  stringValue)))

      (setq w32-registry-widget-alist (cons (cons (w32-registry-get-current-path) widget) w32-registry-widget-alist))

    ;; Create the push button
    (if non-binary
	(progn
	  (let ((button (widget-create 'push-button
				       :key (w32-registry-get-current-path)
				       ;; The edit field that contains the value of the key.
				       :edit widget
				       :notify (lambda (widget &rest ignore)
						 (let ((value (widget-value (widget-get widget :edit))))
						   (w32-registry-set-key (widget-get widget :key) value (w32-registry-get-key-type))))
						 "Set Value")))
 	    (setq w32-registry-widget-alist (cons (cons (w32-registry-get-current-path) button) w32-registry-widget-alist))))
	(progn
	  (let ((button (widget-create 'push-button
				       :key (w32-registry-get-current-path)
				       ;; The edit field that contains the value of the key.
				       :edit widget
				       :notify (lambda (widget &rest ignore)
						 (let ((value (widget-value (widget-get widget :edit))))
						   (w32-registry-set-key (widget-get widget :key) (w32-registry-editable-string-to-binany value) (w32-registry-get-key-type))))
						 "Set Value")))
 	    (setq w32-registry-widget-alist (cons (cons (w32-registry-get-current-path) button) w32-registry-widget-alist))))
    )
    ;; Setup widgets.
    (widget-setup))))


;; FIXME: DONE
(defun w32-registry-set-key ( key value type)
  "w32-registry Internal.
Called when the the 'apply' button is pressed, and a registry key has
been modified.

This function will extract the text from the relevent field, and set the
registry -- Iff write support is enabled.

If `w32-registry-undo-directory' is non-nil undo information will be
saved."
  (if w32-registry-write-enabled
      (progn
	(let ((branch (car key))
	      (subkey (cdr key)))
	  (message "Setting %s %s(%s) to %s" branch subkey type value)
	  (if w32-registry-undo-directory
	      (w32-registry-save-undo-info branch subkey value type))
	  (if subkey
	      (w32-registry-set-value branch subkey value type)
	    (w32-registry-set-value branch "" value type))))
    (message "Write support not enabled!")))


;; NEW:
(defun w32-registry-save-undo-info( branch subkey value type )
  "Backup registry before changes are applied.
This backup solution is done on a key-by-key basis before all
changes are applied, unless there `w32-registry-undo-directory'
is nil.

This undo mechanism could bog you down with many small files, but
for testing purposes it is ideal."
  (message "Saving Undo Information into %s" w32-registry-undo-directory)
  (let ((save-dir (expand-file-name w32-registry-undo-directory))
	(files    nil)    ;; Files in undo directory.
	(next-file nil)   ;; The file we're going to use.
	(name-regexp nil) ;; matches all our undo files.
	)
 
    ;; Ensure the save directory exists.
    (if (not (file-directory-p save-dir))
	(make-directory save-dir))

    ;; Numbered undo files.
    (setq name-regexp "reg-undo-\\([0-9]+\\)\\.reg$")

    ;; Get a list of files in that directory, and
    (setq files (directory-files save-dir))
    (while files
      (if (string-match name-regexp (car files))
	  (setq next-file (match-string 1 (car files))))  ;;(car-files)))
	(setq files (cdr files)))

    ;; No file, use default file.
    (if (not next-file)
	(setq next-file "0"))

    ;; Increment.
    (setq next-file (number-to-string (+ (string-to-number next-file) 1)))
    (setq next-file (concat "reg-undo-" next-file ".reg"))

    (set-buffer (get-buffer-create (concat "*Registry-Undo:" next-file "*")))
    (insert "REGEDIT4")
    (insert "\r\n\r\n")
    (insert (concat "[" branch "]\r\n"))
    (if subkey
	(insert (concat "\"" subkey "\"="))
      (insert "@="))

    (cond ((or (eq type 'REG_DWORD_BIG_ENDIAN)
	       (eq type 'REG_DWORD))
	   (insert "dword:"))
	  ((or (eq type 'REG_SZ)
	       (eq type 'REG_MULTI_SZ))  ;; Probably wrong.
	   (insert (concat "\"" (w32-registry-escape value) "\"")))
	  ((eq type 'REG_BINARY)
	   (insert "hex:")
	   (insert (w32-registry-export-bin value))
	   ))
    (insert "\r\n")

    ;; Write the region, and kill the temp buffer.
    (write-region (point-min) (point-max) (concat w32-registry-undo-directory "\\" next-file) nil 0)
    (kill-buffer (get-buffer-create (concat "*Registry-Undo:" next-file "*")))
    )
  )


(defun w32-registry-escape( string )
  "Escape \"\\\"'s, and \"'s."
  (let ((begin 0 )
	(end 0 ))
    (while (and (string-match "[\\|\"]" string begin)
		(< begin  (length string)))
      (setq begin (match-beginning 0))
      (setq end (match-end 0))
      (setq string (concat (substring string 0 begin) "\\" (match-string 1) (substring string (- end 1))))
      (setq begin (+ 2 begin))
      )
    string
    )
  )

;; NEW:
(defun w32-registry-export-bin( string )
  "Return a properly formatted string that may be written to a .reg file."
  (let ((result (w32-registry-binary-value-to-editable-string string))
	(temp nil))
    (setq temp (split-string result))
    (setq result (mapcar '(lambda (c)
			    (concat (substring c 2) ","))
			 temp))
    (list-to-string result)))


;; NEW:
(defun w32-registry-get-key-type ()
  "Find, and return, the type of the currently selected key.
The value that is returned is a value that can actually be
used for writing to the registry, one of REG_SZ, REG_BINARY,
or REG_DWORD."
  (interactive)
  (let ((position nil)
	(match nil))
    (save-excursion
      (end-of-line)
      (setq position (point))
      (beginning-of-line)
    (re-search-forward (concat ".*" (regexp-quote w32-registry-key-seperator) " *\\(BINARY\\|DWORD\\|STRING\\|RES\\).*Value") position t)
      (setq match (match-string-no-properties 1))
      (message "Key type is : %s" match))
      (cond ((equal "DWORD" match)
	     'REG_DWORD)
	    ((equal "E.STRING" match)
	     'REG_EXPAND_SZ)
	    ((equal "STRING" match)
	     'REG_SZ)
	    ((equal "STRING" match)
	     'REG_SZ)
	    ((equal "RES   " match)
	     'REG_FULL_RESOURCE_DESCRIPTOR)
	    ((equal "BINARY" match)
	     'REG_BINARY)
	    (t nil))
      ))
  

;; FIXME: DONE
(defun w32-registry-get-current-path ()
  "Return the complete path to the currently selected value.
The value is returned as a list, with the key name as the first item,
and the value name as the second item - if present."
  (interactive)
  (let ((path "")
	(temp "")
	(working 1)
	(position (point))
	(length (w32-registry-get-indent-level))
	(finished nil))
    (save-excursion
      (end-of-line)
      (if (eq 0 (w32-registry-get-indent-level))
	  (progn
	    ;;  On the first line.
	    (re-search-backward "^\\[[+\\|-]\\][ ]*\\(.*\\)$" nil t)
	    (setq path (concat (match-string 1) "\\")))
	(progn
	  ;; On a subkey
	  (while (and (not finished)
		      (re-search-backward "^\\([ ]*\\[\\)[+\\|-]\\][ ]*\\(.*\\)$" nil t))
	    (setq temp  (match-string 2))
	    (if (equal length (w32-registry-get-indent-level))
		(progn
		  (setq path (concat temp "\\" path))
		  (setq length (- length w32-registry-indent-value))
		  ))
	    (if (eq 0 (w32-registry-get-indent-level))
		(setq finished t))))))
    (save-excursion
      (let ((end nil)
	    (current (point)))
	(setq end)
	(end-of-line)
	(setq end (point))
	(beginning-of-line)
	(if (re-search-forward (concat "^[ ]*\\(.*\\) " (regexp-quote w32-registry-key-seperator )) end t)
	    (if (equal (w32-registry-remove-trailing-whitespace (match-string 1)) "(Default)")
		(setq path (cons path nil))
	      (setq path (cons path (w32-registry-remove-trailing-whitespace (match-string 1)))))
	  )))
    path))


;; FIXME: Doesn't handle strings of one character properly.
(defun w32-registry-remove-trailing-whitespace ( string )
  "Strip trailing whitespace from the string and return it."
  (interactive)
  (let ((temp-list nil)
	(count  0)
	(space-finished      nil))
    (setq count (- (length string) 1))
    (while (and (>= count 0) (not space-finished))
      ;; Get a character
      (if (not (string-equal " "  (substring string count (+ count 1))))
	  (setq space-finished t))
      (setq count (- count 1)))
    (substring string 0 (+ count 2))))


(defun w32-registry-get-indent-level ()
  "`w32-registry' Internal.
Get the level of indentation of the value under the point."
  (interactive)
  (save-excursion
    (end-of-line)
    (re-search-backward "^\\([ ]*\\)\\[" nil t)
    (length (match-string 1))))



;; FIXME: DONE
(defun w32-registry-collapse-all ()
  "Collapse all the open visible keys.
This works by erasing the contents of the current buffer.

[CONFIRM:  The entries are displayed in sorted order - we
should have HKEY_CURRENT_CONFIG last to match regedit.]"
  (interactive)
  (let ((list w32-registry-widget-alist))
    ;; Destroy all the visible widgets.
    (while list
      (let ((head (car list))
	    (tail (cdr list)))
	(widget-delete (cdr head))
	(setq list tail)))
    )
  (setq w32-registry-widget-alist nil)
  ;; Reset buffer.
  (erase-buffer)
  (insert "\n\tGNU Emacs Registry Viewer ")
  (insert (concat w32-registry-version " (" w32-registry-name ")"))
  (insert "\n\n\n")
  (save-excursion
 (let ((root-keys (w32-registry-root-list)))
    (setq root-keys (sort root-keys 'string<))
    (while (car root-keys)
      (w32-registry-insert-and-pad "" w32-registry-indent-value)
      (insert (concat "[+] " (car root-keys) "\n"))
      (setq root-keys (cdr root-keys))))

  (insert "\n\n")
  (w32-registry-insert-and-pad "" w32-registry-indent-value)
  (widget-create 'push-button
		 ;; The edit field that contains the value of the key.
		 :notify (lambda (widget &rest ignore)
			   (kill-buffer (current-buffer)))
				      "Quit"))
)


;; CHECK: Should see what mailer the user wants to use.
(defun w32-registry-bug-report ()
  "Mail a bug report to the maintainer of `w32-registry'.
This is the person named in `w32-registry-maintainer'."
  (interactive)
  (let ((subject (concat "w32-registry " w32-registry-version)))
    (setq subject (concat subject " [" w32-registry-name "] Bug report."))
    (switch-to-buffer subject)
    (insert (concat "To: " w32-registry-maintainer "\n"))
    (insert (concat "Subject: " subject "\n"))
    (insert "--text follows this line--\n")
    (insert "[Please try to include as much information as possible in this mail.]")
    (mail-mode)))


;; FIXME: DONE
(defun w32-registry-remove-widgets-from-list (key)
  "w32-registry Internal.
Remove all the widgets beneath the specified key from the list of
widgets, and destroy the actual widget, this is necessary as otherwise
re-creating widgets in the same area of the screen confuses
`widget-minor-mode'."
  (let ((list w32-registry-widget-alist)
	(newlist nil))
    (while list
      (let ((head (car list))
	    (tail (cdr list)))
	;;  We want to match all sub-keys of the currently contracting key
	;; as well as the key itself, this means that all expanded branches beneath
	;; the current key will have their associated widgets destroyed.
	(if (w32-registry-string-match (regexp-quote key) (car (car head)))
	    (widget-delete (cdr head))
	  (setq newlist (cons head newlist)))
	(setq list tail)))
    newlist))


;; FIXME: DONE
(defun w32-registry-mode-delete-key ()
  "Delete the currently selected key.
This will only work if the registry mode write support is enabled."
  (interactive)
  (if w32-registry-write-enabled
      (progn
	(if (yes-or-no-p "Really delete this value")
	    (progn
	      (let ((key (w32-registry-get-current-path)))
		(if (listp key)
		    (setq key (car key)))
		(message "Deleting on %s" key)

		(w32-registry-delete-key key)
		(w32-registry-refresh)))))
    (message "Write support not enabled!")))

;; FIXME: DONE
(defun w32-registry-mode-delete-value ()
  "Delete the currently selected value.
This will only work if the registry mode write support is enabled.
We also will save an undo file."
  (interactive)
  (if w32-registry-write-enabled
      (progn
	(if (yes-or-no-p "Really delete this key")
	    (progn
	      (let ((key (w32-registry-get-current-path))
		    (branch nil)
		    (subkey nil)
		    (value nil))
		(if (listp key)
		    (progn
		      (setq branch (car key))
		      (setq subkey (cdr key))
		      ))

		(setq value (cdr (w32-registry-query-value branch subkey)))
		(message "Value we're going to delete contains %s" value)
		(if w32-registry-undo-directory
		    (w32-registry-save-undo-info branch subkey value (w32-registry-get-key-type)))
		(message "Deleting-value on %s %s" branch subkey)
		(w32-registry-delete-value branch subkey)
	      (w32-registry-refresh))))
    (message "Write support not enabled!"))))


;; FIXME:
(defun w32-registry-new-key ( name)
  "Insert a new key in the current position."
  (interactive "sKey Name : ")
  (save-excursion
    (beginning-of-line)
    (re-search-backward "\\[\\-\\]" nil t)
    (w32-registry-create-key (concat (w32-registry-get-current-path) name)))
  (w32-registry-refresh))


;; FIXME: DONE
(defun w32-registry-new-string-value ( name)
  "Insert a new string value in the current position."
  (interactive "sSTRING Value Name : ")
  (let ((key (w32-registry-get-current-path)))
    (if (listp key)
	(setq key (car key)))
    (message "Creating : %s\\%s" key name)
    (w32-registry-set-value key name w32-registry-default-string-value 'REG_SZ)
    (w32-registry-refresh)))

;; FIXME: DONE
(defun w32-registry-new-hex-value ( name)
  "Insert a new binary value in the current position."
  (interactive "sHex Value Name : ")
  (let ((key (w32-registry-get-current-path)))
    (if (listp key)
	(setq key (car key)))
    (message "Creating : %s\\%s" key name)
    (w32-registry-set-value key name
			    (w32-registry-editable-string-to-binany
			     w32-registry-default-hex-value) 'REG_BINARY)
    (w32-registry-refresh)))


;; FIXME: DONE
(defun w32-registry-new-dword-value ( name )
  "Insert a new dword value in the current position."
  (interactive "sDWORD Value Name : ")
  (let ((key (w32-registry-get-current-path)))
    (if (listp key)
	(setq key (car key)))
    (message "Creating : %s\\%s" key name)
    (w32-registry-set-value key name w32-registry-default-dword-value 'REG_DWORD)
    (w32-registry-refresh)))


(defun w32-registry-refresh ()
  "Refresh the current view of the registry.
Doing this will force the display of new keys/values, and removal
of deleted items."
  (interactive)
  (save-excursion
    ;; Get current path
    (re-search-backward "\\[\\-\\]" nil t)
    (beginning-of-line)
    (w32-registry-toggle-key)
    (w32-registry-toggle-key)))


;; FIXME: DONE
(defun w32-registry-add-bookmark ()
  "Add the current key to the list of bookmarks.
This option will only proceed if `w32-registry-enable-bookmarks'
is non-nil, after the item has been successfully added the bookmark file
is written to disk."
  (interactive)
  (if w32-registry-enable-bookmarks
      (progn
	(let ((key (w32-registry-get-current-path)))
	  (if (listp key)
	      (setq key (concat (car key) (cdr key))))
	(w32-registry-bookmark-add-key key)
	))))


(defun w32-registry-edit-bookmarks ()
  "Allow the user to edit the bookmarks they have saved."
  (interactive)
  (message "Editting")
  (let ((buffer (get-buffer-create "*Bookmarks*")))
    (set-buffer buffer)
    (w32-registry-insert-bookmarks buffer)
    (pop-to-buffer buffer)))

(defun w32-registry-insert-bookmarks (buffer)
  "Insert the list of bookmarks into the named buffer.
Also insert widgets to allow the user to delete bookmarks."
  (set-buffer buffer)
  (erase-buffer)
  (insert "\n\tBookmark Editor\n\n")

  (mapcar '(lambda (e)
	     (insert "\t")
	     (widget-create 'push-button
			    :key e
			    ;; The edit field that contains the value of the key.
			    :notify (lambda (widget &rest ignore)
				      (setq w32-registry-bookmark-list (remove (widget-get widget :key)  w32-registry-bookmark-list))
				      (w32-registry-bookmark-save-list)
				      (setq w32-registry-bookmark-update-menu-p t)
				      (erase-buffer)
				      (w32-registry-insert-bookmarks (current-buffer)))
				      "Delete")
	     (insert (format "  %S\n" e)))
	  w32-registry-bookmark-list)

  (insert "\n\n\t")
  (widget-create 'push-button
		 ;; The edit field that contains the value of the key.
		 :notify (lambda (widget &rest ignore)
			   (kill-buffer (current-buffer))
			   (switch-to-buffer (get-buffer-create w32-registry-buffer-name)))
				      "Quit")

  (widget-minor-mode 1)
)

(defun w32-registry-return (&optional pos)
  "Called when return is pressed.
This routine must call `widget-button-press' when over a
widget, and toggle the current key otherwise."
  (interactive)
  (unless pos
    (setq pos (point)))
  (save-excursion
    (end-of-line)
    (let ((eol (point)))
      (beginning-of-line)
      (if (re-search-forward "\\[\\([+\\|-]\\)\\]" eol t)
	    (w32-registry-toggle-key)
	(widget-button-press pos))
      )))



;;;
;; Bookmark Code:
;;
(defun w32-registry-bookmark-initialize ()
  "`w32-registry' Internal.
Initialize the registry key bookmarks, from the `w32-registry-bookmark-save-file'."
  (interactive)
  (unless w32-registry-bookmark-initialized-p
    (setq w32-registry-bookmark-initialized-p t)
    (if (file-readable-p w32-registry-bookmark-save-file)
        (load-file w32-registry-bookmark-save-file))
    (setq w32-registry-bookmark-update-menu-p t)
    (add-hook 'menu-bar-update-hook 'w32-registry-bookmark-update-menu-hook)
    (add-hook 'kill-buffer-hook 'w32-registry-kill-buffer-hook)
    )
  )


(defun w32-registry-bookmark-update-menu-hook ()
  "Update bookmark menu from the current `bookmark-list'."
  (when w32-registry-bookmark-update-menu-p
    (condition-case nil
	(progn
	  (easy-menu-change w32-registry-bookmark-menu-path
			    w32-registry-bookmark-menu-title
			    (w32-registry-bookmark-make-menu-items)
			    w32-registry-bookmark-menu-before)
	  (setq w32-registry-bookmark-update-menu-p nil))
      (error nil))
    )
  )

(defun w32-registry-kill-buffer-hook ()
  "Hook that is called when the `w32-registry' buffer is killed.
Here we remove the hook that is watching for buffer terminations, and we
write the bookmark list."
  (if (equal (buffer-name) w32-registry-buffer-name)
      (progn
	(remove-hook 'kill-buffer-hook 'w32-registry-kill-buffer-hook)
	(w32-registry-bookmark-save-list)
	(run-hooks 'w32-registry-off-hook)
	)
    )
  nil)

(defun w32-registry-bookmark-save-list ()
  "Save the current `w32-registry-bookmark-list'.
The bookmarks are saved in the file `w32-registry-bookmark-save-file'."
  (interactive)
  (let ((saved-list (w32-registry-bookmark-elements w32-registry-bookmark-max-menu-items)))
    (with-temp-buffer
      (erase-buffer)
      (insert "(setq w32-registry-bookmark-list\n      '(\n")
      (mapcar '(lambda (e)
                 (insert (format "        %S\n" e)))
              saved-list)
      (insert "        ))")
      (if (file-writable-p w32-registry-bookmark-save-file)
          (write-region (point-min) (point-max) w32-registry-bookmark-save-file)
	(message "w32-registry bookmark file `%s' is not writable." w32-registry-bookmark-save-file))
      (kill-buffer (current-buffer))))
  nil)


(defun w32-registry-bookmark-make-menu-items ()
  "Make menu items from `w32-registry-bookmark-list'."
  (let ((file-items (mapcar '(lambda (entry)
			       (vector entry (list w32-registry-bookmark-menu-action entry) t))
			    (w32-registry-bookmark-elements w32-registry-bookmark-max-menu-items))))
    (append (or file-items (list ["No Bookmarks" t nil]))
            (and w32-registry-bookmark-menu-items-for-commands
                 (cons ["---" nil nil]
                       w32-registry-bookmark-menu-items-for-commands)))))



(defun w32-registry-bookmark-add-key (keyname)
  "Add or move keyname to the beginning of `w32-registry-bookmark-list'."
  (setq w32-registry-bookmark-list
	(cons keyname (remove keyname  w32-registry-bookmark-list)))
  (setq w32-registry-bookmark-update-menu-p t))


(defun w32-registry-bookmark-elements (n)
  "Return a list of the first N elements of `w32-registry-bookmark-list'."
  (let ((lh nil) (l w32-registry-bookmark-list))
    (while (and l (> n 0))
      (setq lh (cons (car l) lh))
      (setq n (1- n))
      (setq l (cdr l)))
    (nreverse lh)))




;;; ----------------------------------------------------------------------
;; 1990, Sebastian Kremer, Institute for Theoretical Physics, West Germany
;; BITNET: ab027@dk0rrzk0.bitnet
;;
(defsubst list-to-string (list &optional separator)
  "Convert LIST into string.  Optional SEPARATOR defaults to \" \".

Input:

  LIST       '(\"str\" \"str\" ...)
  separator  ' '

Return:
  str"
  (mapconcat
   (function identity)			;returns "as is"
   list
   (or separator " ")
   ))

;;; 08 Jun 1997 Jamie Zawinski <jwz@netscape.com> comp.emacs
;;;
(defun from-hex-string (str)
  "Convert integer hex string to int."
  (if (string-match "\\`0x" str) 
      (setq str (substring str 2)))
  (let ((chars "0123456789abcdefgh")
        (case-fold-search t)
        (n 0)
        (i 0))
    (mapcar '(lambda (c)
               (setq i (string-match (make-string 1 c) chars))
               (if (>= (or i 65536) 16)
                   (error "%c illegal in base %d" c 16))
               (setq n (+ (* n 16) i)))
            (append str nil))
    n))

;; This version by Andrew Innes <andrewi@gnu.org>
(defun w32-registry-binary-value-to-editable-string (string)
  "Convert a binary string read from the registry to hex.
The hex representation is used to allow the user to edit
the data, before it is written back to the registry."
  (let* ((str (string-as-unibyte string))
	 (len (length str))
	 (result (make-string (* 5 len) 0))
	 (i 0)
	 (j 0)
	 (digits "0123456789abcdef")
	 ch)
    (while (< i len)
      (setq ch (aref str i))
      (aset result j ?0)
      (setq j (1+ j))
      (aset result j ?x)
      (setq j (1+ j))
      (aset result j (aref digits (lsh ch -4)))
      (setq j (1+ j))
      (aset result j (aref digits (logand ch 15)))
      (setq j (1+ j))
      (aset result j ?\ )
      (setq j (1+ j))
      (setq i (1+ i)))
    (substring result 0 (- (length result) 1))))


(defun w32-registry-editable-string-to-binany (string)
  "Convert a string of hex characters to a binary form.
This binary form will be used for writing to the registry."
  (let ((temp nil)
	(result nil))
    (setq temp (mapcar '(lambda (c)
			  (char-to-string (from-hex-string c)) )
		       (split-string string)))
    (while temp
      (setq result (concat result (car temp)))
      (setq temp (cdr temp)))
    result))

;; Binding for startup.
(global-set-key "\C-xr" 'w32-registry)



(provide 'w32-registry)

;;; w32-registry.el ends here
