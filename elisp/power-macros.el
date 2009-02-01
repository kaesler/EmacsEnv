;;{{{ Copyright

;; Copyright (C) 1999 Jesper Kjær Pedersen <blackie@ifad.dk>
;;
;; Author: Jesper Kjær Pedersen <blackie@ifad.dk>
;; Home page: http://www.imada.sdu.dk/~blackie/emacs/
;; Created: 25 May. 1999
;; This release: 
;; Version 1.1a2
;; Keywords: macros

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;}}}
;;{{{ Commentary

;;
;; Keyboard Macros are a very powerful tool, if you know how to use them the
;; right way!  It is however a bit difficult in Emacs to define and
;; maintain several macros at a time. This problem is solved with this
;; package.
;;
;; When you have loaded this packages Emacs will, upon comletion of macro
;; definition, ask you which key you want to assign this macro to and ask
;; for a description of the macro. 
;; If something is already bound to this key, Emacs will ask you whether
;; you want to override this binding. Furthermore, this package also takes
;; care of saving the macro to your .emacs file for later Emacs sessions.
;;
;; The description for all the defined macros may be obtained with the
;; function `pm-manage-macros'. Using this function you can also manage you
;; power-macros.
;;
;; This package has been developed as part of my job on writing the book
;; "Sams Teach yourself Emacs in 24 hours".
;; For more information about this book, please go to the page
;; http://www.imada.sdu.dk/~blackie/emacs/

;;}}}
;;{{{ Installation description

;; To install this package, copy this file to a directory in your load-path
;; and insert the following into your .emacs file:
;; (require 'power-macros)
;; 
;; To configure the package, simply type `M-x customize-group RET power-macros'

;;}}}
;;{{{ History Information

;; 0.1 First official release.
;; 0.1.1 Fixed a few spelling errors (thanks to
;;       camille.troillard@worldonline.fr)
;; 0.2 Made the package into a real minor mode, added a menu to the
;;     menu-bar, and made it customizable using customize
;; 0.2.1 Added support for XEmacs menus 
;;       thanks to Jan Vroonhof <vroonhof@math.ethz.ch>)
;;       and added a power-macros-hook.
;;       Fixed installation description
;; 0.3a  - Major rewrites.
;;       - Added support for binding keys either global, local, or for a
;;         given mode.
;;       - Added support for saving and loading macros in several files
;;       - Made the search for a possible collision with the key being
;;         bound
;;       - Made managing of macros very easy using a context sensitive
;;         buffer with the description of all the macros.
;; 0.4a  - Major rewrites (again). This time due to realizing that there
;;         is no such thing as a buffer local key map (which is used
;;         together with major-mode map, minor-modes maps, and the global
;;         map.
;;       - lots of code cleanup, this is the last release before a stable
;;         release is made.
;;       - made it work with XEmacs (again).
;; 1.0   - Minor bug-fixes - and of course first stable release
;; 1.1a1 - Answering with either RET or C-g to the question on
;;         which key to bind keyboard macros now makes power macros
;;         stop. This is for calling power-macros directly after having defined a macro.
;;       - Macros are now by default saved to .emacs. This also means that
;;         the files macros are saved to are edited rather than overwritten.
;;       - Removed pm-load as this function is not necessary anymore,
;;         simply used load-file instead.
;;       - Macros are now properly removed from files in case where the last macro is removed
;;       - When power-macro-mode has been loaded, pm-define will now by default be
;;         invoked when a macro has been recorded. This may be changed with
;;         the vatiable `power-macros-run-on-end-macro'.
;;       - As a result of this new strategy, `power-macros-advice-end-kbd-macro' 
;;         has been removed
;;       - Cleaned up the descriptions, so they now follow the convention
;;         that it should be possible to read the first line of the
;;         description seperatly.
;;       - The keybinding for `pm-manage-macros' has been removed on request
;;         from Richard Stallman. Now you need to invoke it as M-x pm-manage-macros
;;       - Pressing one of the power-macro bound keys in the manage buffer,
;;         now has the same effect on the description text in front of a
;;         field, as it has on the field itself.
;;       - The package is no longer a minor-mode. This is due to a request
;;         from Richard Stallman, who wants to include it with Emacs
;;         version 21. This means that you only need to load this package
;;         and then it will activate upon completing macro definition.
;; 1.1a2 - Fixed a bug which made 1.1a1 break (When the function y-n-question wasn't defined)

;;}}}
;;{{{ User definitions

(defgroup power-macros nil
  "Power Macros makes it much easier to work with macros.
The following features is the reason for that:
- With power-macros you may bind a macro directly to a key after having defined it.
- With power-macros you may give your macros a description, which makes it easier 
to maintain them.
- With power-macros you may defined macros as being global or major-mode specific.
- With power-macros your macros are automaticly saved to your .emacs file for later sessions."

  :group 'Convenience)

(defcustom power-macros-file user-init-file
  "File in which new macros are saved by default."
  :type 'file
  :group 'power-macros)

(defcustom power-macros-save-on-change t
  "Non-nil means save macro to their files whenever a change is made.
That is a change in the macro managing buffer."
  :type 'boolean
  :group 'power-macros)

(defcustom power-macros-run-on-end-macro t
  "Non-nil means that `pm-define' is executed upon macro definition
When disabled you may invoke `pm-define' by pressing \\[pm-define]"
  :type 'boolean
  :group 'power-macros)

;;}}}

;;--------------------------------------------------
;;                   CODE
;;--------------------------------------------------

;;{{{ To-do and wish list from Emacs

; TO-DO:
; - When moving a macro from one file to another when managing the macros,
;   it should be checked for possible collisions with a power macro in the
;   new file.
; - When the user ends edit a keyboard macro (with kbd-edit-kbdmacro) then
;   it should be checked if the power macros should be saved.
; - pm-set-mode-key should check for minor-modes too.

; Wish List from Emacs:
; - It would be nice if C-h c could list the description for the macro
; - It would be very useful if it was possible to continue defining a keyboard
;   macro, which was not the latest defined one.
; - It would be nice if one could stop recording a macro in the mini buffer.

;;}}}

(require 'advice)

;;{{{ Misc. setup

(message "Loading power-macros...")

(defvar pm-macros '()
  "List of available macro defined with power-macros")

(defvar pm-available-modes '()
  "List of available modes - used for major-mode completion")

(defvar pm-warn-buffer nil
  "Buffer used when generating warnings on possible key overriding")

(defvar pm-macro-files '()
  "List of files containing power macro definitions.") 

;; Key bindings for the minor mode
(substitute-key-definition 'end-kbd-macro 'pm-end-kbd-macro global-map)

;;}}}
;;{{{ Definition of macros

(defun pm-end-kbd-macro ()
  "End defining keyboard macros and call power-macros on the new macro"
  (interactive)
  (end-kbd-macro)
  (if power-macros-run-on-end-macro
      (pm-define)))

(defun pm-define ()
  "Bind the latest defined keyboard macro to a key, and describe it"
  (interactive)

  ;; Test if a macro exists
  (or last-kbd-macro
      (error (substitute-command-keys "No keyboard macro has been defined yet. Define one with \\[start-kbd-macro]")))

  (let ((key (read-key-sequence "Bind macro to which key? (RET to quit) ")))
    (if (or (string= (key-description key) "RET")
            (string= (key-description key) "return")
            (string= (key-description key) "C-g"))
        (message "Macro not bound to any key")
    
      (let* ((mode (pm-ask-for-mode "How should it be defined?" t))
             (file (if (y-or-n-p "Should the macro be saved for future sessions? ")
                       (expand-file-name power-macros-file) ""))
             (existing-name (pm-get-macros key mode file))
             (macro-name (if existing-name existing-name (pm-new-macro-name))))
        
        ;; Verify if it is ok to bind to the key given.
        (if (catch 'pm-stop-def
              (pm-possible-override t key mode file)
              t)
            
            ;; Bind and describe the key.
            (pm-describe key mode file macro-name)
          (message "key binding discarded!"))))))


(defun pm-describe (key mode file macro-name)
  "This function sets up a buffer for describing a keyboard macro."
  (let* ((desc (pm-get-desc key mode file))
         (buffer (current-buffer)))
    (switch-to-buffer (pm-create-buffer))
    (text-mode)
    (use-local-map (make-keymap))
    (local-set-key [(control c) (control c)] 
                   `(lambda () (interactive) 
                      (pm-end-description ,key ',mode ,file ',macro-name)))
    (insert "Type a short description of the macro below this line, and type C-c C-c\n")
    (insert "-----------------------------------------------------------------------\n")
    (if desc
        (insert desc))))

(defun pm-end-description (key mode file macro-name)
  "Function invoked when the user has written the macro description.
This function takes care of:
- Assigning the macro to the key.
- Removing the description buffer.
- Invoking the save function.
- Saving the description information"

  (beginning-of-buffer)
  (if (re-search-forward "-------------- *$" nil t)
      (forward-char))

  (pm-set-info mode key (buffer-substring (point) (point-max)) 
               file macro-name)
  (kill-buffer (current-buffer))
  (name-last-kbd-macro macro-name)
  (pm-set-key macro-name)
  (pm-maybe-save)
  (message "Assigned macro to %s" (key-description key)))


(defun pm-ask-for-mode (question offer-current-major-mode)
  "Function used to ask about kind of macro (global or mode specific)
This includes the following possibilities:
- global macro
- major mode macro (if this is selected then the user is asked to name the major-mode"

  (let ((map (make-keymap))
        tp)
    (suppress-keymap map t)
    (define-key map [(g)] 'self-insert-and-exit)
    (define-key map [(m)]  'self-insert-and-exit)
    (define-key map [(control g)]  'abort-recursive-edit)
    (define-key map [(??)]  'pm-help-on-bindings)
    (if offer-current-major-mode
        (define-key map [(c)]  'self-insert-and-exit))
    (define-key map [(control h)]  'pm-help-on-bindings)

    ;; Ask the user for the type of the binding
    (setq tp 
          (read-from-minibuffer 
           (concat question " (g/m/?)") 
           "" map))
    (if (equal tp "g") 
        'global

        ;; Ask for a mode name
        (let ((mode (completing-read 
                     (concat "Mode name "
                             (if offer-current-major-mode
                                 (concat "(default: " (symbol-name major-mode) ") "))
                             ": ")
                     (pm-available-modes))))
          (if (not (string= mode ""))
              (intern mode)
            (if offer-current-major-mode
                major-mode
              (error "No mode name given!")))))))

(defun pm-new-macro-name ()
  "Generates a new name for a macro."
  (let ((num 1)
        (name 'pm-macro-1))
    (while (fboundp name)
      (setq num (+ num 1))
      (setq name (intern (format "pm-macro-%s" num))))
    name))

;;}}}
;;{{{ Help descriptions

(defun pm-help-on-bindings ()
  "Help description for the kind of power-macro
This functions is called when the user presses `?' when emacs asks where to bind the macro"

  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ "The macro you are currently binding may be made available 
in several different places. You have the following options:

(g) The macro will be global for every major mode. 
That is, it will be accessible everywhere.

(m) The macro will be made available only for the given major mode. 
This might be very useful in case you have a macro, which does only have a meaning
for say c-mode or emacs-lisp-mode.")
  (save-excursion
    (set-buffer standard-output)
    (help-mode))))

(defun pm-help-on-move-outsite ()
  "Help description for typing a description in the manage buffer.
This functions is called when you press `?' when emacs tells you that
you have moved the cursor outside the description field."

  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ "Before you continue editing other macros, you must first end
editing the current description. Normally you end this by pressing enter
(for submit) or control-g (for abort) in the description field. Now,
however, you have typed a key outside the description area. Does this mean
that you are finished editing the current description?

You now have the following choices:
s - Submit current description. That is change the description to the new text
a - Abort edits. That is go back to the description as it was before the
    current edits.
c - Continue editing the description.")
    (save-excursion
      (set-buffer standard-output)
      (help-mode))))


(defun pm-help-on-edit-or-copy ()
  "Help description on editing a macro.
This function is called when the user press `?' when emacs asks whether
he wants to copy-and-edit the macro or just edit it."

  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ "Emacs asks what you want to do, you have two possibilities:
copy-and-edit - This means that you copy the macro and edit its
                properties. Thus the macro will still be located 
                on the original key, with the original type in the 
                original file, but in addition it will also be copied 
                to the new location you are about to specify.
edit - Just edit the macros properties.")
    (save-excursion
      (set-buffer standard-output)
      (help-mode))))

;;}}}
;;{{{ Managing buffer

;;{{{ Building the buffer.

(defun pm-manage-macros ()
  "This function brings up a managing buffer for power-macros."
  (interactive)
  
  ;; Switch to the buffer and insert the header
  (switch-to-buffer (pm-create-buffer))
  (pm-macro-manage-mode)
  (use-local-map pm-normal-edit-map)

  (insert (substitute-command-keys 
"Descriptions of macros defined with power macros.

In this buffer you have the following possibilities:
- Press enter or the left mouse button on one of the fields to edit them.
  When you are done editing the key, type or file field, you are asked if
  your edits should form a new macro, or just update the existing one.
- Move from field to field with the tabulator key.
- Move from macro to macro by pressing page up / page down.
- Mark a macro for deletion by pressing 'd'.
- Remove the deletion mark by pressing 'u'.
- Delete macros marked for deletion by pressing 'x'.
- Sort the entries by pressing either 's' or 'S' on top of the field you wish to use
  for sorting.
- To edit one of the macros, simply press 'e' on top of the macro.
- Press C-c C-c or \\[kill-buffer] to kill the buffer.

"))

  (let (key desc file macro-name mode p1 p2 tuple-start)
    (setq pm-token-list '())
    (setq pm-tuple-list '())
    (pm-sort)
    (dolist (macro pm-macros)
     (setq  key (get macro 'key)
            desc (get macro 'documentation)
            file (get macro 'file)
            mode (get macro 'mode)
            )

      ;;---------- Separator
      (setq tuple-physical-start (point))
      (insert "--------------------------------------------------\n")
      (setq tuple-logical-start (point))

      ;; Insert a number of spaces equal to the length of the delete text, to avoid
      ;; that all the other fields moves, when a tuple is marked for deletion.
      (insert (make-string (length pm-del-text) ?\ ) "\n")

      ;;---------- Key
      (setq p1 (point))
      (insert "Key : ")
      (setq p2 (point))
      (insert (key-description key) "\n")
      (push (list 'key p1 p2 (point)) pm-token-list)
      (add-text-properties p2 (point) '(mouse-face highlight))

      ;; ---------- Type
      (setq p1 (point))
      (insert "Type: ")
      (setq p2 (point))
      (insert (if (eq mode 'global) "global"
                (concat "mode - " (symbol-name mode)))
              "\n")
      (push (list 'type p1 p2 (point)) pm-token-list)
      (add-text-properties p2 (point) '(mouse-face highlight))

      ;; ---------- File
      (setq p1 (point))
      (insert "File: ")
      (setq p2 (point))
      (insert (if (string= file "") "none" file) "\n")
      (push (list 'file p1 p2 (point)) pm-token-list)
      (add-text-properties p2 (point) '(mouse-face highlight))

      ;; ---------- Description
      (setq p1 (point))
      (insert "Description: ")
      (setq p2 (point))
      (if (string= desc "")
          (insert "No description")
        (insert desc))
      (add-text-properties p2 (point) '(left-margin 3))
      (fill-region p2 (point))
      (insert "\n")
      (push (list 'desc p1 p2 (point)) pm-token-list)
      (add-text-properties p2 (point) '(mouse-face highlight))

      ;;---------- End of tuple
      (insert "\n\n")
      (push (list tuple-physical-start tuple-logical-start (point) macro) pm-tuple-list))
      

    
    (setq pm-token-list (nreverse pm-token-list))
    (setq pm-tuple-list (nreverse pm-tuple-list))
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    ))

;;}}}
;;{{{ Major mode definition

(define-derived-mode pm-macro-manage-mode fundamental-mode "Macro Manage" 
  "Mode for managing power macros"

  ;; ---------- Normal edit map
  (defvar pm-normal-edit-map (make-keymap)
    "This key map is used when not editing the description field")
  (define-key pm-normal-edit-map [(tab)] 'pm-next-field)
  (define-key pm-normal-edit-map [(meta tab)] 'pm-prev-field)
  (define-key pm-normal-edit-map [(next)] 'pm-next-macro)
  (define-key pm-normal-edit-map [(prior)] 'pm-prev-macro)
  (define-key pm-normal-edit-map [(control c) (control c)] 
    (lambda () (interactive) (kill-buffer (current-buffer))))
  (define-key pm-normal-edit-map [(return)] 'pm-edit-field-return)
  (if (boundp 'xemacs-logo)
      (define-key pm-normal-edit-map [(button1)] 'pm-edit-field-mouse)
    (define-key pm-normal-edit-map [(mouse-1)] 'pm-edit-field-mouse))
  (define-key pm-normal-edit-map [(d)] 'pm-set-delete-mark)
  (define-key pm-normal-edit-map [(u)] 'pm-unset-delete-mark)
  (define-key pm-normal-edit-map [(x)] 'pm-execute-deletion)
  (define-key pm-normal-edit-map [(e)] 'pm-edit-kbd-macro)
  (define-key pm-normal-edit-map [(s)] (lambda () (interactive) (pm-set-sort-func 'string<)))
  (define-key pm-normal-edit-map [(S)] (lambda () (interactive) (pm-set-sort-func 'string>)))

  ;; ---------- Description edit map
  ;; Ordinary movement functions should still exists in the rest of the buffer
  ;; when editing the description field. Therefore only inserting commands are 
  ;; disabled. This does however not disable commands like delete, return and backspace.
  ;; Therefore the rest of the buffer is also made read-only.

  (defvar pm-desc-edit-map (make-keymap)
    "This key map is used when editing the description field")
  (suppress-keymap pm-desc-edit-map)
  (substitute-key-definition 'undefined 'pm-move-outside pm-desc-edit-map)
  (define-key pm-desc-edit-map [(return)] 'pm-move-outside)
  (define-key pm-desc-edit-map [(delete)] 'pm-move-outside)
  (define-key pm-desc-edit-map [(backspace)] 'pm-move-outside)
  (define-key pm-desc-edit-map [(control k)] 'pm-move-outside)
  (define-key pm-desc-edit-map [(control w)] 'pm-move-outside)
  (define-key pm-desc-edit-map [(control g)] 
    (lambda () (interactive) (pm-end-edit-or-abort-desc-field 'abort)))
  
  (defvar pm-token-list nil
    "List of all the locations where info may be changed in the manage buffer.
Each element in the list is a list itself with the following items:
1: The type (either text, key, type, file, or description)
2: The start point of the text in front of the field
3: The start point of the field
4: The end point of the field")

  (defvar pm-tuple-list nil
    "List of (physical start point, logical start point, end point, macro name)
of each of the tuple in the power-macro buffer")
  (defvar pm-start-point nil
    "Location of the start of the description field. 
This list is used when the description is changed")
  (defvar pm-current-end-point nil
    "Location of the end of the description field. 
This is used when the description is changed")
  (defvar pm-old-end-point nil
    "Location of the original end of the description field. 
This is used when the description is changed")
  (defvar pm-old-desc nil
    "Contains a copy of the description.
This is used when the description field is edited in the manage buffer.")
  (defvar pm-current-macro nil
    "The macro for which the description field is currently being edited.")
  (defvar pm-del-text "Marked for deletion"
    "Text used to indicate that a record has been marked for deletion")
  (defvar pm-sort-function 'pm-sort-file
    "Latest function used to sort the content of the management buffer")
 )

;;}}}
;;{{{ Movement functions

(defun pm-next-field ()
  "Jumps to the next field in the managing buffer"
  (interactive)
  (pm-jump (lambda (x y) (< x y)) pm-token-list))

(defun pm-prev-field ()
  "Jumps to the previous field in the managing buffer"
  (interactive)
  (pm-jump (lambda (x y) (> x y)) (reverse pm-token-list)))

(defun pm-next-macro ()
  "Jumps to the key field of the next macro in the managing buffer"
  (interactive)
  (pm-jump (lambda (x y) (< x y)) pm-token-list 'key)  
)

(defun pm-prev-macro ()
  "Jumps to the key field of the previous macro in the managing buffer"
  (interactive)
  (pm-jump (lambda (x y) (> x y)) (reverse pm-token-list) 'key)  
)

(defun pm-jump (dirp l &optional type)
  "Jumps to the next or previous field in the managing buffer
If the optional type is given, then the field must be of the given type"
  (let ((p (point))
        (found nil)
        (first-pos nil))
    (while l
      (when (and (not first-pos) (or (not type) (eq type (caar l))))
        (setq first-pos (cadar l)))
      (if (and (funcall dirp p (cadar l)) (or (not type) (eq type (caar l))))
          (progn 
            (goto-char (cadar l))
            (setq l '())
            (setq found t))
        (setq l (cdr l))))
    (if (not found)
        (goto-char first-pos))))

;;}}}
;;{{{ Editing functions

(defun pm-edit-field-return ()
  "See pm-edit-field"
  (interactive)
  (pm-edit-field nil))

(defun pm-edit-field-mouse (event)
  "See pm-edit-field"
  (interactive "e")
  (if (boundp 'xemacs-logo)
      (goto-char (event-closest-point event)))
  (pm-edit-field t))

(defun pm-edit-field (is-mouse)
  "This is invoked when enter or first mouse is pressed in the managing buffer"

  (let* ((elm (pm-get-context))
        (widget-type (car elm))
        (startText (cadr elm))
        (startField (caddr elm))
        (end (cadddr elm))
        (macro (if (not (eq widget-type 'text))
                   (pm-get-macro-at-point)
                 nil))
        (map (make-keymap))
        (key (get macro 'key))
        (file (get macro 'file))
        (mode (get macro 'mode))
        (desc (get macro 'documentation))
        (do-reload nil)
        answer tuple new-name)

    (suppress-keymap map)
    (define-key map [(c)] 'self-insert-and-exit)
    (define-key map [(e)] 'self-insert-and-exit)
    (define-key map [(control g)] 'abort-recursive-edit)
    (define-key map [(??)] 'pm-help-on-edit-or-copy)
    (define-key map [(control h)] 'pm-help-on-edit-or-copy)

    (if (eq widget-type 'text)
        (if (not is-mouse)
            (beep))
      (if (eq widget-type 'desc)
          (pm-edit-desc startField end macro)
        (progn
          ;; Ask if the macro should be copied or just edited.
          (setq answer 
                (read-from-minibuffer 
                 "Do you want to copy-and-edit the macro or just edit it? (c/e/?) " "" map))

          ;; Call the specific editing functions.
          (if (eq widget-type 'key)
              (setq key (pm-edit-key startField end mode file))
            (if (eq widget-type 'type)
                (setq mode (pm-edit-type startField end key file))
              (setq file (pm-edit-file startField end))))
        
          ;; Now delete the original macro if there is one on the given key.
          (let ((existing-macro (pm-get-macros key mode file)))
            (if existing-macro
                (progn
                  (pm-delete-macro existing-macro)
                  (setq do-reload t))))

          ;; update the key bindings
          (if (equal answer "c")
              (progn
                ;; Copy the macro and insert the copy into the list of macros
                (setq other-macro (pm-new-macro-name))
                (fset other-macro (symbol-function macro))
                (setplist other-macro (copy-sequence (symbol-plist macro)))
                (push other-macro pm-macros)
                (setq macro other-macro) ; this makes the code below common for both paths
                (setq do-reload t)
                )
            ;; Edit the key
            (pm-unset-key macro)
            )
          
          ;; set the new value for the new macro
          (pm-set-info mode key desc file macro)

          ;; define the binding for the new key.
          (pm-set-key macro)

          ;; Update the buffer if type is copy
          (if do-reload
              (pm-manage-macros))))) ;; Just like windows: Lets restart, thats the easiest!
    (set-buffer-modified-p nil)))

;;}}}
;;{{{ Specific editing functions

(defun pm-edit-key (start end mode file)
  "Invoked when enter is pressed on the `key' field of the managing buffer"
  (let* ((key (read-key-sequence "Which key: "))
         (key-descs (key-description key)))

    ;; Check if the new binding is ok.
    (pm-possible-override nil key mode file)

    (pm-update-manage-buffer start end key-descs)
    key ; return value.
    ))


(defun pm-edit-type (start end key file)
  "Invoked when enter is pressed on the `type' field of the managing buffer"
  (let ((mode (pm-ask-for-mode "New type: " nil))
        text)
    (if (not (eq mode 'global))
             (setq text (format "mode - %s" (symbol-name mode)))
      (setq text (symbol-name mode)))

    ;; Check if the new binding is ok.
    (pm-possible-override nil key mode file)


    (pm-update-manage-buffer start end text)
    mode ; return value
    ))

(defun pm-edit-file (start end)
  "Invoked when enter is pressed on the `file' field of the managing buffer"
  (let* ((old-name (buffer-substring start end))
         (file (pm-read-file-name "New file name: " (file-name-directory old-name) nil)))
    (pm-update-manage-buffer start end (if (string= file "") "none" file))
    file))

(defun pm-read-file-name (prompt dir default)
  "Reads a filename from the mini-buffer and checks if it is a directory."
  (let ((save (y-or-n-p "Should the macro be saved for future sessions? "))
        (file-name-history pm-macro-files))
    (if save
        (let* ((f (read-file-name prompt dir default))
               (file (if (string= f "") f (expand-file-name f))))
          (if (and file (file-directory-p file))
              (error "Filename is a name of a directory")
            file))
      "")))
         


(defun pm-edit-desc (start end macro)
  "Invoked when enter is pressed on the description field of the managing buffer"

  ;; Make the desc part writable
  (setq buffer-read-only nil)

  ;; Go to start of region and record information about the text
  (goto-char start)
  (setq pm-start-point (point))
  (setq pm-current-end-point (copy-marker end))
  (setq pm-old-end-point end)
  (setq pm-old-desc (buffer-substring start (- end 1)))
  (setq pm-current-macro macro)

  ;; Change the bindings.
  (use-local-map pm-desc-edit-map)
  (let ((keym (copy-keymap global-map)))
    (define-key keym [(control c) (control c)] 
      (lambda () (interactive) (pm-end-edit-or-abort-desc-field 'end)))
    (define-key keym [(control g)] 
      (lambda () (interactive) (pm-end-edit-or-abort-desc-field 'abort)))
    (if (boundp 'xemacs-logo)
        (add-text-properties start end `(keymap ,keym))
      (add-text-properties start end `(local-map ,keym))))

  ;; make it impossible to insert at the beginning of the buffer.
  (add-text-properties 1 2 '(front-sticky (read-only)))
  
  ;; Make it possible to insert at the first point in the description area.
  (add-text-properties (- start 1) start '(rear-nonsticky (face read-only)))

  ;; Now make everything else than the description area read-only.
  ;; This is to ensure that the user has not bound any strange key, which
  ;; is not disabled by the new key map.
  (make-face 'pm-face)
  (set-face-foreground 'pm-face "grey")
  (add-text-properties (point-min) start '(read-only t face pm-face))
  (add-text-properties (- end 1) (point-max) '(read-only t face pm-face))
  (message "Press C-c C-c to submit changes or C-g to abort")
)

(defun pm-end-edit-or-abort-desc-field (type)
  "Ends editing the description field of the managing buffer"
  (setq inhibit-read-only t)
  (remove-text-properties (point-min) (point-max) '(read-only t local-map t keymap t face t))
  (setq inhibit-read-only nil)

  (if (eq type 'abort)
      (progn
        (delete-region pm-start-point pm-current-end-point)
        (goto-char pm-start-point)
        (insert pm-old-desc "\n"))
    (progn
      (pm-update-info-lists pm-start-point (- pm-current-end-point
                                              pm-old-end-point))
      (set-text-properties pm-start-point (point) '(mouse-face highlight))
      (put pm-current-macro 'documentation 
           (buffer-substring-no-properties pm-start-point pm-current-end-point))))
  (setq pm-current-end-point nil)
  (use-local-map pm-normal-edit-map)
  (setq buffer-read-only t))

(defun pm-move-outside (&rest x)
  "Function called when the user aborts editing the description field
This function is called when the user presses a key outside the description field,
when he is supposed to edit the description field."

  (interactive)
  (let ((map (make-keymap))
        answer)
    (suppress-keymap map)
    (define-key map [(s)] 'self-insert-and-exit)
    (define-key map [(a)] 'self-insert-and-exit)
    (define-key map [(c)] 'self-insert-and-exit)
    (define-key map [(??)] 'pm-help-on-move-outsite)
    (define-key map [(control g)] 'self-insert-and-exit)
    (setq answer
          (read-from-minibuffer "You have moved outside the description area, what to do? (s/a/c/?) " nil map))
    (if (equal answer "s")
        (pm-end-edit-or-abort-desc-field 'end)
      (if (equal answer "a")
          (pm-end-edit-or-abort-desc-field 'abort)
        (progn
          ;; c or control g has been pressed.
          (goto-char pm-start-point)
          (message "Please continue editing the current description. End with either C-c C-c or C-g")
          )))))

;;}}}
;;{{{ Info-list management

(defun pm-get-context ()
  "Returns the type of field at point in the manage buffer.
To elements of the return value is an item from the list `pm-token-list'"

  (let ((p (point))
        (l pm-token-list)
        (last-end (point-min))
        type start end res) 
    (if (null l)
        (list 'text nil nil nil)
      (if (< p (cadr (car l)))
          (list 'text (point-min) nil (cadr (car l)))
        (progn
          (while l
            (setq elm (car l)
                  l (cdr l)
                  type (car elm)
                  startText (cadr elm)
                  startField (caddr elm)
                  end (cadddr elm))
            (if (< p startText)
                (setq res (list 'text nil nil nil))
              (if (< p end)
                  (setq res elm)))
            (if (not (null res))
                (setq l '()))
            (setq last-end end))
          
          (if (null res)
              (list 'text nil nil nil)
            res))))))


(defun pm-update-info-lists (start incr)
  "Updates `token-list' and `tuple-list' with INCR at pos START."

  ;; update the token list
  (let ((res '())
        tuple-type tuple-start tuple-end)
    (dolist (elm (reverse pm-token-list))
      (setq tuple-type (car elm)
           tuple-startText (cadr elm)
           tuple-startField (caddr elm)
           tuple-end (cadddr elm))
      (if (> tuple-startField start)
          (push (list tuple-type (+ tuple-startText incr) (+ tuple-startField incr)
                      (+ tuple-end incr)) res)
        (if (= tuple-startField start)
            (push (list tuple-type tuple-startText tuple-startField (+ tuple-end incr)) res)
          (push elm res))))
    (setq pm-token-list res))

  ;; update the tuple list
  (let ((res '())
        p-start l-start end name)
    (dolist (elm (reverse pm-tuple-list))
      (setq p-start (car elm)
            l-start (cadr elm)
            end (caddr elm)
            name (cadddr elm))
      (if (> p-start start)
          (push (list (+ p-start incr) (+ l-start incr) (+ end incr) name) res)
        (push elm res)))
    (setq pm-tuple-list res)))


(defun pm-update-manage-buffer (start end text)
  "Inserts TEXT in place of content from START to END in the manage buffer.
Furthermore it also makes the text highlight when mouse is over the given region"

  (setq buffer-read-only nil)
  
  (delete-region start end)
  (goto-char start)
  (insert text "\n")
  
  (add-text-properties start (+ start (length text))
                       '(mouse-face highlight))
  (setq buffer-read-only t)

  ;; Now update the info lists.
  (pm-update-info-lists start (+ 1 (- (length text) (- end start))))
)
    
(defun pm-delete-tuple-in-token-list (start end)
  "Removes the info from the token-list between start and end"
  (let ((l pm-token-list) 
        elm t-start t-end)
    (setq pm-token-list '())
    (while l
      (setq elm (car l)
            l (cdr l)
            t-start (cadr elm)
            t-end (cadddr elm))
      (if (not (and (>= t-start start) (<= t-end end)))
          (push elm pm-token-list)))
    (setq pm-token-list (nreverse pm-token-list))))

(defun pm-get-delete-pos ()
  "Returns the position where the word \"delete\" may be inserted.
That is the location in the tuple at point."
  (pm-get-tuple-info 'cadar))

(defun pm-get-macro-at-point ()
  "Returns the name of the macro at point"
  (pm-get-tuple-info 'cadddar))

(defun pm-get-tuple-info (f)
  "Returns information from the tuple at point in the managing buffer.
The actual information is determined by the function F given as argument to the function."
  (let ((p (point))
        (l pm-tuple-list)
        (res nil))
    (if (null l)
        (error "No macros exists!")
      (if (< p (cadar l))
          (error "Not located on a macro!"))
      (while l
        (if (< p (caddar l))
            (setq res (funcall f l)
                  l '())
          (setq l (cdr l))))
      (if (eq res nil)
          (error "Not located on a macro!"))
      res)))

;;}}}
;;{{{ Deletion

(defun pm-set-delete-mark ()
  "Mark the current macro in the managing buffer for deletion"
  (interactive)
  (pm-set-or-unset-delete-mark 'set))

(defun pm-unset-delete-mark ()
  "Remove the deletion mark for the current macro in managing buffer"
  (interactive)
  (pm-set-or-unset-delete-mark 'unset))

(defun pm-set-or-unset-delete-mark (type)
  "Set or unsets the deletion mark in the managing buffer"
  (save-excursion
    (let ((pos (pm-get-delete-pos)))
      (setq buffer-read-only nil)
      (goto-char pos)
      (delete-region pos (+ pos (length pm-del-text)))
      (if (eq type 'set)
          (progn
            (insert pm-del-text)
            (set-text-properties pos (point) '(face modeline)))
        (insert (make-string (length pm-del-text) ?\ )))
      (setq buffer-read-only t)
      (set-buffer-modified-p nil))))


(defun pm-execute-deletion ()
  "Delete the macros marked for deletion in managing buffer"
  (interactive)
  (setq buffer-read-only nil)
  (save-excursion
    (let ((new-list '())
          elm p-start l-start end name)
    
      (while pm-tuple-list
        (setq elm (car pm-tuple-list)
              pm-tuple-list (cdr pm-tuple-list)
              p-start (car elm)
              l-start (cadr elm)
              end (caddr elm)
              name (cadddr elm))
        (goto-char l-start)
        (if (looking-at pm-del-text)
            (progn
              (delete-region p-start end)
              (pm-delete-tuple-in-token-list p-start end)
              (pm-update-info-lists p-start (- p-start end))
              (pm-delete-macro name))
          (push elm new-list)))
      (setq pm-tuple-list (nreverse new-list))))
  (setq buffer-read-only t)
  (set-buffer-modified-p nil)
  (pm-maybe-save))

(defun pm-delete-macro (macro)
  "This functions delete the macro named MACRO, and remove its key-bindings"
  (pm-unset-key macro)
  (unintern macro)
  (setq pm-macros (delete macro pm-macros)))

;;}}}
;;{{{ Edit macro definition

; The tricky part with the function below is that edit-kbd-macro does not
; support a back-end to enter the edit buffer. Thus I need to go trough the
; front-end, which requires that I answer a question in the
; mini-buffer. Luckily this can be solved with a macro ;-)

(defun pm-edit-kbd-macro ()
  "Edits the keyboard macro under point in the managing buffer"
  (interactive)
  (let* ((macro (pm-get-macro-at-point))
         (macro-str (symbol-name macro))
         (new-mac (vconcat [?\M-x] "edit-kbd-macro" [return ?\M-x]
                           macro-str [return])))

    ;; I need to switch buffer to avoid that d,e,x have the meaning
    ;; pm-delete-macro, pm-edit-kbd-macro etc. 
    (switch-to-buffer (get-buffer-create "*scratch*"))
      
    (execute-kbd-macro new-mac)
    ;; Any idea how I can get pm-maybe-save executed when the user finish
    ;; editing the macro? The hook doesn't work when I enter the editing
    ;; the above way.
    ))

;;}}}
;;{{{ Sorting

(defun pm-sort ()
  "Sorts the macros in the manage buffer.
This is done based on the value of pm-sort-function"
  (setq pm-macros (sort pm-macros pm-sort-function)))

(defun pm-set-sort-func (direction)
  "Sorts the tuple in the managing buffer based on the field at point."
  (let* ((elm (pm-get-context))
        (type (car elm)))

    (if (eq type 'text)
        (error "You must press `s' on top of a field")
      (fset pm-sort-function
            `(lambda (x y) (,direction (format "%s" (get x ',type))
                                       (format "%s" (get y ',type))))))
    (pm-manage-macros)))

(defun string> (x y) (not (string< x y)))

(defun pm-sort-file (x y)
  "Default sort function"
  (string< (get x 'file) (get y 'file)))

;;}}}

;;}}}
;;{{{ Saving

(defun pm-maybe-save ()
  "This function is invoked whenever a change is made."
  (if power-macros-save-on-change
      (pm-save)))


(defun pm-save ()
  "Saves the power macros to files"
  (interactive)
  (let (macros)
    (dolist (file pm-macro-files)
      (setq macros (pm-select `(lambda (name) (equal (get name 'file) ,file)) pm-macros))
      (set-buffer (find-file-noselect file))
      (pm-save-delete file)
      
      (dolist (macro macros)        
        (pm-write-tuple-to-buffer (current-buffer) macro))

      (when (null macros)
        (setq pm-macro-files (delete file pm-macro-files)))
      
      (write-file file)
)))


(defun pm-save-delete (file)
  "Delete all the calls to pm-def-macro in the current buffer.
Leave point at the location of the last call, or after the last expression."
  (goto-char (point-min))
  (let ((last-pos (point-max))
        sexp)
    (while (setq sexp 
                 (condition-case nil
                     (read (current-buffer))
                   (end-of-file nil)))

      ; Repeat until we have read the whole buffer.
      (when (and (listp sexp) (eq (car sexp) 'pm-def-macro))
        (delete-region (save-excursion
                         (backward-sexp)
                         (point))
                       (point))

        ; ensure that no spaces are added just because we save a number of times
        (when (and (looking-at "$") (/= (point) (point-max)))
          (delete-char 1)) 
        (when (and (looking-at "$") (/= (point) (point-max)))
          (delete-char 1)) 

        (setq last-pos (point))))
    (goto-char last-pos)))

(defun pm-write-tuple-to-buffer (buf macro)
  "This functions prints out MACRO to BUF"
  (let* ((mode (get macro 'mode))
        (key (get macro 'key))
        (desc (get macro 'documentation))
        )
    
    ;; print header
    (princ (concat "(;-" (pm-center ?- 66 " power-macros ") "-\n") buf)
    (let ((text (concat (key-description key)
                        " - "
                        (if (eq mode 'global)
                            "global definition"
                          (concat "specific for " (symbol-name mode))))))
      (princ (concat " ;-" (pm-center ?\ 66 text) "-\n") buf))
    (princ (concat " ;" (make-string 68 ?-) "\n") buf)
                                    
    (princ "\tpm-def-macro\n\t'" buf)
    (prin1 mode buf) (princ " " buf)
    (prin1 key buf) (princ "\n\t" buf)
    (prin1 desc buf) (princ "\n\t" buf)
    (prin1 (format-kbd-macro macro) buf)
    (princ (concat "\n ;" (make-string 68 ?-) "\n") buf)
    (princ ")\n\n" buf)
))


(defun pm-center (fill-char num text)
  "Returns TEXT with FILL-CHAR around it to center it"
  (let* ((mid (max (/ (- num (length text)) 2) 0))
        (extra (max (- num (length text) (* 2 mid)) 0)))
    (concat (make-string mid fill-char) text (make-string (+ extra mid) fill-char))))

;;}}}
;;{{{ Loading

(defun pm-def-macro (mode key desc macro-def)
  "This function is the one generated to the .emacs file"
  (let* ((file (if load-file-name 
                   load-file-name 
                 (if buffer-file-name
                     buffer-file-name
                   (error "I can't figure out the current file name. You must call pm-def-macro from your .emacs file or another file loaded!"))))
         (existing-name (pm-get-macros key mode file))
         (macro-name (if (null existing-name)
                         (pm-new-macro-name)
                       existing-name)))

    ;; Define the macro
    (fset macro-name (read-kbd-macro macro-def))
    
    ;; Set the info about the macro
    (pm-set-info mode key desc (expand-file-name file) macro-name)

    ;; Bind the key
    (pm-set-key macro-name)))

;;}}}
;;{{{ Database Management

;; These functions takes care of managing the key bindings, descriptions and file information
;; The data is located as properties in the symbol defining the macro. The
;; variable called 'pm-macros is a list of symbol names for each of these macros.
;;
;; The following elements exists:
;;     key           - The key for which this macro is bound
;;     documentation - The description of this macro
;;     file          - The file in which this macro is saved
;;     mode          - The name of the major mode for mode specific macros
;;                     or the symbol global.

(defun pm-get-macros (key mode file &optional all)
  "This function returns the macro bound to KEY for mode MODE for file FILE.
Any of the field might be nil, which means that they are wild-cards. If the
optional variable 'all' is t, then a list of matches is returned."
  (let ((list pm-macros)
        (res '())
        macro)
    (while list
      (setq macro (car list)
            list (cdr list))
      (if (and (or (null key) (equal (get macro 'key) key))
               (or (null mode) (eq (get macro 'mode) mode))
               (or (null file) (equal (get macro 'file) file)))
          (push macro res)))

    (if all
        res
      (if (> (length res) 1)
          (error "Internal error: More that one tuple matched the query")
        (car res)))))

(defun pm-get-desc (key mode file)
  "Returns the description for the given key/mode/file."
  (let ((name (pm-get-macros key mode file)))
    (if (null name)
        ""
      (get name 'documentation))))

(defun pm-set-info (mode key desc file macro-name)
  "Inserts information into the database for the given macro"
  (put macro-name 'mode mode)
  (put macro-name 'key key)
  (put macro-name 'documentation desc)
  (put macro-name 'file file)
  (if (not (member macro-name pm-macros))
      (push macro-name pm-macros))
  (if (and (not (string= file "")) (not (member file pm-macro-files)))
      (push file pm-macro-files)))
  

;;}}}
;;{{{ Key binding Check

;;{{{ Description

; The function pm-possible-override takes care of warning the user if he
; is about to overwrite a key or shadow key. Below the rules for checking
; this is described.

;--------------- meaning of symbols ---------------
; 
; (+) means that the given test is implemented
; (-) means that the given test is not implemented. This is either because
;     I don't know how to do it, or because I find it very unlikely that it
;     will ever be used.
;
; (key, mode, file) in set PM-bind
;     Check if pm-macros contain a definition for the given
;     key,type,mode,file tuple.
;     A dash means that the given field does have a meaning (only used for
;     mode)
;     An asterix is a wildcard meaning that this field should not be
;     included when searching for tuples.

; key in set map 
;    Check if key is bound in the given map
;
; Override
;    This means that if the given check returns true, then the new
;    definition will override the given match(es).
;
; Shadows
;    This means that if the given check returns true, then the new
;    definition will shadow the given match(es). Which means that the
;    existing definition(s) will not be visible.
;
; Invisible
;    This means that if the given check returns true, then the new
;    definition will have no effect. 
;
; Note: that the two clauses above might only be true for the given 
; mode or the given buffer.
; 
; 1)
;    This means test number one for both new bindings and existing bindings
; 1 new)
;    This means test number one. It should, however, only be done for new bindings.

;--------------- Want to bind a global key ---------------
; (+) 1)     Override  - (key,global,-,file) in set PM-bind
; (-) 2)     Override  - (key,global,-,*)    in set PM-bind
; (+) 3)     Override  - key in set global-map
; (-) 4 new) Invisible - (key,active-modes,*) in set PM-bind
; (+) 5 new) Invisible - key in set *-active-mode-map
; (-) 6)     Invisible - (key,*-mode,*) in set PM-bind
; (-) 7)     Invisible - key in set *-mode-map

;--------------- Want to bind a mode specific key ---------------
; (+) 1)      Override  - (key,mode,file) in set PM-bind
; (-) 2)      Override  - (key,mode,*) in set PM-bind
; (+) 3)      Override  - key in set mode-map
; (-) 4)      Shadows   - (key,global,*) in set PM-bind
; (+) 5)      Shadows   - key in set global-map
; (-) 6 new)  Invisible - (key,active-minor-modes,*) in set PM-bind
; (+) 7 new)  Invisible - key in set active-minor-modes-maps
; (-) 8)      Invisible - (key,*-minor-mode,*) in set PM-bind
; (-) 9)      Invisible - key in set *-minor-modes-map

;;}}}
;;{{{ pm-possible-override

(defun pm-possible-override (new-def key mode file) 
  "Check whether the new key binding violates an existing one.
For a list of all the different checks, please see the description in the source code."

  (let* ((buf (pm-create-buffer "*Warning*"))
         (any-output nil)
         main-macro macro-list macro)
    (setq pm-warn-buffer buf)
    (pm-insert "Your new definition might violate an existing definition. Below you
see a report of what will happen if you make your binding.")


    ;; -------------------------------
    ;; Rule no. 1 is the same for both
    ;; -------------------------------
    (setq main-macro (pm-get-macros key mode file))
    (if main-macro
        (progn
          (setq any-output t)
          (pm-insert "\n
----------------------------------------------------------------------

This definition will " (pm-outstand "replace") " an existing power-macro on the same key, 
with the same type and defined in the same file. Its description is:\n")
          (pm-insert (pm-indent (get main-macro 'documentation)))))

      
    ;; -------- The rest of the rules ---------
    (setq any-output (or (cond 
                          ((eq mode 'global) 
                           (pm-override-global new-def key file main-macro))
                          
                          (t
                           (pm-override-mode new-def key file mode main-macro))) 
                         any-output))

    (pm-insert "\n") ; This is to ensure that there is a location, which is
                  ; not with the left-margin property.

    (if any-output
        (save-excursion
          (switch-to-buffer buf)
          (pm-indent-regarding-to-properties)
          (setq buffer-read-only t)
          (goto-char (point-min))
          (if (not (yes-or-no-p "Continue defining the macro? "))
              ;; Stop defining the macro.
              (progn
                (kill-buffer buf)
                (throw 'pm-stop-def nil)))))
    
      ;; Finally kill the buffer.
    (kill-buffer buf)))

;;}}}
;;{{{ pm-override-global

(defun pm-override-global (new-def key file main-macro)
  "Checks if the new global key binding violates an existing binding.
See the source code for all the different checks"

  (let ((any-output nil))
        

    ;; ---------------------------------
    ;; Rule no. 3
    ;; Override  - key in set global-map
    ;; ---------------------------------
    (setq any-output 
          (or (pm-check-binding 'global key 'conflict main-macro) any-output))

    ;; ----------------------------------------
    ;; Rule no. 5 (only for new macros)
    ;; Invisible - key in set *-active-mode-map
    ;; ----------------------------------------
    (if new-def
        (setq any-output
              (or (pm-check-binding 'mode key 'invisible  main-macro major-mode) any-output)))

    (if new-def
        (setq any-output
              (or (pm-check-binding 'minor-mode key 'invisible main-macro) any-output)))


    any-output)) ; return value

;;}}}
;;{{{ pm-override-mode

(defun pm-override-mode (new-def key file mode main-macro)
  "Checks if the new mode key binding violates an existing bindings.
See the source code for all the different checks"

  (let ((any-output nil))

    ;; ---------------------------------------
    ;; Rule no. 3
    ;; Override  - key in set current-mode-map
    ;; ---------------------------------------
    (setq any-output
          (or (pm-check-binding 'mode key 'conflict  main-macro mode) any-output))
    

    ;; ---------------------------------
    ;; Rule no. 5
    ;; Shadows   - key in set global-map
    ;; ---------------------------------
    (setq any-output
          (or (pm-check-binding 'global key 'shadows main-macro) any-output))

    ;; ----------------------------------------------
    ;; Rule no. 7 (only for new macros)
    ;; Invisible - key in set active-minor-modes-maps
    ;; ----------------------------------------------
    (if new-def
        (setq any-output
              (or (pm-check-binding 'minor-mode key 'invisible main-macro) any-output)))


    any-output)) ; return value

;;}}}
;;{{{ pm-check-binding

(defun pm-check-binding (type key warn-type main-macro &optional mode)
  "Verify if a key binding of type TYPE exists for KEY.
If this is the case then this function will insert a warning of type WARN-TYPE, telling 
that there is a problem. 
The function returns whether there was anything to report.

WARN-TYPE is one of: conflict, invisible, shadows
TYPE is one of: global, mode, minor-mode"

  (let* (map-name
         (func (cond ((eq type 'global) (pm-lookup-key global-map key))
                     ((eq type 'mode)
                      (progn
                        (setq map-name (intern (concat (symbol-name mode) "-map")))
                        (if (boundp map-name)
                            (pm-lookup-key (eval map-name) key)
                          nil)))
                     ((eq type 'minor-mode) (pm-get-minor-mode-binding key))
                     (t (error "Internal error: Unknown type"))))
         (where (symbol-name type)))
    (if (and func (not (eq func main-macro)))
        (progn

          (pm-insert "\n----------------------------------------------------------------------\n\n")
          ;; Insert the warning text
          (if (eq warn-type 'conflict)
              (pm-insert "This definition will " (pm-outstand "conflict")
                         " with ")
            (if (eq warn-type 'invisible)
                (pm-insert "This definition will " (pm-outstand "be invisible")
                           " because of ")
              (if (eq warn-type 'shadows)
                  (pm-insert "This definition will " (pm-outstand "shadow")
                             " for the ")
              (error (format "Unknown type %s" warn-type)))))

          ;; Now insert the description for the function
          (if (symbolp func) ;; This is a function name.
              (if (fboundp func) ;; The function exists.

                  (if (string-match "pm-macro-" (format "%s" func))
              
                      ;; This is a power macro.
                      (pm-insert "the " where " power macros loaded from\nthe file '" 
                                 (get func 'file) "'. Its description is:\n"
                                 (pm-indent (get func 'documentation)))
                      
                    ;; This is another existing function
                    (pm-insert "the " where " function '" (symbol-name func) 
                            "'\nwhich has the following description:\n"
                            (pm-indent (documentation func))))

                ;; The function doesn't seem to be defined.
                (pm-insert "the " where " function '" (symbol-name func) "' which,\n"
                        "however, doesn't seem to be defined (At least not at the moment)"))
            
            ;; Hmm it was not a symbol, then it might be a lambda expression
            ;; Does there exists other possibilities?
            (pm-insert "the following " where " definition:\n" (format "%s" func)))


          t) ;; return value
      nil)))

;;}}}
;;{{{ Misc

(defun pm-outstand (text)
  "Adds bold properties to the string given as argument"
  (let ((new-text (concat text))) ;; we need to copy it to avoid adding
                                  ;; properties to the original widget
    (add-text-properties 0 (length new-text) '(face bold) new-text)
    new-text))

(defun pm-indent (text)
  "Returns TEXT but with the exception that it has indentation properties"
  (let ((new-text (concat text))) ;; we need to copy it to avoid adding
                                  ;; properties to the original widget
    (add-text-properties 0 (length new-text) '(left-margin 3) new-text)
    new-text))


(defun pm-indent-regarding-to-properties ()
  "Indent the warning buffer with respect to the text property `left-margin'
This function runs through the buffer with warnings about overriding
keys etc. and executes fill-region on the locations where the text property
left-margin is set."
  (let ((cont t)
        (end (point-min))
        start)
    (while cont
      (setq cont nil)
      (setq start (next-single-property-change end 'left-margin))
      (if start
          (if (not (get-text-property start 'left-margin))
              (setq cont t
                    end (+ end 1))
            (progn
              (setq end (next-single-property-change start 'left-margin))
              (if end
                  (progn
                    (fill-region start end)
                    (setq end (+ end 1))
                    (setq cont t)))))))))
  


(defun pm-lookup-key (map key)
  "Returns the function definition bound to key if it exists, otherwise nil"
  (let ((func (lookup-key map key)))
    (if (and func (not (numberp func)))
        func
      nil)))



(defun pm-get-minor-mode-binding (key)
  "Returns the visible definitions of KEY in the active minor modes"
  (let ((maps (current-minor-mode-maps))
        (binding nil))
    (while maps
      (setq binding (lookup-key (car maps) key))
      (if (and binding (not (numberp binding)))
          (setq maps '())
        (setq binding nil))
      (setq maps (cdr maps)))
    binding))

(defun pm-insert (&rest args)
  "Inserts text into the buffer pm-warn-buffer"
  (save-excursion
    (set-buffer pm-warn-buffer)
    (eval `(insert ,@args))))

;;}}}

;;}}}
;;{{{ Actual Key (un)binding

(defun pm-unset-key (macro)
  "This function removes the binding described in macro."
  (let ((key (get macro 'key))
        (mode (get macro 'mode)))
    (if (eq mode 'global)
        (if (eq (lookup-key global-map key) macro)
            (global-unset-key key))

      ;; Mode specific binding
      (let* ((map (intern (concat (symbol-name mode) "-map"))))
        (if (boundp map)
            (if (eq (lookup-key (eval map) key) macro)
                (define-key (eval map) key nil))

          ;; No map exists, we must then unadvice the function and run
          ;; through all buffers.
          (if (fboundp mode)
              (pm-set-or-unset-mode-key macro mode key 'unset)))))))

(defun pm-set-key (macro)
  "Makes the key binding as described in the given macro"

  (let ((mode (get macro 'mode))
        (key (get macro 'key)))
    (if (eq mode 'global)
        (global-set-key key macro)

      ;; major-mode macro
      (let ((map (intern (concat (symbol-name mode) "-map"))))
        (if (boundp map)
            (define-key (eval map) key macro)

          ;; A mode-map didn't exists so now we need to make the binding
          ;; much more manual.
          (pm-set-or-unset-mode-key macro mode key 'set))))))

(defun pm-set-or-unset-mode-key (macro mode key type)
  "(un)set binding for MACRO in existing and future buffers with mode MODE."

  (let ((all-buffers (buffer-list))
        (adv-name (intern (concat (symbol-name macro) "-advice"))))

    ;; Run through all the buffers and bind the key for buffers with the
    ;; given major mode
    (dolist (buf all-buffers)
      (save-excursion 
        (set-buffer buf)
        (when (eq major-mode mode)
          (if (eq type 'set)
              (local-set-key key macro)
            
            ;; Unset the key if it is bound to the macro.
            (if (and (current-local-map)
                     (eq (lookup-key (current-local-map) key) macro))
                (local-unset-key key))))))
    
    (if (eq type 'set)
        (eval `(defadvice ,mode (after ,adv-name activate)
                 (local-set-key ,key ',macro)))
      (ad-remove-advice mode 'after adv-name))
    ))

;;}}}
;;{{{ Utility functions

(defun pm-create-buffer (&optional name)
  "Switches to the named buffer or to the power-macros buffer.
The buffer is created if it doesn't exists."
  (let* ((nm (if name name "*Power Macros Description*"))
        (old-buf (get-buffer nm)))
    (if old-buf
        (kill-buffer old-buf))
    (get-buffer-create nm)))


(defun pm-ass-remove (var list)
  "Removes var from assoc list list"
  (if (null list)
      '()
    (if (eq (caar list) var)
        (pm-ass-remove var (cdr list))
      (cons (car list) (pm-ass-remove var (cdr list))))))


;; The following function makes it possible to insert "Enter recursive edit"
;; in the power macro menu item.
;; One could argue that these items does not belong in the PM entry,
;; but I believe they are extremely useful, and have a very hard to remember
;; key binding.
(defun pm-enter-recursive-edit ()
  "This is just like C-u C-x q when defining keyboard macros."
  (interactive)
  (message (substitute-command-keys "Press \\[exit-recursive-edit] to leave recursive edit"))
  (kbd-macro-query 1))


;; Does select not exists in core lisp?
(defun pm-select (func list)
  "Returns a list with elements from LIST, which matches the predicate func."
  (let ((res '()))
    (while list
      (if (funcall func (car list))
          (push (car list) res))
      (setq list (cdr list)))
    (nreverse res)))

(defun pm-available-modes ()
  "This function returns the list of available modes"
  (interactive)
  (if (not (null pm-available-modes))
      pm-available-modes

    ;; calculate the available modes ones and for all
    ;; Does anyone know how this list can be extracted more efficiently?
    (progn
      (message "Searching for available modes...")
      (defun pm-extract-modes (x)
        (if (string-match ".*-mode$" (symbol-name x))
            (push (list (symbol-name x)) pm-available-modes)))
      (mapatoms 'pm-extract-modes)
      (message "Searching for available modes...Done")
      pm-available-modes)))

(defun cadddar (l)
  (cadddr (car l)))

;;}}}

(provide 'power-macros)
(message "Loading power-macros...done")

