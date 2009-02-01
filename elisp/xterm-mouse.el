;; -*-Emacs-Lisp-*- xterm-mouse.el
;;
;; Package to enable use of a mouse when Emacs is displaying in an xterm,
;; over a serial line.
;; This is the version for Emacs 19 only.
;;
;; To do:
;;   o implement support for down events.
;;     to do this we need to be able to stuff
;;     mouse-movement events into the input queue,
;;     to be read by 'read-event, inside a track-mouse
;;     Is just one final mouse-movement event sufficient ?
;;     (xterm can't supply anything more than this.)
;;   o implement support for clicks on:
;;     - mode line
;;     - minibuffer
;;     - vertical pane dividers
;;
;;   o implement support for double and triple clicks.
;;   o implement support for 'track-mouse;
;;     I think xterm can be made to generate mouse-movement events.
;;
;;
(provide 'xterm-mouse)

;; Send command to the xterm requesting translation of
;; mouse events into input character sequences.
;;
(defun xterm-mouse-enable ()
  (interactive)
  (send-string-to-terminal "\e[?1000h"))

;; Send command to turn off mouse-event notification.
;;
(defun xterm-mouse-disable ()
  (interactive)
  (send-string-to-terminal "\e[?1000l"))

(defconst xterm-mouse-event-array
  [
   mouse-1                             ; 0
   mouse-2                             ; 1
   mouse-3                             ; 2
   release                             ; 3
   s-mouse-1                           ; 4
   s-mouse-2                           ; 5
   s-mouse-3                           ; 6
   release                             ; 7
   m-mouse-1                           ; 8
   m-mouse-2                           ; 9
   m-mouse-3                           ; 10
   release                             ; 11
   s-m-mouse-1                         ; 12
   s-m-mouse-2                         ; 13
   s-m-mouse-3                         ; 14
   release                             ; 15
   c-mouse-1                           ; 16
   c-mouse-2                           ; 17
   c-mouse-3                           ; 18
   release                             ; 19
   s-c-mouse-1                         ; 20
   s-c-mouse-2                         ; 21
   s-c-mouse-3                         ; 22
   release                             ; 23
   m-c-mouse-1                         ; 24
   m-c-mouse-2                         ; 25
   m-c-mouse-3                         ; 26
   release                             ; 27
   s-m-c-mouse-1                       ; 28
   s-m-c-mouse-2                       ; 29
   s-m-c-mouse-3                       ; 30
   release                             ; 31
   ]
  "Indexable by xterm-generated button-char.")

(defconst xterm-down-mouse-event-array
  [
   down-mouse-1                             ; 0
   down-mouse-2                             ; 1
   down-mouse-3                             ; 2
   release                                  ; 3
   s-down-mouse-1                           ; 4
   s-down-mouse-2                           ; 5
   s-down-mouse-3                           ; 6
   release                                  ; 7
   m-down-mouse-1                           ; 8
   m-down-mouse-2                           ; 9
   m-down-mouse-3                           ; 10
   release                                  ; 11
   s-m-down-mouse-1                         ; 12
   s-m-down-mouse-2                         ; 13
   s-m-down-mouse-3                         ; 14
   release                                  ; 15
   c-down-mouse-1                           ; 16
   c-down-mouse-2                           ; 17
   c-down-mouse-3                           ; 18
   release                                  ; 19
   s-c-down-mouse-1                         ; 20
   s-c-down-mouse-2                         ; 21
   s-c-down-mouse-3                         ; 22
   release                                  ; 23
   m-c-down-mouse-1                         ; 24
   m-c-down-mouse-2                         ; 25
   m-c-down-mouse-3                         ; 26
   release                                  ; 27
   s-m-c-down-mouse-1                       ; 28
   s-m-c-down-mouse-2                       ; 29
   s-m-c-down-mouse-3                       ; 30
   release                                  ; 31
   ]
  "Indexable by xterm-generated button-char.")

(defconst xterm-drag-mouse-event-array
  [
   drag-mouse-1                             ; 0
   drag-mouse-2                             ; 1
   drag-mouse-3                             ; 2
   release                                  ; 3
   s-drag-mouse-1                           ; 4
   s-drag-mouse-2                           ; 5
   s-drag-mouse-3                           ; 6
   release                                  ; 7
   m-drag-mouse-1                           ; 8
   m-drag-mouse-2                           ; 9
   m-drag-mouse-3                           ; 10
   release                                  ; 11
   s-m-drag-mouse-1                         ; 12
   s-m-drag-mouse-2                         ; 13
   s-m-drag-mouse-3                         ; 14
   release                                  ; 15
   c-drag-mouse-1                           ; 16
   c-drag-mouse-2                           ; 17
   c-drag-mouse-3                           ; 18
   release                                  ; 19
   s-c-drag-mouse-1                         ; 20
   s-c-drag-mouse-2                         ; 21
   s-c-drag-mouse-3                         ; 22
   release                                  ; 23
   m-c-drag-mouse-1                         ; 24
   m-c-drag-mouse-2                         ; 25
   m-c-drag-mouse-3                         ; 26
   release                                  ; 27
   s-m-c-drag-mouse-1                       ; 28
   s-m-c-drag-mouse-2                       ; 29
   s-m-c-drag-mouse-3                       ; 30
   release                                  ; 31
   ]
  "Indexable by xterm-generated button-char.")

;; This is the function bound to "\E[M" to catch mouse events.
;; The "\E[M" is followed by three characters.
;; The first encodes button number/modifiers/press-release.
;; The second and third encode the x-coordinate and y-coordinate,
;; respectively.
;; Coordinates are in character positions relative to (0,0) at the
;; top left of the xterm window.
;;

(defvar xterm-mouse-down-event-frame-x-coord -1)
(defvar xterm-mouse-down-event-frame-y-coord -1)
(defvar xterm-mouse-down-event-button-char -1)
(defvar xterm-mouse-down-event-location nil)
(defvar xterm-mouse-click-binding nil)
(defvar xterm-mouse-down-binding nil)
(defvar xterm-mouse-drag-binding nil)
(defvar xterm-mouse-pending-click-type nil)
(defvar xterm-mouse-pending-drag-type nil)

(defun xterm-mouse-handle-event ()
  (interactive)
  
  ;; Read the three chars sent by the xterm.
  ;;
  (let ((button-char (read-char))
        (x-char (read-char))
        (y-char (read-char)))
    
    ;; Allow for the case where characters are truncated to 7-bits
    ;; and so wrap around after 127.  This only fixes part of the problem.

    (if (< x-char 33)
        (setq x-char (+ 128 x-char)))
    (if (< y-char 33)
        (setq y-char (+ 128 y-char)))

    ;; Decode them.
    ;;
    (let* (;; Get the frame-relative coordinates.
           ;;
           (frame-x-coord (- x-char 33))
           (frame-y-coord (- y-char 33))

           ;; Get the window.
           ;;
           (window (window-at frame-x-coord frame-y-coord))

           ;; Get the window-relative coordinates.
           ;;
           (window-x-coord (- frame-x-coord (nth 0 (window-edges window))))
           (window-y-coord (- frame-y-coord (nth 1 (window-edges window))))

           ;; Get the buffer.
           ;;
           (buffer (window-buffer window))

           ;; Get the buffer position.
           ;;
           (buffer-pos (xterm-mouse-window-coords-to-point window window-x-coord window-y-coord))

           ;; Get a timestamp.
           ;;
           (timestamp (xterm-mouse-generate-millisecond-timestamp))

           ;; Get the event modifiers.
           ;;
           (shift (not (zerop (logand button-char 4))))
           (meta (not (zerop (logand button-char 8))))
           (control (not (zerop (logand button-char 16))))

           ;; Get the event type.
           ;;
           (button-char (% button-char 32))
           (click-event-type (aref xterm-mouse-event-array button-char))
           (down-event-type (aref xterm-down-mouse-event-array button-char))
           (drag-event-type (aref xterm-drag-mouse-event-array  button-char)))

      ;; If (the appropriate button-down event is bound)
      ;;     take action immediately, with a 'down-mouse-N event,
      ;; else
      ;;     wait for the release,
      ;;     if (drag event is bound)
      ;;         construct a drag event
      ;;     else
      ;;         construct a click event.
      ;;
      ;; Here's what events look like:
      ;;
      ;; Clicks and downs:
      ;;
      ;;     (EVENT-TYPE
      ;;       (WINDOW BUFFER-POS
      ;;          (COLUMN . ROW) TIMESTAMP)
      ;;       CLICK-COUNT)
      ;;
      ;; Drags:
      ;;
      ;;(EVENT-TYPE
      ;; (WINDOW1 BUFFER-POS1
      ;;  (COLUMN1 . ROW1) TIMESTAMP1)
      ;; (WINDOW2 BUFFER-POS2
      ;;  (COLUMN2 . ROW2) TIMESTAMP2))

      (if (eq 'release click-event-type)

          ;; The mouse went up.
          ;;

          ;; Generate a drag event iff appropriate.
          ;;
          (if (and xterm-mouse-drag-binding
                   (or (/= frame-x-coord xterm-mouse-down-event-frame-x-coord)
                       (/= frame-y-coord xterm-mouse-down-event-frame-y-coord)))
              (funcall xterm-mouse-drag-binding
                       (xterm-mouse-make-drag-event xterm-mouse-pending-drag-type
                                                    xterm-mouse-down-event-location
                                                    (xterm-mouse-make-event-location window
                                                                                     buffer-pos
                                                                                     window-x-coord
                                                                                     window-y-coord
                                                                                     timestamp)))

            ;; ...otherwise generate the right kind of click event.
            ;; We do nothing if down was bound.
            ;; NB. We probably need to shove an even into the read queue,
            ;; since the down handler is almost certainly in a (read-event)
            ;; loop.
            ;; Later.
            ;;
            (if (not xterm-mouse-down-binding)
                (if xterm-mouse-click-binding
                    (funcall xterm-mouse-click-binding
                             (xterm-mouse-make-click-or-down-event xterm-mouse-pending-click-type
                                                                   xterm-mouse-down-event-location
                                                                   1)))))
        ;; The mouse went down.
        ;;
        (setq xterm-mouse-down-event-frame-x-coord frame-x-coord)
        (setq xterm-mouse-down-event-frame-y-coord frame-y-coord)

        (setq xterm-mouse-pending-click-type click-event-type)

        (save-excursion
          (set-buffer buffer)
          (goto-char buffer-pos)
          (setq xterm-mouse-click-binding (key-binding (make-vector 1 click-event-type)))
          ;(setq xterm-mouse-down-binding (key-binding (make-vector 1 down-event-type)))
          (setq xterm-mouse-drag-binding (key-binding (make-vector 1 drag-event-type))))

          ;; If the down event is bound, call it.
          ;;
          ;;(if xterm-mouse-down-binding
          (if nil
              (progn
                (setq xterm-mouse-drag-binding nil)
                (setq xterm-mouse-click-binding nil)
                (funcall xterm-mouse-down-binding
                         (xterm-mouse-make-click-or-down-event down-event-type
                                                               (xterm-mouse-make-event-location window
                                                                                                buffer-pos
                                                                                                window-x-coord
                                                                                                window-y-coord
                                                                                                timestamp)
                                                               1)))
            ;; Otherwise, if a drag event is bound, record enough
            ;; information to create the event on the up-click.
            ;;
            (if xterm-mouse-drag-binding
                (progn
                  (setq xterm-mouse-pending-drag-type drag-event-type)
                  (setq xterm-mouse-down-event-location (xterm-mouse-make-event-location window
                                                                                         buffer-pos
                                                                                         window-x-coord
                                                                                         window-y-coord
                                                                                         timestamp)))

              ;; Neither down nor drag is bound,
              ;; so just record enough information to
              ;; generate a click event on the up-click.
              ;;
              (setq xterm-mouse-down-event-location (xterm-mouse-make-event-location window
                                                                                     buffer-pos
                                                                                     window-x-coord
                                                                                     window-y-coord
                                                                                     timestamp))))))))

(defun xterm-mouse-make-click-or-down-event (event-type location click-count)
  (list event-type
        location
        click-count))

(defun xterm-mouse-make-drag-event (event-type location1 location2)
  (list event-type location1 location2))

(defun xterm-mouse-make-event-location (window buffer-pos window-x-coord window-y-coord timestamp)
  (list window
        buffer-pos
        (cons window-x-coord window-y-coord)
        timestamp))

(defun xterm-mouse-generate-millisecond-timestamp ()
  "Return the current time in millisconds."
  (let ((ct (current-time)))
    (+ (nth 1 ct)
       (/ (nth 2 ct) 1000))))

(defun xterm-mouse-window-coords-to-point (window x y)
  "Return the buffer position corresponding to a set of window-relative
coordinates"
  (let* ((margin-column ())
         (prompt-width ()))
    (save-excursion
      (save-window-excursion
        (select-window window)
        (progn
          (setq prompt-width (if (window-minibuffer-p window)
                                 (if (boundp 'minibuffer-prompt-width)
                                     minibuffer-prompt-width 0)
                               0))
          (move-to-window-line y)
          (setq margin-column
                (if (or truncate-lines (> (window-hscroll) 0))
                    (current-column)
                  (- (current-column)
                     (% (current-column) (1- (window-width))))))
          (move-to-column (+ x (1- (max 1 (window-hscroll)))
                             (if (= (point) 1)
                                 (- prompt-width) 0)
                             margin-column)))
        (point)))))

;; A function to wire the mouse event handler function into
;; the appropriate keymap.
;; Depending on your envirronment, this may have to run at
;; "term-setup-hook"-time.  In any case, we arrange for it to be
;; run at load time and at "term-setup-hook"-time.
;;
(defun xterm-mouse-bind-event-handler ()

      ;; If there's a keymap already bound to "ESC-[",
      ;;     hook the mouse event handler into it
      ;; else
      ;;    construct a sparse keymap (binding "M" to mouse handler)
      ;;    and bind it ti "ESC-["
  (if (keymapp (lookup-key global-map "\e["))
      (global-set-key "\e[M" 'xterm-mouse-handle-event)
    (progn
      (global-unset-key "\e[")
      (define-key global-map "\e[" (let ((new-map (make-sparse-keymap)))
                                     (define-key new-map "M" 'xterm-mouse-handle-event)
                                     new-map)))))

;;(add-hook 'kill-emacs-hook 'xterm-mouse-disable)
;;(add-hook 'suspend-hook 'xterm-mouse-disable)
;;(add-hook 'suspend-resume-hook 'xterm-mouse-enable)

;; Turn things on.

(xterm-mouse-enable)
(setq term-setup-hook 'xterm-mouse-bind-event-handler)
(xterm-mouse-bind-event-handler)
