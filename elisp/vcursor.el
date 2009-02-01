------- start of forwarded message (RFC 934 encapsulation) -------
From: pws@P.Stephenson@swansea.ac.uk (Peter Stephenson)
Newsgroups: gnu.emacs.sources
Subject: `Virtual cursor' editing for Emacs
Date: 17 Jan 1997 09:45:20 GMT
Organization: Deutsches Elektronen-Synchrotron, Zeuthen, Germany.
Message-ID: <PWS.97Jan17104520@hydra.ifh.de>

Here's a package I've been using for some time, so it should be fairly
stable, even though I've just got around to posting it.  You need
Emacs 19 with a windowing system.  I haven't tried it with XEmacs.

The `virtual cursor' is simply another cursor-like object (it's really
an overlay, so doesn't behave completely like a real cursor) which you
manipulate by holding down control and shift at once, then movement
(in particular cursor) keys.  The main object of this is to copy to
the real cursor position, which you can do with C-S-insert; then you
can turn it off with C-S-return.  Another piece of magic is that if
the virtual cursor goes off-screen it will open its own window, so you
can look backwards or forwards in the file without too much tedious
mucking around with marks or swapping windows.

For further instructions see the comments.  I'd be particularly
interested if anybody has any bright ideas for better keybindings.
I'll also put this on my web site at:
  http://www.ifh.de/~pws/computing/vcursor.el

;;; vcursor.el --- manipulate an alternative (`virtual') cursor.

;; Copyright (C) 1994, 1996 Peter Stephenson <pws@ifh.de>

;; Author:   Peter Stephenson <pws@ifh.de>
;; Version:  $Id: vcursor.el,v 1.1 1997/01/17 09:29:14 pws Exp $
;; Keywords: virtual cursor, display, copying

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Virtual cursor commands.  I got this idea from the old BBC micro.
;; Sorry, you need GNU Emacs 19 and a windowing system.  In fact, only
;; overlays are required to work, though some of the key-bindings may
;; need changing.
;;
;; This is much easier to use than the instructions are to read.
;; I suggest you simply load it and play around with holding down Ctrl
;; and Shift and pressing up, down, left, right, tab, return, and see
;; what happens.  (Find a scratch buffer before using C-S-tab: that
;; toggles copying.)
;;
;; Most of the functions described in this documentation are in
;; parentheses so that if you have the package loaded you can type C-h
;; f on top of them for help.
;;
;; Using the cursor keys with both control and shift held down moves
;; around a virtual cursor, which is initially at point.  When active,
;; it appears with an underline through it to distinguish it from the
;; normal cursor.  You can then use one of the other commands to copy
;; characters from the location of the virtual cursor to point.  This
;; is very useful, for example, when copying some previous text while
;; making changes to it at the same time, since you never have to move
;; the `real' cursor away from where you are inserting.
;;
;; The remaining default key bindings are based around the PC-type
;; cluster found above the cursor keys on a lot of keyboards, the
;; function keys which my limited knowledge of X terminals expects to
;; find at the top.  Some functions are duplicated in more obvious
;; places for the X version.  (There are oemacs bindings as well, see
;; the actual binding commands for those.)
;;
;; All the keybindings require you to hold down control and shift at
;; once.  I assumed this combination wouldn't be heavily bound by most
;; people and that it would be easy to type with the left hand.
;; Inevitably it will clash with some other packages, but I can't help
;; that: an intuitive binding is a prerequisite here.  Currently, GNU
;; Emacs only recognises sequences with control and shift for a few
;; special keys (C-S-o, for example, doesn't see the shift), so I've
;; made heavier use of function keys than is entirely natural
;; (starting from f6 in case 1 to 5 are special), and also M-C-S,
;; i.e. with the Meta key held down as well.  If you prefer, you can
;; set vcursor-inhibit-bindings to t before loading vcursor, then
;; define your own bindings.
;; 
;; Holding down control and shift and pressing insert (vcursor-copy)
;; copies one character from wherever the virtual cursor is to point;
;; point and the virtual cursor advance in the separate and equal
;; station to which... (etc.).  M-C-S-return (vcursor-copy-line)
;; copies to the end of the line instead of just one character,
;; C-S-delete or C-S-remove (vcursor-copy-word) copies a word.
;; 
;; A more general way of copying is to use C-S-tab, which is a toggle.
;; In the `on' state, moving the virtual cursor will copy the
;; moved-over text to the normal cursor position (including when going
;; backwards, though each piece of text moved over is copied forwards:
;; compare the behaviour of C-S-up and C-S-left).
;;
;; However, that's just a small part of the magic.  If the virtual
;; cursor goes off the display, it will be redisplayed in some other
;; window.  (See the function (vcursor-find-window) for details of how
;; this window is chosen.)  This gives you fingertip control over two
;; windows at once.
;; 
;; C-S-return (vcursor-disable) `homes' the virtual cursor; actually it
;; removes it, so it starts from point whenever you move it again ---
;; note that simply moving the cursor and virtual cursor on top of one
;; another does not have this effect.
;; 
;; If you gave C-S-return a positive prefix arg, it will also delete the
;; window (unless it's the current one).  Whenever the virtual cursor
;; goes off-screen in its own window, point in that window is moved as
;; well to restore it to view.  (It's easier that way, that's why.
;; However, point doesn't move unless the view in the window does, so
;; it's not tied to the virtual cursor location.)
;;
;; You can also use C-S-return with a negative prefix argument which
;; forces the vcursor to appear at point.  This is particularly useful if
;; you actually want to edit in another window but would like to
;; remember the current cursor location for examining or copying from
;; that buffer.  (I just hit C-S-right C-S-left, but I'm a hopeless
;; lowbrow.)
;; 
;; There is also C-S-f6 (vcursor-other-window) which behaves like
;; other-window on the virtual rather than the real cursor, except
;; that it will create another window if necessary.
;;
;; The keys C-S-prior (vcursor-scroll-down) and C-S-next
;; (vcursor-scroll-up) (i.e., PageUp and PageDown) will scroll the
;; virtual cursor window, appropriately chosen.  They will always
;; create a new window or take over an old one if necessary.
;; Likewise, M-C-S-left and M-C-S-right move you to the
;; beginning or end of a line, C-S-home and C-S-end the
;; beginning or end of a buffer (these are also on M-C-S-up and
;; M-C-S-down for those of us stuck with DEC keyboards).
;;
;; C-S-f7 (vcursor-goto) will take you to the vcursor position
;; (swapping windows if it seems sensible) and (unless you give it a
;; prefix argument) delete the virtual cursor, so this is useful for
;; you to take over editing at the virtual cursor position.  It is not
;; an error if the virtual cursor is not active; it simply leaves you
;; at point, because that is where the virtual cursor would start
;; from.
;;
;; In a similar vein, M-C-S-tab (hope your left hand's flexible;
;; C-S-select on DEC keyboards) (vcursor-swap-point) will take you to
;; the virtual cursor position but simultaneously put the virtual
;; cursor at the old cursor position.  It is also supposed to ensure
;; that both are visible.
;;
;; C-S-f8 (C-S-find on DEC keyboards) (vcursor-isearch-forward)
;; allows you to do an isearch in another window.  It works a bit like
;; vcursor-scroll-*; it moves into another window, calls isearch
;; there, and sets the virtual cursor position to the point found.  In
;; other words, it works just like isearch but with the virtual cursor
;; instead of the real one (that's why it's called a `virtual
;; cursor').  While you are isearching, you are editing in the virtual
;; cursor window, but when you have finished you return to where you
;; started.  Note that once you are in isearch all the keys are normal
;; --- use C-s, not C-S-f8, to search for the next occurrence.
;;
;; If you set the variable vcursor-auto-disable, then any command
;; which does not involve moving or copying from the virtual cursor
;; causes the virtual cursor to be disabled.  If you don't intend to
;; use this, you can comment out the `add-hook' line at the bottom of
;; this file.  (This feature partially emulates the way the `copy' key
;; on the BBC micro worked; actually, the copy cursor was homed when
;; you hit return.  This was in keeping with the line-by-line way of
;; entering BASIC, but is less appropriate here.)
;;
;; There is a way of moving the virtual cursor using ordinary
;; commands: C-S-f9 (vcursor-execute-key) reads a key string,
;; moves to the virtual cursor position, executes the command bound to
;; the string, then returns to the original point.  Thus C-S-f9 M-m
;; moves the virtual cursor back to the first non-whitespace character
;; on its line.  As the command is called interactively all the usual
;; ways of passing information to the command called, such as by a
;; prefix argument, are available.  C-S-f10 (C-S-x)
;; (vcursor-execute-command) behaves the same way but you enter the
;; name of the command.  Of course, only some commands are useful
;; here, mainly simple movement commands.  Killing at the virtual
;; cursor position in this way works as well; you can even save
;; another buffer with C-S-f9 C-x C-s.  To do anything more
;; complicated, you are better off using M-C-S-tab
;; (vcursor-swap-point), doing whatever it is, then calling M-C-S-tab
;; again.
;;
;; If you want to add your own moving or copying functions you should
;; be able to do this fairly easily with (vcursor-relative-move) and
;; (vcursor-copy) together with (vcursor-get-char-count).  If you want to
;; do something in a different window, use (vcursor-window-funcall).
;;
;; Un-features:
;;  - The vcursor will not move to point-max, since otherwise it would
;;    disappear.  However, no error is flagged as point-max is a legal
;;    point in the buffer.  Thus cursor right or down at the second
;;    last point in the file does not flag an error, which is inconsistent,
;;    and if copying is on the last character (typically newline) will
;;    be repeatedly copied.  (I've tried making it flag an error
;;    instead and that's worse since often the vcursor is sent to
;;    point in some other window, which may be point-max.)
;;  - The vcursor widens when over a tab character or right at the
;;    end of the line.  You're welcome to consider this a feature;
;;    it's just a part of how overlays work.
;;  - The vcursor obscures the real cursor.  Creative use of overlays
;;    could cure this.
;;  - The vcursor does not remember its own previous positions.  If
;;    you cycle it back into a window it was in before, it will be at
;;    point in that window.  Often, that is where a previous recenter
;;    left point, not where the vcursor was before.
;;    (Note, however, that the vcursor does remember where it *is*,
;;    even if it's off-screen.  This can also lead to surprises, but I
;;    don't think it's a bug.)
;;  - vcursor-window-funcall could perhaps be smarter about restoring
;;    the previous window state on failure.
;;  - The restrictions on bindings with a C-S- prefix noted above
;;    limit a really instinctive use of key bindings (e.g., C-S-a for
;;    vcursor-beginning-of-line would be much better).
;;  - The logic in vcursor-find-window is rather complicated and
;;    therefore bug-prone, though in practice it seems to work OK.
;;
;; Possible enhnacements:
;; It would be easy to implement vcursor-push (save vcursor position
;; as mark and deactivate) and vcursor-pop (deactivate vcursor and
;; move to last pushed position) functions.

;;; Code:

(or (memq 'vcursor (face-list))
    (progn
      (copy-face 'modeline 'vcursor)
      (if (or (fboundp 'oemacs-version) (x-display-color-p))
	  (progn
	    (set-face-foreground 'vcursor "blue")
	    (set-face-background 'vcursor "cyan")))
      (set-face-underline-p 'vcursor t)))

(defvar vcursor-auto-disable nil
  "*If non-nil, disable the virtual cursor after use.
Any non-vcursor command will force vcursor-disable to be called.")

(defvar vcursor-inhibit-bindings nil
  "*Set to t before loading vcursor to inhibit the usual key bindings.")

(defvar vcursor-overlay nil "Overlay for the virtual cursor.
Nil if that is not enabled.")

(defvar vcursor-window nil
  "Last window to have displayed the virtual cursor.
See the function vcursor-find-window for how this is used.")

(defvar vcursor-last-command nil
  "t if last command was a vcursor command.
Vcursor-copy, vcursor-relative-move and the scrolling commands set
this.  It is used by the vcursor-auto-disable code.")
;; could do some memq-ing with last-command instead, but this will
;; automatically handle any new commands using the primitives.

(defvar vcursor-copy-flag nil 
  "t if moving vcursor should copy characters moved over to point.")

(defvar vcursor-temp-goal-column nil
  "Keeps track of temporary goal columns for the virtual cursor.")

(or vcursor-inhibit-bindings
    (if (fboundp 'oemacs-version)
	;; If you can't get you're own personal key bindings into you're
	;; own software, what is life coming to?
	(progn
	  (global-set-key "\M-[\C-f\M-\C-m" 'vcursor-previous-line) ; C-S-up
	  (global-set-key "\M-[\C-f\M-\C-q" 'vcursor-next-line) ; C-S-down
	  (global-set-key "\M-[\C-fs" 'vcursor-backward-char) ; C-S-left
	  (global-set-key "\M-[\C-ft" 'vcursor-forward-char) ; C-S-right

	  (global-set-key "\M-[\C-fw" 'vcursor-disable) ; C-S-home, sorry
	  (global-set-key "\M-[\C-b\C-o" 'vcursor-toggle-copy) : S-tab, sorry
	  (global-set-key "\M-[\C-f\M-\C-r" 'vcursor-copy) ; C-S-insert
	  (global-set-key "\M-[\C-fu" 'vcursor-copy-line) ; C-S-end
	  (global-set-key "\M-[\C-f\M-\C-s" 'vcursor-copy-word) ; C-S-delete
	  (global-set-key "\M-[\C-f\M-\C-d" 'vcursor-scroll-down) ; C-S-prior
	  (global-set-key "\M-[\C-fv" 'vcursor-scroll-up) ; C-S-next

	  (global-set-key "\M-[\C-f^" 'vcursor-beginning-of-buffer) ; C-S-f1
	  (global-set-key "\M-[\C-f_" 'vcursor-beginning-of-line) ; C-S-f2
	  (global-set-key "\M-[\C-f`" 'vcursor-swap-point) ; C-S-f3
	  (global-set-key "\M-[\C-fa" 'vcursor-end-of-line) ; C-S-f4
	  (global-set-key "\M-[\C-fb" 'vcursor-end-of-buffer) ; C-S-f5
	  (global-set-key "\M-[\C-fc" 'vcursor-other-window) ; C-S-f6
	  (global-set-key "\M-[\C-fd" 'vcursor-goto) ; C-S-f7
	  (global-set-key "\M-[\C-fe" 'vcursor-isearch-forward) ; C-S-f8
	  (global-set-key "\M-[\C-ff" 'vcursor-execute-key) ; C-S-f9
	  (global-set-key "\M-[\C-fg" 'vcursor-execute-command) ; C-S-f10
	  )
      (global-set-key [C-S-up] 'vcursor-previous-line)
      (global-set-key [C-S-down] 'vcursor-next-line)
      (global-set-key [C-S-left] 'vcursor-backward-char)
      (global-set-key [C-S-right] 'vcursor-forward-char)

      (global-set-key [C-S-return] 'vcursor-disable)
      (global-set-key [C-S-insert]  'vcursor-copy)
      (global-set-key [C-S-delete] 'vcursor-copy-word)
      (global-set-key [C-S-remove] 'vcursor-copy-word)
      (global-set-key [C-S-tab] 'vcursor-toggle-copy)
      (global-set-key [C-S-home] 'vcursor-beginning-of-buffer)
      (global-set-key [M-C-S-up] 'vcursor-beginning-of-buffer)
      (global-set-key [C-S-end] 'vcursor-end-of-buffer)
      (global-set-key [M-C-S-down] 'vcursor-end-of-buffer)
      (global-set-key [C-S-prior] 'vcursor-scroll-down)
      (global-set-key [C-S-next] 'vcursor-scroll-up)

      (global-set-key [C-S-f6] 'vcursor-other-window)
      (global-set-key [C-S-f7] 'vcursor-goto)

      (global-set-key [C-S-select] 'vcursor-swap-point) ; DEC keyboards
      (global-set-key [M-C-S-tab] 'vcursor-swap-point)

      (global-set-key [C-S-find] 'vcursor-isearch-forward) ; DEC keyboards
      (global-set-key [C-S-f8] 'vcursor-isearch-forward)

      (global-set-key [M-C-S-left] 'vcursor-beginning-of-line)
      (global-set-key [M-C-S-right] 'vcursor-end-of-line)

      (global-set-key [M-C-S-prior] 'vcursor-backward-word)
      (global-set-key [M-C-S-next] 'vcursor-forward-word)

      (global-set-key [M-C-S-return] 'vcursor-copy-line)

      (global-set-key [C-S-f9] 'vcursor-execute-key)
      (global-set-key [C-S-f10] 'vcursor-execute-command)
  ))

(defun vcursor-locate ()
  "Go to the starting point of the virtual cursor.
If that's disabled, don't go anywhere but don't complain."
  ;; This is where we go off-mass-shell.  Assume there is a
  ;; save-excursion to get us back to the pole, er, point.

  (and (overlayp vcursor-overlay)
       (overlay-buffer vcursor-overlay)
       (set-buffer (overlay-buffer vcursor-overlay))
       (goto-char (overlay-start vcursor-overlay)))
  )

(defun vcursor-find-window (&optional not-this new-win this-frame)
  "Return a suitable window for displaying the virtual cursor.
This is the first window in cyclic order where the vcursor is visible.

With optional NOT-THIS non-nil never return the current window.

With NEW-WIN non-nil, display the virtual cursor buffer in another
window if the virtual cursor is not currently visible \(note, however,
that this function never changes window-point\).

With THIS-FRAME non-nil, don't search other frames for a new window
\(though if the vcursor is already off-frame its current window is
always considered, and the value of pop-up-frames is always respected\).

Returns nil if the virtual cursor is not visible anywhere suitable.
Set vcursor-window to the returned value as a side effect."

  ;; The order of priorities (respecting NOT-THIS) is (1)
  ;; vcursor-window if the virtual cursor is visible there (2) any
  ;; window displaying the virtual cursor (3) vcursor-window provided
  ;; it is still displaying the buffer containing the virtual cursor and
  ;; is not selected (4) any unselected window displaying the vcursor
  ;; buffer (5) with NEW-WIN, a window selected by display-buffer (so
  ;; the variables pop-up-windows and pop-up-frames are significant)
  ;; (6) nil.

  (let ((thiswin (selected-window)) winok winbuf)
    (save-excursion
      (vcursor-locate)
      (or (and (window-live-p vcursor-window)
	       (eq (current-buffer) (window-buffer vcursor-window))
	       (not (and not-this (eq thiswin vcursor-window))))
	  (setq vcursor-window nil))
      (or (and vcursor-window		; choice 1
	       (pos-visible-in-window-p (point) vcursor-window))
	  (progn
	    (walk-windows
	     (function 
	      (lambda (win)
		(and (not winok)
		     (eq (current-buffer) (window-buffer win))
		     (not (and not-this (eq thiswin win)))
		     (cond
		      ((pos-visible-in-window-p (point) win) (setq winok win))
		      ((eq thiswin win))
		      ((not winbuf) (setq winbuf win))))))
	     nil (not this-frame))
	    (setq vcursor-window
		  (cond
		   (winok)		; choice 2
		   ((and vcursor-window	; choice 3
			 (not (eq thiswin vcursor-window))) vcursor-window)
		   (winbuf)		; choice 4
		   (new-win (display-buffer (current-buffer) t)) ; choice 5
		   (t nil)))))))	; default (choice 6)
  vcursor-window
  )

(defun vcursor-toggle-copy (&optional arg nomsg)
  "Toggle copying to point when the vcursor is moved.
With a prefix ARG, turn on if non-negative, off if negative.
Display a message unless optional NOMSG is non-nil."
  (interactive "P")
  (setq vcursor-copy-flag
	(cond ((not arg) (not vcursor-copy-flag))
	      ((< (prefix-numeric-value arg) 0) nil)
	      (t))
	vcursor-last-command t)
  (or nomsg (message "Copying from the vcursor is now %s."
		     (if vcursor-copy-flag "on" "off")))
  )

(defun vcursor-move (pt)
  "Move the virtual cursor to the character to the right of PT.
PT is an absolute location in the current buffer.

If the new virtual cursor location would not be visible, display it in
another window."
  ;; this works even if we're on-mass-shell, but usually we won't be.

  (if (eq pt (point-max)) (setq pt (1- pt)))
  (if (vcursor-check t)
      (move-overlay vcursor-overlay pt (+ pt 1) (current-buffer))
    (setq vcursor-overlay (make-overlay pt (+ pt 1)))
    (overlay-put vcursor-overlay 'face 'vcursor))
  (vcursor-find-window nil t)
  ;; vcursor-window now contains the right buffer
  (or (pos-visible-in-window-p pt vcursor-window)
      (set-window-point vcursor-window pt))
  )

(defun vcursor-relative-move (fn &rest args)
  "Use FUNCTION with arbitrary ARG1 ... to move the virtual cursor.

This is called by most of the virtual-cursor motion commands."
  (let (text opoint)
    (save-excursion
      (vcursor-locate)
      (setq opoint (point))
      (apply fn args)
      (and (eq opoint (point-max)) (eq opoint (point))
	   (signal 'end-of-buffer nil))
      (vcursor-move (point))
      (if vcursor-copy-flag (setq text (buffer-substring opoint (point)))))
    (if text (insert text)))
  (setq vcursor-last-command t)
  )

(defun vcursor-goto (&optional arg)
  "Move the real cursor to the virtual cursor position.
If the virtual cursor is (or was recently) visible in another window,
switch to that first.  Without a prefix ARG, disable the virtual
cursor as well."

  (interactive "P")
  (and (vcursor-find-window) (select-window vcursor-window))
  (let ((buf (and vcursor-overlay (overlay-buffer vcursor-overlay))))
    (and buf (not (eq (current-buffer) buf)) (switch-to-buffer buf)))
  (vcursor-locate)
  (or arg (vcursor-disable))
  )

(defun vcursor-swap-point ()
  "Swap the location of point and that of the virtual cursor.

The virtual cursor window becomes the selected window and the old
window becomes the virtual cursor window.  If the virtual cursor would
not be visible otherwise, display it in another window."

  (interactive)
  (let ((buf (current-buffer)) (here (point)) (win (selected-window)))
    (vcursor-goto) ; will disable the vcursor
    (save-excursion
      (set-buffer buf)
      (setq vcursor-window win)
      (vcursor-move here)))
)

(defun vcursor-scroll-up (&optional n)
  "Scroll up the vcursor window ARG lines or near full screen if none.
The vcursor will always appear in an unselected window."

  (interactive "P")
  (vcursor-window-funcall 'scroll-up n)
)

(defun vcursor-scroll-down (&optional n)
  "Scroll down the vcursor window ARG lines or near-full screen if none.
The vcursor will always appear in an unselected window."

  (interactive "P")
  (vcursor-window-funcall 'scroll-down n)
  )

(defun vcursor-isearch-forward (&optional rep norecurs)
  "Call isearch-forward in the virtual cursor window.
The virtual cursor is moved to the resulting point; the ordinary
cursor stays where it was."

  (interactive "P")
  (vcursor-window-funcall 'isearch-forward rep norecurs)
  )

(defun vcursor-window-funcall (func &rest args)
  "Call FUNC with ARGS ... in a virtual cursor window.
A window other than the currently-selected one will always be used.
The virtual cursor is moved to the value of point when the function
returns."

  (vcursor-find-window t t)
  (let ((sw (selected-window)) text)
    ;; We can't use save-window-excursion because that would restore
    ;; the original display in the window we may want to alter.
    (unwind-protect
	(let ((here (point)))
	  (select-window vcursor-window)
	  (vcursor-locate)
	  (apply func args)
	  (if vcursor-copy-flag (setq text (buffer-substring here (point))))
	  (vcursor-move (point)))
      (select-window sw))
    (if text (insert text)))
  (setq vcursor-last-command t)
  )

(defun vcursor-get-char-count (fn &rest args)
  "Apply FN to ARG1 ... and return the number of characters moved.
Point is temporarily set to the virtual cursor position before FN is
called.

This is called by most of the virtual-cursor copying commands to find
out how much to copy."

  (vcursor-check)
  (save-excursion
    (set-buffer (overlay-buffer vcursor-overlay))
    (let ((start (goto-char (overlay-start vcursor-overlay))))
      (- (progn (apply fn args) (point)) start)))
  )

;; Make sure the virtual cursor is active.  Unless arg is non-nil,
;; report an error if it is not.
(defun vcursor-check (&optional arg)
  (cond
   ((and (overlayp vcursor-overlay) (overlay-start vcursor-overlay))
    t)
   (arg nil)
   (t (error "The virtual cursor is not active now.")))
  )

(defun vcursor-disable (&optional arg)
  "Disable the virtual cursor.
Next time you use it, it will start from point.

With a positive prefix ARG, the first window in cyclic order
displaying the virtual cursor (or which was recently displaying the
virutal cursor) will be deleted unless it's the selected
window.

With a negative prefix argument, undisable the virtual cursor: make it
active at the same point as the real cursor.

Copying mode is always turned off:  the next use of the vcursor will
not copy text until you turn it on again."

  (interactive "P")
  (if (overlayp vcursor-overlay)
      (progn
	(delete-overlay vcursor-overlay)
	(setq vcursor-overlay nil)))
  (cond
   ((not (vcursor-find-window t)))
   ((or (not arg) (< (prefix-numeric-value arg) 0)))
   ((delete-window vcursor-window)))
  (and arg (< (prefix-numeric-value arg) 0)
       (progn
         (vcursor-move (point))
         (setq vcursor-window (selected-window))))
  (setq vcursor-copy-flag nil)
  )

(defun vcursor-other-window (n &optional all-frames)
  "Activate the virtual cursor in another window.
This is the next window cylically after one currently showing the
virtual cursor, or else after the current selected window.  If there
is no other window, the current window is split.

Arguments N and optional ALL-FRAMES are the same as with other-window.
ALL-FRAMES is also used to decide whether to split the window."

  (interactive "p")
  (if (one-window-p nil all-frames)
      (display-buffer (current-buffer) t))
  (save-excursion
    (save-window-excursion
      ;; We don't use fancy vcursor-find-window trickery, since we're
      ;; quite happy to have the vcursor cycle back into the current
      ;; window.
      (let ((sw (selected-window))
	    (win (vcursor-find-window nil nil (not all-frames))))
	(if win (select-window win))
	;; else start from here
	(other-window n all-frames)
	(vcursor-disable -1))))
  )

(defun vcursor-compare-windows (&optional arg)
  "Call compare-windows in the vcursor window.
This has the effect of comparing the vcursor window with whichever
window next-window returns there, which may not be the selected one.

Pass prefix ARG to compare-windows."
  (interactive "P")
  (vcursor-window-funcall 'compare-windows arg))

(defun vcursor-next-line (arg)
  "Move the virtual cursor forward ARG lines."
  ;; This is next-line rewritten for the vcursor.  Maybe it be easier
  ;; simply to rewrite line-move.
  (interactive "p")
  (let (temporary-goal-column opoint text)
    (save-excursion
      (vcursor-locate)
      (setq temporary-goal-column
	    (if (or (eq last-command 'vcursor-next-line)
		    (eq last-command 'vcursor-previous-line))
		(progn
		  (setq last-command 'next-line) ; trick line-move
		  vcursor-temp-goal-column)
	      (if (and track-eol (eolp)
		       (or (not (bolp)) (eq last-command 'end-of-line)))
		  9999
		(current-column)))
	    opoint (point))
      (line-move arg)
      (and (eq opoint (point-max)) (eq opoint (point))
	   (signal 'end-of-buffer nil))
      (if vcursor-copy-flag (setq text (buffer-substring opoint (point))))
      (vcursor-move (point))
      (setq vcursor-temp-goal-column temporary-goal-column
	    vcursor-last-command t))
    (if text (insert text)))
  )

(defun vcursor-previous-line (arg)
  "Move the virtual cursor back ARG lines."
  (interactive "p")
  (vcursor-next-line (- arg))
  )

(defun vcursor-forward-char (arg)
  "Move the virtual cursor forward ARG characters."
  (interactive "p")
  (vcursor-relative-move 'forward-char arg)
  )

(defun vcursor-backward-char (arg)
  "Move the virtual cursor backward ARG characters."
  (interactive "p")
  (vcursor-relative-move 'backward-char arg)
  )

(defun vcursor-forward-word (arg)
  "Move the virtual cursor forward ARG words."
  (interactive "p")
  (vcursor-relative-move 'forward-word arg)
  )

(defun vcursor-backward-word (arg)
  "Move the virtual cursor backward ARG words."
  (interactive "p")
  (vcursor-relative-move 'backward-word arg)
  )

(defun vcursor-beginning-of-line (arg)
  "Move the virtual cursor to beginning of its current line.
ARG is as for beginning-of-line."
  (interactive "P")
  (vcursor-relative-move 'beginning-of-line
			 (if arg (prefix-numeric-value arg)))
  )

(defun vcursor-end-of-line (arg)
  "Move the virtual cursor to end of its current line.
ARG is as for end-of-line."
  (interactive "P")
  (vcursor-relative-move 'end-of-line
			 (if arg (prefix-numeric-value arg)))
  )

(defun vcursor-beginning-of-buffer (&optional arg)
  "Move the virtual cursor to the beginning of its buffer.
ARG is as for beginning-of-buffer."
  (interactive "P")
  (vcursor-relative-move
   (lambda (arg)
     (goto-char (if arg (/ (* arg (- (point-max) (point-min))) 10)
		  (point-min))))
   (if arg (prefix-numeric-value arg)))
  )

(defun vcursor-end-of-buffer (&optional arg)
  "Move the virtual cursor to the end of its buffer.
ARG is as for end-of-buffer.

Actually, the vcursor is moved to the second from last character or it
would be invisible."
  (interactive "P")
  (vcursor-relative-move
   (lambda (arg)
     (goto-char (if arg (- (point-max)
			   (/ (* arg (- (point-max) (point-min))) 10))
		  (point-max))))
   (if arg (prefix-numeric-value arg)))
  )

(defun vcursor-execute-command (cmd)
  "Execute COMMAND for the virtual cursor.
COMMAND is called interactively.  Not all commands (in fact, only a
small subset) are useful."
  (interactive "CCommand: ")
  (let (text opoint)
    (save-excursion
      (vcursor-locate)
      (setq opoint (point))
      (call-interactively cmd)
      (if vcursor-copy-flag (setq text (buffer-substring opoint (point))))
      (vcursor-move (point)))
    (if text (insert text)))
  (setq vcursor-last-command t)
  )

(defun vcursor-execute-key (keys)
  "Execute the command bound to KEYS for the virtual cursor.
The command found is called interactively, so prefix argument etc.
are usable."

  (interactive "kKey sequence: ")
  (let ((cmd (key-binding keys)))
    (if cmd (vcursor-execute-command (key-binding keys))))
  )

(defun vcursor-copy (arg)
  "Copy ARG characters from the virtual cursor position to point."
  (interactive "p")
  (vcursor-check)
  (insert
   (save-excursion
     (set-buffer (overlay-buffer vcursor-overlay))
     (let* ((ostart (overlay-start vcursor-overlay))
	    (end (+ ostart arg)))
       (prog1
	   (buffer-substring ostart end)
	 (vcursor-move end)))))
  (setq vcursor-last-command t)
)

(defun vcursor-copy-word (arg)
  "Copy ARG words from the virtual cursor position to point."
  (interactive "p")
  (vcursor-copy (vcursor-get-char-count 'forward-word arg))
  )

(defun vcursor-copy-line (arg)
  "Copy up to ARGth line after virtual cursor position.
With no argument, copy to the end of the current line.

Behaviour with regard to newlines is similar (but not identical) to
kill-line; the main difference is that whitespace at the end of the
line is treated like ordinary characters."

  (interactive "P")
  (let* ((num (prefix-numeric-value arg))
	 (count (vcursor-get-char-count 'end-of-line num)))
    (vcursor-copy (if (or (= count 0) arg) (1+ count) count)))
  )

(defun vcursor-post-command ()
  (and vcursor-auto-disable (not vcursor-last-command)
       vcursor-overlay (vcursor-disable))
  (setq vcursor-last-command nil)
  )

(add-hook 'post-command-hook 'vcursor-post-command)

(provide 'vcursor)
--
-- 
Peter Stephenson <pws@ifh.de>       Tel: +49 33762 77366
WWW:  http://www.ifh.de/~pws/       Fax: +49 33762 77413
Deutsches Elektronen-Synchrotron --- Institut fuer Hochenergiephysik Zeuthen
DESY-IfH, 15735 Zeuthen, Germany.
------- end -------

-- 
Kevin Esler                 Pure Atria
                            20 Maguire Road
Tel: (617) 676-2626         Lexington, MA 02173-3104
Fax: (617) 676-2600
mailto:esler@PureAtria.com  WWW: http://www.PureAtria.com/

