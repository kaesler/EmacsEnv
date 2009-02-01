; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         pgp-aids.el
; RCS:          $Header: pgp-aids.el,v 1.20 95/01/24 15:26:35 jad Exp $
; Description:  Functions to assist with use of PGP, primarily for email
; Author:       Tai Jin <tai@hpl.hp.com>, John Dilley <jad@hpl.hp.com>
; Created:      Thu Jan 12 01:12:00 1995
; Modified:     Tue Jan 24 15:25:51 1995 (John Dilley) jad@pimlico.hpl.hp.com
; Language:     Emacs-Lisp
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'mail-utils)
(provide 'pgp-aids)

;;;;
;;  Thie file provides some useful PGP assistance routines.  The user
;;  callable routines are:
;;
;; pgp-do-sign-encrypt-message : main routine to encrypt and sign the body
;; of a mail message.  Can use conventional or public key encryption; will
;; prompt for whether to sign and to encrypt the message, and for the key.
;; Note: the value of pgp-message-header-separator must be set correctly for
;; the current type of message.
;;
;; Helper routines to get around all the prompts:
;;
;; pgp-conventional-encrypt-region : encrypt a given region
;; pgp-conventional-encrypt-message : encrypt the message body
;;
;; pgp-sign-message : PGP sign the body of a msil message
;; pgp-encrypt-message : PGP encrypt the message body
;; pgp-sign-and-encrypt-message : PGP encrypt and sign the message body
;; 
;; pgp-decrypt-message : decrypt an encrypted message
;; pgp-mail-decrypt-message : helper function for rmail
;; pgp-mh-decrypt-message : helper function for mh-e
;;
;; pgp-sign-region : PGP sign the current region
;; pgp-encrypt-region : PGP encrypt the current region
;; pgp-sign-and-encrypt-region : PGP encrypt and sign the current region
;;

;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Global variables.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar pgp-content-type "application/x-pgp"
  "*MIME type indicator used in Content-Type: header field in mail messages.")

(defvar pgp-msg-begin-regexp
  "^-----BEGIN PGP \\(SIGNED \\)?MESSAGE-----$"
  "*Regular expression marking the beginning of a PGP message.")

(defvar pgp-msg-begin-signed-regexp
  "^-----BEGIN PGP SIGNED MESSAGE-----$"
  "*Regular expression marking the beginning of a PGP message.")

(defvar pgp-msg-begin-signature-regexp
  "^-----BEGIN PGP SIGNATURE-----$"
  "*Regular expression marking the beginning of a PGP message.")

(defvar pgp-msg-end-signature-regexp
  "^-----END PGP SIGNATURE-----$"
  "*Regular expression marking the end of a PGP message.")

(defvar pgp-msg-end-regexp
  "^-----END PGP .*-----$"
  "*Regular expression marking the end of a PGP message.")

(defvar pgp-message-header-separator
  (concat "^" mail-header-separator "$")
  "*Regular expression denoting the end of a mail message's headers.
You may have to change this for your mailer -- the default works for rmail.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Utility functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pgp-remove-existing-signature ()
  "Remove any existing PGP signature block in this buffer."
  (interactive)
  (goto-char (point-min))
  (let ((beg (and (re-search-forward pgp-msg-begin-signed-regexp nil t)
                  (re-search-backward pgp-msg-begin-signed-regexp nil t)
                  (point)))
        (sig (and (re-search-forward pgp-msg-begin-signature-regexp nil t)
                  (re-search-backward pgp-msg-begin-signature-regexp nil t)
                  (point)))
        (end (and (re-search-forward pgp-msg-end-signature-regexp nil t)
                  (point))))
    (if (null (and sig end))
        (message "Signature not found.")
      (delete-region sig end)
      (if (null beg)
          (message "Signature beginning not found.") ; but not an error
        (goto-char beg)
        (delete-line 1)))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  User-callable encryption functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pgp-do-sign-encrypt-message (conventional
                                    &optional do-sign do-encrypt)
  "Encrypt and optionally sign the body of a mail message.
A non-nil first argument means to use CONVENTIONAL cryptography instead
of public key cryptography.  If CONVENTIONAL is not selected, the caller
will be prompted asked whether to SIGN and ENCRYPT the message."
  (interactive (list (y-or-n-p "Use conventional encryption only? ")))

  (let ((sign (or do-sign
                  (if conventional nil (y-or-n-p "Sign message? "))))
        (encrypt (or do-encrypt
                     conventional (y-or-n-p "Encrypt message? ")))
        (content-header-field "Content-Type: ")
        userids)

    (if (eq sign 'no)  (setq sign nil))
    (if (eq encrypt 'no)  (setq encrypt nil))

    (pgp-remove-existing-signature)

    (goto-char (point-min))
    (or (re-search-forward pgp-message-header-separator nil t)
        (re-search-forward "^$" nil t)
        (error "Could not find separator between message headers and body!"))
    (beginning-of-line 1)
    (narrow-to-region (point-min) (point))

    ;;  Insert content-type header field
    (goto-char (point-min))
    (if (re-search-forward content-header-field nil t)
        nil                                     ; already in there
      (goto-char (point-max))
      (beginning-of-line 1)
      (insert content-header-field pgp-content-type "\n"))

    ;;  Grab mail user IDs for the encryption prompt
    (if (or conventional (not encrypt))
        nil				;; don't worry about userids
      (setq userids (mail-strip-quoted-names
                     (concat (mail-fetch-field "to" nil t) " "
                             (mail-fetch-field "cc" nil t) " "
                             (mail-fetch-field "bcc" nil t))))
      (while (string-match "[,\t]" userids)
        (setq userids (concat (substring userids 0 (match-beginning 0))
                              (substring userids (match-end 0)))))
      (while (string-match "@[^ ]+" userids)
        (setq userids (concat (substring userids 0 (1+ (match-beginning 0)))
                              (substring userids (match-end 0))))))

    (goto-char (point-max))
    (widen)
    (forward-line 1)

    (pgp-on-region (point) (point-max)
	           (if conventional
                     (read-string-no-echo "Conventional encryption key: ")
                     (if sign
                         (read-string-no-echo "PGP pass phrase: ")))
                   (if conventional "-fc"
                     (concat "-fat"
                             (if sign "s")
                             (if encrypt "e")))
                   (if (and encrypt (not conventional))
                       (read-string "Recipient(s): " userids)))
    ))

;;;;
;;  Mailer oriented convenience routines.
;;;;

(defun pgp-conventional-encrypt-message ()
  "Conventionally encrypt this message."
  (interactive)
  (pgp-do-sign-encrypt-message t))

(defun pgp-sign-message ()
  "Sign this message using public key cryptography."
  (interactive)
  (pgp-do-sign-encrypt-message nil t 'no))

(defun pgp-encrypt-message ()
  "Encrypt this message using public key cryptography."
  (interactive)
  (pgp-do-sign-encrypt-message nil 'no t))

(defun pgp-sign-and-encrypt-message ()
  "Encrypt this message using public key cryptography and sign it."
  (interactive)
  (pgp-do-sign-encrypt-message nil t t))

;;;;
;;  Buffer-oriented convenience routines.  Use M-x mark-whole-buffer before
;;  calling these routines to operate on the whole buffer.
;;;;

(defun pgp-conventional-encrypt-region (min max)
  "Encrypt the region using conventional cryptography."
  (interactive (list (region-beginning) (region-end)))
  (pgp-on-region min max
                 (read-string-no-echo "Enter key: ")
                 "-cf"
                 nil))

(defun pgp-encrypt-region (min max)
  "Encrypt the region using public key cryptography."
  (interactive (list (region-beginning) (region-end)))
  (pgp-on-region min max
                 (read-string-no-echo "PGP pass phrase: ")
                 "-fate"
                 (read-string "Recipient(s): ")))

(defun pgp-sign-region (min max)
  "Sign the region (but don't encrypt it)."
  (interactive (list (region-beginning) (region-end)))
  (pgp-on-region (point-min) (point-max)
                 (read-string-no-echo "PGP pass phrase: ")
                 "-fats"
                 nil))

(defun pgp-sign-and-encrypt-region (min max)
  "Sign and encrypt the region using public key cryptography."
  (interactive (list (region-beginning) (region-end)))
  (pgp-on-region (point-min) (point-max)
                 (read-string-no-echo "PGP pass phrase: ")
                 "-feats"
                 (read-string "Recipient(s): ")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Decryption engine.  Reads the user's pass phrase, sets up the region,
;;  and calls the main PGP engine.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pgp-decrypt-message ()
  "Decrypt the first block of encrypted text in this buffer.
Caller will be prompted for the pass phrase to use."
  (interactive)
  (goto-char (point-min))
  (save-excursion
    (let ((beg (and (re-search-forward pgp-msg-begin-regexp nil t)
                    (re-search-backward pgp-msg-begin-regexp nil t)
                    (point)))
          (end (and (re-search-forward pgp-msg-end-regexp nil t)
                    (point)))
          (verify-signature nil)
	  (pgp-passwd nil))
      (if (null (and beg end))
	  (error "No PGP message in this buffer!"))
      (goto-char beg)
      (if (looking-at pgp-msg-begin-signed-regexp)
	  (setq verify-signature 'keep)
	(setq pgp-passwd (read-string-no-echo "PGP pass phrase: "))
	(if (not (and (stringp pgp-passwd)
		      (> (length pgp-passwd) 0)))
	    (setq pgp-passwd nil)))
      (pgp-on-region beg end pgp-passwd "-f" nil verify-signature))))

;;;;
;;  Decryption convenience routines for the mailers ...
;;;;

(defun pgp-mail-decrypt-message ()
  "Decrypt the body of an rmail message."
  (interactive)
  (let (buffer-read-only)
    ;;  go to the rmail show buffer ...
    (pgp-decrypt-message)))

(defun pgp-mh-decrypt-message ()
  "Decrypt the body of an MH message being shown."
  (interactive)
  (set-buffer mh-show-buffer)
  (pgp-decrypt-message))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Main PGP engine for encryption and decryption of a region of text.
;;  Requires the caller pass in a region of plain text to be encrypted, or a
;;  region containing encrypted text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pgp-on-region (begin end passwd options userids &optional keep)
  "Execute a pgp command on a region.
Arguments are BEGIN, END, PASSWD, OPTIONS, USERIDS.
BEGIN and END are the boundaries of the region.
PASSWD should be non-nil if signing or decrypting the region.
OPTIONS should at least be -f for filter mode.
USERIDS should be non-nil if encrypting the region using public keys.
If KEEP is non-nil, don't replace the region with the new text,
just leave it as it was (for signature verification)."
  (let* ((pgp-buf (get-buffer-create "*pgp-output*"))
         (env (if passwd "env PGPPASSFD=0 "))
         (errfile (make-temp-name (concat "/tmp/pgp." (user-login-name) ".")))
         (stderr (concat " 2>" errfile))
         (this-buf (current-buffer))
         (errbuf (get-buffer-create "*pgp-errors*"))
         this-buf-size pgp-errors pgp-signed
         )

    (set-buffer pgp-buf)
    (widen)  (erase-buffer)
    (if passwd
        (insert passwd "\n"))

    (set-buffer this-buf)
    (append-to-buffer pgp-buf begin end)

    (set-buffer pgp-buf)
    (message "Calling PGP.  Just a moment...")
    (shell-command-on-region (point-min) (point-max)
                             (concat env "pgp " options " " userids stderr)
                             t)

    (setq this-buf-size (buffer-size))
    (set-buffer errbuf)  (widen)  (erase-buffer)
    (insert-file-contents errfile)
    (call-process "rm" nil nil nil "-f" errfile)
    (goto-char (point-min))
    (if (re-search-forward "\\(WARNING\\|ERROR\\)\\(.*\\)" nil t)
        (setq pgp-errors (buffer-substring (match-beginning 2) (match-end 2))))
    (goto-char (point-min))
    (if (re-search-forward "\\(Good signature from .*\\)" nil t)
        (setq pgp-signed (buffer-substring (match-beginning 1) (match-end 1))))

    (if (or (stringp pgp-errors)
            (= this-buf-size 0))
        (progn
          (display-buffer errbuf)
          (error "PGP failed %s" pgp-errors)))

    (if keep
        nil                                     ; don't display the buffer...
      (set-buffer this-buf)
      (goto-char begin)
      (kill-region begin end)
      (insert-buffer pgp-buf))

    (if pgp-signed
        (message pgp-signed)
      (message "Done!"))
    ))
