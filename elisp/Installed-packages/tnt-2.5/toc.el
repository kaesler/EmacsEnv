; -*- indent-tabs-mode: nil -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; TOC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Copyright (c) 1998 America Online, Inc. All Rights Reserved.
;;;;
;;;; AOL grants you ("Licensee") a non-exclusive, royalty free, license to
;;;; use, modify and redistribute this software in source and binary code
;;;; form, provided that i) this copyright notice and license appear on all
;;;; copies of the software; and ii) Licensee does not utilize the software
;;;; in a manner which is disparaging to AOL.
;;;; 
;;;; This software is provided "AS IS," without a warranty of any kind. ALL
;;;; EXPRESS OR IMPLIED CONDITIONS, REPRESENTATIONS AND WARRANTIES, INCLUDING
;;;; ANY IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE
;;;; OR NON-INFRINGEMENT, ARE HEREBY EXCLUDED. AOL AND ITS LICENSORS SHALL NOT
;;;; BE LIABLE FOR ANY DAMAGES SUFFERED BY LICENSEE AS A RESULT OF USING,
;;;; MODIFYING OR DISTRIBUTING THE SOFTWARE OR ITS DERIVATIVES. IN NO EVENT
;;;; WILL AOL OR ITS LICENSORS BE LIABLE FOR ANY LOST REVENUE, PROFIT OR DATA,
;;;; OR FOR DIRECT, INDIRECT, SPECIAL, CONSEQUENTIAL, INCIDENTAL OR PUNITIVE
;;;; DAMAGES, HOWEVER CAUSED AND REGARDLESS OF THE THEORY OF LIABILITY, ARISING
;;;; OUT OF THE USE OF OR INABILITY TO USE SOFTWARE, EVEN IF AOL HAS BEEN
;;;; ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
;;;; 
;;;; This software is not designed or intended for use in on-line control of
;;;; aircraft, air traffic, aircraft navigation or aircraft communications;
;;;; or in the design, construction, operation or maintenance of any nuclear
;;;; facility. Licensee represents and warrants that it will not use or
;;;; redistribute the Software for such purposes.

;;;; TODO:
;;;;   turn callbacks into hooks
;;;;   signon time overflows 24 bits


(provide 'toc)
(require 'tocstr)


;; Public hooks.  Multiple "subscribers" may use `add-hook' to get called
;; when the corrosponding protocol event occurs.  Until better documented,
;; see toc-handle-receive to learn what the signature of the hook is.

(defvar toc-opened-hooks nil)
(defvar toc-closed-hooks nil)
(defvar toc-sign-on-hooks nil)
(defvar toc-config-hooks nil)
(defvar toc-nick-hooks nil)
(defvar toc-im-in-hooks nil)
(defvar toc-update-buddy-hooks nil)
(defvar toc-error-hooks nil)
(defvar toc-eviled-hooks nil)
(defvar toc-chat-join-hooks nil)
(defvar toc-chat-in-hooks nil)
(defvar toc-chat-update-buddy-hooks nil)
(defvar toc-chat-invite-hooks nil)
(defvar toc-chat-left-hooks nil)
(defvar toc-goto-url-hooks nil)
(defvar toc-pause-hooks nil)


;;; Private State
(defvar toc-permit-mode nil
  "If non-nil, the toc server believes we are in permit mode.")

(defvar toc-permit-list nil
  "The permit/deny list that the server is maintaining for us.
Depending on toc-permit-mode, it is a permit or deny list.")

;;;----------------------------------------------------------------------------
;;; Public functions
;;;----------------------------------------------------------------------------
(defun toc-open (host port sname)
  (setq tocstr-opened-function  'toc-handle-opened
        tocstr-closed-function  'toc-handle-closed
        tocstr-receive-function 'toc-handle-receive
        toc-permit-mode nil
        toc-permit-list nil)
  (tocstr-open host port sname))

(defun toc-close ()
  (tocstr-close))


(defun toc-signon (host port username password language version)
  (tocstr-send (format "toc_signon %s %d %s %s %s %s"
                       host
                       port
                       (toc-normalize username)
                       (toc-roast password)
                       language
                       (toc-encode version))))

(defun toc-init-done ()
  (tocstr-send "toc_init_done"))

(defun toc-send-im (user message &optional auto)
  (tocstr-send (format "toc_send_im %s %s%s"
                       (toc-normalize user)
                       (toc-encode message)
                       (if auto " auto" ""))))

(defun toc-add-buddies (buddies)
  (if buddies
      (tocstr-send (format "toc_add_buddy %s"
                           (substring (format "%S" buddies) 1 -1)))))

(defun toc-remove-buddies (buddies)
  (if buddies
      (tocstr-send (format "toc_remove_buddy %s"
                           (substring (format "%S" buddies) 1 -1)))))

(defun toc-set-config (config)
  (tocstr-send (format "toc_set_config %s" (toc-encode config))))

(defun toc-evil (user anon)
  "Warn USER.  Do so anonymously if ANON"
  (tocstr-send
   (format "toc_evil %s %s" (toc-normalize user) (if anon "anon" "norm"))))

(defun toc-add-permit (&optional users)
  (if toc-permit-mode
      (setq toc-permit-list (append toc-permit-list users))
    (setq toc-permit-mode nil
          toc-permit-list users))
  (tocstr-send (mapconcat 'identity (cons "toc_add_permit" users) " ")))

(defun toc-add-deny (&optional users)
  (if (not toc-permit-mode)
      (setq toc-permit-list (append toc-permit-list users))
    (setq toc-permit-mode t
          toc-permit-list users))
  (tocstr-send (mapconcat 'identity (cons "toc_add_deny" users) " ")))

(defun toc-permit-all ()
  (if (or toc-permit-mode toc-permit-list)
      (progn
        (toc-add-permit)
        (toc-add-deny nil))))

(defun toc-deny-all ()
  (if (or (not toc-permit-mode) toc-permit-list)
      (progn
        (toc-add-deny)
        (toc-add-permit nil))))

;;; The next two could be optimized a bit.  If we happen to be in the right
;;; mode already, and BUDDIES is a superset of the current list, we should
;;; just add them.  This method means people on our permit list might see
;;; us "offline" for an instant just because we add someone to our permit
;;; list.  (or worse if in deny mode, someone could see us online for an
;;; instant).  Of course, clever users of the toc interface could be
;;; calling toc-add-permit/deny instead.

(defun toc-permit-only (buddies)
  (if (and toc-permit-mode (equal buddies toc-permit-list))
      ()
    (toc-add-deny)                      ; Ensure deny mode
    (toc-add-permit buddies)))          ; Permit only who we want

(defun toc-deny-only (buddies)
  (if (and (not toc-permit-mode) (equal buddies toc-permit-list))
      ()
  (toc-add-permit)                      ; Ensure permit mode
  (toc-add-deny buddies)))              ; Deny only who we want to exclude

(defun toc-chat-join (room)
  (tocstr-send (format "toc_chat_join 4 %s" (toc-encode room))))

(defun toc-chat-send (roomid message)
  (tocstr-send (format "toc_chat_send %s %s" roomid (toc-encode message))))

(defun toc-chat-whisper (roomid user message)
  (tocstr-send
   (format "toc_chat_whisper %s %s %s" roomid user (toc-encode message))))

(defun toc-chat-evil (roomid user anon)
  "Warn USER in ROOMID.  Do so anonymously if ANON."
  (tocstr-send (format "toc_chat_evil %s %s %s"
                       roomid (toc-normalize user) (if anon "anon" "norm"))))

(defun toc-chat-invite (roomid message buddies)
  (tocstr-send (format "toc_chat_invite %s %s %s" roomid (toc-encode message)
                       (mapconcat 'toc-normalize buddies " "))))

(defun toc-chat-leave (roomid)
  (tocstr-send (format "toc_chat_leave %s" roomid)))

(defun toc-chat-accept (roomid)
  (tocstr-send (format "toc_chat_accept %s" roomid)))

(defun toc-get-info (user)
  (tocstr-send (format "toc_get_info %s" (toc-normalize user))))

(defun toc-set-info (info)
  (tocstr-send (format "toc_set_info %s" (toc-encode info))))

(defun toc-set-idle (secs)
  (tocstr-send (format "toc_set_idle %d" secs)))


;; The following two are not well documented in PROTOCOL.
(defun toc-set-away (message)
  (if message
      (tocstr-send (concat "toc_set_away " (toc-encode message)))
    (tocstr-send "toc_set_away")))

(defun toc-keepalive ()
  "Send a keepalive packet to the server."
  (tocstr-send-flap 5 ""))


;;;----------------------------------------------------------------------------
;;; Handlers for tocstr events
;;;----------------------------------------------------------------------------

(defun toc-handle-opened ()
  (toc-run-hooks toc-opened-hooks))


(defun toc-handle-closed ()
  (toc-run-hooks toc-closed-hooks))


(defun toc-handle-receive (str)
  (let* ((index 0)
         (cmd (toc-lop-field str 'index)))
    (cond
     ((string= cmd "SIGN_ON")
      (let ((version (toc-lop-field str 'index)))
        (toc-run-hooks toc-sign-on-hooks version)))

     ((string= cmd "CONFIG")
      (let ((config (toc-lop-field str 'index)))
        (toc-run-hooks toc-config-hooks config)))

     ((string= cmd "NICK")
      (let ((nick (toc-lop-field str 'index)))
        (toc-run-hooks toc-nick-hooks nick)))

     ((string= cmd "IM_IN")
      (let ((user    (toc-lop-field str 'index))
            (auto    (string= "T" (toc-lop-field str 'index)))
            (message (substring str index)))
        (toc-run-hooks toc-im-in-hooks user auto message)))

     ((string= cmd "UPDATE_BUDDY")
      (let ((nick   (toc-lop-field str 'index))
            (online (string= "T" (toc-lop-field str 'index)))
            (evil   (string-to-number (toc-lop-field str 'index)))
            (signon (string-to-number (toc-lop-field str 'index)))
            (idle   (string-to-number (toc-lop-field str 'index)))
            (away (toc-lop-field str 'index)))
        (toc-run-hooks toc-update-buddy-hooks
                       nick online evil signon idle away)))

     ((string= cmd "ERROR")
      (let ((code (string-to-number (toc-lop-field str 'index)))
            (args nil)
            (arg  nil))
        (while (setq arg (toc-lop-field str 'index))
          (setq args (cons arg args)))
        (toc-run-hooks toc-error-hooks code (nreverse args))))

     ((string= cmd "EVILED")
      (let ((evil   (string-to-number (toc-lop-field str 'index)))
            (eviler (toc-lop-field str 'index)))
        (toc-run-hooks toc-eviled-hooks evil eviler)))

     ((string= cmd "CHAT_JOIN")
      (let ((roomid (toc-lop-field str 'index))
            (room   (toc-lop-field str 'index)))
        (toc-run-hooks toc-chat-join-hooks roomid room)))

     ((string= cmd "CHAT_IN")
      (let ((roomid  (toc-lop-field str 'index))
            (user    (toc-lop-field str 'index))
            (whisper (string= "T" (toc-lop-field str 'index)))
            (message (substring str index)))
        (toc-run-hooks toc-chat-in-hooks roomid user whisper message)))

     ((string= cmd "CHAT_UPDATE_BUDDY")
      (let ((roomid (toc-lop-field str 'index))
            (inside (string= "T" (toc-lop-field str 'index)))
            (users  (let (user (users nil))
                      (while (setq user (toc-lop-field str 'index))
                        (setq users (cons user users)))
                      users)))
        (toc-run-hooks toc-chat-update-buddy-hooks roomid inside users)))

     ((string= cmd "CHAT_INVITE")
      (let ((room    (toc-lop-field str 'index))
            (roomid  (toc-lop-field str 'index))
            (sender  (toc-lop-field str 'index))
            (message (substring str index)))
        (toc-run-hooks toc-chat-invite-hooks room roomid sender message)))

     ((string= cmd "CHAT_LEFT")
      (let ((roomid (toc-lop-field str 'index)))
        (toc-run-hooks toc-chat-left-hooks cmd roomid)))

     ((string= cmd "GOTO_URL")
      (let ((windowid (toc-lop-field str 'index))
            (url      (substring str index))) ; Url might have a colon?
        (toc-run-hooks toc-goto-url-hooks windowid url)))

     ;; We probably ought to handle this internally.  Does it ever really
     ;; get sent?
     ((string= cmd "PAUSE")
      (toc-run-hooks toc-pause-hooks cmd)))))


(defun toc-run-hooks (hooks &rest args)
  "Run each function in HOOKS with ARGS."
  (if (symbolp hooks)
      (if hooks (apply hooks args))
    (mapcar '(lambda (f) (apply f args)) hooks)))


;;;----------------------------------------------------------------------------
;;; String utilities
;;;----------------------------------------------------------------------------

(defun toc-lop-field (str index-var)
  ;; Returns the substring of STR that starts at the index given by the
  ;; value of INDEX-VAR and ends at the next colon.  Updates INDEX-VAR
  ;; to index of the character after colon.
  (let ((start-index (eval index-var)))
    (if (< start-index (length str))
        (let ((colon-index (or (string-match ":" str start-index)
                               (length str))))
          (set index-var (1+ colon-index))
          (substring str start-index colon-index)))))


(defun toc-roast (str)
  ;; Obfuscates password STR for network transmission.
  (let* ((roaster "Tic/Toc")
         (rstr "0x")
         (slen (length str))
         (rlen (length roaster))
         (i 0))
    (while (< i slen)
      (setq rstr (concat rstr
                         (format "%02x" (logxor (aref str i)
                                                (aref roaster (% i rlen))))))
      (setq i (1+ i)))
    rstr))

(defun toc-encode (str)
  ;; Encloses STR in quotes and backslashes special characters in it.
  (let ((list nil)
        (pos 0))
    (while (string-match "\\([^][{}()\\'\"$]*\\)\\([][{}()\\'\"$]\\)" str pos)
      (setq list (cons (substring str (match-beginning 1) (match-end 1)) list))
      (setq list (cons "\\" list))
      (setq list (cons (substring str (match-beginning 2) (match-end 2)) list))
      (setq pos (match-end 0)))
    (if (< pos (length str))
        (setq list (cons (substring str pos) list)))
    (apply 'concat "\"" (nreverse (cons "\"" list)))))

(defun toc-normalize (str)
  "Removes spaces and smashes STR to lowercase."
  (mapconcat '(lambda (char)
                (if (not (equal char ? ))
                    (char-to-string (downcase char))))
             str ""))
