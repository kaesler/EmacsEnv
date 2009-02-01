;;; network-util.el --- Network functions

;; Author:  Peter Breton
;; Created: Sun Mar 16 1997
;; Version: $Id: network-utils.el,v 1.1 1998/03/05 16:42:30 esler Exp esler $
;; Keywords: 
;; Time-stamp: <98/03/05 11:43:55 esler>

;;; Commentary:
;;
;; There are three main areas of functionality:
;; 
;; * Wrap common network utility programs (ping, traceroute, netstat,
;; nslookup)
;; 
;; * Implement some very basic protocols in Emacs Lisp (finger and whois)
;; 
;; * Support connections to HOST/PORT, generally for debugging and the like.
;; In other words, for doing much the same thing as "telnet HOST PORT", and
;; then typing commands.

;;; Change log:
;; $Log: network-utils.el,v $
;; Revision 1.1  1998/03/05 16:42:30  esler
;; Initial revision
;;
;; Revision 1.1  1998/03/05 11:31:28  pbreton
;; Initial revision
;;
;; Revision 1.1  1997/07/21 00:30:35  pbreton
;; Initial revision
;;

;;; Code:
(eval-when-compile
  (require 'comint))
(require 'ffap)
(require 'thingatpt) 

(defvar network-util-remove-ctl-m 
  (member system-type (list 'windows-nt 'msdos))
  "If non-nil, remove control-Ms from output.")

(defvar traceroute-program  
  (if (eq system-type 'windows-nt) 
      "Tracert"
    "Traceroute")
  "Program to trace network hops to a destination.")

(defvar ping-program  "ping"
  "Program to send network test packets to a host.")

(defvar ipconfig-program  
  (if (eq system-type 'windows-nt)
      "ipconfig"
    "ifconfig")
  "Program to print network configuration information.")

(defvar netstat-program  "netstat"
  "Program to print network statistics.")

(defvar nslookup-program  "nslookup"
  "Program to interactively query DNS information.")

(defvar nslookup-prompt-regexp "^> "
  "Regexp to match the nslookup prompt.")

(defvar nslookup-font-lock-keywords
  (list
   (list nslookup-prompt-regexp 0 font-lock-reference-face)
   (list "^[A-Za-z0-9 _]+:"     0 font-lock-type-face)
   (list "\\<\\(SOA\\|NS\\|MX\\|A\\|CNAME\\)\\>" 1 font-lock-keyword-face)
   ;; Dotted quads
   (list 
    (mapconcat 'identity
	       (make-list 4 "[0-9]+")
	       "\\.")
    0 font-lock-variable-name-face)
   ;; Host names
   (list 
    (let ((host-expression "[-A-Za-z0-9]+"))
      (concat 
       (mapconcat 'identity
		  (make-list 2 host-expression)
		  "\\.")
       "\\(\\." host-expression "\\)*")
       )
    0 font-lock-variable-name-face)
   )
  "Expressions to font-lock for nslookup.")

(defvar nslookup-abbrev-table (make-abbrev-table)
  "Abbrev table for nslookup.")

(define-abbrev nslookup-abbrev-table "e"   "exit")
(define-abbrev nslookup-abbrev-table "f"   "finger")
(define-abbrev nslookup-abbrev-table "h"   "help")
(define-abbrev nslookup-abbrev-table "lse" "lserver")
(define-abbrev nslookup-abbrev-table "r"   "root")
(define-abbrev nslookup-abbrev-table "s"   "set")
(define-abbrev nslookup-abbrev-table "se"  "server")
(define-abbrev nslookup-abbrev-table "v"   "viewer")

(defun network-util-remove-ctrl-m-filter (process output-string)
  "Remove trailing control Ms."
  (let ((old-buffer (current-buffer))
	(filtered-string output-string))
    (unwind-protect
	(let ((moving))
	  (set-buffer (process-buffer process))
	  (setq moving (= (point) (process-mark process)))
	  
	  (while (string-match "\r" filtered-string)
	       (setq filtered-string
		     (replace-match "" nil nil filtered-string)))

	  (save-excursion
	    ;; Insert the text, moving the process-marker.
	    (goto-char (process-mark process))
	    (insert filtered-string)
	    (set-marker (process-mark process) (point)))
	  (if moving (goto-char (process-mark process))))
      (set-buffer old-buffer))))
  
(defmacro network-util-run-program (name header program &rest args)
  "Run a network information program."
  (` 
   (let ((buf (get-buffer-create (concat "*" (, name) "*"))))
     (set-buffer buf)
     (erase-buffer)
     (insert (, header) "\n")
     (set-process-filter 
      (start-process (, name) buf (, program) (,@ args) )
      'network-util-remove-ctrl-m-filter)
     (display-buffer buf))))

(defun traceroute (target)
  "Run traceroute program for TARGET."
  (interactive "sTarget: ")
  (network-util-run-program
   (concat "Traceroute" " " target)
   (concat "** Traceroute ** " traceroute-program " ** " target)
   traceroute-program
   target))

(defun ping (host)
  "Ping HOST."
  (interactive 
   (list
    (progn
      (read-from-minibuffer 
       "Ping host: " 
       (or (ffap-string-at-point 'machine) "")))))
  (network-util-run-program
   (concat "Ping" " " host)
   (concat "** Ping ** " ping-program " ** " host)
   ping-program
   host))

(defun ipconfig ()
  "Run ipconfig program."
  (interactive)
  (network-util-run-program
   "Ipconfig"
   (concat "** Ipconfig ** " ipconfig-program " ** ")
   ipconfig-program
   (if (eq system-type 'windows-nt)
       "/all" "-a")
   ))

(defun netstat ()
  "Run netstat program."
  (interactive)
  (network-util-run-program
   "Netstat"
   (concat "** Netstat ** " netstat-program " ** ")
   netstat-program
   ))

;; FIXME -- Needs to be a process filter
(defun netstat-with-filter (filter)
  "Run netstat program."
  (interactive "sFilter: ")
  (netstat)
  (set-buffer (get-buffer "*Netstat*"))
  (goto-char (point-min))
  (delete-matching-lines filter)
  )

(defun nslookup ()
  "Run nslookup program."
  (interactive)
  (comint-run nslookup-program)
  (set-process-filter (get-buffer-process "*nslookup*")
   'network-util-remove-ctrl-m-filter)
  (set 
   (make-local-variable 'font-lock-defaults)
   '((nslookup-font-lock-keywords)))
  (set 
   (make-local-variable 'local-abbrev-table)
   nslookup-abbrev-table)
  (abbrev-mode t)
  (make-local-variable 'comint-prompt-regexp)
  (setq comint-prompt-regexp nslookup-prompt-regexp)
  )

;; Full list is available at:
;; ftp://ftp.isi.edu/in-notes/iana/assignments/port-numbers
(defvar network-connection-service-alist 
  (list
    (cons 'echo          7)
    (cons 'active-users 11)
    (cons 'daytime      13)
    (cons 'chargen      19)
    (cons 'ftp          21)
    (cons 'telnet	23)
    (cons 'smtp		25)
    (cons 'time		37)
    (cons 'whois        43)
    (cons 'gopher       70)
    (cons 'finger       79)
    (cons 'www		80)
    (cons 'pop2		109)
    (cons 'pop3		110)
    (cons 'sun-rpc	111)
    (cons 'nntp		119)
    (cons 'ntp		123)
    (cons 'netbios-name 137)
    (cons 'netbios-data 139)
    (cons 'irc		194)
    (cons 'https	443)
    (cons 'rlogin	513)
    )
  "Alist of services and associated TCP port numbers.
This list in not complete.")

;; Workhorse macro
(defmacro run-network-program (process-name host port 
					    &optional initial-string)
  (`
   (let ((tcp-connection)
	 (buf)
	 )
    (setq buf (get-buffer-create (concat "*" (, process-name) "*")))
    (set-buffer buf)
    (or 
     (setq tcp-connection
	   (open-network-stream 
	    (, process-name)
	    buf
	    (, host)
	    (, port)
	    ))
     (error "Could not open connection to %s" (, host)))
    (erase-buffer)
    (set-marker (process-mark tcp-connection) (point-min))
    (set-process-filter tcp-connection 'network-util-remove-ctrl-m-filter)
    (and (, initial-string)
	 (process-send-string tcp-connection 
			      (concat (, initial-string) "\r\n")))
    (display-buffer buf))))

;; Finger protocol
(defun finger (user host)
  "Finger USER on HOST."
  (interactive
    (progn
      (list
       (read-from-minibuffer "Finger User: " (word-at-point))
       (read-from-minibuffer 
	"At Host: " 
	(or (ffap-string-at-point 'machine) "")))))
    (let* (
	 (user-and-host (concat user "@" host))
	 (process-name 
	  (concat "Finger [" user-and-host "]"))
	 )
    (run-network-program 
     process-name 
     host 
     (cdr (assoc 'finger network-connection-service-alist))
     user-and-host
     )))

(defvar whois-server-name "whois.internic.net"
  "Host name for the whois service.")

;; Whois protocol
(defun whois (arg search-string)
  "Send SEARCH-STRING to server defined by the `whois-server-name' variable.
With argument, prompt for whois server."
  (interactive "P\nsWhois: ")
  (let ((host 
	 (if arg
	     (read-from-minibuffer "Whois server name: ")
	   whois-server-name))
	)
    (run-network-program 
     "Whois"
     host
     (cdr (assoc 'whois network-connection-service-alist))
     search-string
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General Network connection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun network-connection-to-service (host service)
  ""
  (interactive 
   (list
    (progn
      (read-from-minibuffer "Host: " 
			    (ffap-string-at-point 'machine)))
    (completing-read "Service: " 
		     (mapcar 
		      (function 
		       (lambda (elt)
			 (list (symbol-name (car elt)))))
		      network-connection-service-alist))))
  (network-connection 
   host 
   (cdr (assoc (intern service) network-connection-service-alist)))
  )

(defun network-service-connection (host service)
  "Open a network connection to PORT on HOST"
  (interactive 
   (list
    (progn
      (read-from-minibuffer "Host: " 
			    (ffap-string-at-point 'machine)))
    (read-from-minibuffer "Port or Service: ")))
  (let (
	(process-name (concat "Network Connection [" host " " service "]"))
	(portnum (string-to-number service))
	)
    (or (zerop portnum) (setq service portnum))
    (make-comint 
     process-name
     (cons host service))
    (pop-to-buffer (get-buffer (concat "*" process-name "*")))
    ))

(defun network-connection (host port)
  "Open a network connection to PORT on HOST"
  (interactive "sHost: \nnPort: ")
  (network-service-connection host (number-to-string port)))

(provide 'network-util)

;;; network-util.el ends here
