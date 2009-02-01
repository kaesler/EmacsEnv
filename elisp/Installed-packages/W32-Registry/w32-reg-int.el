;;;  w32-reg-int.el -- Interface to the registry, using external process.
;;
;;  This is a temporary module that will allow the w32-registry.el
;; package to work without applying any patches to the Emacs source
;; code
;;
;;  (Actually this start off being temporary, but because the registry
;; work hasn't been accepted into the main source tree this could
;; continue being temporary for quite some time.  *grin* )
;;
;;  This interface module will provide the four functions that the
;; registry mode relies upon the most:
;;
;;   Enumerating Keys
;;   Enumerating Values
;;   Finding the Root Keys.
;;   Reading Values
;;

;;;  Limitations

;;  This interface is not yet complete - writing is not supported, thats
;; something that I'll get round to fixing some time in the near future.
;;

;;; Installing

;;  Installing this interface, and using it to access the registry should
;; be fairly simple:
;;
;;   No patching of the Emacs source code is necessary, simply copy the
;;  registry.exe, (Source included), to a directory on your PATH, and
;;  place the following line in your .emacs file:
;;
;;   ;; Load the registry interface
;;   (require 'w32-reg-int)
;;
;;   ;; Load the registry mode
;;   (require 'w32-registry)
;;

;;; Author

;;  Steve Kemp
;;  <skx@tardis.ed.ac.uk>
;;
;;  http://GNUSoftware.com/  -- GNU Software for Windows Users.
;;


(defvar w32-reg-interface-prog "registry.exe"
  "Location of the registry interface program.
If this program is not on your PATH then you should specify
its location completly here - or move it into a directory that
is contained on your path.")


(defun w32-reg-interface-get-root-keys ()
  "Return a list of the registry root keys."
  (interactive)
  (with-temp-buffer
    (let ((buffer (current-buffer))
	  (results nil)
	  (finished nil))
      (call-process w32-reg-interface-prog nil buffer nil
		    "--root")
      (goto-char 0)
      (while (not finished)
	(re-search-forward "^\\(.*\\)$")
	(if (equal "end" (match-string 1))
	    (setq finished t)
	  (add-to-list 'results  (match-string 1)))
	  )
      results)
    )
  )


(defun w32-reg-interface-enum-keys (key)
  "Return a list of named keys subkeys.
The key specified should be complete, for example
\"HKEY_CURRENT_CONFIG\\Software\\Microsoft\\Windows\\CurrentVersion\\Internet Settings\"
(The return key names are not sorted in any way.)"
  (interactive)
  (with-temp-buffer
    (let ((buffer (current-buffer))
	  (results nil)
	  (finished nil))
      (call-process w32-reg-interface-prog nil buffer nil
		    "--enum-keys" key)
      (goto-char 0)
      (while (not finished)
	(re-search-forward "^\\(.*\\)$")
	(if (equal "end" (match-string 1))
	    (setq finished t)
	  (add-to-list 'results  (match-string 1)))
	)
      results)
    )
  )

(defun w32-reg-interface-enum-values (key)
  "Return a list of named keys subkeys.
The key specified should be complete, for example
\"HKEY_CURRENT_CONFIG\\Software\\Microsoft\\Windows\\CurrentVersion\\Internet Settings\"
 (These value names are not sorted in any way.) "
  (interactive)
  (with-temp-buffer
    (let ((buffer (current-buffer))
	  (results nil)
	  (finished nil))
      (call-process w32-reg-interface-prog nil buffer nil
		    "--enum-values" key)
      (goto-char 0)
      (while (not finished)
	(re-search-forward "^\\(.*\\)$")
	(if (equal "end" (match-string 1))
	    (setq finished t)
	  (add-to-list 'results  (match-string 1)))
	)
      results)
    )
  )

(defun w32-reg-interface-read-value (key)
  "Read the value of the named key.
The key specified should be complete, for example
\"HKEY_CURRENT_CONFIG\\Software\\Microsoft\\Windows\\CurrentVersion\\Internet Settings\ProxyServer\""
  (interactive)
  (with-temp-buffer
    (let ((buffer (current-buffer))
	  (results nil)
	  (finished nil))
      (call-process w32-reg-interface-prog nil buffer nil
		    "--read-value" key)
      (goto-char 0)
      (while (and (not finished)
		  (re-search-forward "^\\(.*\\)\t\\(.*\\)$"))
	(if (equal "end" (match-string 1))
	    (setq finished t)
	  (setq results  (cons (match-string 1) (match-string 2))))
	)
      results
      )
    )
  )

(defun w32-reg-interface-query-value( key path )
  "Interface compatability function.
This function is a simple wrapper that mimics the form
of the read function provided by the registry patch."
  (interactive "sRoot\nsKey")
  (w32-reg-interface-read-value (concat key "\\" path)))


;; 
(provide 'w32-reg-int)


;;; Some tests, uncomment and use "M-x eval-print-last-sexp" to test..
;;
;;(w32-reg-interface-get-root-keys)
;; Should print : ("HKEY_CURRENT_CONFIG" "HKEY_USERS" "HKEY_CLASSES_ROOT" "HKEY_LOCAL_MACHINE" "HKEY_CURRENT_USER")

;; (
;;
;; (w32-reg-interface-enum-keys "HKEY_CURRENT_USER\\Software")

;; Something like : ("Xavier" "wnresqnt" "Windows Crawler" "Wang" "VDO" "VB and VBA Program Settings" "thirty4 interactive" "Tennyson Maxwell" "Systems Internals" "Smd" "SCC" "Q3Radiant" "Policies" "PASSWORD" "ODBC" "Numega" "Nico Mak Computing" "Netscape" "MSJ Bugslayer Column" "mozilla" "mlin" "Microsoft" "Logitech" "Local AppWizard-Generated Applications" "Lighttek" "LHI" "Left Side Software" "lcc" "Kahei" "JASC" "Intel" "InstallShield" "id" "HG Screen Savers" "GNU" "GL Saver" "FrontEnd Plus" "Freeware" "FreeAmp" "FerretSoft" "ES-Computing" "Ensoniq" "ediSys" "DMS Freeware" "Cygnus Solutions" "Classes" "c.igaly" "Aureate" "AndNow East" "AnalogX" "Adobe" "AccuImage Diagnostics Corporation")


;(w32-reg-interface-enum-keys "HKEY_CURRENT_USER\\Software\\MSJ Bugslayer Column\\CrashFinder")

;(w32-reg-interface-enum-values "HKEY_CURRENT_CONFIG\\Software\\Microsoft\\Windows\\CurrentVersion\\Internet Settings" )
;;(w32-reg-interface-read-value "HKEY_CURRENT_CONFIG\\Software\\Microsoft\\Windows\\CurrentVersion\\Internet Settings\\ProxyServer")
