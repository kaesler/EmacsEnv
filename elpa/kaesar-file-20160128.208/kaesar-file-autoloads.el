;;; kaesar-file-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "kaesar-file" "kaesar-file.el" (22275 52520
;;;;;;  0 0))
;;; Generated autoloads from kaesar-file.el

(autoload 'kaesar-encrypt-file "kaesar-file" "\
Encrypt a FILE by `kaesar-algorithm'
which contents can be decrypted by `kaesar-decrypt-file-contents'.

MODE: `binary', `base64-with-header', `base64' default is `binary'

\(fn FILE &optional ALGORITHM MODE SAVE-FILE)" nil nil)

(autoload 'kaesar-decrypt-file "kaesar-file" "\
Decrypt a FILE contents with getting string.
FILE was encrypted by `kaesar-encrypt-file'.

\(fn FILE &optional ALGORITHM SAVE-FILE)" nil nil)

(autoload 'kaesar-encrypt-write-region "kaesar-file" "\
Write START END region to FILE with encryption.
Warning: this function may be changed in future release.

\(fn START END FILE &optional ALGORITHM CODING-SYSTEM MODE)" t nil)

(autoload 'kaesar-decrypt-file-contents "kaesar-file" "\
Get decrypted FILE contents.
FILE was encrypted by `kaesar-encrypt-file'.
Warning: this function may be changed in future release.

\(fn FILE &optional ALGORITHM CODING-SYSTEM)" nil nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; kaesar-file-autoloads.el ends here
