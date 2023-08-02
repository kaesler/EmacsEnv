;;; kaesar-file-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from kaesar-file.el

(autoload 'kaesar-file-encrypt "kaesar-file" "\
Encrypt a FILE by `kaesar-algorithm'
which contents can be decrypted by `kaesar-file-decrypt-contents'.

MODE: `binary', `base64-with-header', `base64' default is `binary'

(fn FILE &optional ALGORITHM MODE SAVE-FILE)")
(autoload 'kaesar-file-decrypt "kaesar-file" "\
Decrypt a FILE contents with getting string.
FILE was encrypted by `kaesar-file-encrypt'.

(fn FILE &optional ALGORITHM SAVE-FILE)")
(autoload 'kaesar-file-encrypt-region "kaesar-file" "\
Write START END region to FILE with encryption.
Warning: this function may be changed in future release.

(fn START END FILE &optional ALGORITHM CODING-SYSTEM MODE)" t)
(autoload 'kaesar-file-decrypt-contents "kaesar-file" "\
Get decrypted FILE contents.
FILE was encrypted by `kaesar-file-encrypt'.
Warning: this function may be changed in future release.

(fn FILE &optional ALGORITHM CODING-SYSTEM)")
(register-definition-prefixes "kaesar-file" '("kaesar-file--"))

;;; End of scraped data

(provide 'kaesar-file-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; kaesar-file-autoloads.el ends here
