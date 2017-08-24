;;; magithub-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "magithub-issue-tricks" "magithub-issue-tricks.el"
;;;;;;  (22939 11270 0 0))
;;; Generated autoloads from magithub-issue-tricks.el

(autoload 'magithub-pull-request-checkout "magithub-issue-tricks" "\
Checkout PULL-REQUEST as a local branch.

\(fn PULL-REQUEST)" t nil)

(autoload 'magithub-pull-request-merge "magithub-issue-tricks" "\
Merge PULL-REQUEST with ARGS.
See `magithub-pull-request--completing-read'.  If point is on a
pull-request object, that object is selected by default.

\(fn PULL-REQUEST &optional ARGS)" t nil)

;;;***

;;;### (autoloads nil nil ("magithub-ci.el" "magithub-core.el" "magithub-issue-post.el"
;;;;;;  "magithub-issue-status.el" "magithub-issue.el" "magithub-label.el"
;;;;;;  "magithub-orgs.el" "magithub-pkg.el" "magithub-pr.el" "magithub-proxy.el"
;;;;;;  "magithub.el") (22939 11270 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; magithub-autoloads.el ends here
