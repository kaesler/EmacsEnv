;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "flycheck-haskell" "20241119.1046"
  "Flycheck: Automatic Haskell configuration."
  '((emacs        "24.3")
    (flycheck     "0.25")
    (haskell-mode "13.7")
    (dash         "2.4.0")
    (seq          "1.11")
    (let-alist    "1.0.1"))
  :url "https://github.com/flycheck/flycheck-haskell"
  :commit "0977232112d02b9515e272ab85fe0eb9e07bbc50"
  :revdesc "0977232112d0"
  :keywords '("tools" "convenience")
  :authors '(("Sebastian Wiesner" . "swiesner@lunaryorn.com"))
  :maintainers '(("Sebastian Wiesner" . "swiesner@lunaryorn.com")))
