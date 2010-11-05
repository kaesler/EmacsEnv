;;!emacs
;;
;; FILE:         hash-test.el
;; SUMMARY:      Interactively test functions from hasht.el.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     extensions, maint, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Motorola Inc.
;;
;; ORIG-DATE:    16-Mar-90 at 03:38:48
;; LAST-MOD:     14-Apr-95 at 15:37:49 by Bob Weiner
;;
;; Copyright (C) 1990-1995  Free Software Foundation, Inc.
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;; DESCRIP-END.

(setq hs (hash-make '((("a1" "a2") . "a") (("b1" "b2") . "b") (("c1" "c2") . "c"))))
; => (hasht . [a b c])

(setq cpy (hash-copy hs))
; => (hasht . [a b c])
(eq hs cpy)
; => nil
(equal hs cpy)
; => t
(eq (hash-get "b" hs) (hash-get "b" cpy))
; => t

(setq deep-cpy (hash-deep-copy hs))
; => (hasht . [a b c])
(eq hs deep-cpy)
; => nil
(equal hs deep-cpy)
; => nil ;; Yes, this really should be nil since the symbols in the obarrays
         ;; are not equal for some reason.
(eq (hash-get "b" hs) (hash-get "b" deep-cpy))
; => nil

(hash-lookup "d" hs)
; => nil

(hash-add '("d1" "d2") "d" hs)
; => ("d1" "d2")

(hash-map 'car hs)
; => (("a1" "a2") ("d1" "d2") ("b1" "b2") ("c1" "c2"))

(hash-delete "d" hs)
; => d

(hash-map 'car hs)
; => (("a1" "a2") ("b1" "b2") ("c1" "c2"))

(hash-key-p "a" hs)
; => a

(hash-key-p "d" hs)
; => nil

(hash-lookup "a" hs)
; => ("a1" "a2")

(setq hs2 (hash-make '((("e1" "e2") . "e") (("f1" "f2") . "f"))))
; => (hasht . [f e])

(setq hs3 (hash-merge hs hs2))
; => (hasht . [0 e f b c])
(hash-lookup "e" hs3)
; => ("e1" "e2")
(hash-lookup "b" hs3)
; => ("b1" "b2")

(hash-make -3)
; => (error ("(hash-make): Initializer must be >= 0, not '-3'"))

(hash-replace '("e11" "e22") "e" hs3)
; => ("e11" "e22")
(hash-lookup "e" hs3)
; => ("e11" "e22")

(setq hs3 (hash-resize hs3 11))
; => (hasht . [0 0 a b c 0 e f 0 0 0])

(hash-lookup "c" hs3)
; => ("c1" "c2")
(hash-lookup "e" hs3)
; => ("e11" "e22")

(hash-count hs3)
; => 5

(hash-prin1 hs3 (current-buffer))
; => (
;     (("f1" "f2") . "f")
;     (("e11" "e22") . "e")
;     (("c1" "c2") . "c")
;     (("b1" "b2") . "b")
;     (("a1" "a2") . "a")
;      )









