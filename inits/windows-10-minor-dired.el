;;; windows-10-minor-dired.el --- 設定 - Windows - マイナーモード - `dired'

;; Copyright (C) 2015 Taku Watabe
;; Time-stamp: <2015-02-13T00:37:06+09:00>

;;; Commentary:

;;; Code:

(custom-set-variables
 ;; PATH は通っていないが、`exec-path' は通っている場合を想定
 '(find-ls-option (cons (format "-exec %s -ld {} %s"
                                (executable-find "ls")
                                find-exec-terminator)
                        "-ld")))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; windows-10-minor-dired.el ends here
