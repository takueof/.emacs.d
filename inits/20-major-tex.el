;;; 20-major-tex.el --- 設定 - メジャーモード - TeX

;; Copyright (C) 2014-2015 Taku Watabe
;; Time-stamp: <2015-02-10T19:55:19+09:00>

;;; Commentary:

;;; Code:

;; ----------------------------------------------------------------------------
;; 初期化
;; ----------------------------------------------------------------------------
(defun my-tex-mode-initialize ()
  "Initialize `tex-mode' before file load."
  (setq-local truncate-lines nil))

(add-hook 'tex-mode-hook #'my-tex-mode-initialize)


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 20-major-tex.el ends here
