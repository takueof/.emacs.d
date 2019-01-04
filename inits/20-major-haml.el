;;; 20-major-haml.el --- 設定 - メジャーモード - Haml

;; Copyright (C) 2013-2015 Taku Watabe
;; Time-stamp: <2015-02-18T17:56:57+09:00>

;;; Commentary:

;;; Code:

(package-install 'haml-mode)


;; ----------------------------------------------------------------------------
;; 初期化
;; ----------------------------------------------------------------------------
(defun my-haml-mode-initialize ()
  "Initialize `haml-mode' before file load."
  (setq-local indent-tabs-mode nil))

(add-hook 'haml-mode-hook #'my-haml-mode-initialize)


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 20-major-haml.el ends here
