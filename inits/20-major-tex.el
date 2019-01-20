;;; 20-major-tex.el --- 設定 - メジャーモード - TeX -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019 Taku Watabe
;; Time-stamp: <2019-01-19T13:20:20+09:00>

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
